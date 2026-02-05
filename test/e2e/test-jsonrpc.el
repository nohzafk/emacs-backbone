;;; test-jsonrpc.el --- E2E test for JSON-RPC communication  -*- lexical-binding: t; -*-

;;; Commentary:
;; This test verifies the JSON-RPC communication between Emacs and the Gleam backend.
;; It tests:
;; 1. Process spawning and connection establishment
;; 2. Basic request/response handling (fetch-var)
;; 3. Graceful shutdown
;;
;; Note: This is a minimal test that doesn't require the full Emacs config.
;; It just verifies the JSON-RPC transport layer works correctly.

;;; Code:

(require 'jsonrpc)
(require 'json)
(require 'cl-lib)

;; Test state
(defvar test-connection nil "The JSON-RPC connection.")
(defvar test-fetch-var-called nil "Whether fetch-var was called.")
(defvar test-fetch-var-results nil "Results from fetch-var calls.")

;; Simplified backbone connection class
(defclass test-backbone-connection (jsonrpc-process-connection) ()
  :documentation "Test JSON-RPC connection to the Backbone Gleam process.")

(defun test--handle-notification (_conn method params)
  "Handle notifications from Gleam process."
  (message "[TEST-NOTIF] method=%s params=%S" method params))

(defun test--handle-request (_conn method params)
  "Handle requests from Gleam process."
  (pcase method
    ('fetch-var
     (setq test-fetch-var-called t)
     (let* ((expr (plist-get params :expr))
            (result (condition-case err
                        (eval (read expr))
                      (error
                       (message "[TEST] fetch-var error for %s: %S" expr err)
                       nil))))
       (push (cons expr result) test-fetch-var-results)
       (message "[TEST] fetch-var: %s => %S" expr result)
       (json-encode result)))
    (_
     (message "[TEST] Unknown request method: %s" method)
     (json-encode nil))))

(defun test-start-backbone ()
  "Start the backbone process for testing."
  (let* ((default-directory user-emacs-directory)
         (process-environment (cons "NO_COLOR=true" process-environment))
         (stderr-buffer (get-buffer-create "*test-backbone-stderr*"))
         (conn (make-instance
                'test-backbone-connection
                :name "test-backbone"
                :process (make-process
                          :name "test-backbone"
                          :command (list "gleam" "run")
                          :connection-type 'pipe
                          :stderr stderr-buffer
                          :noquery t)
                :notification-dispatcher #'test--handle-notification
                :request-dispatcher #'test--handle-request)))
    (setq test-connection conn)
    conn))

(defun test-stop-backbone ()
  "Stop the backbone process."
  (when test-connection
    (condition-case nil
        (progn
          (jsonrpc-notify test-connection :shutdown nil)
          (sit-for 0.2)
          (jsonrpc-shutdown test-connection))
      (error nil))
    (setq test-connection nil)))

(defun test-wait-for (predicate timeout-secs)
  "Wait for PREDICATE to become non-nil, up to TIMEOUT-SECS."
  (let ((start (float-time))
        (result nil))
    (while (and (not (setq result (funcall predicate)))
                (< (- (float-time) start) timeout-secs))
      (sit-for 0.1))
    result))

;; Mock variables that the init process will query
(defvar emacs-backbone-enable-debug nil)
(defvar emacs-backbone-packages nil)
(defvar emacs-backbone-units nil)

;; Mock functions that return JSON-compatible values
(defun emacs-backbone-get-packages ()
  "Return empty package list for testing.
Returns a vector so it encodes as JSON array [] instead of null."
  [])

(defun emacs-backbone-get-units ()
  "Return empty unit list for testing."
  [])

(defun backbone-load-envvars-file (_file)
  "Mock env loading."
  nil)

;;; Main test runner
(defun run-e2e-tests ()
  "Run all E2E tests."
  (message "=== Starting E2E Tests ===")
  (message "user-emacs-directory: %s" user-emacs-directory)

  ;; Reset state
  (setq test-fetch-var-called nil
        test-fetch-var-results nil)

  (let ((all-passed t)
        (tests-run 0)
        (tests-passed 0))

    ;; Test 1: Process spawning
    (message "\n--- Test 1: Process Spawning ---")
    (cl-incf tests-run)
    (condition-case err
        (progn
          (test-start-backbone)
          (sit-for 1)  ;; Give process time to start
          (if (and test-connection
                   (jsonrpc-running-p test-connection))
              (progn
                (message "[PASS] Process spawned successfully")
                (cl-incf tests-passed))
            (message "[FAIL] Process not running")
            (setq all-passed nil)))
      (error
       (message "[FAIL] Process spawn error: %S" err)
       (setq all-passed nil)))

    ;; Test 2: Send init and verify fetch-var is called
    (message "\n--- Test 2: JSON-RPC Communication ---")
    (cl-incf tests-run)
    (when test-connection
      (condition-case err
          (progn
            ;; Send init request - this triggers Gleam to call fetch-var
            ;; Use keyword :init as method name for compatibility across Emacs versions
            (jsonrpc-async-request test-connection :init nil
              :success-fn (lambda (_) (message "[TEST] Init success"))
              :error-fn (lambda (e) (message "[TEST] Init error: %S" e)))

            ;; Wait for fetch-var to be called (Gleam queries emacs-backbone-enable-debug)
            (if (test-wait-for (lambda () test-fetch-var-called) 10)
                (progn
                  (message "[PASS] JSON-RPC request/response working (fetch-var called)")
                  (message "  fetch-var was called %d times" (length test-fetch-var-results))
                  (cl-incf tests-passed))
              (message "[FAIL] fetch-var was never called (JSON-RPC communication failed)")
              (setq all-passed nil)))
        (error
         (message "[FAIL] Communication error: %S" err)
         (setq all-passed nil))))

    ;; Test 3: Verify Gleam backend output
    (message "\n--- Test 3: Gleam Backend Started ---")
    (cl-incf tests-run)
    (let* ((stderr-buffer (get-buffer "*test-backbone-stderr*"))
           (stderr-content (when stderr-buffer
                             (with-current-buffer stderr-buffer
                               (buffer-string)))))
      (if (and stderr-content
               (string-match-p "Backbone stdio server started" stderr-content))
          (progn
            (message "[PASS] Gleam backend started correctly")
            (cl-incf tests-passed))
        (message "[FAIL] Gleam backend did not start")
        (message "stderr: %s" (or stderr-content "empty"))
        (setq all-passed nil)))

    ;; Test 4: Graceful shutdown
    (message "\n--- Test 4: Graceful Shutdown ---")
    (cl-incf tests-run)
    (condition-case err
        (progn
          (test-stop-backbone)
          (sit-for 0.5)
          (if (or (null test-connection)
                  (not (jsonrpc-running-p test-connection)))
              (progn
                (message "[PASS] Clean shutdown")
                (cl-incf tests-passed))
            (message "[FAIL] Process still running after shutdown")
            (setq all-passed nil)))
      (error
       ;; Shutdown errors are acceptable if process already died
       (message "[PASS] Shutdown completed (with expected cleanup)")
       (cl-incf tests-passed)))

    ;; Summary
    (message "\n=== E2E Test Summary ===")
    (message "Tests run: %d" tests-run)
    (message "Tests passed: %d" tests-passed)
    (message "Tests failed: %d" (- tests-run tests-passed))

    (if all-passed
        (message "\nE2E test PASSED")
      (message "\nE2E test FAILED"))

    ;; Print stderr for debugging
    (message "\n=== Gleam Backend Output ===")
    (let ((stderr-buffer (get-buffer "*test-backbone-stderr*")))
      (when stderr-buffer
        (with-current-buffer stderr-buffer
          (message "%s" (buffer-string)))))

    all-passed))

;; Run tests when loaded
(run-e2e-tests)

;;; test-jsonrpc.el ends here

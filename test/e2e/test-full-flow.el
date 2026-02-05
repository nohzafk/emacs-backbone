;;; test-full-flow.el --- Full E2E test with package installation  -*- lexical-binding: t; -*-

;;; Commentary:
;; This test exercises the complete backbone initialization flow:
;; 1. Load compose.el and macros
;; 2. Load test config with package! and config-unit! declarations
;; 3. Start backbone and wait for full initialization
;; 4. Verify packages were installed and config units executed

;;; Code:

(require 'cl-lib)

;; Setup paths
(defvar test-project-root
  (or (getenv "PROJECT_ROOT")
      (expand-file-name "../../../" (file-name-directory load-file-name))))

(setq user-emacs-directory test-project-root)

;; Create temp directory for elpaca
(defvar test-elpaca-dir (make-temp-file "elpaca-test" t))
(setq elpaca-directory test-elpaca-dir)

;; Load compose.el (includes all macros)
(message "Loading compose.el from %s" user-emacs-directory)
(load-file (expand-file-name "lisp/compose.el" user-emacs-directory))

;; Load the test config
(message "Loading test config...")
(load-file (expand-file-name "test/e2e/fixtures/test-config.el" user-emacs-directory))

;; Verify macros registered packages and units
(message "\n=== Pre-Init Check ===")
(message "Packages registered: %d" (length emacs-backbone-packages))
(message "Units registered: %d" (length emacs-backbone-units))

(dolist (pkg emacs-backbone-packages)
  (message "  Package: %s" (plist-get pkg :name)))
(dolist (unit emacs-backbone-units)
  (message "  Unit: %s" (plist-get unit :name)))

;; Track test state
(defvar test-init-complete nil)
(defvar test-all-passed t)
(defvar test-results '())

(defun test-add-result (name passed &optional message)
  "Record test result."
  (push (list :name name :passed passed :message message) test-results)
  (message "[%s] %s%s"
           (if passed "PASS" "FAIL")
           name
           (if message (format " - %s" message) "")))

;; Start backbone
(message "\n=== Starting Backbone ===")
(emacs-backbone-start)

;; Wait for initialization (with timeout)
(message "Waiting for initialization (max 120 seconds)...")
(let ((start-time (float-time))
      (timeout 120))
  (while (and (not emacs-backbone--initialized)
              (< (- (float-time) start-time) timeout))
    (sit-for 1)
    (when (= 0 (mod (truncate (- (float-time) start-time)) 10))
      (message "  Still waiting... %d seconds elapsed"
               (truncate (- (float-time) start-time)))))

  (if emacs-backbone--initialized
      (progn
        (message "Initialization complete in %.1f seconds"
                 (- (float-time) start-time))
        (setq test-init-complete t))
    (message "Initialization timed out after %d seconds" timeout)))

;; Run verification tests
(message "\n=== Running Verification Tests ===")

;; Test 1: Backbone initialized
(test-add-result "backbone-initialized" test-init-complete)
(unless test-init-complete
  (setq test-all-passed nil))

;; Test 2: Config units loaded (check markers)
(when test-init-complete
  ;; Give units a moment to execute
  (sit-for 2)

  (let ((unit-1-ok (and (boundp 'test-unit-1-loaded) test-unit-1-loaded))
        (unit-2-ok (and (boundp 'test-unit-2-loaded) test-unit-2-loaded))
        (unit-3-ok (and (boundp 'test-unit-3-loaded) test-unit-3-loaded)))

    (test-add-result "test-unit-1-loaded" unit-1-ok)
    (test-add-result "test-unit-2-loaded" unit-2-ok)
    (test-add-result "test-unit-3-loaded" unit-3-ok)

    (unless unit-1-ok (setq test-all-passed nil))
    (unless unit-2-ok (setq test-all-passed nil))
    (unless unit-3-ok (setq test-all-passed nil))))

;; Test 3: Packages available (check if f library is loadable)
(when test-init-complete
  (let ((f-available (featurep 'f)))
    (test-add-result "f-package-available" f-available
                     (if f-available "f library loaded" "f library not found"))
    (unless f-available (setq test-all-passed nil))))

;; Cleanup
(message "\n=== Cleanup ===")
(condition-case nil
    (emacs-backbone-exit)
  (error nil))
(message "Backbone stopped")

;; Delete temp elpaca directory
(when (and test-elpaca-dir (file-directory-p test-elpaca-dir))
  (delete-directory test-elpaca-dir t)
  (message "Cleaned up temp elpaca directory"))

;; Summary
(message "\n=== Test Summary ===")
(let ((passed (cl-count-if (lambda (r) (plist-get r :passed)) test-results))
      (total (length test-results)))
  (message "Tests: %d/%d passed" passed total)

  (if test-all-passed
      (message "\nFull flow E2E test PASSED")
    (message "\nFull flow E2E test FAILED")
    ;; Print failures
    (dolist (r test-results)
      (unless (plist-get r :passed)
        (message "  FAILED: %s" (plist-get r :name))))))

;; Print backend output for debugging
(message "\n=== Gleam Backend Output ===")
(let ((stderr-buffer (get-buffer "*emacs-backbone-stderr*")))
  (when stderr-buffer
    (with-current-buffer stderr-buffer
      (message "%s" (buffer-string)))))

;;; test-full-flow.el ends here

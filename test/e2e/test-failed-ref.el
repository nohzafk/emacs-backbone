;;; test-failed-ref.el --- E2E test for failed repo ref handling  -*- lexical-binding: t; -*-

;;; Commentary:
;; This test verifies that a failed package ref does not hang the pipeline.
;; It expects config units to run even when a package fails to install.

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

;; Use a short no-progress timeout for the test
(setq emacs-backbone-package-timeout-seconds 5)

;; Load the test config
(message "Loading failed-ref test config...")
(load-file (expand-file-name "test/e2e/fixtures/test-config-failed-ref.el" user-emacs-directory))

;; Track test state
(defvar test-all-passed t)
(defvar test-results '())

(defun test-add-result (name passed &optional message)
  "Record test result."
  (push (list :name name :passed passed :message message) test-results)
  (message "[%s] %s%s"
           (if passed "PASS" "FAIL")
           name
           (if message (format " - %s" message) "")))

(defun test-wait-for (predicate timeout-secs)
  "Wait for PREDICATE to become non-nil, up to TIMEOUT-SECS."
  (let ((start (float-time))
        (result nil))
    (while (and (not (setq result (funcall predicate)))
                (< (- (float-time) start) timeout-secs))
      (sit-for 0.5))
    result))

;; Start backbone
(message "\n=== Starting Backbone (failed ref) ===")
(emacs-backbone-start)

;; Wait for independent unit to load (indicates config phase ran)
(message "Waiting for independent unit (max 60 seconds)...")
(let ((unit-loaded (test-wait-for (lambda ()
                                    (and (boundp 'test-independent-unit-loaded)
                                         test-independent-unit-loaded))
                                  60)))
  (test-add-result "independent-unit-loaded" unit-loaded)
  (unless unit-loaded
    (setq test-all-passed nil)))

;; Failed-ref unit should not load
(let ((failed-unit-loaded (and (boundp 'test-failed-ref-unit-loaded)
                               test-failed-ref-unit-loaded)))
  (test-add-result "failed-ref-unit-skipped" (not failed-unit-loaded)
                   (if failed-unit-loaded "unexpectedly loaded" "skipped as expected"))
  (when failed-unit-loaded
    (setq test-all-passed nil)))

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
      (message "\nFailed ref E2E test PASSED")
    (message "\nFailed ref E2E test FAILED")
    (dolist (r test-results)
      (unless (plist-get r :passed)
        (message "  FAILED: %s" (plist-get r :name))))))

;;; test-failed-ref.el ends here

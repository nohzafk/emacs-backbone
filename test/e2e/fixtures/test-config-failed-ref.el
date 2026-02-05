;;; test-config-failed-ref.el --- Config with a bad repo ref  -*- lexical-binding: t; -*-

;;; Commentary:
;; This config intentionally uses a bad git ref to exercise failure handling.

;;; Code:

;; Package with an invalid branch reference (should fail to install)
(package! f
  :repo "rejeep/f.el"
  :branch "this-branch-does-not-exist")

(config-unit! test-failed-ref-unit
  :requires (f)
  :config
  (defvar test-failed-ref-unit-loaded t "Marker that failed-ref unit loaded."))

(config-unit! test-independent-unit
  :config
  (defvar test-independent-unit-loaded t "Marker that independent unit loaded."))

;;; test-config-failed-ref.el ends here

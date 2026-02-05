;;; test-config.el --- Minimal test configuration for E2E testing  -*- lexical-binding: t; -*-

;;; Commentary:
;; This is a minimal config.el that exercises the full backbone initialization flow:
;; 1. Package declarations with package! macro
;; 2. Config unit declarations with config-unit! macro
;; 3. Dependencies between packages and units

;;; Code:

;; Declare a few small packages (these are real packages that install quickly)
(package! dash)
(package! s)
(package! f :deps (dash s))  ; f depends on dash and s

;; Declare config units
(config-unit! test-unit-1
  :config
  (defvar test-unit-1-loaded t "Marker that unit 1 loaded."))

(config-unit! test-unit-2
  :after (test-unit-1)
  :config
  (defvar test-unit-2-loaded t "Marker that unit 2 loaded."))

(config-unit! test-unit-3
  :requires (f)
  :after (test-unit-2)
  :config
  (defvar test-unit-3-loaded t "Marker that unit 3 loaded.")
  ;; Verify f is available
  (when (fboundp 'f-exists-p)
    (defvar test-f-available t "Marker that f library is loaded.")))

;;; test-config.el ends here

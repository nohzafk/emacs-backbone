;; -*- lexical-binding: t -*-
;; unit-macro.el - Macros for config unit declaration in Emacs Backbone

;; Global variable to store config units information
(defvar emacs-backbone-units '()
  "Global storage for config unit definitions used by the Backbone system.")

(defconst emacs-backbone--anti-pattern-forms '(with-eval-after-load use-package)
  "Forms that should not appear in config-unit! :config bodies.
These defeat backbone's deterministic loading model.")

(defun emacs-backbone--check-anti-patterns (forms unit-name)
  "Walk FORMS recursively, warning if any anti-pattern is found in UNIT-NAME."
  (dolist (form forms)
    (when (consp form)
      (when (memq (car form) emacs-backbone--anti-pattern-forms)
        (display-warning 'emacs-backbone
                         (format "config-unit! '%s': `%s' is an anti-pattern in backbone config. Use :requires/:after instead."
                                 unit-name (car form))
                         :warning))
      (emacs-backbone--check-anti-patterns (cdr form) unit-name))))

(defun emacs-backbone-reset-units ()
  "Reset the global units list."
  (setq emacs-backbone-units '()))

(defmacro config-unit! (name &rest args)
  "Define a configuration unit for the emacs-backbone system.

A config unit is a named, self-contained piece of Emacs configuration with
dependencies and execution conditions.

Arguments:
  NAME    Symbol that identifies this configuration unit.
  ARGS    Property list of attributes and configuration.

Required properties:
  :config       All forms following this keyword constitute the configuration
                body to be executed when this unit is loaded.
                The :config section should be the last property in the unit.

Optional properties:
  :requires     Symbol or list of symbols representing the features this unit
                depends on. The unit will only execute when these features are
                available.
                Example: :requires (avy) or :requires magit

  :after        Symbol or list of symbols representing other units that must be
                loaded before this one. Creates a dependency graph for initialization.
                Example: :after (helm ivy) or :after org

  :env          Symbol, string, or list of them naming environment variables
                that must be set (non-empty) for this unit to run. If any is
                missing the unit fails and downstream units (via :after) are
                skipped.
                Example: :env API_KEY or :env (EDITOR LANG)

  :executable   Symbol, string, or list of them naming executables that must
                be found on PATH (via `executable-find'). If any is missing
                the unit fails and downstream units are skipped.
                Example: :executable git or :executable (git rg)

The macro registers the unit with the `emacs-backbone-units' list for
processing by the backbone system.

Examples:

    (config-unit! avy-config
      :requires avy
      :config
      (setq avy-background nil)
      (global-set-key (kbd \"C-:\") 'avy-goto-char))

    (config-unit! org-config
      :requires org
      :after (avy-config)
      :config
      (setq org-directory \"~/org\")
      (setq org-agenda-files '(\"~/org/agenda.org\")))

    (config-unit! magit-config
      :requires magit
      :executable git
      :config
      (global-set-key (kbd \"C-x g\") 'magit-status))

Units are executed only when their required features are available, their env
vars are set, their executables are on PATH, and in dependency order based on
the :after specifications."
  (declare (indent defun))
  
  (let* ((config-index (cl-position :config args))
         (unit-name (symbol-name name)))

    ;; Verify :config is present
    (unless config-index
      (error "config-unit! %s: Missing :config keyword" unit-name))

    ;; Extract plist before :config
    (let* ((plist-pairs (seq-subseq args 0 config-index))
           ;; Extract forms after :config
           (body (seq-subseq args (1+ config-index)))
           ;; Get the various dependency specifications
           (requires-expr (plist-get plist-pairs :requires))
           (afters-expr (plist-get plist-pairs :after))
           (env-expr (plist-get plist-pairs :env))
           (executable-expr (plist-get plist-pairs :executable))
           ;; Check for anti-patterns before stringifying
           (_ (when body
                (emacs-backbone--check-anti-patterns body unit-name)))
           ;; Convert body forms to a single string
           (body-string (when body
                          (format "%S" `(progn ,@body t)))))

      ;; Normalize single symbols to lists
      (when (and requires-expr (symbolp requires-expr))
        (setq requires-expr (list requires-expr)))

      (when (and afters-expr (symbolp afters-expr))
        (setq afters-expr (list afters-expr)))

      (when (and env-expr (or (symbolp env-expr) (stringp env-expr)))
        (setq env-expr (list env-expr)))

      (when (and executable-expr
                 (or (symbolp executable-expr) (stringp executable-expr)))
        (setq executable-expr (list executable-expr)))

      `(let ((unit (list :name ,unit-name)))
         ;; Process requires
         (setq unit (plist-put unit :requires
                              (mapcar (lambda (x) (if (stringp x) x (symbol-name x)))
                                      ',requires-expr)))

         ;; Process after dependencies - only add if present
         ,(when afters-expr
            `(setq unit (plist-put unit :after
                                  (mapcar (lambda (x) (if (stringp x) x (symbol-name x)))
                                          ',afters-expr))))

         ;; Process env dependencies - only add if present
         ,(when env-expr
            `(setq unit (plist-put unit :env
                                  (mapcar (lambda (x) (if (stringp x) x (symbol-name x)))
                                          ',env-expr))))

         ;; Process executable dependencies - only add if present
         ,(when executable-expr
            `(setq unit (plist-put unit :executable
                                  (mapcar (lambda (x) (if (stringp x) x (symbol-name x)))
                                          ',executable-expr))))

         ;; Store the body code as a string
         (setq unit (plist-put unit :body ,body-string))

         ;; Add to the list of units
         (setq emacs-backbone-units (cons unit emacs-backbone-units))

         ;; Return the config unit symbol
         ',name))))

;; Function for Gleam to access the config units data
(defun emacs-backbone-get-units ()
  "Return all defined config units as a JSON array for Gleam."
  (emacs-backbone--format-plists-for-json emacs-backbone-units))

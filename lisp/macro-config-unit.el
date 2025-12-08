;; -*- lexical-binding: t -*-
;; unit-macro.el - Macros for config unit declaration in Emacs Backbone

;; Global variable to store config units information
(defvar emacs-backbone-units '()
  "Global storage for config unit definitions used by the Backbone system.")

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

The macro registers the unit with the `emacs-backbone-units` list for
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

Units are executed only when their required features are available, and in
dependency order based on the :after specifications."
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
           ;; Get requires and afters
           (requires-expr (plist-get plist-pairs :requires))
           (afters-expr (plist-get plist-pairs :after))
           ;; Convert body forms to a single string
           (body-string (when body
                          (format "%S" `(progn ,@body t)))))

      ;; Ensure we have lists for both requires and afters
      (when (and requires-expr (symbolp requires-expr))
        (setq requires-expr (list requires-expr)))

      (when (and afters-expr (symbolp afters-expr))
        (setq afters-expr (list afters-expr)))

      `(let ((unit (list :name ,unit-name)))
         ;; Process requires
         (setq unit (plist-put unit :requires
                              (mapcar #'symbol-name ',requires-expr)))

         ;; Process after dependencies - only add if present
         ,(when afters-expr
            `(setq unit (plist-put unit :after
                                  (mapcar #'symbol-name ',afters-expr))))

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

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(load-file (expand-file-name "./lisp/package-manager.el" user-emacs-directory))

(load-file (expand-file-name "./lisp/helper.el" user-emacs-directory))
(load-file (expand-file-name "./lisp/macro-package.el" user-emacs-directory))
(load-file (expand-file-name "./lisp/macro-config-unit.el" user-emacs-directory))
(load-file (expand-file-name "./lisp/backbone.el" user-emacs-directory))

(defun emacs-backbone-tangle-config ()
  (interactive)
  (org-babel-tangle-file (expand-file-name "config.org" emacs-backbone-user-directory)))

(defun emacs-backbone-reload-config ()
  (interactive)
  (emacs-backbone-reset-packages)
  (emacs-backbone-reset-units)

  (load-file (expand-file-name "config.el" emacs-backbone-user-directory))
  (when emacs-backbone--process
    (jsonrpc-async-request emacs-backbone--process "init" nil
      :success-fn (lambda (_) (message "[Backbone] Reloaded"))
      :error-fn (lambda (err) (message "[Backbone] Reload failed: %s" err)))))

(defvar-keymap emacs-backbone-prefix-map
  :doc "Backbone Utilities"
  "e" #'emacs-backbone-tangle-config
  "r" #'emacs-backbone-reload-config)

(which-key-add-keymap-based-replacements help-map
  "r" `("Backbone" . ,emacs-backbone-prefix-map))

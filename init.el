;;; init.el --- Emacs Backbone  -*- lexical-binding: t; -*-

(load-file (expand-file-name "./lisp/compose.el" user-emacs-directory))

;; Load user configuration from emacs-backbone-user-directory
(let ((user-config-file (expand-file-name "config.el" emacs-backbone-user-directory)))
  (if (file-exists-p user-config-file)
      (load-file user-config-file)
    (warn "Emacs Backbone user config not found at %s. Please create your configuration directory." user-config-file)))

(emacs-backbone-start)

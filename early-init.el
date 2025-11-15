;;; early-init.el --- Emacs Backbone  -*- lexical-binding: t; -*-

(load-file (expand-file-name "lisp/backbone-early-init.el" user-emacs-directory))

;; Load user's early init from emacs-backbone-user-directory
(let ((user-early-init (expand-file-name "~/.config/backbone/early-init.el")))
  (when (file-exists-p user-early-init)
    (load-file user-early-init)))

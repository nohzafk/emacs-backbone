;;; --- Emacs Backbone  -*- lexical-binding: t; -*-

(defun emacs-backbone--plist-to-alist (plist)
  "Convert PLIST to an alist suitable for json-encode."
  (let ((alist '()))
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        ;; Remove the leading ":" from keyword and convert to symbol
        (push (cons (intern (substring (symbol-name key) 1)) value) alist)))
    (nreverse alist)))

(defun emacs-backbone--format-plists-for-json (plists)
  "Convert a list of plist to a JSON array of objects"
  (mapcar #'emacs-backbone--plist-to-alist plists))

;; Borrowed from doom-emacs: https://github.com/doomemacs/doomemacs
(defun backbone-load-envvars-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
  (if (null (file-exists-p file))
      (unless noerror
        (signal 'file-error (list "No envvar file exists" file)))
    (with-temp-buffer
      (insert-file-contents file)
      (when-let (env (read (current-buffer)))
        (let ((tz (getenv-internal "TZ")))
          (setq-default
           process-environment
           (append env (default-value 'process-environment))
           exec-path
           (append (split-string (getenv "PATH") path-separator t)
                   (list exec-directory))
           shell-file-name
           (or (getenv "SHELL")
               (default-value 'shell-file-name)))
          (when-let (newtz (getenv-internal "TZ"))
            (unless (equal tz newtz)
              (set-time-zone-rule newtz))))
        env))))

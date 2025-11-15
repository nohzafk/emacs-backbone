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

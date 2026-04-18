;; use elpacs as package manager

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil))
      (load "./elpaca-autoloads"))))

(defun emacs-backbone--elpaca-infer-main-file (recipe)
  "Infer `:main' for split-package RECIPEs when Elpaca omits it.

Split-package recipes from archives often include an exact package file inside
`:files' but rely on Elpaca to rediscover it later. That rediscovery is brittle
for monorepo packages such as `magit-section', whose repo directory name does
not match the package name."
  (unless (plist-member recipe :main)
    (when-let* ((package (plist-get recipe :package))
                (files (plist-get recipe :files))
                (package-name (if (symbolp package) (symbol-name package) package)))
      (let ((main-file nil))
        (while (and files (not main-file))
          (let ((file (car files)))
            (when (and (stringp file)
                       (string-match-p
                        (concat "\\(?:^\\|/\\)"
                                (regexp-quote package-name)
                                "\\.el\\'")
                        file))
              (setq main-file file)))
          (setq files (cdr files)))
        (when main-file
          (list :main main-file))))))

(defun emacs-backbone--elpaca-shared-source-dir (orig-fn id dir)
  "Return the nearest prior shared-source owner for ID at DIR.

Elpaca's queued list can contain entries from later queues before entries from
the current queue. For split packages sharing a source checkout, that can make
`elpaca--shared-source-dir' incorrectly choose a future queue entry as the
source owner. We only want owners from the same or an earlier queue."
  (let* ((queued (elpaca--queued))
         (index (cl-position id queued :key #'car :test #'eq))
         (current (and index (cdr (nth index queued)))))
    (if (not current)
        (funcall orig-fn id dir)
      (cl-loop for i from (1- index) downto 0
               for e = (cdr (nth i queued))
               when (and e
                         (equal dir (elpaca<-source-dir e))
                         (<= (elpaca<-queue-id e) (elpaca<-queue-id current)))
               return e))))

(defun emacs-backbone--elpaca-wait-for-shared-git-source (orig-fn e)
  "Block E on an earlier queued git package sharing the same source checkout.

Elpaca only blocks on a shared source owner when the repo directory does not
yet exist. For split packages from one monorepo, the directory can already
exist while the earlier package is still cloning/checking out, which lets the
later package race into dependency detection too early.

Example: `magit` and the split package `magit-section` both use the shared
`.../elpaca/sources/magit/` checkout, but `magit-section` needs
`lisp/magit-section.el` to exist before dependency detection can succeed."
  (if (not (eq (plist-get (elpaca<-recipe e) :type) 'git))
      (funcall orig-fn e)
    (if-let* ((shared (elpaca--shared-source-dir
                       (elpaca<-id e)
                       (elpaca<-source-dir e)))
              ((not (or (memq (elpaca<-id shared) (elpaca<-blockers e))
                        (elpaca<-builtp shared)))))
        (progn
          (push (elpaca<-id shared) (elpaca<-blockers e))
          (push (elpaca<-id e) (elpaca<-blocking shared))
          (setf (elpaca<-statuses e) (list 'blocked 'queued)))
      (funcall orig-fn e))))

(defun emacs-backbone--elpaca-wait-for-main-file (e main attempts)
  "Continue E once MAIN exists under its source dir, retrying ATTEMPTS times."
  (let ((file (expand-file-name main (elpaca<-source-dir e))))
    (cond
     ((file-exists-p file)
      (elpaca--continue-build e (format "Shared source ready: %s" main)))
     ((<= attempts 0)
      (elpaca--fail e (format "Timed out waiting for shared source file: %s" file)))
     (t
      (run-at-time 0.1 nil #'emacs-backbone--elpaca-wait-for-main-file
                   e main (1- attempts))))))

(defun emacs-backbone--elpaca-wait-on-shared-main-before-clone (orig-fn e)
  "Delay clone-skip continuation until E's main file exists in the shared repo."
  (let* ((recipe (elpaca<-recipe e))
         (main (plist-get recipe :main))
         (source-dir (elpaca<-source-dir e))
         (main-file (and main (expand-file-name main source-dir))))
    (if (and main
             (file-exists-p source-dir)
             (not (file-exists-p main-file)))
        (progn
          (elpaca--signal e (format "%s exists. Waiting for %s." source-dir main)
                          'cloning)
          (emacs-backbone--elpaca-wait-for-main-file e main 600))
      (funcall orig-fn e))))

(add-hook 'elpaca-recipe-functions #'emacs-backbone--elpaca-infer-main-file)
(advice-add 'elpaca--shared-source-dir :around #'emacs-backbone--elpaca-shared-source-dir)
(advice-add 'elpaca-source :around #'emacs-backbone--elpaca-wait-for-shared-git-source)
(advice-add 'elpaca-git--clone :around #'emacs-backbone--elpaca-wait-on-shared-main-before-clone)

(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Block until current queue processed - required before using use-package
(elpaca-wait)

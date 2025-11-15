;; -*- lexical-binding: t -*-
;; package-macro.el - Macros for package declaration in Emacs Backbone

;; Global variable to store package information
(defvar emacs-backbone-packages '()
  "Global storage for package definitions used by Gleam conductor.")

(defun emacs-backbone-reset-packages ()
  "Reset the global packages list."
  (setq emacs-backbone-packages '()))

(cl-defmacro package!
  (name &rest args &key repo host branch tag files no-compilation deps)
  "Define a package for installation and management by the emacs-backbone system.

This macro registers a package specification with the emacs-backbone package
management system. It supports both standard packages from package archives
and packages fetched directly from version control repositories.

Arguments:
  NAME      Symbol or string that identifies the package.
  ARGS      Property list of package attributes (all optional).

Supported properties:
  :repo STRING            Repository path (e.g., \"user/repo\").
                          When specified, the package is fetched from version control
                          rather than a package archive.

  :host STRING            Repository host service (default: \"github\").
                          Other common values might include \"gitlab\", \"sourcehut\", etc.

  :branch STRING          Specific branch to use (default: main branch).
                          Only used when :repo is specified.

  :tag STRING             Specific tag to use instead of a branch.
                          Only used when :repo is specified.

  :ref STRING             Specific commit hash to use.
                          Only used when :repo is specified.

  :files (\"file1\" \"file2\")  List of files to include from the repository.
                          Useful for repositories containing multiple packages
                          or when only specific files should be loaded.

  :no-compilation BOOL    When non-nil, disables byte-compilation for this package.

  :deps (pkg1 pkg2)       Symbol or list of symbols representing package dependencies
                          that must be installed before this package. Each entry
                          can be a symbol or string. The emacs-backbone system
                          automatically resolves these dependencies and ensures
                          they are installed in the correct order.
                          Example: :deps (avy magit) or :deps magit

                          Important: All packages listed in :deps must also be
                          registered with their own package! declaration. The
                          emacs-backbone system verifies the entire dependency
                          graph to ensure all required packages are properly
                          defined and managed.
Examples:

  ;; Standard package from an archive:
  (package! magit)

  ;; Package from a specific GitHub repository:
  (package! evil-collection
    :repo \"emacs-evil/evil-collection\"
    :no-compilation t)

  ;; Package with dependencies:
  (package! emacsql)
  (package! dash)
  (package! org-roam
    :deps (emacsql dash))
    ;; emacsql and dash will be guaranteed to install before org-roam
    ;; and emacs-backbone verifies that both are properly registered

  ;; Package from a non-GitHub source:
  (package! modus-themes
    :repo \"protesilaos/modus-themes\"
    :host \"gitlab\")

  ;; Dependencies can be nested - emacs-backbone resolves the full graph:
  (package! package-c)
  (package! package-d)
  (package! package-b :deps (package-c package-d))
  (package! package-a :deps package-b)
  ;; Installation order: package-c, package-d, package-b, package-a

After evaluation, the package specification is added to `emacs-backbone-packages`
list for later processing by emacs-backbone's package management system, which
handles dependency resolution, verification and installation sequencing."
  (declare (indent defun))

  (when files
    (cl-callf plist-put args :files `(quote ,files)))

  (let* ((pkg-name (if (stringp name) name (symbol-name name)))
         (repo (plist-get args :repo))
         (host (or (plist-get args :host) "github"))
         (branch (plist-get args :branch))
         (tag (plist-get args :tag))
         (ref (plist-get args :ref))
         (files (plist-get args :files))
         (no-compilation (plist-get args :no-compilation))
         (deps (plist-get args :deps)))

    ;; Ensure we have lists for deps
    (when (and deps (symbolp deps))
      (setq deps (list deps)))

    `(let ((pkg (list :name ,pkg-name)))
       ;; Add recipe if repo is provided
       (when ,repo
         (setq pkg (plist-put pkg :recipe
                              (list :host ,host
                                    :repo ,repo
                                    :branch ,branch
                                    :tag ,tag
                                    :ref ,ref
                                    :files ,files))))

       ;; Add no-compilation if provided
       (when ,no-compilation
         (setq pkg (plist-put pkg :no-compilation ,no-compilation)))

       ,(when deps
          `(setq pkg (plist-put pkg :deps
                                (mapcar #'symbol-name ',deps))))

       ;; Add to global list
       (setq emacs-backbone-packages (cons pkg emacs-backbone-packages))

       ;; Return the package symbol
       ',name)))

(defun emacs-backbone--plist-to-alist (plist)
  "Convert PLIST to an alist suitable for json-encode."
  (let ((alist '()))
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        ;; Remove the leading ":" from keyword and convert to symbol
        (push (cons (intern (substring (symbol-name key) 1)) value) alist)))
    (nreverse alist)))

;; Function for Gleam to access the package data
(defun emacs-backbone-get-packages ()
  "Return all defined packages as a JSON array for Gleam."
  (emacs-backbone--format-plists-for-json emacs-backbone-packages))

;;; backbone.el --- JSON-RPC connection to Backbone Gleam process  -*- lexical-binding: t; -*-

(require 'jsonrpc)
(require 'json)

(defvar emacs-backbone-buffer-name "*emacs-backbone*")
(defvar emacs-backbone-enable-debug nil)
(defvar emacs-backbone--initialized nil)
(defvar emacs-backbone--process nil)

(defcustom emacs-backbone-user-directory
  (expand-file-name "~/.config/backbone")
  "Directory containing user configuration for Emacs Backbone.
This directory should contain config.org, config.el, and user-specific
configuration files (config/, clis/, etc.)."
  :type 'directory
  :group 'emacs-backbone)

(defcustom emacs-backbone-gleam-executable
  (or (and (file-executable-p "/opt/homebrew/bin/gleam")
           "/opt/homebrew/bin/gleam")
      (executable-find "gleam")
      "gleam")
  "Path to the Gleam executable.
Defaults to /opt/homebrew/bin/gleam on macOS with Homebrew,
or searches PATH for 'gleam' executable."
  :type 'string
  :group 'emacs-backbone)

(defclass emacs-backbone-connection (jsonrpc-process-connection) ()
  :documentation "JSON-RPC connection to the Backbone Gleam process.")

(defun emacs-backbone--handle-notification (_conn method params)
  "Handle incoming JSON-RPC notifications from the Gleam process.
METHOD is an interned symbol. PARAMS is a plist."
  (pcase method
    ('show-message (message "%s" (plist-get params :content)))
    ('eval-code
     (condition-case err
         (eval (read (plist-get params :content)))
       (error (message "[Backbone] eval-code error: %S" err))))))

(defun emacs-backbone--handle-request (_conn method params)
  "Handle incoming JSON-RPC requests from the Gleam process.
METHOD is an interned symbol. PARAMS is a plist.
Return value is sent back as the JSON-RPC response."
  (pcase method
    ('fetch-var
     (let ((result (eval (read (plist-get params :expr)))))
       (json-encode result)))))

(defun emacs-backbone-start ()
  "Start the Backbone process and establish JSON-RPC connection."
  (interactive)
  (if emacs-backbone--process
      (message "[Backbone] Process already running.")
    (let* ((default-directory user-emacs-directory)
           (process-environment (cons "NO_COLOR=true" process-environment))
           (stderr-buffer (get-buffer-create "*emacs-backbone-stderr*"))
           (conn (make-instance
                  'emacs-backbone-connection
                  :name "emacs-backbone"
                  :process (make-process
                            :name "emacs-backbone"
                            :command (list emacs-backbone-gleam-executable "run")
                            :connection-type 'pipe
                            :stderr stderr-buffer
                            :noquery t)
                  :notification-dispatcher #'emacs-backbone--handle-notification
                  :request-dispatcher #'emacs-backbone--handle-request)))

      (setq emacs-backbone--process conn)

      ;; Send init request to Gleam
      (jsonrpc-async-request conn "init" nil
        :success-fn (lambda (_result)
                      (setq emacs-backbone--initialized t))
        :error-fn (lambda (err)
                    (message "[Backbone] Init failed: %s" err))))))

(defun emacs-backbone-exit ()
  "Shut down the Backbone process."
  (interactive)
  (when emacs-backbone--process
    (condition-case nil
        (jsonrpc-shutdown emacs-backbone--process)
      (error nil))
    (setq emacs-backbone--process nil
          emacs-backbone--initialized nil)
    (message "[Backbone] Disconnected")))

(add-hook 'kill-emacs-hook #'emacs-backbone-exit)

(defun emacs-backbone--call (&rest func-args)
  "Send a JSON-RPC message to the Backbone process.
Preserves the same call signature used by generated packages.el code."
  (when emacs-backbone--process
    (pcase func-args
      (`("init" . ,_)
       (jsonrpc-async-request emacs-backbone--process "init" nil
         :success-fn (lambda (_) nil)
         :error-fn (lambda (err) (message "[Backbone] init error: %s" err))))
      (`("package_installed" ,name . ,_)
       (jsonrpc-notify emacs-backbone--process "package_installed"
                       `(:name ,name)))
      (`(,method . ,args)
       (jsonrpc-notify emacs-backbone--process method
                       (when args `(:args ,(vconcat args))))))))

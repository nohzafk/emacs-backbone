;;; backbone.el --- JSON-RPC connection to Backbone Gleam process  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'jsonrpc)
(require 'json)

(defvar emacs-backbone-buffer-name "*emacs-backbone*")
(defvar emacs-backbone-status-buffer-name "*emacs-backbone-status*")
(defvar emacs-backbone-enable-debug nil)
(defvar emacs-backbone--initialized nil)
(defvar emacs-backbone--process nil)
(defvar emacs-backbone--packages-finished-sent nil)
(defvar emacs-backbone--packages-installation-active nil)
(defvar emacs-backbone--package-timeout-timer nil)

(defcustom emacs-backbone-package-timeout-seconds 30
  "No-progress timeout (seconds) before proceeding with configuration.
This is a global idle timer: it is reset whenever a package reports
`package_installed'. When it fires, any packages that never reported
success are treated as failed. Bump this on cold installs where the
first git clone/byte-compile may take longer than the default."
  :type 'integer
  :group 'emacs-backbone)

(defcustom emacs-backbone-package-timeout-log-limit 10
  "Maximum number of unfinished Elpaca orders to include in timeout diagnostics."
  :type 'integer
  :group 'emacs-backbone)

(defcustom emacs-backbone-open-debug-buffers-on-timeout t
  "When non-nil, open Backbone/Elpaca debug buffers after a package timeout."
  :type 'boolean
  :group 'emacs-backbone)

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

(defun emacs-backbone-running-p ()
  "Return non-nil when the Backbone JSON-RPC process is alive."
  (not (null (and emacs-backbone--process
                  (jsonrpc-running-p emacs-backbone--process)))))

(defun emacs-backbone--log-diagnostic (fmt &rest args)
  "Write a formatted diagnostic message to `*Messages*' and Backbone stderr."
  (let ((line (apply #'format fmt args)))
    (message "%s" line)
    (when-let ((buffer (get-buffer "*emacs-backbone-stderr*")))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert line "\n")))))

(defun emacs-backbone--stderr-tail (&optional max-lines)
  "Return the last MAX-LINES non-empty lines from Backbone stderr."
  (when-let ((buffer (get-buffer "*emacs-backbone-stderr*")))
    (with-current-buffer buffer
      (let* ((lines (split-string (buffer-string) "\n" t))
             (max-lines (or max-lines 12))
             (start (max 0 (- (length lines) max-lines))))
        (when lines
          (mapconcat #'identity (nthcdr start lines) "\n"))))))

(defun emacs-backbone--startup-hint (err stderr-tail)
  "Return a targeted startup hint based on ERR and STDERR-TAIL."
  (cond
   ((string-match-p "The program `bun` was not found" (or stderr-tail ""))
    "[Backbone] Hint: install Bun or switch `[javascript].runtime` in gleam.toml to an available runtime.")
   ((string-match-p "ReferenceError: Bun is not defined" (or stderr-tail ""))
    "[Backbone] Hint: the backend is running under Node, but the transport still uses Bun-only APIs.")
   ((string-match-p "No such file or directory" (format "%s" err))
    "[Backbone] Hint: set `emacs-backbone-gleam-executable` to a valid Gleam binary.")
   ((string-match-p "Program not found" (or stderr-tail ""))
    "[Backbone] Hint: install the JavaScript runtime required by gleam.toml or change it to one that exists locally.")))

(defun emacs-backbone--report-startup-failure (err command)
  "Log a detailed startup diagnostic for ERR from COMMAND."
  (let ((stderr-tail (emacs-backbone--stderr-tail))
        (hint nil))
    (setq hint (emacs-backbone--startup-hint err stderr-tail))
    (emacs-backbone--log-diagnostic
     "[Backbone] Startup failed: %s" err)
    (emacs-backbone--log-diagnostic
     "[Backbone] Command: %s"
     (mapconcat #'shell-quote-argument command " "))
    (when stderr-tail
      (emacs-backbone--log-diagnostic
       "[Backbone] stderr tail:\n%s"
       stderr-tail))
    (when hint
      (emacs-backbone--log-diagnostic "%s" hint))
    (emacs-backbone--log-diagnostic
     "[Backbone] Inspect *emacs-backbone-stderr* for full backend output.")))

(defun emacs-backbone--active-elpaca-queue ()
  "Return the current incomplete Elpaca queue, or nil when unavailable."
  (when (boundp 'elpaca--queues)
    (cl-find 'incomplete (reverse elpaca--queues) :key #'elpaca-q<-status)))

(defun emacs-backbone--unfinished-elpaca-orders (&optional queue)
  "Return unfinished Elpaca orders from QUEUE.
If QUEUE is nil, inspect the current incomplete queue."
  (when-let* ((q (or queue (emacs-backbone--active-elpaca-queue))))
    (cl-loop for (_id . order) in (elpaca-q<-elpacas q)
             unless (eq (elpaca--status order) 'finished)
             collect order)))

(defun emacs-backbone--format-elpaca-order (order)
  "Return a compact diagnostic summary for Elpaca ORDER."
  (format "%s status=%s steps=%S blockers=%S source=%s build=%s"
          (elpaca<-id order)
          (elpaca--status order)
          (elpaca<-statuses order)
          (elpaca<-blockers order)
          (or (elpaca<-source-dir order) "<none>")
          (or (elpaca<-build-dir order) "<none>")))

(defun emacs-backbone--log-package-timeout-context ()
  "Log diagnostic context for a stalled Elpaca queue."
  (if-let* ((queue (emacs-backbone--active-elpaca-queue)))
      (let* ((orders (emacs-backbone--unfinished-elpaca-orders queue))
             (count (length orders))
             (limit emacs-backbone-package-timeout-log-limit)
             (displayed (cl-subseq orders 0 (min count limit))))
        (emacs-backbone--log-diagnostic
         "[Backbone] Package installation timed out. queue=%s status=%s processed=%s total=%s unfinished=%s"
         (elpaca-q<-id queue)
         (elpaca-q<-status queue)
         (elpaca-q<-processed queue)
         (length (elpaca-q<-elpacas queue))
         count)
        (dolist (order displayed)
          (emacs-backbone--log-diagnostic
           "[Backbone] stalled order: %s"
           (emacs-backbone--format-elpaca-order order)))
        (when (> count limit)
          (emacs-backbone--log-diagnostic
           "[Backbone] ... %s more unfinished orders omitted"
           (- count limit))))
    (emacs-backbone--log-diagnostic
     "[Backbone] Package installation timed out, but no active Elpaca queue was found.")))

(defun emacs-backbone--handle-package-timeout-visibility ()
  "Surface timeout debugging entry points to the user."
  (emacs-backbone--log-diagnostic
   "[Backbone] Run M-x emacs-backbone-open-package-debug for live timeout details.")
  (when (and emacs-backbone-open-debug-buffers-on-timeout
             (not noninteractive))
    (run-at-time 0 nil #'emacs-backbone-open-package-debug)))

(defun emacs-backbone--package-timeout-deadline ()
  "Return the timeout deadline as an Emacs time value, or nil if inactive."
  (when (timerp emacs-backbone--package-timeout-timer)
    (timer--time emacs-backbone--package-timeout-timer)))

(defun emacs-backbone--seconds-until-timeout ()
  "Return seconds remaining until the package timeout fires, or nil."
  (when-let ((deadline (emacs-backbone--package-timeout-deadline)))
    (max 0 (ceiling (float-time (time-subtract deadline (current-time)))))))

(defun emacs-backbone--insert-status-line (label value)
  "Insert a LABEL/VALUE pair into the current buffer."
  (insert (format "%-18s %s\n" label value)))

(defun emacs-backbone--insert-status-order (order)
  "Insert a human-readable summary of Elpaca ORDER into the current buffer."
  (insert (format "- %s\n" (emacs-backbone--format-elpaca-order order))))

(defun emacs-backbone-status ()
  "Display the current Backbone and Elpaca package-installation state."
  (interactive)
  (let* ((buffer (get-buffer-create emacs-backbone-status-buffer-name))
         (queue (emacs-backbone--active-elpaca-queue))
         (unfinished (emacs-backbone--unfinished-elpaca-orders queue))
         (remaining (emacs-backbone--seconds-until-timeout)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Emacs Backbone Status\n")
      (insert "====================\n\n")
      (emacs-backbone--insert-status-line
       "process"
       (if (emacs-backbone-running-p) "running" "stopped"))
      (emacs-backbone--insert-status-line
       "initialized"
       emacs-backbone--initialized)
      (emacs-backbone--insert-status-line
       "install active"
       emacs-backbone--packages-installation-active)
      (emacs-backbone--insert-status-line
       "packages finished"
       emacs-backbone--packages-finished-sent)
      (emacs-backbone--insert-status-line
       "timeout timer"
       (if remaining
           (format "%ss remaining" remaining)
         "inactive"))
      (if queue
          (progn
            (insert "\nActive Elpaca Queue\n")
            (insert "------------------\n")
            (emacs-backbone--insert-status-line "queue id" (elpaca-q<-id queue))
            (emacs-backbone--insert-status-line "queue status" (elpaca-q<-status queue))
            (emacs-backbone--insert-status-line "processed" (elpaca-q<-processed queue))
            (emacs-backbone--insert-status-line "total" (length (elpaca-q<-elpacas queue)))
            (emacs-backbone--insert-status-line "unfinished" (length unfinished))
            (when unfinished
              (insert "\nUnfinished Orders\n")
              (insert "-----------------\n")
              (dolist (order unfinished)
                (emacs-backbone--insert-status-order order))))
        (insert "\nNo active Elpaca queue.\n"))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buffer)))

(defun emacs-backbone-open-package-debug ()
  "Open Backbone and Elpaca buffers useful for package-installation debugging."
  (interactive)
  (let ((status-buffer (save-window-excursion
                         (emacs-backbone-status)
                         (current-buffer)))
        (stderr-buffer (get-buffer "*emacs-backbone-stderr*"))
        log-buffer
        manager-buffer)
    (when (require 'elpaca-log nil t)
      (setq log-buffer (save-window-excursion
                         (elpaca-log)
                         (current-buffer))))
    (when (require 'elpaca-manager nil t)
      (setq manager-buffer (save-window-excursion
                             (elpaca-manager)
                             (current-buffer))))
    (delete-other-windows)
    (pop-to-buffer status-buffer)
    (when log-buffer
      (display-buffer log-buffer
                      '((display-buffer-reuse-window display-buffer-below-selected))))
    (when manager-buffer
      (display-buffer manager-buffer
                      '((display-buffer-reuse-window display-buffer-below-selected))))
    (when stderr-buffer
      (display-buffer stderr-buffer
                      '((display-buffer-reuse-window display-buffer-below-selected))))))

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
  (if (emacs-backbone-running-p)
      (message "[Backbone] Process already running.")
    (let* ((default-directory user-emacs-directory)
           (process-environment (cons "NO_COLOR=true" process-environment))
           (stderr-buffer (get-buffer-create "*emacs-backbone-stderr*"))
           (command (list emacs-backbone-gleam-executable "run")))
      (setq emacs-backbone--initialized nil)
      (setq emacs-backbone--process nil)
      (condition-case err
          (let ((conn
                 (make-instance
                  'emacs-backbone-connection
                  :name "emacs-backbone"
                  :events-buffer-config `(:size ,(if emacs-backbone-enable-debug nil 0))
                  :process (make-process
                            :name "emacs-backbone"
                            :command command
                            :connection-type 'pipe
                            :stderr stderr-buffer
                            :noquery t)
                  :notification-dispatcher #'emacs-backbone--handle-notification
                  :request-dispatcher #'emacs-backbone--handle-request)))
            (setq emacs-backbone--process conn)
            ;; Send init request to Gleam.
            (jsonrpc-async-request
             conn "init" nil
             :success-fn (lambda (_result)
                           (setq emacs-backbone--initialized t))
             :error-fn (lambda (init-err)
                         (unless (emacs-backbone-running-p)
                           (setq emacs-backbone--process nil))
                         (emacs-backbone--report-startup-failure
                          init-err command))))
        (file-missing
         (setq emacs-backbone--process nil)
         (emacs-backbone--report-startup-failure err command))))))

(defun emacs-backbone--begin-package-installation ()
  "Reset package completion state and start the no-progress timer."
  (setq emacs-backbone--packages-installation-active t)
  (setq emacs-backbone--packages-finished-sent nil)
  (emacs-backbone--start-package-timeout))

(defun emacs-backbone--start-package-timeout ()
  "Start or restart the no-progress timer."
  (emacs-backbone--cancel-package-timeout)
  (setq emacs-backbone--package-timeout-timer
        (run-at-time emacs-backbone-package-timeout-seconds nil
                     #'emacs-backbone--package-timeout)))

(defun emacs-backbone--reset-package-timeout ()
  "Reset the no-progress timer if installation is active."
  (when (and emacs-backbone--packages-installation-active
             emacs-backbone--package-timeout-timer)
    (emacs-backbone--start-package-timeout)))

(defun emacs-backbone--cancel-package-timeout ()
  "Cancel the no-progress timer if it exists."
  (when emacs-backbone--package-timeout-timer
    (cancel-timer emacs-backbone--package-timeout-timer)
    (setq emacs-backbone--package-timeout-timer nil)))

(defun emacs-backbone--package-timeout ()
  "Handle no-progress timeout by proceeding with configuration."
  (setq emacs-backbone--package-timeout-timer nil)
  (emacs-backbone--log-package-timeout-context)
  (emacs-backbone--handle-package-timeout-visibility)
  (emacs-backbone--notify-packages-finished "timeout"))

(defun emacs-backbone--notify-packages-finished (&optional reason)
  "Send a packages_finished notification once per installation."
  (when emacs-backbone--packages-installation-active
    (unless emacs-backbone--packages-finished-sent
      (setq emacs-backbone--packages-finished-sent t)
      (setq emacs-backbone--packages-installation-active nil)
      (emacs-backbone--cancel-package-timeout)
      (if reason
          (emacs-backbone--call "packages_finished" reason)
        (emacs-backbone--call "packages_finished")))))

(defun emacs-backbone--elpaca-queues-finished ()
  "Notify Backbone after Elpaca finishes all queues."
  (emacs-backbone--notify-packages-finished "completed"))

(when (boundp 'elpaca--post-queues-hook)
  (remove-hook 'elpaca--post-queues-hook #'emacs-backbone--elpaca-queues-finished)
  (add-hook 'elpaca--post-queues-hook #'emacs-backbone--elpaca-queues-finished))

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
      (`("packages_finished" . ,rest)
       (let ((reason (car rest)))
         (jsonrpc-notify emacs-backbone--process "packages_finished"
                         (when reason `(:reason ,reason)))))
      (`(,method . ,args)
       (jsonrpc-notify emacs-backbone--process method
                       (when args `(:args ,(vconcat args))))))))

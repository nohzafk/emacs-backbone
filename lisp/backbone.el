;; Add the directory containing websocket.el to load-path

(require 'websocket)
(require 'ansi-color)

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

(defun emacs-backbone-get-free-port ()
  (save-excursion
    (let* ((process-buffer " *temp*")
           (process (make-network-process
                     :name process-buffer
                     :buffer process-buffer
                     :family 'ipv4
                     :server t
                     :host "127.0.0.1"
                     :service t))
           port)
      (setq port (process-contact process))
      (delete-process process)
      (kill-buffer process-buffer)
      (format "%s" (cadr port)))))

(cl-defmacro emacs-backbone-start ()
  (if (not (null emacs-backbone--process))
      (message "[Backbone] processs has started.")
    (let* ((backbone-port (emacs-backbone-get-free-port))
           (emacs-port (emacs-backbone-get-free-port))
           (server (intern "emacs-backbone-server"))
           (process (intern "emacs-backbone-process"))
           (process-buffer emacs-backbone-buffer-name)
           (client (intern "emacs-backbone-client")))
      `(let ((process-environment (cons "NO_COLOR=true" process-environment))
             (default-directory user-emacs-directory))
         (defvar ,process nil)
         (defvar ,server nil)
         (defvar ,client nil)

         (setq ,server
               (websocket-server
                ,emacs-port
                :host 'local
                :on-message (lambda (_websocket frame)
                              (let ((text (websocket-frame-text frame))
                                    (opcode (websocket-frame-opcode frame))) ; Use this accessor

                                ;; Only process text frames
                                (when (eq opcode 'text)
                                  (condition-case err
                                      (let* ((info (json-parse-string text))
                                             (info-type (gethash "type" info nil)))
                                        (pcase info-type
                                          ("show-message" (message (gethash "content" info nil)))
                                          ("eval-code" (eval (read (gethash "content" info nil))))
                                          ("fetch-var" (thread-last
                                                         (eval (read (gethash "content" info nil)))
                                                         (json-encode)
                                                         (websocket-send-text _websocket)))))
                                    (json-parse-error
                                     (when emacs-backbone-enable-debug
                                       (message "Received malformed JSON in text frame: %S" text)))))))

                :on-open (lambda (_websocket)
                           (setq ,client (websocket-open (format "ws://127.0.0.1:%s" ,backbone-port)))
                           (unless emacs-backbone--initialized
                             (emacs-backbone--call "init")
                             (setq emacs-backbone--initialized t)))

                :on-close (lambda (_websocket))
                :on-error (lambda (_websocket type err)
                            (display-warning 'websocket
                                             (format "in callback `%S': %s"
                                                     type
                                                     (websocket-format-error err))
                                             :error)
                            ;; Check if the connection is still open before sending
                            (when (and (websocket-openp _websocket)
                                       (eq (websocket-ready-state _websocket) 'open))
                              (condition-case nil
                                  (thread-last
                                    (list "error" (websocket-format-error err))
                                    (json-encode)
                                    (websocket-send-text _websocket))
                                (error nil))))))

         (setq ,process
               (start-process "emacs-backbone" ,process-buffer emacs-backbone-gleam-executable "run" "--" ,backbone-port ,emacs-port))

         (set-process-query-on-exit-flag ,process nil)

         ;; Make sure ANSI color render correctly.
         (set-process-sentinel
          ,process
          (lambda (p _m)
            (when (eq 0 (process-exit-status p))
              (with-current-buffer (process-buffer p)
                (ansi-color-apply-on-region (point-min) (point-max))))))

         (setq emacs-backbone--process ,process)))))

(defun emacs-backbone-exit ()
  "Gracefully shut down the Emacs Backbone connection."
  (interactive)
  (unless emacs-backbone--process
    (user-error "[Backbone] No active process to terminate"))

  ;; Close client connection first
  ;; Send shutdown signal to Gleam process
  (when-let* ((client-sym (intern-soft "emacs-backbone-client"))
              (client (and (boundp client-sym) (symbol-value client-sym))))
    (condition-case nil
        (progn
          (->> (list "data" '("shutdown"))
               (json-encode)
               (websocket-send-text client))
          ;; Give the server a moment to process the shutdown message
          (sleep-for 0.1))
      (error nil))
    (websocket-close client)
    (makunbound client-sym))

  ;; Close server
  (when-let* ((server-sym (intern-soft "emacs-backbone-server"))
              (server (and (boundp server-sym) (symbol-value server-sym))))
    (websocket-server-close server)
    (makunbound server-sym))

  ;; Kill process buffer if it exists
  (when-let* ((proc-sym (intern-soft "emacs-backbone-process"))
              (proc-buffer (get-buffer emacs-backbone-buffer-name)))
    (when (buffer-live-p proc-buffer)
      (kill-buffer proc-buffer))
    (when (boundp proc-sym)
      (makunbound proc-sym)))

  (setq emacs-backbone--initialized nil
        emacs-backbone--process nil)
  (message "[Backbone] Successfully disconnected"))

(add-hook 'kill-emacs-hook #'emacs-backbone-exit)

(defun emacs-backbone--call (&rest func-args)
  "Call emacs-backbone function from Emacs."
  (if (null emacs-backbone--process)
      (message "[Backbone] process has exited.")
    (websocket-send-text (symbol-value (intern-soft "emacs-backbone-client"))
                         (json-encode (list "data" func-args)))))

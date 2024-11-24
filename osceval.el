;;; osceval.el --- Evaluate Elisp code received over OSC -*- lexical-binding: t; -*-

;;; Code:

(require 'osc)

(defvar osceval-host
  "127.0.0.1"
  "IP address on which to listen for OSC messages (default: 127.0.0.1).")

(defvar osceval-port
  9473
  "Port on which to listen for OSC messages (default: 9473).")

(defvar osceval-handler
  #'osceval-default-handler
  "Handler function to run on incoming OSC messages (default: #'osceval-default-handler).")

(defvar osceval-buffer
  "*osceval*"
  "The name of the osceval process buffer (default: *osceval*).")

(defvar osceval-process
  nil
  "The OSC server process.")

(defun osceval-make-server (host port buffer default-handler)
  "Create an OSC server to listen for code to be evaluated."
  (make-network-process
   :name "osceval"
   :buffer buffer
   :coding 'binary
   :filter #'osc-filter
   :host host
   :service port
   :server t
   :type 'datagram
   :plist (list :generic default-handler)))

(defun osceval-message (format-string &rest args)
  (let ((process osceval-process)
        (buffer (get-buffer osceval-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (set-marker (process-mark process) (point-max))
        (let ((moving (= (point) (process-mark process))))
          (save-excursion
            (goto-char (process-mark process))
            (insert (apply #'format format-string args))
            (insert "\n")
            (set-marker (process-mark process) (point)))
          (when moving
            (goto-char (process-mark process))
            (walk-windows
             (lambda (window)
               (when (eq buffer (window-buffer window))
                 (set-window-point window (process-mark process))))
             nil t)))))))

(defun osceval-start ()
  "Start the osceval process."
  (interactive)
  (osceval-stop)
  (let ((host osceval-host)
        (port osceval-port)
        (buffer osceval-buffer)
        (default-handler osceval-handler))
    (setq osceval-process
          (osceval-make-server host port buffer default-handler))
    (when osceval-process
      (display-buffer (get-buffer osceval-buffer))
      (osceval-message "osceval is listening on %s:%s" host port))))

(defun osceval-stop ()
  "Stop the osceval process."
  (interactive)
  (when (and osceval-process (process-live-p osceval-process))
    (delete-process osceval-process))
  (setq osceval-process nil))

(defun osceval-default-handler (path &rest args)
  (pcase path
    ("/eval" (let ((code (nth 0 args)))
               (osceval-message "%s" code)
               (eval (read code))))
    (t nil)))

(provide 'osceval)

;;; osceval.el ends here

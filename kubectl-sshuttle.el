(defvar kubectl--sshuttle-login-synchronous-callback)
(defvar kubectl--shuttle-gateway "stg-gw.pd")

(defun kubectl--shuttle-refresh-gateway (&optional gateway)
  (interactive)
  (let* ((gw (or gateway kubectl--shuttle-gateway))
         (default-directory (format "/ssh:%s:" gw)))
    (cd default-directory)
    (message "gateway refreshed from %s" (pwd)))
  (cd default-directory))


(defun kubectl--sshuttle-login-synchronous (&optional callback)
  (interactive)
  (unless callback (setq callback 'ignore))
  (let* ((process-name "sshuttle-login-synchronous" )
         (buffer (format "*%s*" process-name))
         (command (s-split " " "sshuttle --verbose --remote stg-gw.pd 10.0.0.0/8"))
         (proc nil))
    (when (get-buffer buffer) (kill-buffer buffer))
    (setq proc (make-process
                :name process-name
                :connection-type 'pipe
                :buffer buffer
                :coding 'no-conversion
                :command command
                :filter 'kubectl--sshuttle-process-filter
                :stderr nil
                :sentinel (lambda (proc signal)
                            (kubectl--sshuttle-login-synchronous-callback))
                :noquery t))
    (with-current-buffer (get-buffer buffer)
      (make-variable-buffer-local 'kubectl--sshuttle-login-synchronous-callback)
      (fset 'kubectl--sshuttle-login-synchronous-callback callback)
      (special-mode))))

(defun kubectl--sshuttle-process-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc)))
            (inhibit-read-only t)
            (mfa nil))
        (when (s-match "Passcode or option" string)
          (process-send-string proc (format "%s\n" (read-string "please press your yubbykey for sshuttle: ")))
          (message "sending yubbykey to sshuttle"))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(provide 'kubectl-sshuttle)

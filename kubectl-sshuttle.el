(defvar kubectl--sshuttle-login-synchronous-callback)

(defun kubectl--sshuttle-login-synchronous (callback)
  (interactive)
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

(defvar kubectl--sshuttle-login-synchronous-callback)
(defvar kubectl--shuttle-gateway "stg-gw.pd")

(defun kubectl--shuttle-refresh-gateway (&optional gateway)
  (interactive)
  (let* ((gw (or gateway kubectl--shuttle-gateway))
         (default-directory (format "/ssh:%s:" gw)))
    (cd default-directory)
    (message "gateway refreshed from %s" (pwd)))
  (cd default-directory))

(defun kubectl--sshuttle-login-synchronous (&optional gateway callback)
  (interactive)
  (unless callback (setq callback 'ignore))
  (let* ((process-name "sshuttle-login-synchronous" )
         (buffer (format "*%s*" process-name))
         (command (s-split " " (format "sshuttle --verbose --remote %s 10.0.0.0/8" gateway)))
         (proc nil))
    (when (get-buffer buffer) (kill-buffer buffer))
    (setq proc (make-process
                :name process-name
                :connection-type 'pipe
                :buffer buffer
                :coding 'no-conversion
                :command command
                :stderr nil
                :sentinel (lambda (proc signal)
                            (kubectl--sshuttle-login-synchronous-callback))
                :noquery t))
    (with-current-buffer (get-buffer buffer)
      (make-variable-buffer-local 'kubectl--sshuttle-login-synchronous-callback)
      (fset 'kubectl--sshuttle-login-synchronous-callback callback)
      (special-mode))))

(provide 'kubectl-sshuttle)

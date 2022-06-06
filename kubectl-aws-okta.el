(defvar kubectl--aws-okta-login-synchronous-callback)
(defvar kubectl-current-aws-profile "")
(defvar kubectl-current-cluster "")

(defun kubectl--aws-okta-login-synchronous (aws-profile callback)
  (interactive)
  ;; (unless callback (setq callback 'ignore))
  ;; (let* ((process-name "aws-okta-login-synchronous" )
  ;;        (buffer (format "*%s*" process-name))
  ;;        (command (s-split " " (format "aws-okta env %s --mfa-duo-device token" aws-profile)))
  ;;        (proc nil))
  ;;   (when (get-buffer buffer) (kill-buffer buffer))
  ;;   (setq proc (make-process
  ;;               :name process-name
  ;;               :connection-type 'pipe
  ;;               :buffer buffer
  ;;               :coding 'no-conversion
  ;;               :command command
  ;;               :filter 'kubectl--aws-okta-process-filter
  ;;               :stderr nil
  ;;               :sentinel (lambda (proc signal)
  ;;                           (kubectl--aws-okta-setenv proc)
  ;;                           (kubectl--aws-okta-login-synchronous-callback))
  ;;               :noquery t))
  ;;   (with-current-buffer (get-buffer buffer)
  ;;     (make-variable-buffer-local 'kubectl--aws-okta-login-synchronous-callback)
  ;;     (fset 'kubectl--aws-okta-login-synchronous-callback callback)
  ;;     (special-mode)))
  )

(defun kubectl--aws-okta-setenv (proc)
  (let* ((process-buffer (process-buffer proc))
         (process-buffer-contents (with-current-buffer process-buffer (buffer-substring-no-properties (point-min) (point-max))))
         (env-lines (--map (s-replace "export " "" it) (s-split "\n" process-buffer-contents)))
         (env-values (--map (s-split "=" it) (--filter (s-matches? "AWS_" it) env-lines))))
    (--each env-values (setenv (car it) (cadr it)))))

(defun kubectl--aws-okta-process-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc)))
            (inhibit-read-only t)
            (mfa nil))
        (when (s-match "Requesting MFA" string)
          (process-send-string proc (format "%s\n" (read-string "please press your yubbykey for aws-okta: ")))
          (message "sending yubbykey to aws-okta"))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(provide 'kubectl-aws-okta)

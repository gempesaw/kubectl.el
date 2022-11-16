(require 'bpr)

(defvar kubectl-process-buffer-name "*kubectl-process*")

(defun kubectl--get-process-buffer ()
  (let ((buffer (get-buffer-create kubectl-process-buffer-name)))
    (with-current-buffer buffer
      (kubectl-command-mode))
    buffer))

(defun kubectl--update-process-buffer (process &optional inhibit-redraw)
  (let ((buffer (process-buffer process))
        (contents nil))
    (with-current-buffer buffer
      (setq contents (buffer-substring-no-properties (point-min) (point-max))))
    (kubectl--update-process-buffer-string contents)
    (when (not inhibit-redraw)
      (kubectl-redraw contents))
    (kill-buffer buffer)))

(defun kubectl--update-process-buffer-string (string &optional header)
  (with-current-buffer (kubectl--get-process-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (if header
                  (format "---\n\n%s\n\n" string)
                string)))))

(defun kubectl--run-process (command)
  (let* ((bpr-on-completion 'kubectl--update-process-buffer)
         (bpr-open-after-error nil))
    (kubectl--update-process-buffer-string command t)
    (bpr-spawn (kubectl--set-options command))))

(defun kubectl--run-process-bg (command on-success)
  (let* ((bpr-show-progress nil)
         (bpr-on-success on-success)
         (bpr-open-after-error nil))
    (bpr-spawn (kubectl--set-options command))))

(defun kubectl--run-process-and-pop (command &optional editing)
  (let* ((bpr-on-completion 'kubectl--pop-process)
         (bpr-process-mode (if editing 'kubectl-edit-mode 'kubectl-command-mode))
         (bpr-erase-process-buffer t)
         (proc (bpr-spawn (kubectl--set-options command))))
    (kubectl--update-process-buffer-string command t)))

(defun kubectl--pop-process (process)
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (pop-to-buffer buffer)
        (goto-char (point-min))))))

(defun kubectl--set-options (command)
  (if (s-starts-with? "kubectl " command)
      (s-replace "kubectl " (format "kubectl %s " kubectl--command-options) command)
    command))

;; (defun kubectl--make-proxy-process-current ()
;;   (kubectl--make-proxy-process kubectl-current-aws-profile kubectl-current-context))

;; (defun kubectl--make-proxy-process (aws-profile context)
;;   (let ((buffer "*kubectl-proxy-process*")
;;         ;; (port (kubectl--find-random-port))
;;         )
;;     (setq kubectl--command-options (format " --server http://127.0.0.1:42042 " port))
;;     ;; (message "connecting to %s as %s, proxying port %s" context aws-profile port)
;;     ;; (when (get-buffer buffer)
;;     ;;   (kill-buffer buffer))
;;     ;; (let* ((process-name context)
;;     ;;        (final-command (s-split " " (format "aws-okta exec %s --mfa-duo-device token -- kubectl --context=%s -v 4 proxy -p %s" aws-profile context port)))
;;     ;;        (proc (make-process
;;     ;;               :name process-name
;;     ;;               :connection-type 'pipe
;;     ;;               :buffer buffer
;;     ;;               :coding 'no-conversion
;;     ;;               :command final-command
;;     ;;               :filter 'kubectl--make-proxy-filter
;;     ;;               ;; :sentinel
;;     ;;               :stderr buffer
;;     ;;               :noquery t)))
;;     ;;   (with-current-buffer (get-buffer buffer) (special-mode))
;;     ;;   proc))
;;     ))

;; (defun kubectl--make-proxy-filter (proc string)
;;   (when (buffer-live-p (process-buffer proc))
;;     (with-current-buffer (process-buffer proc)
;;       (let ((moving (= (point) (process-mark proc)))
;;             (inhibit-read-only t)
;;             (mfa nil))
;;         (when (s-match "Starting to serve" string)
;;           (message "connection is open")
;;           (kubectl-update-context kubectl-current-context))
;;         (save-excursion
;;           (goto-char (process-mark proc))
;;           (insert string)
;;           (set-marker (process-mark proc) (point)))
;;         (if moving (goto-char (process-mark proc)))))))

;; (defun kubectl--find-random-port ()
;;   (let ((process (make-network-process :name " *kubectl.el-find-random-port*"
;;                                        :family 'ipv4
;;                                        :service 0
;;                                        :server 't)))
;;     (prog1 (process-contact process :service)
;;       (delete-process process))))

(provide 'kubectl-process)

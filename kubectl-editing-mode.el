;; -*- lexical-binding: t -*-

(defvar-local kubectl-edit--resource "")
(defvar kubectl-edit--edit-buffer)

(define-derived-mode kubectl-edit-mode yaml-mode "kubectl-edit"
  (define-key kubectl-edit-mode-map (kbd "C-c C-g") 'kubectl-edit-refresh)
  (define-key kubectl-edit-mode-map (kbd "C-c C-c") 'kubectl-edit-apply)
  (define-key kubectl-edit-mode-map (kbd "M-s") 'kubectl-edit-chide)
  (define-key kubectl-edit-mode-map (kbd "C-c C-k") 'kubectl-edit-cancel)
  )

(defun kubectl-edit-refresh ()
  (interactive)
  (message kubectl-edit-command)
  (kubectl--run-command kubectl-command-command t))

(defun kubectl-edit-resource-at-point ()
  (interactive)
  (let* ((resource-at-point (car (s-split " " (substring-no-properties (current-line-contents))))))
    (kubectl--run-command (format "kubectl get %s --output yaml" resource-at-point) t)))

(setq lexical-binding t)
(defun kubectl-edit-apply ()
  (interactive)
  (when (y-or-n-p (format "Confirm apply (cluster: %s | context: %s | namespace: %s) ?"
                          kubectl-current-cluster
                          kubectl-current-context
                          kubectl-current-namespace))
    (message "applying resource...")
    (let* ((command "kubectl apply -f -")
           (proc-name (s-replace " " "." command))
           (original-buffer (current-buffer))
           (apply-buffer (get-buffer-create "kubectl-edit-apply-buffer"))
           (new-resource (buffer-substring-no-properties (point-min) (point-max)))
           (proc (start-process-shell-command proc-name apply-buffer command)))
      (when (process-live-p proc)
        (setq kubectl-edit--edit-buffer (current-buffer))
        (set-process-sentinel proc 'kubectl-edit--process-sentinel)
        (process-send-string proc new-resource)
        (process-send-string proc "\n")
        (process-send-eof proc)))))

(defun kubectl-edit-cancel ()
  (interactive)
  (quit-window t))

(defun kubectl-edit-chide ()
  (interactive)
  (message "Don't use M-s in this buffer. Use C-c C-c to apply or C-c C-k to cancel."))

(defun kubectl-edit--process-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (let* ((output (buffer-substring-no-properties (point-min) (point-max))))
      (message (format "applying finished: %s" output))
      (kill-buffer (current-buffer))
      (when (s-matches-p "configured" output)
        (kill-buffer kubectl-edit--edit-buffer)))))

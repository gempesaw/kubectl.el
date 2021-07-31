(require 'bpr)
(require 'kubectl-process)

(defvar-local kubectl-edit--resource "")
(defvar kubectl-edit--edit-buffer)

(define-derived-mode kubectl-edit-mode yaml-mode "kubectl-edit"
  (define-key kubectl-edit-mode-map (kbd "C-c C-c") 'kubectl-edit-apply)
  (define-key kubectl-edit-mode-map (kbd "M-s") 'kubectl-edit-chide)
  (define-key kubectl-edit-mode-map (kbd "C-c C-k") 'kubectl-edit-cancel))

(defun kubectl-edit-resource-at-point ()
  (interactive)
  (let ((resource-at-point (car (s-split " " (substring-no-properties (current-line-contents))))))
    (kubectl--run-process-and-pop (format "kubectl get %s --output yaml" resource-at-point) t)))

(defun kubectl-edit-apply ()
  (interactive)
  (when (y-or-n-p (format "Confirm apply (cluster: %s | context: %s | namespace: %s) ?"
                          kubectl-current-cluster
                          kubectl-current-context
                          kubectl-current-namespace))
    (let* ((new-resource (buffer-substring-no-properties (point-min) (point-max)))
           (cmd-from-buffer (s-chop-prefix "*" (s-trim (car (s-split "--" (buffer-name))))))
           (filename (make-temp-file (s-replace "/" "." (car (--filter (s-matches-p "/" it) (s-split " " cmd-from-buffer)))) nil nil new-resource))
           (command (format "kubectl apply -f %s" filename))
           (bpr-on-success 'kubectl-edit--cleanup)
           (bpr-process-mode 'kubectl-command-mode))
      (setq kubectl-edit--edit-buffer (current-buffer))
      (kubectl--update-process-buffer-string command t)
      (bpr-spawn command))))

(defun kubectl-edit-cancel ()
  (interactive)
  (quit-window t))

(defun kubectl-edit-chide ()
  (interactive)
  (message "Don't use M-s in this buffer. Use C-c C-c to apply or C-c C-k to cancel."))

(defun kubectl-edit--cleanup (process)
  (kubectl--update-process-buffer process t)
  (kill-buffer kubectl-edit--edit-buffer)
  (delete-window))

(provide 'kubectl-edit-mode)

(require 'bpr)
(require 'kubectl-process)
(require 'kubectl-get-resources)

(defvar kubectl-edit--edit-buffer)

(defvar kubectl-edit--folder (f-expand (f-join kubectl--my-directory "tmp")))
(defvar kubectl-edit--current-resource nil)

(define-derived-mode kubectl-edit-mode yaml-mode "kubectl-edit"
  (define-key kubectl-edit-mode-map (kbd "C-c C-c") 'kubectl-edit-apply)
  (define-key kubectl-edit-mode-map (kbd "M-s") 'kubectl-edit-chide)
  (define-key kubectl-edit-mode-map (kbd "C-c C-k") 'kubectl-edit-cancel))



(defun kubectl-edit-resource-at-point ()
  (interactive)
  (setq kubectl-edit--current-resource (s-trim (kubectl-current-line-resource-as-string)))
  (let* ((yaml (shell-command-to-string (format "kubectl get %s --output yaml" (kubectl-current-line-resource-as-string))))
         (name (apply 'format "%s/%s-%s.yaml" (-insert-at 1 (format-time-string "%s" (current-time)) (s-split "/" kubectl-edit--current-resource))))
         (filename (f-join kubectl-edit--folder name)))
    (f-mkdir (f-dirname filename))
    (f-write-text yaml 'utf-8 filename)
    (find-file-other-window filename)
    (kubectl-edit-mode)))

(defun kubectl-edit-apply ()
  (interactive)
  (when (y-or-n-p (format "Confirm apply (cluster: %s | context: %s | namespace: %s) ?"
                          kubectl-current-cluster
                          kubectl-current-context
                          kubectl-current-namespace))
    (let* ((command (format "kubectl apply -f %s" buffer-file-name))
           (bpr-on-success 'kubectl-edit--cleanup)
           (bpr-process-mode 'kubectl-command-mode))
      (save-buffer)
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

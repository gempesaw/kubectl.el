(defun kubectl-copy-resource-at-point ()
  (interactive)
  (let ((resource-at-point (s-trim (kubectl-current-line-resource-as-string))))
    (kill-new resource-at-point)
    (message resource-at-point)))

(defun current-line-contents ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun kubectl-shell-at-point ()
  (interactive)
  (let* ((current-line-resource-name (car (s-split " " (substring-no-properties (current-line-contents)))))
         (current-line-resource-kind (car (s-split "/" current-line-resource-name))))
    (if (s-equals-p current-line-resource-kind "pod")
        (kubectl--pod-exec current-line-resource-name)
      (kubectl--node-debug current-line-resource-name))))

(defun kubectl--pod-exec (current-line-resource-name)
  (interactive)
  (kubectl--open-shell-with-command (format "pk shell %s" (s-join "-" (-take-last 2 (s-split "-" current-line-resource-name))))))

(defun kubectl--node-debug (current-line-resource-name)
  (interactive)
  (let ((node-ip-string (cadr (s-split "/\\|\\." current-line-resource-name))))
    (kubectl--open-shell-with-command
     (format "kubectl debug --namespace kube-system node/%s -it --image=public.ecr.aws/lts/ubuntu:20.04_stable --attach=false && pk ns kube-system && pk shell %s"
             current-line-resource-name node-ip-string))))

(defun kubectl-pod-logs ()
  (interactive)
  (let ((thing-at-point (car (s-split " " (substring-no-properties (current-line-contents))))))
    (kubectl--open-shell-with-command (format "kubectl logs --tail=50 -f %s" thing-at-point))))

(defun kubectl--open-shell-with-command (command)
  (interactive)
  (let* ((buf nil))
    (setq buf (create-new-shell-here))
    (select-window (display-buffer buf))
    (goto-char (point-max))
    (insert command)
    (comint-send-input)))

(defun kubectl-port-forward (port)
  (interactive "sPort to forward: ")
  (let* ((cmd (format "kubectl port-forward %s %s" (kubectl-current-line-resource-as-string) port)))
    (message cmd)
    (async-shell-command cmd)))

(defun kubectl-get-yaml-at-point ()
  (interactive)
  (kubectl--run-process-and-pop (format "kubectl get %s --output yaml" (kubectl-current-line-resource-as-string))))

(defun kubectl-delete-resource-at-point ()
  (interactive)
  (let* ((resource-at-point (kubectl-current-line-resource-as-string))
         (prompt (format "Confirm DELETE %s (cluster: %s | context: %s | namespace: %s) ?"
                         resource-at-point
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace))
         (bpr-process-mode 'kubectl-command-mode))
    (when (y-or-n-p prompt)
      (bpr-spawn (format "kubectl delete %s" resource-at-point)))))

(provide 'kubectl-at-point)

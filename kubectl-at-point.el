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

(defun kubectl-debug-at-point ()
  (interactive)
  (let* ((current-line-resource-name (car (s-split " " (substring-no-properties (current-line-contents)))))
         (current-line-resource-kind (car (s-split "/" current-line-resource-name))))
    (if (s-equals-p current-line-resource-kind "pod")
        (kubectl--pod-exec current-line-resource-name "debug")
      (kubectl--node-debug current-line-resource-name))))

(defun kubectl--pod-exec (current-line-resource-name &optional command)
  (interactive)
  (let ((cmd (if command
                 command
               "shell")))
    (kubectl--open-shell-with-command (format "pk %s %s" cmd (s-right 5 current-line-resource-name)))))

(defun kubectl--node-debug (current-line-resource-name)
  (interactive)
  (let ((node-ip-string (cadr (s-split "/\\|\\." current-line-resource-name))))
    (kubectl--open-shell-with-command
     (format "kubectl debug --namespace kube-system %s --stdin --tty --image=748801462010.dkr.ecr.us-west-1.amazonaws.com/pk-debug:nonroot"
             current-line-resource-name node-ip-string))))

(defun kubectl-pod-logs ()
  (interactive)
  (let ((has-one-container (s-contains? "/1 " (current-line-contents)))
        (pod-at-point (car (s-split " " (substring-no-properties (current-line-contents))))))
    (if has-one-container
        (kubectl--open-shell-with-command (format "kubectl logs --tail=50 -f %s" pod-at-point))
      (let ((container (completing-read
                        "choose a container"
                        (->> (format "kubectl get %s -ojson | jq -r  '.spec.containers[].name'" pod-at-point)
                             (shell-command-to-string)
                             (s-trim)
                             (s-split "\n"))
                        nil
                        t)))
        (kubectl--open-shell-with-command (format "kubectl logs --tail=50 -f %s --container=%s" pod-at-point container)))
      )
    ))

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
         (default-directory kubectl--my-directory)
         (bpr-process-mode 'kubectl-command-mode))
    (when (y-or-n-p prompt)
      (bpr-spawn (format "kubectl delete %s" resource-at-point)))))

(defun kubectl-unmark-last-applied-configuration-at-point ()
  (interactive)
  (let* ((resource-at-point (kubectl-current-line-resource-as-string))
         (annotation "kubectl.kubernetes.io/last-applied-configuration")
         (prompt (format "Confirm REMOVE annotation %s %s- (cluster: %s | context: %s | namespace: %s) ?"
                         resource-at-point
                         annotation
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace))
         (default-directory kubectl--my-directory)
         (bpr-process-mode 'kubectl-command-mode))
    (when (y-or-n-p prompt)
      (message "saving a copy first")
      (let* ((yaml (shell-command-to-string (format "kubectl get %s --output yaml" resource-at-point)))
             (name (apply 'format "%s/%s-%s.yaml" (-insert-at 1 (format-time-string "%s" (current-time)) (s-split "/" kubectl-edit--current-resource))))
             (filename (f-join kubectl-edit--folder name)))
        (f-mkdir (f-dirname filename))
        (f-write-text yaml 'utf-8 filename))
      (bpr-spawn (format "kubectl annotate %s %s-" resource-at-point annotation)))))

(defun kubectl-restart-workload-at-point ()
  (interactive)
  (let* ((resource-at-point (kubectl-current-line-resource-as-string))
         (current-line-resource-kind (car (s-split "/" resource-at-point)))
         (restart-command (if (s-contains-p "rollout" current-line-resource-kind)
                              (format "kubectl patch %s -p '{\"spec\":{\"restartAt\":\"%s\"}}' --type merge"
                                      resource-at-point
                                      (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil "UTC"))
                            (format "kubectl rollout restart %s" resource-at-point)))
         (prompt (format "Confirm restart workload %s (%s) (cluster: %s | context: %s | namespace: %s) ?"
                         resource-at-point
                         restart-command
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace))
         (default-directory kubectl--my-directory)
         (bpr-process-mode 'kubectl-command-mode))
    (when (y-or-n-p prompt)
      (bpr-spawn restart-command))))

(defun kubectl-cordon-nodes-at-point ()
  (interactive)
  (let* ((nodes (kubectl-get-resources-at-point-or-region))
         (command (format "kubectl cordon %s" (s-join " " nodes)))
         (prompt (format "Confirm cordon %s nodes (cluster: %s | context: %s | namespace: %s) %s?"
                         (length nodes)
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace
                         command)))
    (when (y-or-n-p prompt)
      (bpr-spawn command))
    ))

(defun kubectl-drain-nodes-at-point ()
  (interactive)
  (let* ((nodes (kubectl-get-resources-at-point-or-region))
         (commands (->> nodes
                        (--map (format "kubectl drain --ignore-daemonsets --delete-emptydir-data %s" it))))
         (prompt (format "Confirm drain %s nodes (cluster: %s | context: %s | namespace: %s)? %s"
                         (length nodes)
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace
                         (car commands)
                         )))
    (when (y-or-n-p prompt)
      (--map (kubectl--run-process-bg it) commands))
    ))

(defun kubectl-run-cronjob-at-point ()
  (interactive)
  (let* ((resources (kubectl-get-resources-at-point-or-region))
         (commands (->> resources
                        (--map (format "kubectl create job --from=%s %s-trigger-dgempesaw-%s" it (cadr (s-split "/" it)) (floor (float-time))))))
         (prompt (format "Confirm create job (cluster: %s | context: %s | namespace: %s)? %s"
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace
                         (car commands)
                         )))
    (when (y-or-n-p prompt)
      (--map (kubectl--run-process-bg it) commands))))

(defun kubectl-get-resources-at-point-or-region ()
  (if (region-active-p)
      (->> (buffer-substring-no-properties (region-beginning) (region-end))
           (s-split "\n")
           (-map 'kubectl-line-resource-as-string))
    `(,(kubectl-current-line-resource-as-string))))

(provide 'kubectl-at-point)

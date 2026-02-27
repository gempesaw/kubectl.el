;;; -*- lexical-binding: t; -*-

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
         (current-line-resource-kind (car (s-split "/" current-line-resource-name)))
         (current-namespace kubectl-current-namespace))
    (if (s-equals-p current-line-resource-kind "pod")
        (kubectl--pod-exec current-line-resource-name current-namespace)
      (kubectl--node-debug current-line-resource-name))))

(defun kubectl-debug-at-point ()
  (interactive)
  (let* ((current-line-resource-name (car (s-split " " (substring-no-properties (current-line-contents)))))
         (current-line-resource-kind (car (s-split "/" current-line-resource-name))))
    (if (s-equals-p current-line-resource-kind "pod")
        (kubectl--pod-debug current-line-resource-name)
      (kubectl--node-debug current-line-resource-name))))

(defun kubectl--pod-exec (current-line-resource-name current-namespace &optional command)
  (interactive)
  (let ((cmd (if command
                 command
               "sh")))
    (kubectl--open-shell-with-command (format "kubectl --namespace %s exec -it %s -- %s" current-namespace current-line-resource-name cmd))))

(defun kubectl--pod-debug (current-line-resource-name)
  (interactive)
  (kubectl--open-shell-with-command (format "kubectl debug %s  --stdin --tty --image=public.ecr.aws/docker/library/alpine:3.20" current-line-resource-name)))

(defun kubectl--node-debug (current-line-resource-name)
  (interactive)
  (let ((node-ip-string (cadr (s-split "/\\|\\." current-line-resource-name))))
    (kubectl--open-shell-with-command
     (format "kubectl debug %s --stdin --tty --image=public.ecr.aws/docker/library/alpine:3.20"
             current-line-resource-name node-ip-string))))

(defun kubectl--get-pod-containers (pod)
  (->> (format "kubectl get %s -ojson | jq -r  '.spec.initContainers + .spec.containers | .[] | .name'" pod)
       (shell-command-to-string)
       (s-trim)
       (s-split "\n")))

(defun kubectl--choose-container (pod &optional optional-containers)
  (let ((containers (if optional-containers
                        optional-containers
                      (kubectl--get-pod-containers pod))))
    (completing-read "choose a container" containers nil nil)))

(defun kubectl-pod-logs ()
  (interactive)
  (let* ((default-directory kubectl--my-directory)
         (pod-at-point (car (s-split " " (substring-no-properties (current-line-contents)))))
         (containers (kubectl--get-pod-containers pod-at-point)))
    (if (= 1 (length containers))
        (kubectl--open-shell-with-command (format "kubectl logs --tail=50 -f %s" pod-at-point))
      (let ((container (kubectl--choose-container pod-at-point containers)))
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

(defun kubectl-port-forward ()
  (interactive)
  (let ((ports (->> (format "kubectl get %s -ojson | jq -r  '.spec.containers[].ports[].containerPort'" (kubectl-current-line-resource-as-string))
                    (shell-command-to-string)
                    (s-trim)
                    (s-split "\n"))))
    (let* ((port (completing-read "choose a port to forward: " ports nil nil nil t))
           (local-port (if (s-equals-p "80" port) "8080" port))
           (cmd (format "kubectl port-forward %s %s:%s"
                        (kubectl-current-line-resource-as-string)
                        local-port
                        port)))
      (message cmd)
      (async-shell-command cmd)
      (browse-url (format "http://localhost:%s" local-port)))))

(defun kubectl-get-yaml-at-point ()
  (interactive)
  (kubectl--run-process-and-pop (format "kubectl get %s --output yaml" (kubectl-current-line-resource-as-string))))

(defun kubectl-act-on-point-or-region (prompt action-fn)
  (let* ((resource-at-point (kubectl-current-line-resource-as-string))
         (resources-at-point (if (region-active-p)
                                 (--> (buffer-substring-no-properties (region-beginning) (region-end))
                                      (s-split "\n" it t)
                                      (-map 'kubectl-line-resource-as-string it))
                               (list resource-at-point)))
         (default-directory kubectl--my-directory)
         (bpr-process-mode 'kubectl-command-mode)
         (prompt-with-resources (format "%s\n\n%s\n\n" prompt (s-join "\n" resources-at-point))))
    (when (y-or-n-p prompt-with-resources)
      (--map (apply action-fn (list it)) resources-at-point))))

(defun kubectl-delete-resource-at-point ()
  (interactive)
  (let* ((prompt (format "Confirm DELETE (cluster: %s | context: %s | namespace: %s)?"
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace)))
    (kubectl-act-on-point-or-region prompt (lambda (resource) (bpr-spawn (format "kubectl delete %s" resource))))))

(defun kubectl-force-delete-resource-at-point ()
  (interactive)
  (let* ((prompt (format "Confirm FORCE DELETE (cluster: %s | context: %s | namespace: %s)?"
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace)))
    (kubectl-act-on-point-or-region prompt (lambda (resource) (bpr-spawn (format "kubectl delete --force %s" resource))))))


(defun kubectl-scale-workload-at-point ()
  (interactive)
  (let* ((replicas (read-from-minibuffer "Number of replicas to scale to: "))
         (prompt (format "Confirm scale: %s replicas (cluster: %s | context: %s | namespace: %s)?"
                         replicas
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace)))
    (kubectl-act-on-point-or-region
     prompt
     (lambda (resource)
       (let ((command (if (s-contains-p "autoscalingrunnerset" resource)
                          (format "TIME=\"%s\" kubectl patch %s --type=merge -p '{\"spec\":{\"minRunners\":%s}}'" (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil "UTC") resource replicas)
                        (format "TIME=\"%s\" kubectl scale --replicas=%s %s" (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil "UTC") replicas resource))))
         (bpr-spawn command))))))

(defun kubectl-unmark-last-applied-configuration-at-point ()
  (interactive)
  (let* ((annotation "kubectl.kubernetes.io/last-applied-configuration")
         (prompt (format "Confirm REMOVE annotation (cluster: %s | context: %s | namespace: %s)?\n    %s-"
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace
                         annotation)))
    (kubectl-act-on-point-or-region
     prompt
     (lambda (resource-at-point)
       (message (format "%s: saving a copy first..." resource-at-point))
       (let* ((yaml (shell-command-to-string (format "kubectl get %s --output yaml" resource-at-point)))
              (name (apply 'format "%s/%s-%s.yaml" (-insert-at 1 (format-time-string "%s" (current-time)) (s-split "/" kubectl-edit--current-resource))))
              (filename (f-join kubectl-edit--folder name)))
         (f-mkdir (f-dirname filename))
         (f-write-text yaml 'utf-8 filename))
       (bpr-spawn (format "kubectl annotate %s %s-" resource-at-point annotation))))))

(defun kubectl-remove-finalizers ()
  (interactive)
  (let* ((prompt (format "Confirm REMOVE finalizer (cluster: %s | context: %s | namespace: %s)?\n"
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace)))
    (kubectl-act-on-point-or-region
     prompt
     (lambda (resource-at-point)
       (message (format "%s: saving a copy first..." resource-at-point))
       (let* ((yaml (shell-command-to-string (format "kubectl get %s --output yaml" resource-at-point)))
              (name (apply 'format "%s/%s-%s.yaml" (-insert-at 1 (format-time-string "%s" (current-time)) (s-split "/" kubectl-edit--current-resource))))
              (filename (f-join kubectl-edit--folder name)))
         (f-mkdir (f-dirname filename))
         (f-write-text yaml 'utf-8 filename))
       (bpr-spawn (format "kubectl patch %s -p '{\"metadata\":{\"finalizers\":[]}}' --type=merge" resource-at-point))))))

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

(defun kubectl-open-grafana-workload-at-point (&optional context)
  (interactive)
  (let* ((resource-at-point (kubectl-current-line-resource-as-string))
         (current-line-resource-kind (car (s-split "/" resource-at-point)))
         (query-parameters (->> `(
                                  (refresh "10s")
                                  (var-namespace ,kubectl-current-namespace)
                                  (var-type ,(car (s-split "\\." current-line-resource-kind)))
                                  (var-workload ,(cadr (s-split "/" resource-at-point)))
                                  (var-pod ,(cadr (s-split "/" resource-at-point)))
                                  )
                                (--map (format "%s=%s" (car it) (cadr it)))
                                (s-join "&")))
         (grafana-url (format "http://kps-grafana.kube-prometheus-stack.svc.%s.local" (if context context kubectl-current-context)))
         (path (cond
                ((s-equals-p "pod" current-line-resource-kind) (s-replace "%" "%%" "d/6581e46e4e5c7ba40a07646395ef7b23/kubernetes-compute-resources-pod"))
                ((s-equals-p "service" current-line-resource-kind) (setq grafana-url (format "http://%s.%s.svc.%s.local"
                                                                                             (cadr (s-split "/" resource-at-point))
                                                                                             kubectl-current-namespace
                                                                                             (if context context kubectl-current-context))
                                                                         path ""
                                                                         query-parameters ""))
                (t (s-replace "%" "%%" "d/a164a7f0339f99e89cea5cb47e9be617/kubernetes-%2f-compute-resources-%2f-workload")))))
    (browse-url (format "%s/%s?%s" grafana-url path query-parameters))))

(defun kubectl-open-grafana-workload-at-point-all-clusters ()
  (interactive)
  (->> kubectl-available-contexts
       (--map (kubectl-open-grafana-workload-at-point (cadr (s-split "/" it))))))



(defun kubectl-cordon-nodes-at-point ()
  (interactive)
  (let* ((nodes (kubectl-get-resources-at-point-or-region))
         (command (format "kubectl cordon %s" (s-join " " nodes)))
         (default-directory kubectl--my-directory)
         (prompt (format "Confirm cordon %s nodes (cluster: %s | context: %s | namespace: %s) %s?"
                         (length nodes)
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace
                         command)))
    (when (y-or-n-p prompt)
      (bpr-spawn command))
    ))

(defun kubectl-uncordon-nodes-at-point ()
  (interactive)
  (let* ((nodes (kubectl-get-resources-at-point-or-region))
         (command (format "kubectl uncordon %s" (s-join " " nodes)))
         (default-directory kubectl--my-directory)
         (prompt (format "Confirm uncordon %s nodes (cluster: %s | context: %s | namespace: %s) %s?"
                         (length nodes)
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace
                         command)))
    (when (y-or-n-p prompt)
      (bpr-spawn command))
    ))

(defun kubectl-view-node-on-line ()
  (interactive)
  (let* ((node-like (s-trim (car (s-match " i-.*?internal" (substring-no-properties (current-line-contents)))))))
    (when node-like
      (kubectl--run-process-and-pop (format "kubectl describe node/%s" node-like)))))


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
                        (--map (format "kubectl create job --from=%s %s" it (s-chop-prefix "-" (s-right 60 (format "%s-trigger-dgempesaw-%s" (cadr (s-split "/" it)) (floor (float-time)))))))))
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

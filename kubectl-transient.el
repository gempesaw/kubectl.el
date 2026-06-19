;; -*- lexical-binding: t; -*-


(require 'transient)
(require 'ht)
(require 'kubectl-aws)

(defvar kubectl-previous-namespace "")


(transient-define-prefix kubectl-transient-help ()
  "Available kubectl actions"
  ["Set contexts"
   [("R" "resource" kubectl-transient-choose-resource)]
   [("N" "namespace" kubectl-choose-namespace)]
   [("C" "context" kubectl-transient-choose-context)]]
  ["Resource at point"
   [("e" "edit" kubectl-edit-resource-at-point)
    ("k" "delete" kubectl-delete-resource-at-point)]
   [("o" "output yaml" kubectl-get-yaml-at-point)
    ("<RET>" "describe" kubectl-describe-resource-at-point)
    ("u" "remove annotation" kubectl-unmark-last-applied-configuration-at-point)]]
  ["Pod at point"
   [("x" "open a shell" kubectl--pod-exec)]
   [("l" "view logs" kubectl-pod-logs)]]
  ["utility"
   [("g" "refresh" kubectl-init)]
   [("$" "show log buffer" kubectl-show-log-buffer)]
   [(":" "run custom command" kubectl-run-custom-command)]])

(transient-define-prefix kubectl-transient-action-at-point ()
  "do something to the resource at point"
  ["all"
   ("K K" "delete resource" kubectl-delete-resource-at-point)
   ("K F F" "delete resource" kubectl-force-delete-resource-at-point)
   ("K f" "remove finalizers:" kubectl-remove-finalizers)
   ("K B" "bulk delete pods by status" kubectl-bulk-delete-pods-by-status)
   ("e" "events for resource (posframe)" kubectl-show-events-at-point)
   ("u" "remove annotation: remove last-applied-configuration annotation" kubectl-unmark-last-applied-configuration-at-point)
   ("p" "pop to create buffer" kubectl-pop-to-create-resource-buffer)]

  ["workloads"
   ("s" "scale workload" kubectl-scale-workload-at-point)
   ("r" "restart workload" kubectl-restart-workload-at-point)
   ("j" "create job from cronjob/job" kubectl-run-cronjob-at-point)
   ("g" "open in grafana" kubectl-open-grafana-workload-at-point)
   ("G" "open in grafana (all clusters)" kubectl-open-grafana-workload-at-point-all-clusters)
   ]

  ["nodes"
   ("n n" "view node on line" kubectl-view-node-on-line)
   ("n w" "copy node name" kubectl-copy-as-kill-node-on-line)
   ("n p" "pods on node (posframe)" kubectl-show-pods-on-node)
   ("n c" "cordon nodes" kubectl-cordon-nodes-at-point)
   ("n u" "uncordon nodes" kubectl-uncordon-nodes-at-point)
   ("n d" "drain nodes" kubectl-drain-nodes-at-point)]
  )

(transient-define-prefix kubectl-transient-choose-resource ()
  "Choose resources to query for"

  [
   "In a single namespace"
   [
    ("r" (lambda () (format "Reset to default (%s)" kubectl-resources-default)) kubectl-reset-resources)
    ("c" (lambda () (format "add to Current (%s)" kubectl-resources-current)) kubectl-add-current-resource)
    ("s" "Specify your own list" kubectl-set-resource)
    ]
   ]

  ["Presets"
   [
    ("a" (lambda () (format "all (%s)" kubectl-resources-default)) kubectl-reset-resources)
    ("b" "rBac (roles,rolebindings)" kubectl-set-resources-rbac)
    ("B" "rBac (clusterroles,clusterrolebindings)" kubectl-set-resources-cluster-rbac)
    ("k" "karpenter" kubectl-set-resources-karpenter)
    ("K" "karpenter CRDs" kubectl-set-resources-karpenter-crds)
    ("j" "jobs (cronjobs,jobs,pods)" kubectl-set-resources-jobs)
    ("e" "externalsecrets (clusterexternalsecrets,clustersecretstores,externalsecrets,secretstores,secrets)" kubectl-set-resources-secrets)
    ("g" "Github" kubectl-set-resources-github)
    ("v" "volumes" kubectl-set-resources-volumes)

    ("p" "all no pods (ds,sts,deploy,svc,ing,cm)" kubectl-set-resources-all-no-pods)
    ]
   ]
  )

(transient-define-prefix kubectl-transient-choose-resource-all-ns ()
  "Choose resources to query for in all namespaces"
  ["All namespaces"
   ("c" (lambda () (format "add to Current (%s)" kubectl-resources-current-all-ns)) kubectl-add-current-resource-all-ns)
   ("s" "Specify your own list" kubectl-set-resource-all-ns)
   ("r" "Refresh / use current" (lambda (&optional args)
                                  (interactive)
                                  (setq kubectl-all-namespaces t
                                        kubectl-previous-namespace kubectl-current-namespace
                                        )
                                  (kubectl-get-resources)))])

(defun kubectl--extract-account-id-from-arn (arn)
  "Extract AWS account ID from an EKS cluster ARN.
ARN format: arn:aws:eks:REGION:ACCOUNT_ID:cluster/NAME"
  (when (s-matches-p "^arn:aws:eks:" arn)
    (nth 4 (s-split ":" arn))))

(defun kubectl--extract-account-id-from-cluster (context)
  "Extract AWS account ID from the cluster ARN for CONTEXT.
Returns the 12-digit account ID or nil if not found.
Tries context name, cluster reference, and cluster name for ARN."
  (or (kubectl--extract-account-id-from-arn context)
      (let* ((cluster-ref (s-trim (shell-command-to-string
                                   (format "kubectl config view -o jsonpath='{.contexts[?(@.name==\"%s\")].context.cluster}'" context)))))
        (or (kubectl--extract-account-id-from-arn cluster-ref)
            (let* ((clusters-json (shell-command-to-string "kubectl config view -o json"))
                   (server-url (s-trim (shell-command-to-string
                                        (format "kubectl config view -o jsonpath='{.clusters[?(@.name==\"%s\")].cluster.server}'" cluster-ref)))))
              (when (not (string-empty-p server-url))
                (let ((arn-cluster (s-trim (shell-command-to-string
                                            (format "kubectl config view -o jsonpath='{.clusters[?(@.cluster.server==\"%s\")].name}'" server-url)))))
                  (kubectl--extract-account-id-from-arn arn-cluster))))))))

(defun kubectl--parse-aws-config ()
  "Parse ~/.aws/config and return alist of (account-id . profiles).
Each entry maps an account ID to a list of profile names."
  (let ((config-file (expand-file-name "~/.aws/config"))
        (profiles-by-account (make-hash-table :test 'equal))
        current-profile
        current-account)
    (when (file-exists-p config-file)
      (with-temp-buffer
        (insert-file-contents config-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ((s-matches-p "^\\[profile " line)
              (when (and current-profile current-account)
                (puthash current-account
                         (cons current-profile (gethash current-account profiles-by-account))
                         profiles-by-account))
              (setq current-profile (s-trim (s-chop-suffix "]" (s-chop-prefix "[profile " line)))
                    current-account nil))
             ((s-matches-p "^sso_account_id" line)
              (setq current-account (s-trim (cadr (s-split "=" line)))))))
          (forward-line 1))
        (when (and current-profile current-account)
          (puthash current-account
                   (cons current-profile (gethash current-account profiles-by-account))
                   profiles-by-account))))
    profiles-by-account))

(defun kubectl--select-best-profile (profiles)
  "Select the best profile from PROFILES list.
Priority: super-useri > power-user > kubernetes-admin > read-only."
  (or (--first (s-contains-p "super-user" it) profiles)
      (--first (s-contains-p "power-user" it) profiles)
      (--first (s-contains-p "kubernetes-admin" it) profiles)
      (--first (s-contains-p "read-only" it) profiles)
      (car profiles)))

(defun kubectl--get-aws-role (context)
  "Get the AWS role/profile for CONTEXT by looking up the cluster's account ID."
  (if-let* ((account-id (kubectl--extract-account-id-from-cluster context))
            (profiles-by-account (kubectl--parse-aws-config))
            (profiles (gethash account-id profiles-by-account)))
      (kubectl--select-best-profile profiles)
    nil))


(transient-define-prefix kubectl-transient-choose-context ()
  "Choose cluster and AWS profile alias"

  ["Options"
   ("c" "Kubernetes context" "c="
    :always-read t
    :init-value (lambda (ob)
                  (setf (slot-value ob 'value) kubectl-current-context))
    :reader (lambda (prompt initial-input history)
              (kubectl--refresh-available-contexts)
              (completing-read prompt (kubectl--get-available-contexts) nil nil initial-input history)))
   ("n" "Namespace" "ns="
    :always-read t
    :init-value (lambda (ob)
                  (setf (slot-value ob 'value) kubectl-current-namespace))
    :reader (lambda (prompt initial-input history)
              (completing-read prompt kubectl-cached-namespaces nil nil initial-input history)))
   ("r" "Resources" "r="
    :always-read t
    ;; :multi-value t
    :init-value (lambda (ob)
                  (setf (slot-value ob 'value) kubectl-resources-current))
    :reader (lambda (prompt initial-input history)
              (s-join "," (completing-read-multiple prompt kubectl-api-resource-names nil nil initial-input history))))
   ]
  ["Connect"
   [("SPC" "Connect"
     (lambda (&optional args)
       (interactive (list (transient-args transient-current-command)))
       (let* ((context (transient-arg-value "c=" args))
              (namespace (transient-arg-value "ns=" args))
              (resources (transient-arg-value "r=" args))
              (aws-role (kubectl--get-aws-role context)))
         (kubectl--auth-aws aws-role)
         (setq kubectl-current-context context
               kubectl-current-namespace namespace
               kubectl-resources-current resources
               kubectl-current-role aws-role
               )
         (kubectl--run-process-bg
          (format "kubectx %s && (kubens %s || kubens default)" (if (s-contains-p "/" context) (cadr (s-split "/" context)) context) namespace )
          (lambda (process)
            (kubectl-init)
            (run-at-time 10 nil 'kubectl-get-namespaces)
            (run-at-time 10 nil 'kubectl-get-api-resources)))
         )))
    ]])

(defun kubectl-add-kubeconfig ()
  "Merge kubeconfig from region or kill-ring into ~/.kube/config."
  (interactive)
  (let* ((kubeconfig-content
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-kill 0)))
         (temp-file (make-temp-file "kubeconfig-"))
         (kube-dir (expand-file-name "~/.kube"))
         (config-file (expand-file-name "config" kube-dir)))

    ;; Validate it looks like a kubeconfig
    (unless (string-match-p "apiVersion" kubeconfig-content)
      (error "Content doesn't look like a kubeconfig (no apiVersion found)"))

    ;; Clean up context names - remove "user@" prefix
    (setq kubeconfig-content
          (replace-regexp-in-string "user@" "" kubeconfig-content))

    ;; Create ~/.kube directory if it doesn't exist
    (unless (file-directory-p kube-dir)
      (make-directory kube-dir t))

    ;; Write kubeconfig content to temp file
    (with-temp-file temp-file
      (insert kubeconfig-content))

    ;; Backup existing config if it exists
    (when (file-exists-p config-file)
      (let ((backup-file (expand-file-name
                          (format "config.backup-%s" (format-time-string "%s"))
                          kube-dir)))
        (copy-file config-file backup-file)
        (message "📦 Backed up existing config")))

    ;; Merge configs using kubectl
    (let* ((kubeconfig-env (if (file-exists-p config-file)
                               (format "KUBECONFIG=%s:%s" config-file temp-file)
                             (format "KUBECONFIG=%s" temp-file)))
           (merge-command (format "%s kubectl config view --merge --flatten --raw" kubeconfig-env))
           (new-config-file (concat config-file ".new")))

      ;; Run kubectl merge and capture output
      (with-temp-buffer
        (let ((exit-code (call-process-shell-command merge-command nil t nil)))
          (if (= exit-code 0)
              (progn
                ;; Write merged config to new file
                (write-region (point-min) (point-max) new-config-file)
                ;; Replace original config
                (rename-file new-config-file config-file t)
                (message "✅ Kubeconfig merged successfully!")
                ;; Show contexts
                (shell-command "kubectl config get-contexts | tail -5"))
            (error "Failed to merge kubeconfig: %s" (buffer-string))))))

    ;; Clean up temp file
    (delete-file temp-file)))

(defun kubectl--make-unique-resource-prefixes (resources &optional prefixes length)
  (setq length (if length length 1)
        prefixes (if prefixes prefixes (--map `(,(s-left length it) ,it) resources)))

  (let* ((keys (-map #'car prefixes))
         (frequencies (-frequencies keys) ))

    (if (--every (= 1 it) (-map #'cdr frequencies))
        (--map (list (s-join " " (s-split "" (car it) t)) (cadr it)) prefixes)
      (let ((duplicate-keys (->> frequencies
                                 (--filter (> (cdr it) 1))
                                 (-map #'car ))))
        (setq length (1+ length))
        (kubectl--make-unique-resource-prefixes resources
                                                (->> prefixes (--map (let ((key (nth 0 it))
                                                                           (resource (nth 1 it) ))
                                                                       (if (-contains? duplicate-keys key)
                                                                           (list (s-left length resource) resource)
                                                                         (list key resource)))))
                                                length)))))



(transient-define-prefix kubectl-transient-jump-to-resource ()
  "Jump to a resource section"
  [:class transient-column
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'kubectl-transient--jump-to-resource
             (->> kubectl-current-display
                  (s-split "\n")
                  (--filter (s-matches-p ".*/.*[[:digit:]]" it))
                  (--map (car (s-split "/" it)))
                  (-uniq)
                  (kubectl--make-unique-resource-prefixes)
                  (--map (let ((char (car it))
                               (resource-name (cadr it)))
                           (list char
                                 resource-name
                                 (lambda ()
                                   (interactive)
                                   (goto-char (point-min))
                                   (search-forward (format "%s/" resource-name) nil t 1)
                                   (beginning-of-line))))))))])

(defvar kubectl-resources-current-all-ns "pods")

(defun kubectl-set-current-as-default ()
  (interactive)
  (setq kubectl-resources-default kubectl-resources-current
        kubectl-all-namespaces nil
        kubectl-current-namespace kubectl-previous-namespace)
  (when (s-matches-p kubectl-current-namespace "All ")
    (setq kubectl-current-namespace kubectl-previous-namespace))
  (kubectl-get-resources))

(defun kubectl-reset-resources ()
  (interactive)
  (setq kubectl-resources-current kubectl-resources-default
        kubectl-all-namespaces nil
        kubectl-current-namespace kubectl-previous-namespace)
  (kubectl-get-resources))

(defun kubectl-set-resources-cluster-rbac ()
  (interactive)
  (setq kubectl-resources-current "clusterroles,clusterrolebindings"
        kubectl-all-namespaces nil
        kubectl-current-namespace kubectl-previous-namespace)
  (kubectl-get-resources))

(defun kubectl-set-resources-rbac ()
  (interactive)
  (setq kubectl-resources-current "roles,rolebindings,sa"
        kubectl-all-namespaces nil
        kubectl-current-namespace kubectl-previous-namespace)
  (kubectl-get-resources))

(defun kubectl-set-resources-jobs ()
  (interactive)
  (setq kubectl-resources-current "cronjobs,jobs,pods,cm"
        kubectl-all-namespaces nil
        kubectl-current-namespace kubectl-previous-namespace)
  (kubectl-get-resources))

(defun kubectl-set-resources-secrets ()
  (interactive)
  (setq kubectl-resources-current "clusterexternalsecrets,clustersecretstores,externalsecrets.external-secrets.io,secretstores,secrets"
        kubectl-all-namespaces nil
        kubectl-current-namespace kubectl-previous-namespace)
  (kubectl-get-resources))

(defun kubectl-set-resources-volumes ()
  (interactive)
  (setq kubectl-resources-current "pvc,pv,volumeattachments,storageclasses"
        kubectl-all-namespaces nil
        kubectl-current-namespace kubectl-previous-namespace)
  (kubectl-get-resources))

(defun kubectl-set-resources-all-no-pods ()
  (interactive)
  (setq kubectl-resources-current "ds,sts,deploy,svc,ing,cm"
        kubectl-all-namespaces nil
        kubectl-current-namespace kubectl-previous-namespace)
  (kubectl-get-resources))

(defun kubectl-set-resources-karpenter ()
  (interactive)
  (let ((ns "kube-system"))
    (shell-command-to-string (format "kubectl ns %s" ns))
    (setq kubectl-resources-current "deploy,po,ec2nodeclass,nodepool,nodeclaims"
          kubectl-all-namespaces nil
          kubectl-current-namespace ns)
    (kubectl-get-resources "karpenter")))

(defun kubectl-set-resources-github ()
  (interactive)
  (setq kubectl-resources-current "autoscalingrunnersets,ephemeralrunnersets,ephemeralrunners,deploy,rdeploy,rrs,po"
        kubectl-all-namespaces nil)
  (kubectl-get-resources))

(defun kubectl-set-resources-karpenter-crds ()
  (interactive)
  (let ((ns "kube-system"))
    (shell-command-to-string (format "kubectl ns %s" ns))
    (setq kubectl-resources-current "nodeclaims,ec2nodeclass,nodepool"
          kubectl-all-namespaces nil
          kubectl-current-namespace ns)
    (kubectl-get-resources)))

(defun kubectl-add-resource (resource)
  (interactive (list (completing-read (format "Resource to query for: %s," kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names) nil nil)))
  (setq kubectl-resources-current (format "%s,%s" kubectl-resources-default resource)
        kubectl-all-namespaces nil
        kubectl-current-namespace kubectl-previous-namespace)
  (kubectl-get-resources))

(defun kubectl-add-current-resource (resource)
  (interactive (list (completing-read (format "Resource to query for: %s," kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names) nil nil)))
  (setq kubectl-resources-current (format "%s,%s" kubectl-resources-current resource)
        kubectl-all-namespaces nil
        kubectl-current-namespace kubectl-previous-namespace)
  (kubectl-get-resources))

(defun kubectl-set-resource (resource)
  (interactive (list (completing-read (format "Resource to query for: " kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names) nil nil)))
  (setq kubectl-resources-current resource
        kubectl-all-namespaces nil
        kubectl-current-namespace kubectl-previous-namespace)
  (kubectl-get-resources))

(defun kubectl-add-current-resource-all-ns (resource)
  (interactive (list (completing-read (format "Resource to query for: %s," kubectl-resources-current-all-ns) (-concat kubectl-api-abbreviations kubectl-api-resource-names) nil nil)))
  (setq kubectl-resources-current-all-ns (format "%s,%s" kubectl-resources-current-all-ns resource)
        kubectl-all-namespaces t)
  (when (not (s-matches-p "All" kubectl-current-namespace))
    (setq kubectl-previous-namespace kubectl-current-namespace))
  (kubectl-get-resources))

(defun kubectl-set-resource-all-ns (resource)
  (interactive (list (completing-read (format "Resource to query for: " kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names nil t))))
  (setq kubectl-resources-current-all-ns resource
        kubectl-all-namespaces t)
  (when (not (s-matches-p "All" kubectl-current-namespace))
    (setq kubectl-previous-namespace kubectl-current-namespace))
  (kubectl-get-resources))

(defun kubectl-remove-cluster-from-kubeconfig (cluster-name)
  "Remove a cluster from the kubectl config.
Removes entries from clusters[], contexts[], and users[] arrays."
  (interactive
   (list (completing-read "Cluster to remove: "
                          (s-split "\n"
                                   (shell-command-to-string "kubectl config get-clusters | grep -v NAME")
                                   t))))

  (let ((config-file (expand-file-name "~/.kube/config")))
    ;; Backup existing config
    (when (file-exists-p config-file)
      (let ((backup-file (expand-file-name
                          (format "config.backup-%s" (format-time-string "%s"))
                          (file-name-directory config-file))))
        (copy-file config-file backup-file)
        (message "📦 Backed up existing config to %s" (file-name-nondirectory backup-file))))

    ;; Remove cluster entry
    (shell-command (format "kubectl config delete-cluster %s" cluster-name))

    ;; Remove any contexts that use this cluster
    (let* ((contexts-output (shell-command-to-string "kubectl config get-contexts -o name"))
           (all-contexts (s-split "\n" contexts-output t)))
      (dolist (context all-contexts)
        (when (not (string-empty-p context))
          (let ((context-cluster (s-trim (shell-command-to-string
                                          (format "kubectl config view -o jsonpath='{.contexts[?(@.name==\"%s\")].context.cluster}'" context)))))
            (when (string= context-cluster cluster-name)
              (shell-command (format "kubectl config delete-context %s" context))
              (message "🗑️  Removed context: %s" context))))))

    ;; Remove any users that might be orphaned (users with same name as cluster)
    (let* ((users-output (shell-command-to-string "kubectl config view -o jsonpath='{.users[*].name}'"))
           (all-users (s-split " " users-output t)))
      (dolist (user all-users)
        (when (string= user cluster-name)
          (shell-command (format "kubectl config delete-user %s" user))
          (message "🗑️  Removed user: %s" user))))

    (message "✅ Successfully removed cluster '%s' from kubectl config" cluster-name)))

(provide 'kubectl-transient)

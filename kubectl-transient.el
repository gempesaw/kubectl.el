;; -*- lexical-binding: t; -*-

(require 'transient)

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
   ("u" "remove annotation: remove last-applied-configuration annotation" kubectl-unmark-last-applied-configuration-at-point)
   ("p" "pop to create buffer" kubectl-pop-to-create-resource-buffer)]

  ["workloads"
   ("s" "scale workload" kubectl-scale-workload-at-point)
   ("r" "restart workload" kubectl-restart-workload-at-point)
   ("j" "create job from cronjob" kubectl-run-cronjob-at-point)
   ("g" "open in grafana" kubectl-open-grafana-workload-at-point)
   ("G" "open in grafana (all clusters)" kubectl-open-grafana-workload-at-point-all-clusters)
   ]

  ["nodes"
   ("n" "view node on line" kubectl-view-node-on-line)
   ("c" "cordon nodes" kubectl-cordon-nodes-at-point)
   ("d" "drain nodes" kubectl-drain-nodes-at-point)]
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
    ("v" "volumes (pvc,pv,volumeattachments)" kubectl-set-resources-volumes)

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

(transient-define-prefix kubectl-transient-choose-context ()
  "Choose cluster and AWS profile alias"

  ["Options"
   ("c" "Kubernetes context" "c="
    :always-read t
    :init-value (lambda (ob)
                  (setf (slot-value ob 'value) kubectl-current-context))
    :reader (lambda (prompt initial-input history)
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
   ("a" "AWS Profile" "a="
    :always-read t
    ;; :multi-value t
    :init-value (lambda (ob)
                  (setf (slot-value ob 'value) kubectl-current-role))
    :reader (lambda (prompt initial-input history)
              (completing-read prompt
                               (--> "aws configure list-profiles"
                                    (shell-command-to-string it)
                                    (s-split "\n" it t))
                               nil
                               nil
                               initial-input
                               history)))
   ]
  ["Connect"
   [("SPC" "Connect"
     (lambda (&optional args)
       (interactive (list (transient-args transient-current-command)))
       (let* ((context (transient-arg-value "c=" args))
              (namespace (transient-arg-value "ns=" args))
              (resources (transient-arg-value "r=" args))
              (aws-role (transient-arg-value "a=" args)))
         (dg-modular-ensure-aws-profile-login aws-role)
         (setq kubectl-current-context context
               kubectl-current-namespace namespace
               kubectl-resources-current resources
               kubectl-current-role aws-role
               )
         (kubectl--run-process-bg
          (format "kubectx %s && kubens %s" (if (s-contains-p "/" context) (cadr (s-split "/" context)) context) namespace )
          (lambda (process)
            (kubectl-init)
            (run-at-time 10 nil 'kubectl-get-namespaces)
            (run-at-time 10 nil 'kubectl-get-api-resources)))
         )))
    ]])

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
  (setq kubectl-resources-current "pvc,pv,volumeattachments"
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

(provide 'kubectl-transient)

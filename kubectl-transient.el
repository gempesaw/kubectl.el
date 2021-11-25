(require 'transient)

(transient-define-prefix kubectl-transient-help ()
  "Available kubectl actions"
  ["Set contexts"
   [("R" "resource" kubectl-choose-resource)]
   [("N" "namespace" kubectl-choose-namespace)]
   [("C" "context" kubectl-transient-choose-context)]]
  ["Resource at point"
   [("e" "edit" kubectl-edit-resource-at-point)
    ("k" "delete" kubectl-delete-resource-at-point)]
   [("o" "output yaml" kubectl-get-yaml-at-point)
    ("<RET>" "describe" kubectl-describe-resource-at-point)]]
  ["Pod at point"
   [("x" "open a shell" kubectl-pod-exec)]
   [("l" "view logs" kubectl-pod-logs)]]
  ["utility"
   [("g" "refresh" kubectl-init)]
   [("G" "toggle auto refresh" kubectl-toggle-autorefresh)]
   [("$" "show log buffer" kubectl-show-log-buffer)]
   [(":" "run custom command" kubectl-run-custom-command)]])

(transient-define-prefix kubectl-transient-choose-resource ()
  "Choose resources to query for"

  ["In a single namespace"
   [("r" (lambda () (format "Reset to default (%s)" kubectl-resources-default)) kubectl-reset-resources)
    ("a" "Add a resource to the defaults" kubectl-add-resource)
    ("c" (lambda () (format "add to Current (%s)" kubectl-resources-current)) kubectl-add-current-resource)
    ("n" "Use the current list as the new default" kubectl-set-current-as-default)
    ("s" "Specify your own list" kubectl-set-resource)]
   ]

  ["All namespaces"
   ("C" (lambda () (format "add to Current (%s)" kubectl-resources-current-all-ns)) kubectl-add-current-resource-all-ns)
   ("S" "Specify your own list" kubectl-set-resource-all-ns)
   ]
  )

(transient-define-prefix kubectl-transient-choose-resource-all-ns ()
  "Choose resources to query for in all namespaces"
  ["All namespaces"
   ("c" (lambda () (format "add to Current (%s)" kubectl-resources-current-all-ns)) kubectl-add-current-resource-all-ns)
   ("s" "Specify your own list" kubectl-set-resource-all-ns)
   ])

(transient-define-prefix kubectl-transient-choose-context ()
  "Choose cluster and AWS profile alias"

  ["Options"
   ("a" "AWS Profile" "aws="
    :always-read t
    :init-value (lambda (ob)
                  (setf (slot-value ob 'value) kubectl-current-aws-profile))
    :choices (lambda (complete-me filter-p completion-type) (kubectl--get-aws-profiles)))
   ("c" "Kubernetes context" "k8s="
    :always-read t
    :init-value (lambda (ob)
                  (setf (slot-value ob 'value) kubectl-current-context))
    :choices (lambda (complete-me filter-p completion-type) (kubectl--get-available-contexts)))
   ("n" "Namespace" "ns="
    :always-read t
    :init-value (lambda (ob)
                  (setf (slot-value ob 'value) kubectl-current-namespace))
    :choices (lambda (complete-me filter-p completion-type) kubectl-cached-namespaces))
   ("r" "Resources" "r="
    :always-read t
    :init-value (lambda (ob)
                  (setf (slot-value ob 'value) kubectl-resources-current)))
   ]
  ["Connect" ("SPC" "Connect"
              (lambda (&optional args)
                (interactive (list (transient-args 'kubectl-transient-choose-context)))
                (let ((aws-profile (cadr (s-split "=" (car args))))
                      (context (cadr (s-split "=" (cadr args))))
                      (namespace (cadr (s-split "=" (caddr args))))
                      (resources (transient-arg-value "r=" (transient-args'kubectl-transient-choose-context))))
                  (setq kubectl-current-aws-profile aws-profile)
                  (setq kubectl-current-context context)
                  (setq kubectl-current-namespace namespace)
                  (shell-command-to-string (format "kubectl config use-context %s" context))
                  (shell-command-to-string (format "kubectl config set-context --current --namespace %s" namespace))
                  (setq kubectl-resources-current resources)
                  (kubectl--aws-okta-login-synchronous aws-profile
                                                       'kubectl--make-proxy-process-current))))])

(defvar kubectl--aws-okta-login-synchronous-callback)
(defvar kubectl-current-aws-profile "")
(defvar kubectl-current-cluster "")

(defun kubectl--aws-okta-login-synchronous (aws-profile callback)
  (interactive)
  (let* ((process-name "aws-okta-login-synchronous" )
         (buffer (format "*%s*" process-name))
         (command (s-split " " (format "aws-okta env %s --mfa-duo-device token" aws-profile)))
         (proc nil))
    (when (get-buffer buffer) (kill-buffer buffer))
    (setq proc (make-process
                :name process-name
                :connection-type 'pipe
                :buffer buffer
                :coding 'no-conversion
                :command command
                :filter 'kubectl--aws-okta-process-filter
                :stderr nil
                :sentinel (lambda (proc signal)
                            (kubectl--aws-okta-setenv proc)
                            (kubectl--aws-okta-login-synchronous-callback))
                :noquery t))
    (with-current-buffer (get-buffer buffer)
      (make-variable-buffer-local 'kubectl--aws-okta-login-synchronous-callback)
      (fset 'kubectl--aws-okta-login-synchronous-callback callback)
      (special-mode))))

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
          (process-send-string proc (format "%s\n" (read-string "please press your yubbykey: ")))
          (message "sending yubbykey to process"))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

;; (kubectl--aws-okta-login-synchronous "terraform" (lambda (&rest awoo)))

(defvar kubectl-resources-current-all-ns "pods")

(defun kubectl-set-current-as-default ()
  (interactive)
  (setq kubectl-resources-default kubectl-resources-current
        kubectl-all-namespaces nil)
  (kubectl-get-resources))

(defun kubectl-reset-resources ()
  (interactive)
  (setq kubectl-resources-current kubectl-resources-default
        kubectl-all-namespaces nil)
  (kubectl-get-resources))

(defun kubectl-add-resource (resource)
  (interactive (list (completing-read (format "Resource to query for: %s," kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names nil t))))
  (setq kubectl-resources-current (format "%s,%s" kubectl-resources-default resource)
        kubectl-all-namespaces nil)
  (kubectl-get-resources))

(defun kubectl-add-current-resource (resource)
  (interactive (list (completing-read (format "Resource to query for: %s," kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names nil t))))
  (setq kubectl-resources-current (format "%s,%s" kubectl-resources-current resource)
        kubectl-all-namespaces nil)
  (kubectl-get-resources))

(defun kubectl-set-resource (resource)
  (interactive (list (completing-read (format "Resource to query for: " kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names nil t))))
  (setq kubectl-resources-current resource
        kubectl-all-namespaces nil)
  (kubectl-get-resources))

(defun kubectl-add-current-resource-all-ns (resource)
  (interactive (list (completing-read (format "Resource to query for: %s," kubectl-resources-current-all-ns) (-concat kubectl-api-abbreviations kubectl-api-resource-names nil t))))
  (setq kubectl-resources-current-all-ns (format "%s,%s" kubectl-resources-current-all-ns resource)
        kubectl-all-namespaces t)
  (kubectl-get-resources))

(defun kubectl-set-resource-all-ns (resource)
  (interactive (list (completing-read (format "Resource to query for: " kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names nil t))))
  (setq kubectl-resources-current-all-ns resource
        kubectl-all-namespaces t)
  (kubectl-get-resources))

(provide 'kubectl-transient)

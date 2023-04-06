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
    ("a" (lambda () (format "add to Current (%s)" kubectl-resources-current)) kubectl-add-current-resource)
    ("c" (lambda () (format "add to Current (%s)" kubectl-resources-current)) kubectl-add-current-resource)
    ("n" "Use the current list as the new default" kubectl-set-current-as-default)
    ("s" "Specify your own list" kubectl-set-resource)]
   ]

  ["All namespaces"
   [("C" (lambda () (format "add to Current (%s)" kubectl-resources-current-all-ns)) kubectl-add-current-resource-all-ns)
    ("S" "Specify your own list" kubectl-set-resource-all-ns)
    ("R" "Refresh / use current" (lambda (&optional args)
                                   (interactive)
                                   (setq kubectl-all-namespaces t
                                         kubectl-previous-namespace kubectl-current-namespace)
                                   (kubectl-get-resources)))]
   [("k" "kube-capacity" kubectl--kube-capacity)]
   ]
  )

(progn
  (defvar kubectl--transient-grep-needle nil)
  (defvar kubectl--transient-grep-invert nil)

  (transient-define-argument kubectl--transient-grep-value ()
    :description "what grep value to filter with"
    :class 'transient-option
    :key "g"
    :argument ""
    :init-value (lambda (ob) (setf (slot-value ob 'value) kubectl--transient-grep-needle)))

  (transient-define-prefix kubectl-transient-grep ()
    "Filter the displayed resources"
    ["Options"
     (kubectl--transient-grep-value)
     ("-v" "invert match" "--invert-match"
      :init-value (lambda (ob) (setf (slot-value ob 'value) kubectl--transient-grep-invert)))
     ]
    ["Actions"
     [("SPC" "Apply filter"
       (lambda (&optional args)
         (interactive (list (transient-args transient-current-command)))
         (let ((needle (car args))
               (invert (transient-arg-value "--invert-match" args)))
           (if invert
               (setq kubectl--transient-grep-invert "--invert-match")
             (setq kubectl--transient-grep-invert nil))

           (when (s-equals-p needle "--invert-match")
             (setq needle nil))
           (setq kubectl--transient-grep-needle needle)
           (kubectl-print-buffer))))
      ("<return>" "Apply filter"
       (lambda (&optional args)
         (interactive (list (transient-args transient-current-command)))
         (let ((needle (car args))
               (invert (transient-arg-value "--invert-match" args)))
           (if invert
               (setq kubectl--transient-grep-invert "--invert-match")
             (setq kubectl--transient-grep-invert nil))

           (when (s-equals-p needle "--invert-match")
             (setq needle nil))
           (setq kubectl--transient-grep-needle needle)
           (kubectl-print-buffer))))]]))


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
   ("a" "AWS Role" "a="
    :always-read t
    :init-value (lambda (ob)
                  (setf (slot-value ob 'value) kubectl-current-role))
    :reader (lambda (prompt initial-input history)
              (completing-read prompt (->> (shell-command-to-string "pk role --list")
                                           (funcall (lambda (it) (s-split "\n" it t)))
                                           (--filter (not (s-matches-p "^>" it)))
                                           (s-join "\n")
                                           (json-parse-string)
                                           (ht-map (lambda (key value)
                                                     (mapcar (lambda (rn) (format "%s" rn)) value)))
                                           (-flatten)
                                           (-distinct))
                               nil nil initial-input history))
    )
   ]
  ["Connect"
   [("SPC" "Connect"
     (lambda (&optional args)
       (interactive (list (transient-args transient-current-command)))
       (let ((context (transient-arg-value "c=" (transient-args transient-current-command)))
             (namespace (transient-arg-value "ns=" (transient-args transient-current-command)))
             (resources (transient-arg-value "r=" (transient-args transient-current-command)))
             (aws-role (transient-arg-value "a=" (transient-args transient-current-command)))
             (buf (create-new-shell-here)))
         (if aws-role
             (shell-command-to-string (format "pk role %s" aws-role))
           (shell-command-to-string "pk role --clear"))
         (with-current-buffer buf
           (kubectl-redraw-harmless)
           (setq kubectl--pk-buffer-p nil)
           (setq-local kubectl--pk-buffer-p t)
           (add-hook 'comint-output-filter-functions 'kubectl--comint-shell-filter-function)
           (select-window (display-buffer buf))
           (insert (format "pk connect %s %s" context namespace))
           (comint-send-input))
         (setq kubectl-current-context context
               kubectl-current-namespace namespace
               kubectl-resources-current resources
               kubectl-current-role aws-role)
         ;; (kubectl-init)
         )))]])

(defun kubectl--comint-shell-filter-function (string)
  (let ((name (buffer-name)))
    (with-current-buffer name
      (when (and kubectl--pk-buffer-p (s-contains? "Success" string))
        (remove-hook 'comint-output-filter-functions 'kubectl--comint-shell-filter-function)
        (delete-window (get-buffer-window name))
        (run-at-time 3 nil 'kill-buffer name)
        (kubectl-init)
        (run-at-time 10 nil 'kubectl-get-namespaces)
        (run-at-time 10 nil 'kubectl-get-api-resources)))))

(defvar kubectl-resources-current-all-ns "pods")

(defun kubectl-set-current-as-default ()
  (interactive)
  (setq kubectl-resources-default kubectl-resources-current
        kubectl-all-namespaces nil)
  (when (s-matches-p kubectl-current-namespace "All ")
    (setq kubectl-current-namespace kubectl-previous-namespace))
  (kubectl-get-resources))

(defun kubectl-reset-resources ()
  (interactive)
  (setq kubectl-resources-current kubectl-resources-default
        kubectl-all-namespaces nil)
  (kubectl-get-resources))

(defun kubectl-add-resource (resource)
  (interactive (list (completing-read (format "Resource to query for: %s," kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names) nil nil)))
  (setq kubectl-resources-current (format "%s,%s" kubectl-resources-default resource)
        kubectl-all-namespaces nil)
  (kubectl-get-resources))

(defun kubectl-add-current-resource (resource)
  (interactive (list (completing-read (format "Resource to query for: %s," kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names) nil nil)))
  (setq kubectl-resources-current (format "%s,%s" kubectl-resources-current resource)
        kubectl-all-namespaces nil)
  (kubectl-get-resources))

(defun kubectl-set-resource (resource)
  (interactive (list (completing-read (format "Resource to query for: " kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names) nil nil)))
  (setq kubectl-resources-current resource
        kubectl-all-namespaces nil)
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

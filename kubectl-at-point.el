;;; -*- lexical-binding: t; -*-

(require 'posframe)

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
     (format "kubectl debug %s --profile=sysadmin --stdin --tty --image=public.ecr.aws/docker/library/alpine:3.20"
             current-line-resource-name node-ip-string))))

(defun kubectl--get-pod-containers (pod)
  (->> (format "kubectl get %s -ojson | jq -r  '.spec.initContainers + .spec.containers | .[] | .name'" pod)
       (kubectl--shell)
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
  (kubectl-with-aws-env
    (let* ((buf nil))
      (setq buf (create-new-shell-here))
      (select-window (display-buffer buf))
      (goto-char (point-max))
      (insert command)
      (comint-send-input))))

(defun kubectl-port-forward ()
  (interactive)
  (let ((ports (->> (format "kubectl get %s -ojson | jq -r  '.spec.containers[].ports[].containerPort'" (kubectl-current-line-resource-as-string))
                    (kubectl--shell)
                    (s-trim)
                    (s-split "\n"))))
    (let* ((port (completing-read "choose a port to forward: " ports nil nil nil t))
           (local-port (if (s-equals-p "80" port) "8080" port))
           (cmd (format "kubectl port-forward %s %s:%s"
                        (kubectl-current-line-resource-as-string)
                        local-port
                        port)))
      (message cmd)
      (kubectl-with-aws-env (async-shell-command cmd))
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
         (dir kubectl--my-directory)
         (prompt-with-resources (format "%s\n\n%s\n\n" prompt (s-join "\n" resources-at-point))))
    (kubectl-confirm prompt-with-resources
                     (lambda ()
                       (kubectl-with-aws-env
                         (let ((default-directory dir)
                               (bpr-process-mode 'kubectl-command-mode))
                           (--map (apply action-fn (list it)) resources-at-point)))))))

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
         (dir kubectl--my-directory))
    (kubectl-confirm prompt
                     (lambda ()
                       (kubectl-with-aws-env
                         (let ((default-directory dir)
                               (bpr-process-mode 'kubectl-command-mode))
                           (bpr-spawn restart-command)))))))

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
         (dir kubectl--my-directory)
         (prompt (format "Confirm cordon %s nodes (cluster: %s | context: %s | namespace: %s) %s?"
                         (length nodes)
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace
                         command)))
    (kubectl-confirm prompt
                     (lambda ()
                       (kubectl-with-aws-env
                         (let ((default-directory dir))
                           (bpr-spawn command)))))))

(defun kubectl-uncordon-nodes-at-point ()
  (interactive)
  (let* ((nodes (kubectl-get-resources-at-point-or-region))
         (command (format "kubectl uncordon %s" (s-join " " nodes)))
         (dir kubectl--my-directory)
         (prompt (format "Confirm uncordon %s nodes (cluster: %s | context: %s | namespace: %s) %s?"
                         (length nodes)
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace
                         command)))
    (kubectl-confirm prompt
                     (lambda ()
                       (kubectl-with-aws-env
                         (let ((default-directory dir))
                           (bpr-spawn command)))))))

(defun kubectl-view-node-on-line ()
  (interactive)
  (let* ((node-like (s-trim (car (s-match " i-.*?internal" (substring-no-properties (current-line-contents)))))))
    (when node-like
      (kubectl--run-process-and-pop (format "kubectl describe node/%s" node-like)))))

(defvar kubectl--popup-buffer "*kubectl-popup*")
(defvar kubectl--popup-prev-frame nil
  "Frame to return focus to when the popup is dismissed.")
(defvar kubectl--popup-frame nil
  "The child frame currently displaying the popup, if any.")
(defvar kubectl--popup-preferred-width 150
  "Preferred width (columns) for the popup child frame.
Long lines (e.g. event messages) bleed past this and are clipped by
`truncate-lines'; toggle wrapping with \\`w' to read them.")
(defvar-local kubectl--popup-has-pods nil
  "Non-nil in popups listing pods, enabling pod-aware n/p and delete bindings.")

(define-derived-mode kubectl-popup-mode special-mode "kubectl-popup"
  "Read-only mode for kubectl info popups. Dismiss with q or C-g."
  (setq truncate-lines t)
  (setq buffer-read-only t)
  ;; CSS-style horizontal padding. `internal-border-width' is shadowed by
  ;; `child-frame-border-width' in posframes, so margins are the only knob
  ;; that actually inset the text from the colored border.
  (setq-local left-margin-width 2)
  (setq-local right-margin-width 2)
  ;; A visible cursor so text can be selected/copied (e.g. from events).
  (setq-local cursor-type 'box)
  ;; Highlight the current line so n/p navigation is visible.
  (hl-line-mode 1))

(define-key kubectl-popup-mode-map (kbd "q") 'kubectl--popup-hide)
(define-key kubectl-popup-mode-map (kbd "C-g") 'kubectl--popup-hide)
(define-key kubectl-popup-mode-map (kbd "<escape>") 'kubectl--popup-hide)
(define-key kubectl-popup-mode-map (kbd "n") 'kubectl--popup-next-line)
(define-key kubectl-popup-mode-map (kbd "p") 'kubectl--popup-previous-line)
(define-key kubectl-popup-mode-map (kbd "w") 'kubectl--popup-toggle-wrap)
(define-key kubectl-popup-mode-map (kbd "K K") 'kubectl--popup-delete-pod-at-point)
(define-key kubectl-popup-mode-map (kbd "F") 'kubectl--popup-force-delete-pod-at-point)
(define-key kubectl-popup-mode-map (kbd "E") 'kubectl--popup-events-for-pod-at-point)

(defun kubectl--popup-next-line ()
  "Next pod in pod popups, otherwise the next display line."
  (interactive)
  (if kubectl--popup-has-pods (kubectl--popup-next-pod) (forward-line 1)))

(defun kubectl--popup-previous-line ()
  "Previous pod in pod popups, otherwise the previous display line."
  (interactive)
  (if kubectl--popup-has-pods (kubectl--popup-previous-pod) (forward-line -1)))

(defun kubectl--popup-toggle-wrap ()
  "Toggle line wrapping in the popup.
Off (the default) lets long event messages bleed off the right edge;
on wraps them at word boundaries so the whole line is visible."
  (interactive)
  (setq-local truncate-lines (not truncate-lines))
  (setq-local word-wrap (not truncate-lines))
  (when (and (display-graphic-p) (frame-live-p kubectl--popup-frame))
    (kubectl--popup-display))
  (message "popup wrap %s" (if truncate-lines "off" "on")))

(defun kubectl--popup-pod-line-p ()
  "Non-nil when the current line carries a pod (tagged by `kubectl--popup-insert-pod')."
  (get-text-property (line-beginning-position) 'kubectl-pod-name))

(defun kubectl--popup-next-pod ()
  "Move point to the next pod line, skipping headers and blank lines.
Stays put if there is no further pod line."
  (interactive)
  (let ((origin (point)))
    (forward-line 1)
    (while (and (not (eobp)) (not (kubectl--popup-pod-line-p)))
      (forward-line 1))
    (if (kubectl--popup-pod-line-p)
        (beginning-of-line)
      (goto-char origin))))

(defun kubectl--popup-previous-pod ()
  "Move point to the previous pod line, skipping headers and blank lines.
Stays put if there is no earlier pod line."
  (interactive)
  (let ((origin (point)))
    (forward-line -1)
    (while (and (not (bobp)) (not (kubectl--popup-pod-line-p)))
      (forward-line -1))
    (if (kubectl--popup-pod-line-p)
        (beginning-of-line)
      (goto-char origin))))

(defun kubectl--node-name-on-line ()
  "Return the node name from the current line, or nil if line doesn't start with node/."
  (let* ((first-token (car (s-split " +" (s-trim (substring-no-properties (current-line-contents))) t))))
    (when (s-starts-with? "node/" first-token)
      (s-chop-prefix "node/" first-token))))

(defun kubectl--popup-hide ()
  "Hide the kubectl popup and restore focus to the previous frame."
  (interactive)
  (when (get-buffer kubectl--popup-buffer)
    (posframe-hide kubectl--popup-buffer))
  (setq kubectl--popup-frame nil)
  (when (and kubectl--popup-prev-frame (frame-live-p kubectl--popup-prev-frame))
    (select-frame-set-input-focus kubectl--popup-prev-frame)))

(defun kubectl--popup-width ()
  "Fixed popup width in columns, capped to the parent frame so it never overflows."
  (let ((parent (if (frame-live-p kubectl--popup-prev-frame)
                    kubectl--popup-prev-frame
                  (selected-frame))))
    (min kubectl--popup-preferred-width (max 80 (- (frame-width parent) 6)))))

(defun kubectl--popup-display ()
  "Display `kubectl--popup-buffer' as a centered child frame with focus.
Width is fixed (see `kubectl--popup-width') so long lines clip rather than
ballooning the frame. Falls back to `pop-to-buffer' on a TTY."
  (if (and (display-graphic-p) (posframe-workable-p))
      (let ((frame (posframe-show kubectl--popup-buffer
                                  :poshandler 'posframe-poshandler-frame-center
                                  :width (kubectl--popup-width)
                                  :border-width 3
                                  :border-color "#aa77fd"
                                  :internal-border-width 24
                                  :left-fringe 0
                                  :right-fringe 0
                                  :accept-focus t)))
        (setq kubectl--popup-frame frame)
        (when (frame-live-p frame)
          (when-let ((win (frame-first-window frame)))
            (set-window-margins win 2 2))
          (select-frame-set-input-focus frame)))
    (pop-to-buffer kubectl--popup-buffer)))

(defun kubectl--popup-show ()
  "Pad `kubectl--popup-buffer' and display it. Dismiss with q or C-g.
Records the parent frame for focus restore, unless we're already inside the
popup (e.g. drilling into a pod's events) — then the original parent stands."
  (with-current-buffer (get-buffer-create kubectl--popup-buffer)
    (unless (derived-mode-p 'kubectl-popup-mode)
      (kubectl-popup-mode))
    ;; Vertical padding: there's no window-margin analog for top/bottom, so
    ;; bracket the content with blank rows. Two top, two bottom feels
    ;; proportional to the 2-char horizontal margins.
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (insert "\n\n")
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n"))
    (goto-char (point-min)))
  (unless (eq (selected-frame) kubectl--popup-frame)
    (setq kubectl--popup-prev-frame (selected-frame)))
  (kubectl--popup-display))

(defun kubectl--popup-insert-pod (fmt p)
  "Insert pod P formatted via FMT, tagging the line with its ns/name.
The `kubectl-pod-ns'/`kubectl-pod-name' text properties let
`kubectl--popup-delete-pod-at-point' recover the pod identity from the
line at point without re-parsing the formatted columns."
  (insert (propertize (funcall fmt p)
                      'kubectl-pod-ns (plist-get p :ns)
                      'kubectl-pod-name (plist-get p :name))
          "\n"))

(defun kubectl--popup-pod-at-point ()
  "Return (NS . NAME) for the pod on the current line, or signal an error."
  (let ((ns (get-text-property (line-beginning-position) 'kubectl-pod-ns))
        (name (get-text-property (line-beginning-position) 'kubectl-pod-name)))
    (unless (and ns name)
      (user-error "No pod on this line"))
    (cons ns name)))

(defun kubectl--popup-delete-pod (force)
  "Delete the pod on the current line, FORCE-deleting when non-nil.
Mirrors the `K K' flow on the main table: confirm via `kubectl-confirm',
then delete with `bpr-spawn'. The popup is dismissed first so the
confirmation lives in the parent frame, not the child."
  (let* ((pod (kubectl--popup-pod-at-point))
         (ns (car pod))
         (resource (format "pod/%s" (cdr pod)))
         (command (format "kubectl delete%s --namespace %s %s"
                          (if force " --force" "") ns resource))
         (dir kubectl--my-directory)
         (prompt (format "Confirm %s %s (namespace: %s | cluster: %s | context: %s)?\n\n%s"
                         (if force "FORCE DELETE" "DELETE")
                         resource ns
                         kubectl-current-cluster
                         kubectl-current-context
                         command)))
    (kubectl--popup-hide)
    (kubectl-confirm prompt
                     (lambda ()
                       (kubectl-with-aws-env
                         (let ((default-directory dir)
                               (bpr-process-mode 'kubectl-command-mode))
                           (bpr-spawn command)))))))

(defun kubectl--popup-delete-pod-at-point ()
  "Delete the pod on the current line, with confirmation."
  (interactive)
  (kubectl--popup-delete-pod nil))

(defun kubectl--popup-force-delete-pod-at-point ()
  "Force-delete the pod on the current line, with confirmation."
  (interactive)
  (kubectl--popup-delete-pod t))

(defun kubectl--popup-events-for-pod-at-point ()
  "Replace the popup with the events for the pod on the current line."
  (interactive)
  (let* ((pod (kubectl--popup-pod-at-point))
         (ns (car pod))
         (name (cdr pod))
         (parsed (kubectl--fetch-events name ns)))
    (with-current-buffer (get-buffer-create kubectl--popup-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq kubectl--popup-has-pods nil)
        (insert (propertize (format "pod/%s (namespace: %s) — %d event(s)\n\n" name ns (length parsed))
                            'face 'bold))
        (kubectl--insert-events parsed)
        (goto-char (point-min))))
    (kubectl--popup-show)))

(defun kubectl-show-pods-on-node ()
  "Show Running pods on the node at point grouped by DaemonSet vs other.
Non-Running pods are summarized by phase with a few examples.
Useful for deciding whether a node can be restarted without draining."
  (interactive)
  (let ((node (kubectl--node-name-on-line)))
    (unless node
      (user-error "No node/<name> on this line"))
    (let* ((cmd (format "kubectl get pods -A --field-selector=spec.nodeName=%s -o jsonpath='{range .items[*]}{.metadata.namespace}{\"\\t\"}{.metadata.name}{\"\\t\"}{.status.phase}{\"\\t\"}{.metadata.ownerReferences[0].kind}{\"\\n\"}{end}' 2>/dev/null"
                        (shell-quote-argument node)))
           (raw (kubectl--shell cmd))
           (lines (--filter (not (s-blank-p it)) (s-split "\n" (s-trim raw))))
           (parsed (--map (let ((parts (s-split "\t" it)))
                            (list :ns (or (nth 0 parts) "")
                                  :name (or (nth 1 parts) "")
                                  :phase (or (nth 2 parts) "")
                                  :owner (let ((o (nth 3 parts))) (if (or (null o) (s-blank? o)) "—" o))))
                          lines))
           (running (--filter (s-equals-p (plist-get it :phase) "Running") parsed))
           (other (--remove (s-equals-p (plist-get it :phase) "Running") parsed))
           (ds (--filter (s-equals-p (plist-get it :owner) "DaemonSet") running))
           (non-ds (--remove (s-equals-p (plist-get it :owner) "DaemonSet") running))
           (phase-counts (let ((h (make-hash-table :test 'equal)))
                           (--each other
                             (let ((k (plist-get it :phase)))
                               (puthash k (1+ (gethash k h 0)) h)))
                           h))
           (phase-summary (let (parts)
                            (maphash (lambda (k v) (push (format "%s=%d" k v) parts))
                                     phase-counts)
                            (s-join " " (-sort 'string< parts))))
           (max-ns (apply 'max 1 (--map (length (plist-get it :ns)) parsed)))
           (max-name (apply 'max 1 (--map (length (plist-get it :name)) parsed)))
           (max-owner (apply 'max 1 (--map (length (plist-get it :owner)) running)))
           (fmt (lambda (p)
                  (format "  %s  %s  %s  %s"
                          (s-pad-right max-ns " " (plist-get p :ns))
                          (s-pad-right max-name " " (plist-get p :name))
                          (s-pad-right (max max-owner 1) " " (plist-get p :owner))
                          (plist-get p :phase))))
           (example-cap 5))
      (with-current-buffer (get-buffer-create kubectl--popup-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq kubectl--popup-has-pods t)
          (insert (propertize (format "node/%s — %d Running (%d non-DS, %d DS), %d other\n"
                                      node (length running) (length non-ds) (length ds) (length other))
                              'face 'bold))
          (insert (propertize "  [n/p] move  [K K] delete  [F] force-delete  [E] pod events  [w] wrap  [q] close\n"
                              'face 'kubectl-timestamp-face))
          (insert (propertize (if non-ds
                                  (format "%d non-DaemonSet Running pod(s) — drain first\n" (length non-ds))
                                "no non-DaemonSet Running pods — safe to restart\n")
                              'face (if non-ds 'kubectl-warning-face 'kubectl-success-face)))
          (when non-ds
            (insert (propertize "\nRunning, Non-DaemonSet:\n" 'face 'kubectl-section-header-face))
            (--each non-ds (kubectl--popup-insert-pod fmt it)))
          (when ds
            (insert (propertize (format "\nRunning, DaemonSet (%d):\n" (length ds)) 'face 'kubectl-section-header-face))
            (--each ds (kubectl--popup-insert-pod fmt it)))
          (when other
            (insert (propertize (format "\nOther (%d): %s\n" (length other) phase-summary)
                                'face 'kubectl-section-header-face))
            (--each (-take example-cap other) (kubectl--popup-insert-pod fmt it))
            (when (> (length other) example-cap)
              (insert (propertize (format "  … (+%d more)\n" (- (length other) example-cap))
                                  'face 'kubectl-timestamp-face))))
          (let ((node-events (kubectl--fetch-events node)))
            (insert (propertize (format "\nNode events (%d):\n" (length node-events))
                                'face 'kubectl-section-header-face))
            (kubectl--insert-events node-events))
          (goto-char (point-min))))
      (kubectl--popup-show))))

(defun kubectl--relative-time-ago (iso-timestamp)
  "Format ISO-TIMESTAMP (e.g. 2026-05-17T04:40:55Z) as relative ago: 5s, 3m, 1h, 2d."
  (if (or (null iso-timestamp) (s-blank? iso-timestamp))
      ""
    (condition-case nil
        (let* ((then (float-time (date-to-time iso-timestamp)))
               (delta (max 0 (truncate (- (float-time) then)))))
          (cond
           ((< delta 60) (format "%ds" delta))
           ((< delta 3600) (format "%dm" (/ delta 60)))
           ((< delta 86400) (format "%dh" (/ delta 3600)))
           (t (format "%dd" (/ delta 86400)))))
      (error ""))))

(defun kubectl--resource-on-line ()
  "Parse the resource on the current line into (:namespace :kind :name :display)."
  (let* ((raw (s-trim (kubectl-current-line-resource-as-string)))
         (parts (s-split " +" raw t))
         (last (car (last parts)))
         (kind-name (s-split "/" last)))
    (list :namespace (when (s-starts-with? "--namespace" raw)
                       (cadr parts))
          :kind (car kind-name)
          :name (cadr kind-name)
          :display last)))

(defun kubectl--fetch-events (name &optional ns)
  "Fetch events for involvedObject NAME (optionally in namespace NS).
Returns a list of plists (:when :type :reason :count :from :msg) sorted
oldest-first."
  (let* ((selectors (s-join "," (-non-nil
                                 (list (format "involvedObject.name=%s" name)
                                       (when ns (format "involvedObject.namespace=%s" ns))))))
         (cmd (format "kubectl get events -A --field-selector=%s -o jsonpath='{range .items[*]}{.lastTimestamp}{\"\\t\"}{.eventTime}{\"\\t\"}{.type}{\"\\t\"}{.reason}{\"\\t\"}{.count}{\"\\t\"}{.source.component}{\"\\t\"}{.reportingComponent}{\"\\t\"}{.message}{\"\\n\"}{end}' 2>/dev/null"
                      (shell-quote-argument selectors)))
         (raw (kubectl--shell cmd))
         (lines (--filter (not (s-blank-p it)) (s-split "\n" (s-trim raw))))
         (clean (lambda (s) (if (or (null s) (s-equals-p s "null")) "" s)))
         (parsed (--map (let* ((parts (s-split "\t" it))
                               (last-ts (funcall clean (nth 0 parts)))
                               (event-ts (funcall clean (nth 1 parts)))
                               (src (funcall clean (nth 5 parts)))
                               (rpt (funcall clean (nth 6 parts))))
                          (list :when (if (s-blank? last-ts) event-ts last-ts)
                                :type (funcall clean (nth 2 parts))
                                :reason (funcall clean (nth 3 parts))
                                :count (funcall clean (nth 4 parts))
                                :from (if (s-blank? src) rpt src)
                                :msg (funcall clean (nth 7 parts))))
                        lines)))
    (-sort (lambda (a b) (string< (plist-get a :when) (plist-get b :when))) parsed)))

(defun kubectl--insert-events (parsed)
  "Insert formatted event PARSED list (from `kubectl--fetch-events') at point."
  (if (null parsed)
      (insert (propertize "  no events\n" 'face 'kubectl-timestamp-face))
    (let* ((max-reason (apply 'max 6 (--map (length (plist-get it :reason)) parsed)))
           (max-from (apply 'max 4 (--map (length (plist-get it :from)) parsed)))
           (fmt (lambda (e)
                  (format "  %s  %s  %s  %s  %sx  %s  %s"
                          (s-pad-right 20 " " (plist-get e :when))
                          (s-pad-left 4 " " (kubectl--relative-time-ago (plist-get e :when)))
                          (s-pad-right 7 " " (plist-get e :type))
                          (s-pad-right max-reason " " (plist-get e :reason))
                          (s-pad-left 3 " " (if (s-blank? (plist-get e :count)) "1" (plist-get e :count)))
                          (s-pad-right max-from " " (plist-get e :from))
                          (plist-get e :msg)))))
      (--each parsed
        (let ((face (cond ((s-equals-p (plist-get it :type) "Warning") 'kubectl-warning-face)
                          ((s-equals-p (plist-get it :type) "Error") 'kubectl-error-face)
                          (t nil))))
          (insert (if face
                      (propertize (funcall fmt it) 'face face)
                    (funcall fmt it))
                  "\n"))))))

(defun kubectl-show-events-at-point ()
  "Show recent events for the resource at point in a popup near the cursor."
  (interactive)
  (let* ((info (kubectl--resource-on-line))
         (name (plist-get info :name))
         (kind (plist-get info :kind))
         (ns (or (plist-get info :namespace)
                 (and (not (member kind '("node" "nodes" "namespace" "namespaces"
                                          "clusterrole" "clusterrolebinding"
                                          "persistentvolume" "storageclass")))
                      (not (s-blank? kubectl-current-namespace))
                      kubectl-current-namespace)))
         (display (plist-get info :display)))
    (unless name
      (user-error "No resource on this line"))
    (let ((parsed (kubectl--fetch-events name ns)))
      (with-current-buffer (get-buffer-create kubectl--popup-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq kubectl--popup-has-pods nil)
          (insert (propertize (format "%s — %d event(s)\n\n" display (length parsed)) 'face 'bold))
          (kubectl--insert-events parsed)
          (goto-char (point-min))))
      (kubectl--popup-show))))

(defun kubectl-copy-as-kill-node-on-line ()
  (interactive)
  (let* ((node-like (s-trim (car (s-match " i-.*?internal" (substring-no-properties (current-line-contents)))))))
    (when node-like
      (kill-new node-like)
      (message "Copied: %s" node-like))))


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
                         (car commands))))
    (kubectl-confirm prompt
                     (lambda ()
                       (--map (kubectl--run-process-bg it) commands)))))

(defun kubectl-run-cronjob-at-point ()
  (interactive)
  (let* ((resources (kubectl-get-resources-at-point-or-region))
         (commands (->> resources
                        (--map (format "kubectl create job --from=%s %s" it (s-chop-prefix "-" (s-right 60 (format "%s-trigger-dgempesaw-%s" (cadr (s-split "/" it)) (floor (float-time)))))))))
         (prompt (format "Confirm create job (cluster: %s | context: %s | namespace: %s)? %s"
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace
                         (car commands))))
    (kubectl-confirm prompt
                     (lambda ()
                       (--map (kubectl--run-process-bg it) commands)))))

(defun kubectl-get-resources-at-point-or-region ()
  (if (region-active-p)
      (->> (buffer-substring-no-properties (region-beginning) (region-end))
           (s-split "\n")
           (-map 'kubectl-line-resource-as-string))
    `(,(kubectl-current-line-resource-as-string))))

(defun kubectl--extract-pod-status (line)
  (when (s-starts-with? "pod/" line)
    (let* ((parts (s-split " +" line t))
           (ready-idx (--find-index (s-matches-p "^[0-9]+/[0-9]+$" it) parts)))
      (when ready-idx
        (nth (1+ ready-idx) parts)))))

(defun kubectl--get-pods-by-status ()
  (let ((pods-by-status (make-hash-table :test 'equal))
        (output (kubectl--shell "kubectl get pods --no-headers")))
    (--each (s-split "\n" (s-trim output))
      (let* ((parts (s-split " +" it t))
             (name (nth 0 parts))
             (status (nth 2 parts)))
        (when (and name status)
          (puthash status
                   (cons (format "pod/%s" name) (gethash status pods-by-status))
                   pods-by-status))))
    pods-by-status))

(defun kubectl--get-pods-detail (pod-names)
  (let ((names (->> pod-names (--map (s-chop-prefix "pod/" it)) (s-join ","))))
    (kubectl--shell
     (format "kubectl get pods --field-selector=metadata.name!=_ --no-headers -o wide 2>/dev/null | grep -E '%s'"
             (s-join "\\|" (->> pod-names (--map (s-chop-prefix "pod/" it))))))))

(defvar kubectl--confirm-prompt ""
  "Description shown in `kubectl-confirm-transient'.")
(defvar kubectl--confirm-action nil
  "Zero-arg function invoked when the user confirms via `kubectl-confirm'.")

(defun kubectl--confirm-execute ()
  (interactive)
  (let ((fn kubectl--confirm-action))
    (setq kubectl--confirm-prompt ""
          kubectl--confirm-action nil)
    (when fn (funcall fn))))

(transient-define-prefix kubectl-confirm-transient ()
  "Generic confirmation transient. Don't invoke directly — use `kubectl-confirm'."
  [:description (lambda () kubectl--confirm-prompt)
                ("y" "confirm" kubectl--confirm-execute)
                ("<return>" "confirm" kubectl--confirm-execute)
                ("n" "cancel" transient-quit-one)
                ("q" "cancel" transient-quit-one)])

(defun kubectl-confirm (prompt action-fn)
  "Show PROMPT in a centered transient; on y / RET call ACTION-FN with no args.
Replaces `y-or-n-p' so confirmation lives in the same visual layer as the
rest of the kubectl transient flow."
  (setq kubectl--confirm-prompt prompt
        kubectl--confirm-action action-fn)
  (kubectl-confirm-transient))

(defvar kubectl--bulk-delete-pods nil)
(defvar kubectl--bulk-delete-detail nil)
(defvar kubectl--bulk-delete-status nil)

(defun kubectl--bulk-delete-execute (force)
  (kubectl-with-aws-env
    (let ((pods kubectl--bulk-delete-pods)
          (default-directory kubectl--my-directory)
          (force-flag (if force " --force" "")))
      (--each pods
        (bpr-spawn (format "kubectl delete%s %s" force-flag it)))
      (message "%s %d pods..." (if force "Force deleting" "Deleting") (length pods)))))

(transient-define-prefix kubectl-bulk-delete-confirm-transient ()
  "Confirm bulk pod deletion"
  [:description
   (lambda ()
     (format "DELETE %d %s pods (%s | %s | %s)\n\n%s"
             (length kubectl--bulk-delete-pods)
             kubectl--bulk-delete-status
             kubectl-current-cluster
             kubectl-current-context
             kubectl-current-namespace
             kubectl--bulk-delete-detail))
   ("y" "delete" (lambda () (interactive) (kubectl--bulk-delete-execute nil)))
   ("!" "force delete" (lambda () (interactive) (kubectl--bulk-delete-execute t)))
   ("n" "cancel" transient-quit-one)
   ("q" "cancel" transient-quit-one)])

(defun kubectl-bulk-delete-pods-by-status ()
  (interactive)
  (let* ((pods-by-status (kubectl--get-pods-by-status))
         (status-choices (let (keys)
                           (maphash (lambda (k v)
                                      (push (format "%s (%d)" k (length v)) keys))
                                    pods-by-status)
                           keys))
         (chosen (completing-read "Delete pods with status: " status-choices nil t))
         (chosen-status (s-trim (car (s-split " (" chosen)))))
    (setq kubectl--bulk-delete-pods (gethash chosen-status pods-by-status)
          kubectl--bulk-delete-status chosen-status
          kubectl--bulk-delete-detail (kubectl--get-pods-detail kubectl--bulk-delete-pods))
    (kubectl-bulk-delete-confirm-transient)))

(provide 'kubectl-at-point)

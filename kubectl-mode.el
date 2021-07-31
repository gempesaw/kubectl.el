(require 's)
(require 'dash)
(require 'kubectl-process)
(require 'kubectl-edit-mode)


(defvar kubectl-available-contexts '())
(defvar kubectl-available-namespaces '())
(defvar kubectl-current-context "")
(defvar kubectl-current-cluster "")
(defvar kubectl-current-namespace "")
(defvar kubectl-current-display "")

(defvar kubectl-main-buffer-name "*kubectl*")
(defvar kubectl-resources-default "ds,sts,deploy,rs,po,svc,ing")


(defun kubectl ()
  (interactive)
  (with-current-buffer (get-buffer-create kubectl-main-buffer-name)
    (setq buffer-read-only t)
    (switch-to-buffer (current-buffer))
    (kubectl-mode)
    (kubectl-init)))

(define-derived-mode kubectl-mode special-mode "kubectl"
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (define-key kubectl-mode-map (kbd "r") 'kubectl-choose-resource)
  (define-key kubectl-mode-map (kbd "R") 'kubectl-choose-resource)
  (define-key kubectl-mode-map (kbd "N") 'kubectl-choose-namespace)
  (define-key kubectl-mode-map (kbd "C") 'kubectl-choose-context)

  (define-key kubectl-mode-map (kbd "e") 'kubectl-edit-resource-at-point)
  (define-key kubectl-mode-map (kbd "k") 'kubectl-delete-resource-at-point)
  (define-key kubectl-mode-map (kbd "o") 'kubectl-get-yaml-at-point)
  (define-key kubectl-mode-map (kbd "<return>") 'kubectl-describe-resource-at-point)

  (define-key kubectl-mode-map (kbd "x") 'kubectl-pod-exec)
  (define-key kubectl-mode-map (kbd "l") 'kubectl-pod-logs)

  (define-key kubectl-mode-map (kbd "n") 'kubectl-next-line)
  (define-key kubectl-mode-map (kbd "p") 'kubectl-previous-line)

  (define-key kubectl-mode-map (kbd "g") 'kubectl-init)
  (define-key kubectl-mode-map (kbd "$") 'kubectl-show-log-buffer)
  (define-key kubectl-mode-map (kbd ":") 'kubectl-run-custom-command))


(defun kubectl-init ()
  (interactive)
  (with-current-buffer (get-buffer-create kubectl-main-buffer-name)
    (let ((inhibit-read-only t))
      (kubectl-print-buffer)
      (kubectl-get-resources)
      (kubectl-get-namespaces)
      (kubectl-get-api-resources))))

(defun kubectl--get-current-context ()
  (let* ((kube-config-filename "~/.kube/config")
         (current-context-name (s-chomp (shell-command-to-string (format "yq eval '.current-context' %s" kube-config-filename))))
         (current-context (s-split "\n" (s-chomp (shell-command-to-string (format "yq eval '.contexts.[] | select(.name == \"%s\") | .context' %s" current-context-name kube-config-filename)))))
         (available-contexts (s-split "\n" (s-chomp (shell-command-to-string (format "yq eval '.contexts.[].name' %s" kube-config-filename)))))
         (parts (--map (s-trim (cadr (s-split-up-to ":" it 1))) current-context)))
    (setq kubectl-available-contexts available-contexts
          kubectl-current-context current-context-name
          kubectl-current-cluster (car parts)
          kubectl-current-namespace (cadr parts))
    (-concat `(,(format "Context: %s" current-context-name)) (--map (s-capitalize it) current-context))))

(defun kubectl-get-resources ()
  (when (kubectl-ensure-logged-in)
    (kubectl--run-process (format "kubectl get %s" kubectl-resources-current))))

(defun kubectl-get-namespaces ()
  (when (kubectl-ensure-logged-in "don't force login")
    (kubectl--run-process-bg "kubectl get namespaces" 'kubectl--parse-namespaces)))

(defun kubectl--parse-namespaces (process)
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (setq kubectl-available-namespaces
            (--filter (and (not (s-equals? "" it))
                           (not (s-equals? "NAME" it)))
                      (--map (car (s-split " " it))
                             (s-split "\n" (buffer-substring-no-properties (point-min) (point-max)))))))
    (kill-buffer buffer)))

(defun kubectl-get-api-resources ()
  (when (kubectl-ensure-logged-in "don't force login")
    (kubectl--run-process-bg "kubectl api-resources" 'kubectl--parse-api-resources)))

(defun kubectl--parse-api-resources (process)
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (let ((lines (-slice (--map (s-split " +" it) (s-split "\n" (buffer-substring-no-properties (point-min) (point-max)))) 1)))
        (setq kubectl-api-abbreviations (--map (cadr it) (--filter (eq (length it) 5) lines)))
        (setq kubectl-api-resource-names (--map (car it) lines))))))

(defun kubectl-print-buffer ()
  (with-current-buffer (get-buffer-create kubectl-main-buffer-name)
    (let ((inhibit-read-only t)
          (context (kubectl--get-current-context)))
      (erase-buffer)
      (insert (s-join "\n" context))
      (insert (format "\nResources: %s" kubectl-resources-current))
      (insert "\n\n\n")
      (when (not (eq kubectl-current-display ""))
        (insert kubectl-current-display)))))

(defun kubectl-redraw (text-to-display)
  (with-current-buffer (get-buffer-create kubectl-main-buffer-name)
    (let ((inhibit-read-only t))
      (setq kubectl-current-display text-to-display)
      (kubectl-print-buffer))))

(defun kubectl-process-sentinel (process signal)
  (with-current-buffer (process-buffer process)
    (let* ((output (buffer-substring-no-properties (point-min) (point-max))))
      (kubectl-update-process-buffer output)
      (message "Finished")
      (kubectl-redraw output)
      (kill-buffer (current-buffer)))))

(defun kubectl-show-log-buffer ()
  (interactive)
  (pop-to-buffer (kubectl--get-process-buffer)))

(defun kubectl-find-next-line ()
  (save-excursion
    (end-of-line)
    (if (search-forward-regexp "^[[:alpha:].[:digit:]]+?/" nil t)
        (progn (beginning-of-line)
               (point))
      (message "no additional sections available")
      nil)))

(defun kubectl-find-previous-line ()
  (save-excursion
    (if (search-backward-regexp "^[[:alpha:].[:digit:]]+?/" nil t)
        (progn (beginning-of-line)
               (point))
      (message "no additional sections available")
      nil)))

(defun kubectl-next-line ()
  (interactive)
  (let ((next-line (kubectl-find-next-line)))
    (when next-line
      (goto-char next-line))))

(defun kubectl-previous-line ()
  (interactive)
  (let ((previous-line (kubectl-find-previous-line)))
    (when previous-line
      (goto-char previous-line))))

(defun kubectl-ensure-logged-in (&optional force-login-arg)
  (let* ((expiration (seconds-to-time (string-to-number (getenv "AWS_OKTA_SESSION_EXPIRATION"))))
         (expired (time-subtract expiration (current-time)))
         (force-login (or force-login-arg "t")))
    (if (< (time-to-seconds expired) 0)
        (progn
          (when (s-equals? force-login "t")
            (call-interactively #'dg-aws-okta-login))
          nil)
      t)))

(defun kubectl-describe-resource-at-point ()
  (interactive)
  (let ((resource-at-point (car (s-split " " (substring-no-properties (current-line-contents))))))
    (kubectl--run-process-and-pop (format "kubectl describe %s" resource-at-point))))

(defun kubectl-run-custom-command (command)
  (interactive "sCommand to run: kubectl ")
  (kubectl--run-process-and-pop (format "kubectl %s" command)))

(defun kubectl-choose-resource (resource)
  (interactive (list (completing-read (format "Resource to query for: (%s)" kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names nil t))))
  (if (s-equals? resource "")
      (setq kubectl-resources-current kubectl-resources-default)
    (let ((new-resources (s-join "," `(,kubectl-resources-current ,resource))))
      (setq kubectl-resources-current new-resources)))
  (kubectl-get-resources))

(defun kubectl-choose-namespace (ns)
  (interactive (list (completing-read "namespace to switch to: " kubectl-available-namespaces nil t)))
  (shell-command-to-string (format "kubectl config set-context --current --namespace %s" ns))
  (setq kubectl-current-display "")
  (kubectl-init))

(defun kubectl-choose-context (context)
  (interactive (list (completing-read "context to switch to: " kubectl-available-contexts nil t)))
  (shell-command-to-string (format "kubectl config use-context %s" context))
  (setq kubectl-current-display "")
  (kubectl-init))

(defun kubectl-pod-exec ()
  (interactive)
  (let* ((pod (car (s-split " " (substring-no-properties (current-line-contents)))))
         (pod-p (s-equals-p "pod" (car (s-split "/" pod))))
         (buf nil))
    (if pod-p
        (progn
          (setq buf (create-new-shell-here))
          (select-window (display-buffer buf))
          (insert (format "aws-okta exec sk8 -- kubectl exec -it %s -- bash" pod)))
      (message (format "expected a pod, but %s is not a pod" pod)))))

(defun kubectl-pod-logs ()
  (interactive)
  (let* ((pod (car (s-split " " (substring-no-properties (current-line-contents)))))
         (pod-p (s-equals-p "pod" (car (s-split "/" pod))))
         (cmd (format "kubectl logs --tail=50 -f %s" pod))
         (buf nil))
    (if pod-p
        (async-shell-command cmd)
      (message (format "expected a pod, but %s is not a pod" pod)))))

(defun kubectl-get-yaml-at-point ()
  (interactive)
  (let* ((resource-at-point (car (s-split " " (substring-no-properties (current-line-contents))))))
    (kubectl--run-process-and-pop (format "kubectl get %s --output yaml" resource-at-point))))

(defun kubectl-delete-resource-at-point ()
  (interactive)
  (let* ((resource-at-point (car (s-split " " (substring-no-properties (current-line-contents)))))
         (prompt (format "Confirm DELETE %s (cluster: %s | context: %s | namespace: %s) ?"
                         resource-at-point
                         kubectl-current-cluster
                         kubectl-current-context
                         kubectl-current-namespace))
         (bpr-process-mode 'kubectl-command-mode))
    (when (y-or-n-p prompt)
      (bpr-spawn (format "kubectl delete %s" resource-at-point)))))

(provide 'kubectl-mode)

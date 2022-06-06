(require 's)
(require 'dash)
(require 'kubectl-process)
(require 'kubectl-edit-mode)
(require 'kubectl-command-mode)
(require 'kubectl-transient)
(require 'kubectl-mode-nav)
(require 'kubectl-summary)
(require 'kubectl-autorefresh)

(defvar kubectl-available-contexts '())
(defvar kubectl-available-namespaces '())
(defvar kubectl-cached-namespaces '())
(defvar kubectl-current-context "")
(defvar kubectl-current-aws-profile "")
(defvar kubectl-current-cluster "")
(defvar kubectl-current-namespace "")
(defvar kubectl-all-namespaces nil)
(defvar kubectl-current-display "")
(defvar kubectl-is-pulling "false")

(defvar kubectl-main-buffer-name "*kubectl*")
(defvar kubectl-resources-default "ds,sts,deploy,po,svc,ing,cm")
(defvar kubectl-resources-current "ds,sts,deploy,po,svc,ing,cm")
(defvar kubectl-api-abbreviations '())
(defvar kubectl-api-resource-names '())
(defvar kubectl--command-options "")
(defvar kubectl-aws-expiration "")

(setenv "AWS_EC2_METADATA_DISABLED" "true")

;;;###autoload
(define-derived-mode kubectl-mode special-mode "kubectl"
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (define-key kubectl-mode-map (kbd "r") 'kubectl-transient-choose-resource)
  (define-key kubectl-mode-map (kbd "A") 'kubectl-transient-choose-resource-all-ns)
  (define-key kubectl-mode-map (kbd "R") 'kubectl-transient-choose-resource)
  (define-key kubectl-mode-map (kbd "G") 'kubectl-toggle-autorefresh)
  (define-key kubectl-mode-map (kbd "N") 'kubectl-choose-namespace)
  (define-key kubectl-mode-map (kbd "C") 'kubectl-transient-choose-context)

  (define-key kubectl-mode-map (kbd "e") 'kubectl-edit-resource-at-point)
  (define-key kubectl-mode-map (kbd "k") 'kubectl-delete-resource-at-point)
  (define-key kubectl-mode-map (kbd "o") 'kubectl-get-yaml-at-point)
  (define-key kubectl-mode-map (kbd "<return>") 'kubectl-describe-resource-at-point)

  (define-key kubectl-mode-map (kbd "f") 'kubectl-port-forward)
  (define-key kubectl-mode-map (kbd "x") 'kubectl-pod-exec)
  (define-key kubectl-mode-map (kbd "l") 'kubectl-pod-logs)

  (define-key kubectl-mode-map (kbd "n") 'kubectl-next-line)
  (define-key kubectl-mode-map (kbd "p") 'kubectl-previous-line)

  (define-key kubectl-mode-map (kbd "M-n") 'kubectl-next-section)
  (define-key kubectl-mode-map (kbd "M-p") 'kubectl-previous-section)

  (define-key kubectl-mode-map (kbd "g") 'kubectl-init)
  (define-key kubectl-mode-map (kbd "$") 'kubectl-show-log-buffer)
  (define-key kubectl-mode-map (kbd ":") 'kubectl-run-custom-command)

  (define-key kubectl-mode-map (kbd "?") 'kubectl-transient-help)
  (define-key kubectl-mode-map (kbd "h") 'kubectl-transient-help)
  )


(defun kubectl-init ()
  (interactive)
  (with-current-buffer (get-buffer-create kubectl-main-buffer-name)
    (let ((inhibit-read-only t))
      (setq kubectl-is-pulling t)
      (kubectl-print-buffer)
      (kubectl-get-resources))))

(defun kubectl--get-available-contexts ()
  (cdr (s-split "\n" (s-chomp (shell-command-to-string (format "yq eval '.contexts.[].name' %s" "~/.kube/config"))))))

(defun kubectl--get-aws-profiles ()
  (cdr (reverse (--map (s-chop-suffix "]" it) (s-split "\n" (shell-command-to-string "awk '/^\\[profile/ { print $2}' ~/.aws/config"))))))

(defun kubectl-get-resources ()
  (if kubectl-all-namespaces
      (kubectl--run-process (format "kubectl get %s --all-namespaces" kubectl-resources-current-all-ns))
    (kubectl--run-process (format "kubectl get %s" kubectl-resources-current)))
  (setq kubectl-is-pulling nil)
  (kubectl-maybe-autorefresh))

(defun kubectl-get-namespaces ()
  (kubectl--run-process-bg "kubectl get namespaces" 'kubectl--parse-namespaces))

(defun kubectl--cache-namespaces (namespaces)
  (setq kubectl-cached-namespaces (-uniq (-sort 'string-lessp (-concat namespaces kubectl-cached-namespaces)))))

(defun kubectl--parse-namespaces (process)
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (setq kubectl-available-namespaces
            (--filter (and (not (s-equals? "" it))
                           (not (s-equals? "NAME" it)))
                      (--map (car (s-split " " it))
                             (s-split "\n" (buffer-substring-no-properties (point-min) (point-max))))))
      (kubectl--cache-namespaces kubectl-available-namespaces))
    (kill-buffer buffer)))

(defun kubectl-get-api-resources ()
  (kubectl--run-process-bg "kubectl api-resources" 'kubectl--parse-api-resources))

(defun kubectl--parse-api-resources (process)
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (let ((lines (-slice (--map (s-split " +" it) (s-split "\n" (buffer-substring-no-properties (point-min) (point-max)))) 1)))
        (setq kubectl-api-abbreviations (--map (cadr it) (--filter (eq (length it) 5) lines)))
        (setq kubectl-api-resource-names (--map (car it) lines))))))

(defun kubectl-print-buffer ()
  (with-current-buffer (get-buffer-create kubectl-main-buffer-name)
    (let ((inhibit-read-only t)
          (context (kubectl--get-summary))
          (point-before-print (point)))
      (erase-buffer)
      (insert (s-join "\n"
                      (--map (s-join
                              " " (-concat `(,(s-pad-right 10 " " (format "%s:" (s-capitalize (car it)))))
                                           `(,(if (and kubectl-all-namespaces
                                                       (s-equals-p (car it) "namespace"))
                                                  "All Namespaces"
                                                (cadr it)))))
                             context)))
      (insert "\n\n\n")
      (when (not (eq kubectl-current-display ""))
        (insert kubectl-current-display)
        (goto-char point-before-print )))))

(defun kubectl-redraw (text-to-display)
  (with-current-buffer (get-buffer-create kubectl-main-buffer-name)
    (let ((inhibit-read-only t))
      (setq kubectl-current-display text-to-display)
      (kubectl-print-buffer))))

(defun kubectl-show-log-buffer ()
  (interactive)
  (pop-to-buffer (kubectl--get-process-buffer))
  (goto-char (point-max)))

(defun kubectl-current-line-resource-as-string ()
  (let* ((parts (s-split " +" (substring-no-properties (current-line-contents))))
         (resource (if kubectl-all-namespaces (cadr parts) (car parts)))
         (namespace-flag (if kubectl-all-namespaces (format "--namespace %s" (car parts)) ""))
         (type-prefix (if (and kubectl-all-namespaces
                               (not (s-contains-p "," kubectl-resources-current-all-ns)))
                          kubectl-resources-current-all-ns
                        (if (and (not kubectl-all-namespaces)
                                 (not (s-contains-p "," kubectl-resources-current)))
                            kubectl-resources-current
                          ""))))
    (format "%s %s %s" namespace-flag type-prefix resource)))

(defun kubectl-describe-resource-at-point ()
  (interactive)
  (kubectl--run-process-and-pop (format "kubectl describe %s" (kubectl-current-line-resource-as-string))))

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
  (interactive (list (completing-read (format "namespace to switch to: [%s]" kubectl-current-namespace) kubectl-available-namespaces nil nil)))
  (when (not (s-blank-p ns))
    (shell-command-to-string (format "kubectl config set-context --current --namespace %s" ns))
    (setq kubectl-current-display ""
          kubectl-all-namespaces (s-blank-p ns))
    (kubectl-get-resources)))

(defun kubectl-update-context (context)
  (when (not (s-blank-p context))
    (shell-command-to-string (format "kubectl config use-context %s" context))
    (setq kubectl-current-display "")
    (kubectl-get-namespaces)
    (kubectl-get-api-resources)
    (kubectl-init)))

(defun kubectl-pod-exec ()
  (interactive)
  (let ((thing-at-point (car (s-split " " (substring-no-properties (current-line-contents))))))
    (kubectl--do-to-thing-at-point (format "aws-okta exec %s -- kubectl exec -it %s -- sh" kubectl-current-aws-profile thing-at-point))))

(defun kubectl-pod-logs ()
  (interactive)
  (let ((thing-at-point (car (s-split " " (substring-no-properties (current-line-contents))))))
    (kubectl--do-to-thing-at-point (format "kubectl logs --tail=50 -f %s" thing-at-point))))

(defun kubectl--do-to-thing-at-point (command)
  (interactive)
  (let* (
         (buf nil))
    (setq buf (create-new-shell-here))
    (select-window (display-buffer buf))
    (insert command)))

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

(provide 'kubectl-mode)

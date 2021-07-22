(require 's)
(require 'dash)

(setq kubectl-main-buffer (get-buffer-create "kubectl")
      kubectl-process-buffer (get-buffer-create "kubectl-process")
      kubectl-process-temp-buffer-name "kubectl-process-temp"
      kubectl-resources-default "ds,sts,deploy,rs,po,svc,ing"
      ;; kubectl-resources-current kubectl-resources-default
      kubectl-current-display ""
      kubectl-waiting-for-redraw nil
      kubectl-current-namespaces '())

(defun kubectl ()
  (interactive)
  (with-current-buffer kubectl-process-buffer
    (setq buffer-read-only t))
  (switch-to-buffer kubectl-main-buffer)
  (kubectl-mode)
  (kubectl-init))

(define-derived-mode kubectl-mode special-mode "kubectl"
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (define-key kubectl-mode-map (kbd "r") 'kubectl-choose-resource)
  (define-key kubectl-mode-map (kbd "R") 'kubectl-choose-resource)
  (define-key kubectl-mode-map (kbd "N") 'kubectl-choose-namespace)

  (define-key kubectl-mode-map (kbd "n") 'kubectl-next-line)
  (define-key kubectl-mode-map (kbd "p") 'kubectl-previous-line)

  (define-key kubectl-mode-map (kbd "g") 'kubectl-init)
  (define-key kubectl-mode-map (kbd "<return>") 'kubectl-dwim)
  (define-key kubectl-mode-map (kbd "$") 'kubectl-popup-process-window)
  (define-key kubectl-mode-map (kbd ":") 'kubectl-run-custom-command)
  )

(defun kubectl-get-current-context ()
  (let* ((kube-config-filename "~/.kube/config")
         (current-context-name (s-chomp (shell-command-to-string (format "yq eval '.current-context' %s" kube-config-filename))))
         (current-context (s-split "\n" (s-chomp (shell-command-to-string (format "yq eval '.contexts.[] | select(.name == \"%s\") | .context' %s" current-context-name kube-config-filename))))))
    (-concat `(,(format "Context: %s" current-context-name)) (--map (s-capitalize it) current-context))))

(defun kubectl-update-process-buffer (output)
  (with-current-buffer kubectl-process-buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert output))))

(defun kubectl-run-command (&optional commandArg)
  (when (not commandArg)
    (message (format "attempting to fetch %s..." kubectl-resources-current)))
  (when (kubectl-ensure-logged-in)
    (let* ((buf (generate-new-buffer kubectl-process-temp-buffer-name))
           (command (or commandArg (format "kubectl get %s" kubectl-resources-current)))
           (proc (apply 'start-process (-concat `("kubectl-process" ,buf) (s-split " " command)))))
      (if (process-live-p proc)
          (progn (set-process-sentinel proc #'kubectl-process-sentinel)
                 (kubectl-update-process-buffer (format "---\n%s\n\n" command)))))))

(defun kubectl-get-namespaces ()
  (when (kubectl-ensure-logged-in "don't force login")
    (let* ((buf (generate-new-buffer (format "%s-ns" kubectl-process-temp-buffer-name)))
           (proc (start-process "kubectl-background-process" buf "kubectl" "get" "namespaces")))
      (setq kubectl-namespace-command-output '())
      (when (process-live-p proc)
        (kubectl-update-process-buffer (format "---\n%s\n\n" "kubectl get namespaces"))
        (set-process-filter proc '(lambda (process output) (setq kubectl-namespace-command-output (cons output kubectl-namespace-command-output))))
        (set-process-sentinel proc '(lambda (process output)
                                      (kill-buffer (process-buffer process))
                                      (setq kubectl-current-namespaces
                                            (--filter (and (not (s-equals? "" it)) (not (s-equals? "NAME" it))) (-map (lambda (line) (car (s-split " " line))) (s-split "\n" (s-join "" (reverse kubectl-namespace-command-output))))))))))))

(defun kubectl-get-api-resources ()
  (when (kubectl-ensure-logged-in "don't force login")
    (let* ((buf (generate-new-buffer (format "%s-api-resources" kubectl-process-temp-buffer-name)))
           (proc (start-process "kubectl-background-process" buf "kubectl" "api-resources")))
      (setq kubectl-api-resources '())
      (setq kubectl-api-abbreviations '())
      (setq kubectl-api-resource-names '())
      (when (process-live-p proc)
        (kubectl-update-process-buffer (format "---\n%s\n\n" "kubectl api-resources"))
        (set-process-filter proc '(lambda (process output) (setq kubectl-api-resources (cons output kubectl-api-resources))))
        (set-process-sentinel proc '(lambda (process output)
                                      (kill-buffer (process-buffer process))
                                      (let ((lines (-slice (--map (s-split " +" it) (s-split "\n" (s-join "" (reverse kubectl-api-resources)))) 1)))
                                        (setq kubectl-api-abbreviations (--map (cadr it) (--filter (eq (length it) 5) lines)))
                                        (setq kubectl-api-resource-names (--map (car it) lines)))))))))

(defun kubectl-print-buffer ()
  (with-current-buffer kubectl-main-buffer
    (let ((inhibit-read-only t)
          (context (kubectl-get-current-context)))
      (erase-buffer)
      (insert (s-join "\n" context))
      (insert (format "\nResources: %s" kubectl-resources-current))
      (insert "\n\n\n")
      (when (not (eq kubectl-current-display ""))
        (insert kubectl-current-display)))))

(defun kubectl-init ()
  (interactive)
  (with-current-buffer kubectl-main-buffer
    (let ((inhibit-read-only t))
      (kubectl-print-buffer)
      (setq kubectl-waiting-for-redraw t)
      (kubectl-run-command)
      (kubectl-get-namespaces))))

(defun kubectl-redraw (text-to-display)
  (when kubectl-waiting-for-redraw
    (setq kubectl-waiting-for-redraw nil)
    (with-current-buffer kubectl-main-buffer
      (let ((inhibit-read-only t))
        (setq kubectl-current-display text-to-display)
        (kubectl-print-buffer)))))

(defun kubectl-process-sentinel (process signal)
  (with-current-buffer (process-buffer process)
    (let* ((output (buffer-substring-no-properties (point-min) (point-max))))
      (kubectl-update-process-buffer output)
      (message "Finished")
      (kubectl-redraw output)
      (kill-buffer (current-buffer)))))

(defun kubectl-popup-process-window ()
  (interactive)
  (pop-to-buffer kubectl-process-buffer))

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

(defun kubectl-dwim ()
  (interactive)
  (let ((thing (car (s-split " " (substring-no-properties (current-line-contents))))))
    (async-shell-command (format "kubectl describe %s" thing) (get-buffer-create "ayaya"))))

(defun kubectl-run-custom-command (command)
  (interactive "sCommand to run: kubectl ")
  (async-shell-command (format "kubectl %s" command) (get-buffer-create (s-join "-" (s-split " " command)))))

(defun kubectl-choose-resource (resource)
  (interactive (list (completing-read (format "Resource to query for: (%s)" kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names nil t))))
  (if (s-equals? resource "")
      (setq kubectl-resources-current kubectl-resources-default)
    (let ((new-resources (s-join "," `(,kubectl-resources-current ,resource))))
      (setq kubectl-resources-current new-resources)))
  (setq kubectl-waiting-for-redraw t)
  (kubectl-run-command))

(defun kubectl-choose-namespace (ns)
  (interactive (list (completing-read "namespace to switch to: " kubectl-current-namespaces nil t)))
  (shell-command-to-string (format "kubectl config set-context --current --namespace %s" ns))
  (setq kubectl-current-display "")
  (kubectl-init))

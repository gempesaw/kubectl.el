(require 's)
(require 'dash)
(require 'kubectl-process)
(require 'kubectl-edit-mode)
(require 'kubectl-command-mode)
(require 'kubectl-transient)
(require 'kubectl-mode-nav)
(require 'kubectl-summary)
(require 'kubectl-autorefresh)
(require 'kubectl-at-point)

(defvar kubectl-available-contexts '())
(defvar kubectl-available-namespaces '())
(defvar kubectl-cached-namespaces '())
(defvar kubectl-current-context "")
(defvar kubectl-current-namespace "")
(defvar kubectl-current-role "")
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
  (define-key kubectl-mode-map (kbd "x") 'kubectl-shell-at-point)
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
  (let ((contexts-lookup "~/opt/pd-kubectx-cli/pd_kubectx_cli/clusters.json"))
    (s-split "\n" (s-chomp (shell-command-to-string (format "jq -r .clusters[].name < %s" contexts-lookup))))))

(defun kubectl-get-namespaces ()
  (kubectl--cache-namespaces (--map (s-chop-prefix "> " it) (s-split "\n" (shell-command-to-string "pk ns --list")))))

(defun kubectl--cache-namespaces (namespaces)
  (setq kubectl-cached-namespaces (-uniq (-sort 'string-lessp (-concat namespaces kubectl-cached-namespaces)))))

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
        (insert (kubectl--colorize-percentages kubectl-current-display))
        (goto-char point-before-print )))))

(defun kubectl--colorize-percentages (content)
  (--> content
       (s-split "\n" it)
       (--map (if (s-matches-p "%" it)
                  it
                it) it)
       (s-join "\n" it)))

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

;; (defun kubectl-choose-resource (resource)
;;   (interactive (list (completing-read (format "Resource to query for: (%s)" kubectl-resources-current) (-concat kubectl-api-abbreviations kubectl-api-resource-names nil t))))
;;   (if (s-equals? resource "")
;;       (setq kubectl-resources-current kubectl-resources-default)
;;     (let ((new-resources (s-join "," `(,kubectl-resources-current ,resource))))
;;       (setq kubectl-resources-current new-resources)))
;;   (kubectl-get-resources))

(defun kubectl-choose-namespace (ns)
  (interactive (list (completing-read (format "namespace to switch to: [%s]" kubectl-current-namespace) kubectl-cached-namespaces nil nil)))
  (when (not (s-blank-p ns))
    (shell-command-to-string (format "pk ns %s" ns))
    (setq kubectl-current-display ""
          kubectl-all-namespaces (s-blank-p ns))
    (kubectl-get-resources)))

(provide 'kubectl-mode)

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
(require 'kubectl-draw)
(require 'kubectl-font-lock-keywords)

(defvar kubectl-available-contexts '())
(defvar kubectl-available-namespaces '())
(defvar kubectl-cached-namespaces '())
(defvar kubectl-current-context "")
(defvar kubectl-current-namespace "")
(defvar kubectl-previous-current-namespace "")
(defvar kubectl-current-role "")
(defvar kubectl-current-display "")
(defvar kubectl-is-pulling "false")

(defvar kubectl-main-buffer-name "*kubectl*")
(defvar kubectl-resources-default "ro,ds,sts,deploy,po,svc,ing,cm")
(defvar kubectl-resources-current "ro,ds,sts,deploy,po,svc,ing,cm")
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
  (define-key kubectl-mode-map (kbd "t") 'kubectl-toggle-capacity)
  (define-key kubectl-mode-map (kbd "|") 'kubectl-transient-grep)
  (define-key kubectl-mode-map (kbd "A") 'kubectl-transient-choose-resource-all-ns)
  (define-key kubectl-mode-map (kbd "R") 'kubectl-transient-choose-resource)
  (define-key kubectl-mode-map (kbd "G") 'kubectl-toggle-autorefresh)
  (define-key kubectl-mode-map (kbd "N") 'kubectl-choose-namespace)
  (define-key kubectl-mode-map (kbd "C") 'kubectl-transient-choose-context)
  (define-key kubectl-mode-map (kbd "s") 'kubectl-sort-by)


  (define-key kubectl-mode-map (kbd "w") 'kubectl-copy-resource-at-point)
  (define-key kubectl-mode-map (kbd "0 w") 'kubectl-copy-line-at-point)
  (define-key kubectl-mode-map (kbd "e") 'kubectl-edit-resource-at-point)
  (define-key kubectl-mode-map (kbd "k") 'kubectl-delete-resource-at-point)
  (define-key kubectl-mode-map (kbd "o") 'kubectl-get-yaml-at-point)
  (define-key kubectl-mode-map (kbd "<return>") 'kubectl-describe-resource-at-point)

  (define-key kubectl-mode-map (kbd "f") 'kubectl-port-forward)
  (define-key kubectl-mode-map (kbd "x") 'kubectl-shell-at-point)
  (define-key kubectl-mode-map (kbd "d") 'kubectl-debug-at-point)
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
  (font-lock-add-keywords nil kubectl-font-lock-keywords))


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

(defun kubectl-current-line-resource-as-string ()
  (let* ((parts (s-split " +" (substring-no-properties (current-line-contents))))
         (resource (if kubectl-all-namespaces (cadr parts) (car parts)))
         (namespace-flag (if kubectl-all-namespaces (format "--namespace %s" (car parts)) "")))
    (format "%s %s" namespace-flag resource)))

(defun kubectl-describe-resource-at-point ()
  (interactive)
  (kubectl--run-process-and-pop (format "kubectl describe %s" (kubectl-current-line-resource-as-string))))

(defun kubectl-run-custom-command (command)
  (interactive "sCommand to run: kubectl ")
  (kubectl--run-process-and-pop (format "kubectl %s" command)))

(defun kubectl-choose-namespace (ns)
  (interactive (list (completing-read
                      (format "namespace to switch to: [%s]" kubectl-current-namespace)
                      (->> "kubectl get namespaces -oname | sort"
                           (shell-command-to-string)
                           (s-trim)
                           (s-split "\n")
                           (--map (cadr (s-split "/" it))))
                      nil
                      nil)))
  (when (not (s-blank-p ns))
    (shell-command-to-string (format "pk ns %s" ns))
    (setq kubectl-current-display ""
          kubectl-all-namespaces (s-blank-p ns)
          kubectl-current-namespace ns)
    (kubectl-get-resources)))

(provide 'kubectl-mode)

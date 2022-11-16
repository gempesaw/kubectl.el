(defvar kubectl--my-directory
  (expand-file-name (if load-file-name
                        ;; File is being loaded.
                        (file-name-directory load-file-name)
                      ;; File is being evaluated using, for example, `eval-buffer'.
                      default-directory)))

(defvar kubectl--get-fancy-filename "kubectl-get-fancy.sh")

(defvar kubectl--get-fancy-command (format "%s%s %%s \"%%s\" \"%%s\" \"%%s\"" kubectl--my-directory kubectl--get-fancy-filename))

(defun kubectl-get-resources ()
  (if kubectl-all-namespaces
      (kubectl--run-process (format "kubectl get %s --all-namespaces" kubectl-resources-current-all-ns))
    (kubectl-get-resources-for-namespace))
  (setq kubectl-is-pulling nil)
  (kubectl-maybe-autorefresh))

(defun kubectl-get-resources-for-namespace ()
  (let ((resources (--map (if (or (s-equals-p it "pod")
                                  (s-equals-p it "pods"))
                              "po"
                            it)
                          (s-split "," kubectl-resources-current))))
    (if (-contains-p resources "po")
        (kubectl-get-resources-for-namespace-with-pods resources)
      (kubectl--run-process (format "kubectl get %s -owide" kubectl-resources-current)))))

(defun kubectl-get-resources-for-namespace-with-pods (resources)
  (let ((before-resources (--take-while (not (s-equals-p "po" it)) resources))
        (after-resources (reverse (--take-while (not (s-equals-p "po" it)) (reverse resources)))))
    (kubectl-get-fancy-pods before-resources after-resources)))

(defun kubectl-get-fancy-pods (before-resources after-resources)
  (kubectl--run-process (format kubectl--get-fancy-command
                                kubectl-current-namespace
                                kubectl--command-options
                                (s-join "," before-resources)
                                (s-join "," after-resources))))

(provide 'kubectl-get-resources)

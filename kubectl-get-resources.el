(defvar kubectl--my-directory
  (expand-file-name (if load-file-name
                        ;; File is being loaded.
                        (file-name-directory load-file-name)
                      ;; File is being evaluated using, for example, `eval-buffer'.
                      default-directory)))

(defvar kubectl--watch-process nil)
(defun kubectl-get-resources ()
  (when (process-live-p kubectl--watch-process)
    (delete-process kubectl--watch-process))
  (let ((resources (if kubectl-all-namespaces
                       kubectl-resources-current-all-ns
                     kubectl-resources-current))
        (namespace (if kubectl-all-namespaces
                       "All Namespaces"
                     kubectl-current-namespace)))
    (setq kubectl--watch-process
          (start-process
           "kubectl-watch"
           kubectl-process-buffer-name
           "python" (f-expand (f-join kubectl--my-directory "watch.py")) resources namespace))
    (kubectl--refresh-current-display)
    (kubectl--refresh-kcnodes)
    (kubectl--get-resources-cancel)))

(defun kubectl--refresh-current-display ()
  (let* ((data-directory (f-expand (f-join kubectl--my-directory "data")))
         (resources (s-split "," (if kubectl-all-namespaces kubectl-resources-current-all-ns kubectl-resources-current)))
         (contents (->> resources
                        (--map (let ((filename (f-expand (f-join data-directory it))))
                                 (if (f-exists-p filename)
                                     (f-read-text filename)
                                   nil)))
                        (-remove-item "")
                        (s-join "\n"))))
    (when (and (process-live-p kubectl--watch-process)
               (not (s-equals-p contents kubectl-current-display)))
      (kubectl-redraw contents))
    (run-with-timer 2 nil 'kubectl--refresh-current-display)))

(defun kubectl--get-resources-cancel ()
  "cancel the watch cuz the window is unfocused"
  (when (process-live-p kubectl--watch-process)
    (when (not (get-buffer-window kubectl-main-buffer-name))
      (message "kubectl.el: cancelling watch")
      (delete-process kubectl--watch-process))
    (run-with-timer 60 nil 'kubectl--get-resources-cancel)))

(provide 'kubectl-get-resources)

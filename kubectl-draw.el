(defvar kubectl-replace-buffer " *kubectl--replace-buffer*")

(defvar kubectl--show-nodes nil
  "Whether to render the nodes section in the dashboard. Toggle with `kubectl-toggle-nodes'.")

(defun kubectl-toggle-nodes ()
  "Toggle visibility of the nodes section in the kubectl dashboard."
  (interactive)
  (setq kubectl--show-nodes (not kubectl--show-nodes))
  (kubectl-print-buffer))

(defun kubectl-print-buffer ()
  (let ((inhibit-read-only t)
        (context (kubectl--get-summary))
        (nodes (ht-get kubectl--resource-contents "kcnodes" "")))
    (with-current-buffer (get-buffer-create kubectl-replace-buffer)
      (erase-buffer)
      (insert (s-join "\n"
                      (--map (s-join
                              " " (-concat `(,(s-pad-right 10 " " (format "%s:" (s-capitalize (car it)))))
                                           `(,(if (and kubectl-all-namespaces
                                                       (s-equals-p (car it) "namespace"))
                                                  "All Namespaces"
                                                (cadr it)))))
                             context)))
      (when (and kubectl--show-nodes
                 (not (s-blank? (s-trim nodes))))
        (insert (format "\n\n%s" nodes)))

      (when (not (eq kubectl-current-display ""))
        (insert (s-replace-regexp "\n\\{2,\\}" "\n\n" (format "\n\n%s" kubectl-current-display)))))

    (with-current-buffer (get-buffer-create kubectl-main-buffer-name)
      (replace-buffer-contents kubectl-replace-buffer))))

(defun kubectl-redraw (text-to-display)
  (setq kubectl-current-display text-to-display)
  (kubectl-print-buffer))

(defun kubectl-redraw-harmless ()
  (kubectl-print-buffer))

(defun kubectl-show-log-buffer ()
  (interactive)
  (pop-to-buffer (kubectl--get-process-buffer))
  (goto-char (point-max)))



(provide 'kubectl-draw)

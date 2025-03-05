(defvar kubectl-replace-buffer " *kubectl--replace-buffer*")

(defun kubectl-print-buffer ()
  (let ((inhibit-read-only t)
        (context (kubectl--get-summary)))
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
      (when (and (not (eq (s-trim (s-replace "\n" "" kubectl--merged-nodes-capacity)) ""))
                 kubectl--view-kube-capacity)
        (insert (format "\n\n%s" kubectl--merged-nodes-capacity)))

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

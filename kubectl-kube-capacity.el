(defvar kubectl--view-kube-capacity nil)
(defvar kubectl--merged-nodes-capacity "")

(defun kubectl-toggle-capacity ()
  (interactive)
  (setq kubectl--view-kube-capacity (not kubectl--view-kube-capacity))
  (kubectl-print-buffer))

(defun kubectl--refresh-kcnodes ()
  (when (process-live-p kubectl--watch-process)
    (let* ((filename (f-expand (f-join kubectl--my-directory "data" "kcnodes")))
           (contents (f-read-text filename)))
      (when (and (f-exists-p filename)
                 (not (s-equals-p contents kubectl--merged-nodes-capacity)))
        (setq kubectl--merged-nodes-capacity contents)
        (kubectl-print-buffer))
      (run-with-timer 2 nil 'kubectl--refresh-kcnodes))))


(provide 'kubectl-kube-capacity)

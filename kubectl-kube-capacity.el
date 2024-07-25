(require 'kubectl-get-resources)

(defvar kubectl--view-kube-capacity nil)
(defvar kubectl--merged-nodes-capacity "")

(defun kubectl-toggle-capacity ()
  (interactive)
  (setq kubectl--view-kube-capacity (not kubectl--view-kube-capacity))
  (kubectl-print-buffer))

(defun kubectl--refresh-kcnodes ()
  (when (process-live-p kubectl--watch-process)
    (let* ((filename (f-expand (f-join kubectl--my-directory "data" "kcnodes")))
           (contents (if (f-exists-p filename)
                         (f-read-text filename)
                       ""))
           (count (- (->> contents
                           (s-split "\n")
                           (length))
                     2)))
      (when (not (and (s-equals-p contents kubectl--merged-nodes-capacity)
                      (s-equals-p contents "")))
        (setq kubectl--merged-nodes-capacity (s-replace "NAME    " (format "NAME %-3d" count) contents))
        (kubectl-print-buffer))
      (run-with-timer 2 nil 'kubectl--refresh-kcnodes))))


(provide 'kubectl-kube-capacity)

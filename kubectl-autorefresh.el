(defvar kubectl-autorefresh nil)
(defvar kubectl-timer)
(defvar kubectl-autorefresh-frequency 10)

(defun kubectl-toggle-autorefresh ()
  (interactive)
  (setq kubectl-autorefresh (not kubectl-autorefresh))
  (message "auto refresh is %s" kubectl-autorefresh)
  (kubectl-get-resources))

(defun kubectl-maybe-autorefresh ()
  (when kubectl-autorefresh
    (run-with-timer kubectl-autorefresh-frequency nil 'kubectl-init)))

(provide 'kubectl-autorefresh)

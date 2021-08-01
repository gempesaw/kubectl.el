(define-derived-mode kubectl-command-mode special-mode "kubectl-command"
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (define-key kubectl-command-mode-map (kbd "q") 'kubectl--command-quit-window))

(defun kubectl--command-quit-window ()
  (interactive)
  (if (s-equals-p (buffer-name) kubectl-process-buffer-name)
      (quit-window)
    (quit-window t)))

(provide 'kubectl-command-mode)

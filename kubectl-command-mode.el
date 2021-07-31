(define-derived-mode kubectl-command-mode special-mode "kubectl-command"
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (define-key kubectl-command-mode-map (kbd "q") '(lambda () (interactive) (quit-window t))))

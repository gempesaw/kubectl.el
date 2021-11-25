(require 's)
(require 'dash)
(require 'kubectl-process)
(require 'kubectl-edit-mode)
(require 'kubectl-command-mode)
(require 'kubectl-transient)
(require 'kubectl-mode-nav)
(require 'kubectl-summary)
(require 'kubectl-mode)

;;;###autoload
(defun kubectl (prefix)
  (interactive "P")
  (let ((cwd (cwd)))
    (with-current-buffer (get-buffer-create kubectl-main-buffer-name)
      (setq buffer-read-only t)
      (switch-to-buffer (current-buffer))
      (cd cwd)
      (kubectl-mode)
      (when prefix
        (kubectl-toggle-fetch-after-set t))
      (kubectl-transient-choose-context))))

(provide 'kubectl)

;; (use-package epc
;;   :ensure t
;;   :demand t)

(require 's)
(require 'dash)
(require 'epc)
(require 'kubectl-process)
(require 'kubectl-edit-mode)
(require 'kubectl-command-mode)
(require 'kubectl-transient)
(require 'kubectl-mode-nav)
(require 'kubectl-summary)
(require 'kubectl-get-resources)
(require 'kubectl-mode)

;;;###autoload
(defun kubectl (prefix)
  (interactive "P")

  (when prefix
    (kubectl-transient-choose-context))

  (let ((cwd (cwd)))
    (with-current-buffer (get-buffer-create kubectl-main-buffer-name)
      (setq buffer-read-only t)
      (switch-to-buffer (current-buffer))
      (cd cwd)
      (kubectl-mode))))

(defun kubectl-prompt ()
  (interactive)
  (kubectl 4))

(global-set-key (kbd "M-s-k") 'kubectl-prompt)


(provide 'kubectl)

(defvar-local kubectl-command-command ""
  "buffer-local kubectl command used to generate the output of the current buffer")

(define-derived-mode kubectl-command-mode special-mode "kubectl-command"
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (define-key kubectl-command-mode-map (kbd "g") 'kubectl-command-refresh)
  (define-key kubectl-command-mode-map (kbd "q") '(lambda () (interactive) (quit-window t))))

(defun kubectl-command-refresh ()
  (interactive)
  (message kubectl-command-command)
  (kubectl--run-command kubectl-command-command))

(defun kubectl--run-command (command &optional editing)
  (let* ((hyphenated-command (s-replace " " "-" command))
         (buf (get-buffer-create command))
         (proc (apply 'start-process (-concat `(,hyphenated-command ,buf) (s-split " " command)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pop-to-buffer (current-buffer))
        (if editing
            (kubectl-edit-mode)
          (kubectl-command-mode))
        (setq-local kubectl-command-command command)
        (when (process-live-p proc)
          (set-process-query-on-exit-flag proc nil)
          (set-process-filter proc 'kubectl-command-ordinary-insertion-filter)
          (set-process-sentinel proc 'kubectl-command-process-sentinel))))
    proc))

(defun kubectl-command-ordinary-insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc)))
            (inhibit-read-only t))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun kubectl-command-process-sentinel (proc output)
  (with-current-buffer (process-buffer proc)
    (message (format "finished: %s" kubectl-command-command))
    (goto-char (point-min))))

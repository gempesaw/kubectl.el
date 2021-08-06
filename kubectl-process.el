(require 'bpr)

(defvar kubectl-process-buffer-name "*kubectl-process*")

(defun kubectl--get-process-buffer ()
  (let ((buffer (get-buffer-create kubectl-process-buffer-name)))
    (with-current-buffer buffer
      (kubectl-command-mode))
    buffer))

(defun kubectl--update-process-buffer (process &optional inhibit-redraw)
  (let ((buffer (process-buffer process))
        (contents nil))
    (with-current-buffer buffer
      (setq contents (buffer-substring-no-properties (point-min) (point-max))))
    (kubectl--update-process-buffer-string contents)
    (when (not inhibit-redraw)
      (kubectl-redraw contents))
    (kill-buffer buffer)))

(defun kubectl--update-process-buffer-string (string &optional header)
  (with-current-buffer (kubectl--get-process-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (if header
                  (format "---\n\n%s\n\n" string)
                string)))))

(defun kubectl--run-process (command)
  (let* ((bpr-on-completion 'kubectl--update-process-buffer))
    (kubectl--update-process-buffer-string command t)
    (bpr-spawn command)))

(defun kubectl--run-process-bg (command on-success)
  (let* ((bpr-show-progress nil)
         (bpr-on-success on-success))
    (bpr-spawn command)))

(defun kubectl--run-process-and-pop (command &optional editing)
  (let* ((bpr-on-completion 'kubectl--pop-process)
         (bpr-process-mode (if editing 'kubectl-edit-mode 'kubectl-command-mode))
         (bpr-erase-process-buffer t)
         (proc (bpr-spawn command)))
    (kubectl--update-process-buffer-string command t)))

(defun kubectl--pop-process (process)
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (pop-to-buffer buffer)
        (goto-char (point-min))))))

(provide 'kubectl-process)

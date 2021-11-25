(defun kubectl-find-line (movement)
  (save-excursion
    (funcall movement)
    (beginning-of-line)
    (if (or (eq (point) (point-max))
            (eq (point) (point-min)))
        (progn
          (message "no additional sections available")
          nil)
      (let ((current-line-contents (current-line-contents)))
        (if (or (s-match "^NAME" current-line-contents)
                (s-contains? ":" (car (s-split "\s" current-line-contents)))
                (string= "" current-line-contents))
            (kubectl-find-line movement)
          (beginning-of-line)
          (point))))))

(defun kubectl-next-line ()
  (interactive)
  (let ((next-line (kubectl-find-line 'next-line)))
    (when next-line
      (goto-char next-line))))

(defun kubectl-previous-line ()
  (interactive)
  (let ((previous-line (kubectl-find-line 'previous-line)))
    (when previous-line
      (goto-char previous-line))))

(defun kubectl-find-next-section ()
  (let ((case-fold-search nil))
    (save-excursion
      (next-line)
      (if (search-forward-regexp "NAME" nil t)
          (progn (next-line)
                 (beginning-of-line)
                 (point))
        (message "no additional sections available")
        nil))))

(defun kubectl-next-section ()
  (interactive)
  (let ((next-section (kubectl-find-next-section)))
    (when next-section
      (goto-char next-section))))

(defun kubectl-find-previous-section ()
  (let ((case-fold-search nil))
    (save-excursion
      (beginning-of-line)
      (if (search-backward-regexp "NAME" nil t 2)
          (progn (beginning-of-line)
                 (next-line)
                 (point))
        (message "no additional sections available")
        nil))))

(defun kubectl-previous-section ()
  (interactive)
  (let ((previous-section (kubectl-find-previous-section)))
    (when previous-section
      (goto-char previous-section))))

(defun kubectl-goto-first-section ()
  (interactive)
  (beginning-of-buffer)
  (kubectl-next-section))

(provide 'kubectl-mode-nav)

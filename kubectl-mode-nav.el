(defun kubectl-find-next-line ()
  (save-excursion
    (end-of-line)
    (if (or (search-forward-regexp "^[[:alpha:].[:digit:]]+?/" nil t)
            (search-forward-regexp "^[[:alpha:].[:digit:]]+?/" nil t))
        (progn (beginning-of-line)
               (point))
      (message "no additional sections available")
      nil)))

(defun kubectl-find-previous-line ()
  (save-excursion
    (if (search-backward-regexp "^[[:alpha:].[:digit:]]+?/" nil t)
        (progn (beginning-of-line)
               (point))
      (message "no additional sections available")
      nil)))

(defun kubectl-next-line ()
  (interactive)
  (let ((next-line (kubectl-find-next-line)))
    (when next-line
      (goto-char next-line))))

(defun kubectl-previous-line ()
  (interactive)
  (let ((previous-line (kubectl-find-previous-line)))
    (when previous-line
      (goto-char previous-line))))

(defun kubectl-find-next-section ()
  (let ((case-fold-search nil))
    (save-excursion
      (next-line)
      (if (search-forward-regexp "AGE" nil t)
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
        (if (search-backward-regexp "AGE" nil t 2)
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

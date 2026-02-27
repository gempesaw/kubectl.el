;; -*- lexical-binding: t; -*-
(setq kubectl--command-refresh-restore-line nil)

;; Helper function to reload kubectl faces
(defun kubectl-reload-faces ()
  "Reload all kubectl-command-mode faces."
  (interactive)
  (mapc (lambda (face)
          (when (string-prefix-p "kubectl-" (symbol-name face))
            (face-spec-recalc face (selected-frame))))
        (face-list))
  (message "kubectl faces reloaded"))

;; Byte conversion functions
(defun kubectl--bytes-to-human (bytes)
  "Convert BYTES to human-readable format (GB, TB, etc.)."
  (cond
   ((>= bytes (* 1024 1024 1024 1024)) (format "%.2fTi" (/ bytes (* 1024.0 1024 1024 1024))))
   ((>= bytes (* 1024 1024 1024)) (format "%.2fGi" (/ bytes (* 1024.0 1024 1024))))
   ((>= bytes (* 1024 1024)) (format "%.2fMi" (/ bytes (* 1024.0 1024))))
   ((>= bytes 1024) (format "%.2fKi" (/ bytes 1024.0)))
   (t (format "%dB" bytes))))

(defun kubectl--parse-k8s-quantity (quantity-str)
  "Parse Kubernetes quantity string (e.g., '157208556Ki', '2092981804Ki') to bytes."
  (when (string-match "\\([0-9]+\\)\\([KMGTP]i\\|[KMGTP]\\)?$" quantity-str)
    (let ((number (string-to-number (match-string 1 quantity-str)))
          (suffix (match-string 2 quantity-str)))
      (cond
       ((string= suffix "Ki") (* number 1024))
       ((string= suffix "Mi") (* number 1024 1024))
       ((string= suffix "Gi") (* number 1024 1024 1024))
       ((string= suffix "Ti") (* number 1024 1024 1024 1024))
       ((string= suffix "Pi") (* number 1024 1024 1024 1024 1024))
       ((string= suffix "K") (* number 1000))
       ((string= suffix "M") (* number 1000 1000))
       ((string= suffix "G") (* number 1000 1000 1000))
       ((string= suffix "T") (* number 1000 1000 1000 1000))
       ((string= suffix "P") (* number 1000 1000 1000 1000 1000))
       (t number)))))

(defun kubectl--humanize-storage-values ()
  "Add human-readable annotations to storage values in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (re-search-forward "\\(ephemeral-storage\\|memory\\):[[:space:]]+\"?\\([0-9]+\\)\\(Ki\\|Mi\\|Gi\\|Ti\\|Pi\\)?\"?" nil t)
        (let* ((field-name (match-string 1))
               (value-str (match-string 2))
               (suffix (match-string 3))
               (full-quantity (concat value-str (or suffix "")))
               (bytes (kubectl--parse-k8s-quantity full-quantity))
               (human (when bytes (kubectl--bytes-to-human bytes))))
          (when human
            (end-of-line)
            (insert (propertize (format "  (%s)" human)
                                'face 'kubectl-timestamp-face))))))))

;; Font lock faces for kubectl output
(defface kubectl-field-name-face
  '((t :foreground "#3dd6fc" :weight bold))
  "Face for field names like 'Name:', 'Status:', etc.")

(defface kubectl-section-header-face
  '((t :foreground "#aa77fd" :weight bold :height 1.1))
  "Face for section headers like 'Events:', 'Containers:', etc.")

(defface kubectl-warning-face
  '((t :foreground "#ffea00" :weight bold))
  "Face for Warning events and status.")

(defface kubectl-error-face
  '((t :foreground "#f9564c" :weight bold))
  "Face for Error events and failed status.")

(defface kubectl-success-face
  '((t :foreground "#73fea6"))
  "Face for successful status like 'Running', 'True'.")

(defface kubectl-pending-face
  '((t :foreground "#fef36e"))
  "Face for pending status.")

(defface kubectl-timestamp-face
  '((t :foreground "#cacac1"))
  "Face for timestamps and ages in events.")

(defvar kubectl-command-font-lock-keywords
  `(
    ;; Section headers (Events:, Containers:, etc.)
    ("^\\([A-Z][A-Za-z ]+:\\)$" 1 'kubectl-section-header-face)

    ;; Field names (Name:, Status:, etc.) at start of line
    ("^\\([A-Z][A-Za-z /-]+:\\)" 1 'kubectl-field-name-face)

    ;; Field names with indentation (for nested fields)
    ("^[ \t]+\\([A-Z][A-Za-z /-]+:\\)" 1 'kubectl-field-name-face)

    ;; Warning status/events
    ("\\(Warning\\|FailedScheduling\\|Failed\\|Unhealthy\\)" 1 'kubectl-warning-face)

    ;; Error status
    ("\\(Error\\|CrashLoopBackOff\\|ImagePullBackOff\\|ErrImagePull\\)" 1 'kubectl-error-face)

    ;; Success status
    ("\\(Running\\|Completed\\|Ready\\|True\\|Succeeded\\)" 1 'kubectl-success-face)

    ;; Pending status
    ("\\(Pending\\|False\\|Unknown\\|ContainerCreating\\)" 1 'kubectl-pending-face)

    ;; Timestamps and ages (e.g., "5s", "2m", "3h", "4d")
    ("\\b\\([0-9]+[smhd]\\)\\b" 1 'kubectl-timestamp-face)

    ;; Event table headers
    ("^[ \t]*\\(Type\\|Reason\\|Age\\|From\\|Message\\)[ \t]+" 1 'font-lock-keyword-face)

    ;; Highlight event separators
    ("^[ \t]*----.*----" 0 'font-lock-comment-face)
    )
  "Font lock keywords for kubectl-command-mode.")

(define-derived-mode kubectl-command-mode special-mode "kubectl-command"
  (buffer-disable-undo)
  (when (not (s-equals-p (buffer-name) kubectl-process-buffer-name))
    (erase-buffer))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (so-long-minor-mode 1)
  (setq font-lock-defaults '(kubectl-command-font-lock-keywords))
  (font-lock-mode 1)
  (define-key kubectl-command-mode-map (kbd "q") 'kubectl--command-quit-window)
  (define-key kubectl-command-mode-map (kbd "g") 'kubectl--command-refresh))

(defun kubectl--command-quit-window ()
  (interactive)
  (if (s-equals-p (buffer-name) kubectl-process-buffer-name)
      (quit-window)
    (quit-window t)))

(defun kubectl--command-refresh ()
  (interactive)
  (let ((command (->> (buffer-name)
                      (s-split "*")
                      (cadr)
                      (s-split "(")
                      (car)))
        (current-line (line-number-at-pos)))
    (kill-buffer-and-window)
    (setq kubectl--command-refresh-restore-line current-line)
    (kubectl--run-process-and-pop command))
  )

(provide 'kubectl-command-mode)

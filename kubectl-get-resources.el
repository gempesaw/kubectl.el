(require 'epc)
(require 'ht)

(require 'server)
(unless (server-running-p)
  (server-start))

(defvar kubectl--my-directory
  (expand-file-name (if load-file-name
                        ;; File is being loaded.
                        (file-name-directory load-file-name)
                      ;; File is being evaluated using, for example, `eval-buffer'.
                      default-directory)))

(defvar kubectl--watch-sidecar-process nil)
(defvar kubectl--transient-grep-auto nil)
(defvar kubectl--transient-grep-needle "-")

(defvar kubectl--display-redraw-timer nil)
(defvar kubectl--cancel-watch-timer nil)

(defun kubectl--cancel-timer (sym)
  "Cancel the timer stored in SYM if any, and nil out the var."
  (when (timerp (symbol-value sym))
    (cancel-timer (symbol-value sym))
    (set sym nil)))

(defvar kubectl--resource-contents (ht-create)
  "Hash table mapping resource buffer names to rendered contents.
Populated by the socket filter. The keys keep the historical
\" kubectl--resource-buffer-<name>\" prefix so the wire protocol stays stable.")

(defun kubectl--ensure-sidecar-built ()
  "Build the Go sidecar via make; return the binary path. Errors if build fails."
  (let* ((sidecar-dir (f-join kubectl--my-directory "sidecar"))
         (binary (f-join sidecar-dir "bin" "kubectl-watch-sidecar"))
         (default-directory sidecar-dir)
         (status (call-process "make" nil "*kubectl-sidecar-build*" nil)))
    (unless (and (= status 0) (file-executable-p binary))
      (display-buffer "*kubectl-sidecar-build*")
      (error "kubectl: sidecar build failed (exit %s); see *kubectl-sidecar-build*" status))
    binary))

(defvar kubectl--watch-server-process nil)
(defvar kubectl--watch-socket-path nil)

(defun kubectl--watch-socket-path ()
  (or kubectl--watch-socket-path
      (setq kubectl--watch-socket-path
            (expand-file-name "kubectl-watch.sock" server-socket-dir))))

(defun kubectl--watch-server-start ()
  (unless (process-live-p kubectl--watch-server-process)
    (let ((path (kubectl--watch-socket-path)))
      (when (file-exists-p path)
        (delete-file path))
      (setq kubectl--watch-server-process
            (make-network-process
             :name "kubectl-watch-server"
             :family 'local
             :service path
             :server t
             :filter #'kubectl--watch-filter
             :coding 'utf-8
             :noquery t)))))

(defun kubectl--watch-filter (proc chunk)
  (let* ((acc (concat (or (process-get proc :kubectl-buf) "") chunk))
         (lines (split-string acc "\n"))
         (partial (car (last lines)))
         (complete (butlast lines)))
    (process-put proc :kubectl-buf partial)
    (dolist (line complete)
      (when (> (length line) 0)
        (kubectl--watch-handle-message line)))))

(defun kubectl--watch-handle-message (line)
  (condition-case err
      (let* ((msg (json-parse-string line :object-type 'plist))
             (buf (plist-get msg :buffer))
             (contents (plist-get msg :contents)))
        (when (and buf contents)
          (kubectl--write-buffer-contents buf contents)))
    (error
     (message "kubectl-watch: bad message: %s" (error-message-string err)))))

(defun kubectl-get-resources (&optional grep-needle-arg)
  (when (process-live-p kubectl--watch-sidecar-process)
    (delete-process kubectl--watch-sidecar-process))
  (kubectl--cancel-timer 'kubectl--display-redraw-timer)
  (kubectl--cancel-timer 'kubectl--cancel-watch-timer)
  (ht-clear! kubectl--resource-contents)
  (kubectl--watch-server-start)
  (let* ((resources (if kubectl-all-namespaces kubectl-resources-current-all-ns kubectl-resources-current))
         (namespace (if kubectl-all-namespaces "All Namespaces" kubectl-current-namespace))
         (sort-column (if kubectl-current-sort-column kubectl-current-sort-column "NAME"))
         (grep-needle (if grep-needle-arg
                          grep-needle-arg
                        (if kubectl--transient-grep-needle
                            kubectl--transient-grep-needle
                          "-")))
         (sidecar-binary (kubectl--ensure-sidecar-built))
         (socket-path (kubectl--watch-socket-path)))
    (setq kubectl--watch-sidecar-process
          (with-environment-variables (("KUBECTL_WATCH_SOCKET" socket-path))
            (start-process
             "kubectl-watch-sidecar"
             kubectl-process-buffer-name
             sidecar-binary
             resources
             namespace
             sort-column
             grep-needle))
          kubectl--transient-grep-needle grep-needle)
    (kubectl--schedule-redraw)
    (kubectl--get-resources-cancel)))

(defun kubectl--write-buffer-contents (buf contents)
  (ht-set! kubectl--resource-contents buf contents)
  (kubectl--schedule-redraw))

(defun kubectl--schedule-redraw ()
  "Schedule a debounced visible-buffer redraw. Idle-timer so we never preempt user input."
  (kubectl--cancel-timer 'kubectl--display-redraw-timer)
  (setq kubectl--display-redraw-timer
        (run-with-idle-timer 0.1 nil 'kubectl--do-redraw)))

(defun kubectl--do-redraw ()
  "Read the current hash-table state and refresh the visible buffer."
  (setq kubectl--display-redraw-timer nil)
  (when (process-live-p kubectl--watch-sidecar-process)
    (let* ((resources (s-split "," (if kubectl-all-namespaces kubectl-resources-current-all-ns kubectl-resources-current)))
           (contents (->> resources
                          (--map (ht-get kubectl--resource-contents
                                         (format " kubectl--resource-buffer-%s" it)
                                         ""))
                          (--remove (s-blank? it))
                          (s-join "\n\n"))))
      (unless (s-equals-p contents kubectl-current-display)
        (kubectl-redraw contents)))))

(defun kubectl--get-resources-cancel ()
  "cancel the watch cuz the window is unfocused"
  (when (process-live-p kubectl--watch-sidecar-process)
    (when (not (get-buffer-window kubectl-main-buffer-name))
      (message "kubectl.el: cancelling watch")
      (delete-process kubectl--watch-sidecar-process))
    (kubectl--cancel-timer 'kubectl--cancel-watch-timer)
    (setq kubectl--cancel-watch-timer
          (run-with-timer 60 nil 'kubectl--get-resources-cancel))))

(defvar kubectl-current-sort-column "NAME")
(defun kubectl-sort-by (sort-column)
  (interactive (list (completing-read
                      (format "column to sort by: [%s]" kubectl-current-sort-column)
                      (->> kubectl-current-display
                           (s-split "\n")
                           (--filter (s-starts-with? "NAME" it))
                           (--map (s-split "[ ]+" it))
                           (-flatten)
                           (-uniq))
                      nil
                      nil)))
  (setq kubectl-current-sort-column sort-column)
  (kubectl-get-resources))

(setq kubectl--transient-grep-needle-history '())
(defun kubectl-get-resources-grep ()
  (interactive)
  (let* ((workload-kinds-regex (s-join "\\|" '("deployment" "statefulset" "daemonset")))
         (workloads (->> kubectl-current-display
                         (s-split "\n")
                         (--filter (s-matches-p workload-kinds-regex it))
                         (--map (nth 1 (s-split "[/ ]" it t)))))
         (history (-uniq (-concat kubectl--transient-grep-needle-history workloads)))
         (grep-needle (completing-read "needle to search for: " history nil nil)))
    (add-to-list 'kubectl--transient-grep-needle-history grep-needle)
    (kubectl-get-resources grep-needle)))

(provide 'kubectl-get-resources)

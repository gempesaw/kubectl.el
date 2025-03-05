(require 'epc)

(require 'server)
(server-start)

(defvar kubectl--my-directory
  (expand-file-name (if load-file-name
                        ;; File is being loaded.
                        (file-name-directory load-file-name)
                      ;; File is being evaluated using, for example, `eval-buffer'.
                      default-directory)))

(defvar kubectl--watch-process nil)
(defvar kubectl--transient-grep-auto nil)
(defvar kubectl--transient-grep-needle "-")
(defun kubectl-get-resources (&optional grep-needle-arg)
  (when (process-live-p kubectl--watch-process)
    (delete-process kubectl--watch-process))
  (let ((resources (if kubectl-all-namespaces kubectl-resources-current-all-ns kubectl-resources-current))
        (namespace (if kubectl-all-namespaces "All Namespaces" kubectl-current-namespace))
        (sort-column (if kubectl-current-sort-column kubectl-current-sort-column "NAME"))
        (grep-needle (if grep-needle-arg
                         grep-needle-arg
                       (if kubectl--transient-grep-needle
                           kubectl--transient-grep-needle
                         "-"))))
    (setq kubectl--watch-process
          (with-environment-variables (("EMACS_SERVER_SOCKET_DIR" server-socket-dir))
            (start-process
             "kubectl-watch"
             kubectl-process-buffer-name
             "python" (f-expand (f-join kubectl--my-directory "watch.py")) resources namespace sort-column grep-needle))
          kubectl--transient-grep-needle grep-needle)
    (kubectl--refresh-current-display)
    (kubectl--refresh-kcnodes)
    (kubectl--get-resources-cancel)))

(defun kubectl--write-buffer-contents (buf contents)
  (with-current-buffer (get-buffer-create buf t)
    (erase-buffer)
    (insert contents)))

(setq kubectl--data-directory "/private/var/tmp/kubectl-data")
(defun kubectl--refresh-current-display ()
  (let* ((resources (s-split "," (if kubectl-all-namespaces kubectl-resources-current-all-ns kubectl-resources-current)))
         (contents (->> resources
                        (--map (let ((buf-name (format " kubectl--resource-buffer-%s" it)))
                                 (with-current-buffer (get-buffer-create buf-name)
                                   (buffer-substring-no-properties (point-min) (point-max)))))
                        (s-join "\n\n"))))
    (when (and (process-live-p kubectl--watch-process)
               (not (s-equals-p contents kubectl-current-display)))
      (kubectl-redraw contents))
    (run-with-timer 5 nil 'kubectl--refresh-current-display)))

(defun kubectl--get-resources-cancel ()
  "cancel the watch cuz the window is unfocused"
  (when (process-live-p kubectl--watch-process)
    (when (not (get-buffer-window kubectl-main-buffer-name))
      (message "kubectl.el: cancelling watch")
      (delete-process kubectl--watch-process))
    (run-with-timer 60 nil 'kubectl--get-resources-cancel)))

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

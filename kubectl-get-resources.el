(require 'epc)
(require 'ht)
(require 'kubectl-aws)

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

(defvar kubectl--sidecar-connection nil
  "Process object for the accepted Go-sidecar socket connection.
Set by the watch filter on first byte. Use `kubectl--sidecar-send' to write to it.")

(defvar kubectl--expanded-aliases (ht-create)
  "Hash table tracking which resource aliases are in their non-default limit state.
Key is the alias (\"po\", \"deploy\", \"kcnodes\", ...); value is non-nil if toggled.")

(defvar kubectl--default-limits
  '(("po" . 20) ("pod" . 20) ("pods" . 20)
    ("ds" . 20) ("sts" . 20) ("deploy" . 20)
    ("svc" . 20) ("ing" . 20) ("cm" . 20)
    ("kcnodes" . 0))
  "Default display row limit per alias. 0 means no limit.")

(defvar kubectl--sort-overrides (ht-create)
  "Hash table tracking per-alias sort-column overrides (set via `s' on a section).
Key is the alias; value is a column-name string. Persisted to
`kubectl--sort-overrides-file' so user-chosen sorts survive Emacs restarts.")

(defvar kubectl--sort-overrides-file
  (expand-file-name "kubectl-sort-overrides" user-emacs-directory)
  "Where `kubectl--sort-overrides' is persisted. One \"alias\tcolumn\" line each.")

(defun kubectl--save-sort-overrides ()
  "Persist `kubectl--sort-overrides' to disk."
  (with-temp-file kubectl--sort-overrides-file
    (ht-each (lambda (alias column)
               (when (and alias column (not (s-blank? column)))
                 (insert (format "%s\t%s\n" alias column))))
             kubectl--sort-overrides)))

(defun kubectl--load-sort-overrides ()
  "Hydrate `kubectl--sort-overrides' from disk (best-effort, ignores errors)."
  (when (file-exists-p kubectl--sort-overrides-file)
    (with-temp-buffer
      (insert-file-contents kubectl--sort-overrides-file)
      (dolist (line (s-split "\n" (buffer-string) t))
        (let ((parts (s-split "\t" line)))
          (when (= 2 (length parts))
            (ht-set! kubectl--sort-overrides (car parts) (cadr parts))))))))

(kubectl--load-sort-overrides)

(defvar kubectl--default-sorts
  '(("kcnodes" . "NAME"))
  "Default sort column per alias for the completing-read prompt.
Anything not listed falls back to \"AGE\". Keep in sync with sidecar's
`defaultSortFor' in cmd/kubectl-watch-sidecar/main.go.")

(defvar kubectl--display-redraw-timer nil)

(defun kubectl--cancel-timer (sym)
  "Cancel the timer stored in SYM if any, and nil out the var."
  (when (timerp (symbol-value sym))
    (cancel-timer (symbol-value sym))
    (set sym nil)))

(defvar kubectl--resource-contents (ht-create)
  "Hash table mapping resource alias (e.g. \"po\", \"deploy\", \"kcnodes\") to
the rendered contents string. Populated by the socket filter.")

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
  ;; Remember the accepted-connection process so we can write back to the sidecar.
  ;; On a fresh connection, replay any persisted overrides so the sidecar
  ;; reflects the user's session state from before its restart.
  (unless (eq kubectl--sidecar-connection proc)
    (setq kubectl--sidecar-connection proc)
    (kubectl--replay-overrides))
  (let* ((acc (concat (or (process-get proc :kubectl-buf) "") chunk))
         (lines (split-string acc "\n"))
         (partial (car (last lines)))
         (complete (butlast lines)))
    (process-put proc :kubectl-buf partial)
    (dolist (line complete)
      (when (> (length line) 0)
        (kubectl--watch-handle-message line)))))

(defun kubectl--replay-overrides ()
  "Re-send all stored sort overrides to the freshly-connected sidecar."
  (ht-each (lambda (alias column)
             (when (and alias column (not (s-blank? column)))
               (kubectl--sidecar-send
                (list :type "set_sort" :alias alias :column column))))
           kubectl--sort-overrides))

(defun kubectl--sidecar-send (plist)
  "Send PLIST as a JSON line to the connected Go sidecar.
Returns non-nil on success, nil if the sidecar isn't connected (with a message)."
  (if (process-live-p kubectl--sidecar-connection)
      (progn (process-send-string kubectl--sidecar-connection
                                  (concat (json-encode plist) "\n"))
             t)
    (message "kubectl: sidecar not connected — press g to restart")
    nil))

(defun kubectl--active-resources ()
  "Return the resource-alias list active right now (single ns vs all-ns)."
  (s-split "," (if kubectl-all-namespaces
                   kubectl-resources-current-all-ns
                 kubectl-resources-current)))

(defun kubectl--alias-in-active (&rest aliases)
  "Return the first of ALIASES that's in the active resources list, or the first arg."
  (let ((current (kubectl--active-resources)))
    (or (--first (member it current) aliases)
        (car aliases))))

(defun kubectl--alias-for-row-kind (kind)
  "Map a buffer row's kubectl `--show-kind' prefix to the user-facing resource alias.
KIND examples: \"pod\", \"deployment.apps\", \"node\". Returns nil for unknown kinds."
  (cond
   ((string= kind "pod") (kubectl--alias-in-active "po" "pod" "pods"))
   ((string= kind "service") (kubectl--alias-in-active "svc" "service" "services"))
   ((string= kind "configmap") (kubectl--alias-in-active "cm" "configmap" "configmaps"))
   ((string= kind "deployment.apps") (kubectl--alias-in-active "deploy" "deployment" "deployments"))
   ((string= kind "daemonset.apps") (kubectl--alias-in-active "ds" "daemonset" "daemonsets"))
   ((string= kind "statefulset.apps") (kubectl--alias-in-active "sts" "statefulset" "statefulsets"))
   ((string= kind "ingress.networking.k8s.io") (kubectl--alias-in-active "ing" "ingress" "ingresses"))
   ((string= kind "node") "kcnodes")))

(defun kubectl--current-section-alias ()
  "Return the alias of the resource section the cursor is in.
Works by walking backward through the buffer until a row like \"pod/foo\" is found."
  (save-excursion
    (let ((alias nil)
          (limit (max (point-min) (- (point) 50000)))) ; safety: don't walk forever
      (beginning-of-line)
      (while (and (not alias) (> (point) limit))
        (if (looking-at "^\\([a-z][a-z0-9.]*\\)/")
            (setq alias (kubectl--alias-for-row-kind (match-string 1)))
          (forward-line -1)))
      alias)))

(defun kubectl--default-limit (alias)
  "Return the default row limit for ALIAS (0 = no limit). Falls back to 20."
  (or (cdr (assoc alias kubectl--default-limits)) 20))

(defun kubectl-toggle-expand-section ()
  "Toggle full-vs-default row display for the section at point.
For sections with a default limit (e.g. pods, deployments) this flips between top-N and full.
For unlimited sections (nodes) it flips between full and top-20."
  (interactive)
  (let ((alias (kubectl--current-section-alias)))
    (if (not alias)
        (message "kubectl: cursor isn't in a resource section")
      (let* ((default (kubectl--default-limit alias))
             (was-expanded (ht-get kubectl--expanded-aliases alias))
             (now-expanded (not was-expanded))
             (limit (cond
                     ((not now-expanded) default)
                     ((= default 0) 20)      ; default unlimited → collapse to 20
                     (t 0))))                ; default limited → expand to all
        (when (kubectl--sidecar-send
               (list :type "set_limit" :alias alias :limit limit))
          (ht-set! kubectl--expanded-aliases alias now-expanded))))))

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
  (ht-clear! kubectl--resource-contents)
  (ht-clear! kubectl--expanded-aliases)
  ;; `kubectl--sort-overrides' is preserved across restarts so a user's chosen
  ;; sort sticks (replayed to the new sidecar in `kubectl--watch-filter').
  (setq kubectl--sidecar-connection nil)
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
          (kubectl-with-aws-env
            (with-environment-variables (("KUBECTL_WATCH_SOCKET" socket-path))
              (start-process
               "kubectl-watch-sidecar"
               kubectl-process-buffer-name
               sidecar-binary
               resources
               namespace
               sort-column
               grep-needle)))
          kubectl--transient-grep-needle grep-needle)
    (kubectl--schedule-redraw)))

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
                          (--map (ht-get kubectl--resource-contents it ""))
                          (--remove (s-blank? it))
                          (s-join "\n\n"))))
      (unless (s-equals-p contents kubectl-current-display)
        (kubectl-redraw contents)))))

;; Previously: `kubectl--get-resources-cancel' polled every 60s and killed the
;; sidecar whenever *kubectl* had no visible window. That was a python-era
;; defense against an expensive long-running watch; the Go sidecar is cheap so
;; we just let it keep running until the next `g' explicitly restarts it.

(defvar kubectl-current-sort-column "NAME"
  "Legacy global default; still passed as argv[3] to the sidecar but the sidecar
ignores it now that defaults are per-resource and overrides are per-section.")

(defun kubectl--default-sort (alias)
  "Return the default sort column for ALIAS (falls back to \"AGE\")."
  (or (cdr (assoc alias kubectl--default-sorts)) "AGE"))

(defun kubectl--effective-sort (alias)
  "Return the currently effective sort column for ALIAS (override if any, else default)."
  (or (ht-get kubectl--sort-overrides alias)
      (kubectl--default-sort alias)))

(defun kubectl--columns-for-section-at-point ()
  "Return the column-name list from the NAME header line of the section at point.
Walks backward to find the closest \"NAME ...\" line in the visible buffer."
  (save-excursion
    (beginning-of-line)
    (when (or (looking-at "^NAME ")
              (re-search-backward "^NAME " nil t))
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        ;; Split on whitespace runs of 2+ to keep multi-word column names intact
        ;; ("NOMINATED NODE", "READINESS GATES"). Strip the count suffix on NAME.
        (->> (split-string line "  +" t)
             (--map (s-trim it))
             (--map (if (s-starts-with? "NAME " it) "NAME" it)))))))

(defun kubectl-sort-by (sort-column)
  "Sort the section the cursor is in by SORT-COLUMN.
Sends `set_sort' to the Go sidecar; only that section re-renders."
  (interactive
   (let ((alias (kubectl--current-section-alias)))
     (if (not alias)
         (user-error "kubectl: cursor isn't in a resource section")
       (list (completing-read
              (format "sort %s by [%s]: " alias (kubectl--effective-sort alias))
              (kubectl--columns-for-section-at-point)
              nil nil)))))
  (let ((alias (kubectl--current-section-alias)))
    (when (and alias (not (s-blank? sort-column)))
      (when (kubectl--sidecar-send
             (list :type "set_sort" :alias alias :column sort-column))
        (ht-set! kubectl--sort-overrides alias sort-column)
        (kubectl--save-sort-overrides)))))

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

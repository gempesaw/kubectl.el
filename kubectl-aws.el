;;; -*- lexical-binding: t; -*-

(require 's)
(require 'dash)

(defvar kubectl--aws-creds nil
  "Plist of AWS credentials owned by kubectl, kept separate from
`process-environment' so other transients (`,/' for pulumi etc.) can
clobber global AWS_PROFILE without affecting kubectl operations.
Keys: :profile (agent-prefixed name), :access-key, :secret-key,
:session-token. Populated by `kubectl--auth-aws'.")

(defun kubectl--auth-aws (profile)
  "Source AWS creds for PROFILE via `dg-modular-ensure-aws-profile-login',
then stash the returned plist in `kubectl--aws-creds'. After this,
kubectl sub-processes spawned through the helpers in `kubectl-process.el'
(and the sidecar) get these creds bound on `process-environment'
regardless of what the rest of Emacs has done to the env."
  (when profile
    (setq kubectl--aws-creds (dg-modular-ensure-aws-profile-login profile))))

(defun kubectl--aws-env ()
  "Return a `process-environment'-shaped list with kubectl-owned AWS creds
applied: global AWS_PROFILE / AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY /
AWS_SESSION_TOKEN are stripped, then ours are prepended. If we haven't
authenticated yet, returns `process-environment' unchanged."
  (if (not kubectl--aws-creds)
      process-environment
    (let ((stripped (--remove (or (s-starts-with? "AWS_PROFILE=" it)
                                  (s-starts-with? "AWS_ACCESS_KEY_ID=" it)
                                  (s-starts-with? "AWS_SECRET_ACCESS_KEY=" it)
                                  (s-starts-with? "AWS_SESSION_TOKEN=" it))
                              process-environment))
          (creds (list (format "AWS_PROFILE=%s" (plist-get kubectl--aws-creds :profile))
                       (format "AWS_ACCESS_KEY_ID=%s" (plist-get kubectl--aws-creds :access-key))
                       (format "AWS_SECRET_ACCESS_KEY=%s" (plist-get kubectl--aws-creds :secret-key))
                       (format "AWS_SESSION_TOKEN=%s" (plist-get kubectl--aws-creds :session-token)))))
      (append creds stripped))))

(defmacro kubectl-with-aws-env (&rest body)
  "Run BODY with `process-environment' bound to kubectl-owned AWS creds."
  (declare (indent 0))
  `(let ((process-environment (kubectl--aws-env)))
     ,@body))

(defun kubectl--shell (command)
  "Like `shell-command-to-string' but uses kubectl-owned AWS creds."
  (kubectl-with-aws-env (shell-command-to-string command)))

(provide 'kubectl-aws)

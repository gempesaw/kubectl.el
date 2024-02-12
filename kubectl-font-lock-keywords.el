(let (
      (fairy-sky-100      "#def8ff")
      (fairy-sky-200      "#cdf5ff")
      (fairy-sky-300      "#bbf1ff")
      (fairy-sky-400      "#aaeeff")
      (fairy-sky-500      "#99ebff")
      (fairy-sky-600      "#7ae4fe")
      (fairy-sky-700      "#5bddfd")
      (fairy-sky-800      "#3dd6fc")
      (fairy-sky-900      "#1ecffb")
      (fairy-sky-1000     "#00c0f0")
      (fairy-grape-100    "#ede7f3")
      (fairy-grape-200    "#e3d6f6")
      (fairy-grape-300    "#d9c5f9")
      (fairy-grape-400    "#cfb4fc")
      (fairy-grape-500    "#c5a3ff")
      (fairy-grape-600    "#b78dfe")
      (fairy-grape-700    "#aa77fd")
      (fairy-grape-800    "#9d62fd")
      (fairy-grape-900    "#904cfc")
      (fairy-grape-1000   "#5a00f5")
      (fairy-mint-100     "#fbfefc")
      (fairy-mint-200     "#f2fdf6")
      (fairy-mint-300     "#eafcf0")
      (fairy-mint-400     "#e1fbeb")
      (fairy-mint-500     "#c2ffdf")
      (fairy-mint-600     "#b9fbd1")
      (fairy-mint-700     "#a2fcc3")
      (fairy-mint-800     "#8afdb4")
      (fairy-mint-900     "#73fea6")
      (fairy-mint-1000    "#5cff98")
      (fairy-gold-100     "#fef9b8")
      (fairy-gold-200     "#fef693")
      (fairy-gold-300     "#fef36e")
      (fairy-gold-400     "#fff352")
      (fairy-gold-500     "#feed24")
      (fairy-gold-600     "#ffea00")
      (fairy-gold-700     "#efdb00")
      (fairy-gold-800     "#e6c000")
      (fairy-gold-900     "#d0be01")
      (fairy-gold-1000    "#c0b001")
      (fairy-carrot-100   "#ffd4d1")
      (fairy-carrot-200   "#fec4c0")
      (fairy-carrot-300   "#fdb4af")
      (fairy-carrot-400   "#fca49e")
      (fairy-carrot-500   "#fb948d")
      (fairy-carrot-600   "#fb847c")
      (fairy-carrot-700   "#fa6d64")
      (fairy-carrot-800   "#f9564c")
      (fairy-carrot-900   "#f84034")
      (fairy-carrot-1000  "#f7291c")
      (fairy-blush-100    "#fec8db")
      (fairy-blush-200    "#fdb6cf")
      (fairy-blush-300    "#ffb8d1")
      (fairy-blush-400    "#fc92b8")
      (fairy-blush-500    "#fb80ac")
      (fairy-blush-600    "#fb6ea0")
      (fairy-blush-700    "#fa5c95")
      (fairy-blush-800    "#fa4a89")
      (fairy-blush-900    "#f9387d")
      (fairy-blush-1000   "#f92672")
      (fairy-space-100    "#f8f8f2")
      (fairy-space-200    "#e1e1d9")
      (fairy-space-300    "#cacac1")
      (fairy-space-400    "#b3b3a9")
      (fairy-space-500    "#9c9c91")
      (fairy-space-600    "#868678")
      (fairy-space-700    "#6f6f60")
      (fairy-space-800    "#585848")
      (fairy-space-900    "#414130")
      (fairy-space-1000   "#2b2b18")
      )

  (defconst kubectl-font-lock-keywords
    `(
      ("Session: +5..*" 0 '(:foreground ,fairy-mint-600))
      ("Session: +4..*" 0 '(:foreground ,fairy-mint-600))
      ("Session: +3..*" 0 '(:foreground ,fairy-gold-600))
      ("Session: +2..*" 0 '(:foreground ,fairy-gold-600))
      ("Session: +1..*" 0 '(:foreground ,fairy-carrot-600))
      ("Session: +-.*" 0 '(:foreground ,fairy-carrot-600))

      ("Context: +eks-stg-playground[^ ]+ " 0 '(:foreground ,fairy-mint-800))
      ("Context: +eks-stg-[^ ]+ " 0 '(:foreground ,fairy-sky-600))
      ("Context: +eks-lt-[^ ]+ " 0 '(:foreground ,fairy-sky-600))
      ("Context: +eks-prod[^ ]+ " 0 '(:foreground ,fairy-carrot-600))
      ("Context: +eks-euprod[^ ]+ " 0 '(:foreground ,fairy-carrot-600))

      ("kubernetes-admin" 0 '(:foreground ,fairy-carrot-600))

      (" | " 0 '(:foreground ,fairy-mint-600))
      (" || " 0 '(:foreground ,fairy-mint-600))
      (" ||| " 0 '(:foreground ,fairy-mint-600))
      (" |||| " 0 '(:foreground ,fairy-gold-600))
      (" |||||+" 0 '(:foreground ,fairy-carrot-600))

      ("\\b\\([0-9]\\|[12][0-9]\\)%\\b" 0 '(:foreground ,fairy-mint-600))
      ("\\b\\([345][0-9]%\\)\\b" 1 '(:foreground ,fairy-gold-600))
      ("\\b\\([67][0-9]%\\)\\b" 1 '(:foreground ,fairy-grape-600))
      ("\\b\\([89][0-9]%\\|[0-9][0-9][0-9]%\\)\\b" 1 '(:foreground ,fairy-carrot-600))
      (" \\(Running\\) +\\([^0] +(.*?)\\) +"
       (1 '(:foreground ,fairy-gold-600))
       (2 '(:foreground ,fairy-carrot-600))
       )
      ("\\bRunning +0" 0 '(:foreground ,fairy-mint-600))
      ("\\(pod[^ ]+\\).*\\(CrashLoopBackOff\\)"
       (1 '(:foreground ,fairy-carrot-600))
       (2 '(:foreground ,fairy-carrot-600))
       )

      ("CrashLoopBackOff +\\([0-9]+ ([^ ]+ ago)\\)" 1 '(:foreground ,fairy-carrot-600))
      ("\\([0-9]+ ([^ ]+ ago)\\)" 1 '(:foreground ,fairy-gold-600))


      ;; if it has ready N/N then it should be green
      ("pod.* +\\(\\([0-9]+\\)/\\2\\) +" 1 '(:foreground ,fairy-mint-600))

      ("pod.* +\\(\\([0-9]+\\)/.\\) +Completed" 1 '(:foreground ,fairy-sky-100))

      ;; if the above didn't apply, then it's ready N/M so it should be red

      ("pod.* +\\(\\([0-9]+\\)/.\\) +" 1 '(:foreground ,fairy-carrot-600))

      ("^\\(pod/[^ ]+\\).* +\\([0-9]%\\|[12][0-9]%\\) +.* +\\([0-9]%\\|[12][0-9]%\\) +.*\\b\\(Running +0\\)"
       (1 '(:foreground ,fairy-sky-600))
       (2 '(:foreground ,fairy-mint-600))
       (3 '(:foreground ,fairy-mint-600))
       (4 '(:foreground ,fairy-mint-600))
       )

      ("^\\(pod/[^ ]+\\).*Completed" 1 '(:foreground ,fairy-sky-100))
      ("^\\(pod/[^ ]+\\).* +" 1 '(:foreground ,fairy-gold-600))


      ("poddisruptionbudget[^ ]+ +[^ ]+ +[^ ]+ +0.*" 0 '(:foreground ,fairy-carrot-600))
      (".*poddisruptionbudget[^ ]+ +[^ ]+ +[^ ]+ +[^0].*" 0 '(:foreground ,fairy-mint-600))

      ("\\b\\(deployment\\)[^/ ]+/[^ ]+ +\\([0-9]+\\)/\\2 +\\2 +\\2" 0 '(:foreground ,fairy-mint-600))
      ("\\b\\(deployment\\)[^/ ]+/[^ ]+ +\\([0-9]+\\)/[0-9]+ +[0-9]+ +[0-9]+" 0 '(:foreground ,fairy-gold-600))

      ("\\b\\(job\\)[^/ ]+/[^ ]+ +\\([0-9]+\\)/\\2" 0 '(:foreground ,fairy-mint-600))
      ("\\b\\(job\\)[^/ ]+/[^ ]+ +\\([0-9]+\\)/[0-9]+" 0 '(:foreground ,fairy-gold-600))

      ("\\b\\(rollout\\)[^/ ]+/[^ ]+ +\\([0-9]+\\) +\\2 +\\2 +\\2" 0 '(:foreground ,fairy-mint-600))
      ("\\b\\(rollout\\)[^/ ]+/[^ ]+ +\\([0-9]+\\) +[0-9]+ +[0-9]+[mdh]" 0 '(:foreground ,fairy-carrot-600))
      ("\\b\\(rollout\\)[^/ ]+/[^ ]+ +\\([0-9]+\\) +[0-9]+ +[0-9]+ +[0-9]+[mdh]" 0 '(:foreground ,fairy-carrot-600))
      ("\\b\\(rollout\\)[^/ ]+/[^ ]+ +\\([0-9]+\\) +[0-9]+ +[0-9]+ +[0-9]+" 0 '(:foreground ,fairy-gold-600))

      ("\\b\\(daemonset\\)[^/ ]+/[^ ]+ +\\([0-9]+\\) +\\2 +\\2 +\\2 +\\2" 0 '(:foreground ,fairy-mint-600))
      ("\\b\\(daemonset\\)[^/ ]+/[^ ]+ +[0-9]+ +[0-9]+ +[0-9]+ +[0-9]+ +[0-9]+" 0 '(:foreground ,fairy-carrot-600))

      ("\\b\\(statefulset\\)[^/ ]+/[^ ]+ +\\([0-9]+\\)/\\2" 0 '(:foreground ,fairy-mint-600))
      ("\\b\\(statefulset\\)[^/ ]+/[^ ]+ +[0-9]+/[0-9]+" 0 '(:foreground ,fairy-carrot-600))

      ("^\\(node/ip-[^ ]+\\).*\\(Ready\\)"
       (1 '(:foreground ,fairy-sky-600))
       (2 '(:foreground ,fairy-mint-600))
       )

      ))

  (when (get-buffer "*kubectl*")
    (with-current-buffer "*kubectl*"
      (fundamental-mode)
      (kubectl-mode)
      (font-lock-mode -1)
      (font-lock-mode 1)
      (font-lock-update))))


(provide 'kubectl-font-lock-keywords)

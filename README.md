# kubectl.el

yet another kubernetes interface for Emacs! there's almost no chance in heck
that you want this project, though. there's other "kubernetes-in-Emacs"
alternatives that are more popular, so before proceeding any further, please
consider

- https://github.com/kubernetes-el/kubernetes-el
- https://github.com/abrochard/kubel
- just, not using this project

I wrote this specifically for my use case, and without any concerns for
portability.

- I liked k9s but I didn't like its keybindings and I couldn't invoke k9s from emacs
- I use `aws-okta` to auth to my cluster, so this project assumes its usage
- I wanted to write some transients like magit
- I wanted a nice header area that kubel doesn't have
- I wanted to do things to/with CRDs without having to wait for kubernetes-el

## installation

uh, well, if you insist. this package is just a small elisp wrapper around a
bunch of shell commands. so you must have some binaries installed. here's how
you could do it with homebrew.

```
brew install yq kubectl aws-okta
```

another uh, it depends on some Emacs packages too. I forget how to make the
package declare its own dependencies, so uhhh here's the list, you can
`M-x package-install` them:

```
- s
- dash
- transient
- bpr
```

then .. well this isn't in melpa, so you probably gotta clone this directory and
add it to your emacs path

```
git clone https://github.com/gempesaw/kubectl.el.git ~/somewhere/kubectl.el
```

```elisp
(add-to-list 'load-path (expand-file-name "~/somewhere/kubectl.el"))
```

and then uhhh call `kubectl`, if you want. I use a keychord for it :shrug:

```elisp
(key-chord-define-global "xk" 'kubectl)
```

## keybinds

The most useful key is gonna be `C`, which lets you switch context. Then, `R`,
which lets you specify a comma separated list of resources, and `N` for choosing
the namespace.

#### choose your destiny

| key | command                                  | description                                              |
|-----|------------------------------------------|----------------------------------------------------------|
| `r` | kubectl-transient-choose-resource        |                                                          |
| `A` | kubectl-transient-choose-resource-all-ns |                                                          |
| `R` | kubectl-transient-choose-resource        |                                                          |
| `N` | kubectl-choose-namespace                 |                                                          |
| `C` | kubectl-transient-choose-context         | switch aws profile/context/ns/resources in one transient |

### do things to resource at point

| key        | command                            | description                                                                 |
|------------|------------------------------------|-----------------------------------------------------------------------------|
| `e`        | kubectl-edit-resource-at-point     | pop into a new buffer rin yaml mode, with `C-c C-c` to `kubectl apply` back |
| `k`        | kubectl-delete-resource-at-point   |                                                                             |
| `o`        | kubectl-get-yaml-at-point          |                                                                             |
| `<return>` | kubectl-describe-resource-at-point |                                                                             |

### do things to pods/workloads

| key | command              | description                                 |
|-----|----------------------|---------------------------------------------|
| `f` | kubectl-port-forward | prompt for a port to forward                |
| `x` | kubectl-pod-exec     | pop a new shell with an exec command primed |
| `l` | kubectl-pod-logs     | pop a new shell with a log command primed   |


### navigation

| key   | command                  | description |
|-------|--------------------------|-------------|
| `n`   | kubectl-next-line        |             |
| `p`   | kubectl-previous-line    |             |
| `M-n` | kubectl-next-section     |             |
| `M-p` | kubectl-previous-section |             |


### the rest of them

| key | command                    | description                                                |
|-----|----------------------------|------------------------------------------------------------|
| `g` | kubectl-init               | start `kubectl.el`                                         |
| `$` | kubectl-show-log-buffer    | show the results of all the kubectl commands you've issued |
| `:` | kubectl-run-custom-command | run a kubectl command in the current context               |
| `?` | kubectl-transient-help     | show a transient with all the available commands           |
| `h` | kubectl-transient-help     | ""                                                         |

## license

MIT, with extra emphasis on 0 fitness for a particular purpose

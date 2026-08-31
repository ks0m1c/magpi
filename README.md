<p align="center">
  <img src="magpi.svg" width="240" alt="Twin Magpies in wait.">
</p>

# Magpi

State your why. Bind the context that matters. Spawn an action. Interact. React.

Tiny Emacs porcelain for deliberate agentic work in your Git repository, we support Pi out of the box.

## Dependencies

Magpi does not replace its priors. It is porcelain over them, as Magit is over Git.

| Prior | Owns | Magpi |
|-------|------|-------|
| Emacs 29.1 | point, buffers, `project.el` | bind, status, `C-c m` |
| Git | objects, worktrees, branches | identity; missing stays missing |
| [Magit](https://magit.vc) | diff, stage, log, commit, `magit-section` | glance in Magit-mode; depth stays Magit |
| [Pimacs](https://github.com/ananthakumaran/pimacs.el) | chat, Pi session, protocol | one adapter |
| [Pi](https://pi.dev) | the agent | spawn, observe, react to asks |

`Package-Requires` is Emacs, Magit, and Pimacs. 
Magit already pulls Transient and `magit-section`.  

## Use

`C-c m m` opens Magpi status. `C-c m i` and `C-c m @` work anywhere.

| Key | Meaning |
|-----|---------|
| `i` | create an intention |
| `@` | bind the surface at point |
| `s` | spawn an action |
| `RET` | intention → Magit; action → chat; ask → React |
| `g` | reconcile and repaint |
| `m` `d` `l` `c` | Magit status, diff, log, commit |
| `a` | React |
| `?` `!` | pending ask, disconnection |

Launch defaults to writer (`w`); press `w` for reader (`r`). The objective
never enters chat — you author the task as the first message.

## Install

Have Magit and Pimacs first. Doom:

```elisp
(package! magpi
  :recipe (:local-repo "magpi"
           :files ("emacs/*.el")))

(use-package! magpi
  :commands (magpi-status magpi-spawn))
```

## Checks

```sh
make test          # unit + seams
make test-unit     # no Magit package tree
make test-seams    # real Magit + Transient
```

## Spec

The names, invariants, composition, and build order live in
[`specs/MAGPI.org`](specs/MAGPI.org).

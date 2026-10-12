# Emacs configuration

A personal configuration and a place to experiment. Maintenance, refactoring
and rethinking workflows are all in scope; shedding weight is always welcome.

## Shape

Bottom up: self-contained components in `lisp/my-*.el`, wired into workflows
in `config.org`, which tangles to `init.el` (never edit it directly).
org-note-graph and struktur.el, a structural editing experiment, live in
`~/projects/` with their own guidelines.
`plan.org` is the current improvement plan; `backlog.org` collects open ideas.

## Taste

- No bloat. Prefer built-in features; Use existing components/packages to reduce maintainance.
- Functional style: small functions, explicit data flow, side effects at the edges.
- Elegance and readability; clever solutions are welcome when they stay clear. Fewer concepts over fewer lines.
- Short prose near its subject. Remove what a change makes obsolete.
- Designs are open. Propose alternatives, and ask before changing habits.

## Practicalities

- `emacs` is an emacsclient alias here; use `/usr/bin/emacs` for batch runs.
- Tangle: `/usr/bin/emacs -Q --batch --eval '(progn (require (quote org)) (org-babel-tangle-file "config.org"))'`
- Smoke test: `/usr/bin/emacs -Q --batch -l tests/smoke.el` tangles config.org into a
  sandbox and loads it; straight rebuilds there, not in the daemon.
- Upgrades: `straight-pull-all`, the smoke test, then freeze with
  `/usr/bin/emacs -Q --batch -l tests/smoke.el --eval '(straight-freeze-versions t)'`.

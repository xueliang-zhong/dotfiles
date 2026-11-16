# Design: `xl-idle-require` — explicit idle loading

Date: 2026-11-16
Scope: `emacs/xueliang.el`

## Goal

Make first-use latency work explicit. Instead of named warm-up functions that
hide which packages load when, `xueliang.el` states it directly:

```elisp
(xl-idle-require 'org 1)
(xl-idle-require 'magit 2)
```

`PRIORITY` is measured in half-seconds: Emacs waits `priority × 0.5` seconds of
idle time, then loads the feature. Higher priority numbers load later.

## API

`(xl-idle-require PACKAGE PRIORITY)`

- `PACKAGE` is a feature symbol, the same argument `require` takes (`org`,
  `magit`, `eshell`); not a package name.
- Delay is hard-coded as `(* 0.5 priority)`. No step variable, no exponential
  backoff: one formula, predictable.
- One-shot timer: it removes itself after running, so `require` never repeats
  and a manually loaded package makes the timer a no-op.
- Returns the timer object so a caller can `cancel-timer` it.
- Loading is silent. On failure, one line is reported through `message`, so a
  broken package is visible in `*Messages*` instead of disappearing.

## Implementation

Placed in the existing "First-use latency - pay one-time costs while idle"
section, which shrinks to the helper plus three call sites:

```elisp
(defun xl-idle-require (package priority)
  "Load PACKAGE once Emacs has been idle for PRIORITY half-seconds."
  (run-with-idle-timer
   (* 0.5 priority) nil
   (lambda ()
     (unless (xueliang-require-silently package)
       (message "xl-idle-require: could not load %s" package)))))
```

- `xueliang-require-silently` is kept and reused; its comment about
  `inhibit-message` still logging to `*Messages*` stays, since that is why the
  helper exists.
- The failure `message` is outside the helper's silent `let`, which is what
  makes it visible.
- Call sites replace the old code exactly: `org` at priority 1, `eshell` and
  `magit` at priority 2.
- `xueliang-warmup-org` and `xueliang-warmup-modules` are deleted, along with
  their two `run-with-idle-timer` lines.

## Deliberately not included

- **No queue.** Each `xl-idle-require` registers its own independent timer;
  packages do not block each other's deadlines. If one load runs long, the next
  timer fires as soon as it finishes — unavoidable in single-threaded Emacs.
- **No on/off switch.** No `xl-idle-require-enabled` variable: adding an option
  nobody toggles is dead weight.
- **No change to `xl-packages`.** That list is the install set read by
  `init.el`. `xl-idle-require` only decides *when* a feature is loaded, never
  whether a package is installed; a feature absent from the install set simply
  reports a failure.

## Error handling

`xueliang-require-silently` wraps `require` in `ignore-errors`, so a missing
file and a load error are treated the same: the call returns nil and
`xl-idle-require` reports `xl-idle-require: could not load <package>`. The
error's own text is not preserved — a second `require` by hand shows it.

## Verification

- `just emacs-smoke`: the profile loads in batch with no init error (idle timers
  never fire in batch, so this only proves the form is well-formed).
- In a real Emacs: `(xl-idle-require 'org 3)` followed by `(featurep 'org)`
  after 1.5 s of not touching the keyboard.
- Failure path: `(xl-idle-require 'no-such-feature 1)` and confirm the one-line
  message appears in `*Messages*` and the echo area.

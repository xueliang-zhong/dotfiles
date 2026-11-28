# Refactoring plan

A review of how `tuned-emacs/` is put together, and eight steps that would tidy
it up. The base is already sound. The process layer (`tuned-async`) and the UI
layer (`tuned-spinner`) are separate, most modules can be installed on their own,
and each has ERT tests. This plan is not a rewrite. It moves a few pieces so
that dependencies point one way and each file has one job.

Status: implemented. All eight steps have landed; the headings are kept as a
record of what changed.

## Target layers

```
L4 user      personal.el · example/ (xueliang.el and its own helpers)
L3 distro    init.el (a manifest) · tuned-bootstrap · tuned-key-bindings · generated autoloads
L2 features  git-* · dired · org-babel-async · consult-spinner · eln-opt · perfetto
L1 services  tuned-child-emacs (child Emacs) · native-compile queue · package-fetch
L0 base      tuned-async (processes) · tuned-spinner (UI)
```

Rule: a module depends only on layers below it. A few places today point
upwards or form a cycle; the steps below remove them.

## Steps, lowest risk first

### ~~1. Split tests by module~~

- `tests/smoke-tests.el` is 3751 lines. About 35 of its tests cover
  `tuned-git-status` and 10 cover `tuned-org-babel-async`.
- Move them into per-module files, as `tuned-dired-tests.el` already does:
  `tuned-git-status-tests.el`, `tuned-org-babel-async-tests.el`, and so on.
  `smoke-tests.el` keeps only profile-level checks: keymaps, hooks, line counts.
- `cmd_test` in `install/emacs-setup.sh` loads each suite with its own `-l`
  line. Run every `tests/*-tests.el` with a glob instead, so a new suite needs no
  script change.

### ~~2. Keep `xueliang-utils.el` with the example~~

- `xueliang-utils.el` holds the logic behind `example/xueliang.el`. It uses the
  `xueliang-` prefix and declares variables that the example sets.
- It sits in `lisp/`, and the README describes it as "small shared helpers".
- Moving it to `example/` makes the line between the distribution and the
  author's own configuration clear. People who copy parts of the example into
  `personal.el` still need it on `load-path`, so `init.el` adds `example/` to
  `load-path` either way.

### ~~3. One launcher for child Emacs processes, and no cycle~~

- Three places build a child Emacs command line
  (`invocation-name` + `--batch --init-directory`):
  `tuned-async-evaluate.el` (`tuned-async-evaluate--program`), `tuned-eln-opt.el` and
  `tuned-package-install.el`.
- Keep one function, `tuned-async-child-command`, and have the other two call it.
- `tuned-async-evaluate` and `tuned-native-compile` each `declare-function` the other.
  Moving `tuned-async-native-compile` into `tuned-native-compile.el` makes the
  dependency one-way: native-compile → evaluate.

### ~~4. Move package bootstrap out of `init.el`~~

- About 170 of the 308 lines in `init.el` are the install path: archive
  refresh, keyring rebuild, the retry without signature checks, and
  `package-vc`.
- Move them to `lisp/tuned-bootstrap.el`, next to `tuned-package-fetch`. `init.el`
  then reads as a manifest: which Crafted modules, which packages, which layers.
  That matches the README's claim that the personal layer is small.
- `tuned-report` and `tuned-progress` move with it.

### ~~5. Generate autoloads instead of writing them by hand~~

- `init.el` lists autoloads by hand: seven `tuned-dired` commands, seven
  `tuned-perf-trace` entries, three async helpers.
- Put `;;;###autoload` cookies on those definitions. Have the setup and check
  steps run `loaddefs-generate` into `lisp/tuned-autoloads.el`, and have `init.el`
  load that one file.
- Add a smoke test that each autoload points to a real definition, so a stale
  generated file is caught.

### ~~6. Split `tuned-git-status.el` along its sections~~

- At 1549 lines it already has clear sections: loading indicator, parsing, diff
  view, sections and folding, Git processes, operations, commit view,
  rendering, mode, auto refresh.
- Split into:
  - `tuned-git-core`: Git processes and parsing; pure functions, easiest to test.
  - `tuned-git-show`: diff and commit views.
  - `tuned-git-status`: rendering, the mode, auto refresh.
- `tuned-git-commands` depends on the status module only for its faces and
  `--show-commit`. After the split it depends on `tuned-git-show` alone.

### ~~7. perfetto: a core plus probes~~

Do this after the current perfetto work is committed.

- The recorder knows every subsystem it traces: `tuned-async`, pyim, completion,
  files, major modes. Each new one makes `tuned-perf-trace.el` larger
  (1793 lines).
- Keep the ring, JSON output, recording state and an install registry in
  `tuned-perf-trace`. Move each probe into a small file, or register it with an
  `tuned-perf-define-probe` macro (feature, install, uninstall) that installs
  it inside `with-eval-after-load`.
- Every span advice repeats the same shape:

  ```elisp
  (if (or (not tuned-perf--recording) tuned-perf--inside)
      (apply original args)
    (let ((start (tuned-perf--elapsed)))
      (unwind-protect (apply original args)
        (tuned-perf--record "X" NAME CATEGORY start ...))))
  ```

  An `tuned-perf-define-span-advice` macro would cover it.

### ~~8. Make `tuned-async` independent of `tuned-spinner`~~

- The process layer does `(require 'tuned-spinner)` so that old
  `(require 'tuned-async)` calls still get the loader. That makes L0 depend on UI.
- Either have `tuned-async` run start and finish hooks that the spinner
  subscribes to, or have callers require both modules.
- This step touches the most callers, so it comes last.

## Follow-up

A review of the eight commits added five more:

- `git: name the split modules' functions after their files` renamed the
  functions in `tuned-git-core` and `tuned-git-show` to their module prefixes, kept
  the faces' user-facing names, and moved the status refresh registration
  behind a core setter.
- `perfetto: name the target in generated span docstrings` made
  `tuned-perf-define-span-advice` document what it records and around which
  function, and tested every generated advice.
- `emacs: give the generated autoloads one header` removed the generator's
  GNU Emacs preamble from `tuned-autoloads.el`.
- `emacs: load tuned-async-evaluate on first use` stopped `tuned-eln-opt` from
  pulling the evaluator into startup.
- `perfetto: move package probes out of the core` split the tuned-async,
  vertico, corfu and pyim probes into `tuned-perf-probes.el`.

## What not to do

- Do not build a Doom-style module system or macro DSL. Being a thin layer over
  Crafted is the point of this distribution.
- Do not rename every module or prefix at once. Each step above is its own
  commit, keeps user-visible behaviour unchanged, and must pass
  `just emacs-check`.

## Keeping documents in sync

Each step updates `doc/lisp-modules.md` and the Layout section of the README in
the same commit. Strike a step from this plan once it has landed.

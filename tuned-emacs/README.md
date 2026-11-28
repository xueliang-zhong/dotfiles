# tuned-emacs

A thin personal layer on top of a pinned [Crafted Emacs](https://github.com/SystemCrafters/crafted-emacs). It is a distribution in its own right, not just a Git tweak.

**Key feature: tuned-eln-opt, background native compilation of what you use.** Once per session, after Emacs has been idle for five seconds, a child Emacs native-compiles the files behind your commands, major modes, minor modes and recent files. Nothing is compiled while you work, and code you never use is never compiled. Details: [doc/tuned-eln-opt.md](doc/tuned-eln-opt.md).

**Main features:**

- **Background native compilation (tuned-eln-opt).** The key feature described above. See [Background compilation](#background-compilation).
- **Asynchronous rewrite of popular packages/commands** like magit-status, package-install, git blame, etc. Status, staging, commit, blame and the commit view run as asynchronous processes, with a spinner instead of a frozen interface. See [doc/lisp-modules.md](doc/lisp-modules.md).
- **Perfetto trace profiling support.** `tuned-perf-trace` records commands, timers, process callbacks, GC, font-lock and redisplay frames (with jank and window/phase detail) as a Perfetto timeline, with zero cost when off. See [doc/tuned-perf-trace.md](doc/tuned-perf-trace.md).
- **Async extras.** Asynchronous dired, Org babel with live output and cancellation, persistent scratch, a completion stack (vertico, consult, corfu, orderless, embark), and a macOS UK keyboard fix for `#`.
- **Async spell checking (tuned-consult-spell).** Buffer and region scans behind a consult list, using the installed `aspell`, `hunspell` or `ispell`. See [doc/tuned-consult-spell.md](doc/tuned-consult-spell.md).
- **Tagbar-style imenu sidebar (tuned-tagbar).** `F8` shows the tags of the current buffer in a narrow right side window, grouped, foldable, with type icons and following point. The candidates come from Consult's imenu and Embark acts on the tag at point. See [doc/tuned-tagbar.md](doc/tuned-tagbar.md).
- **Chinese input inside Emacs (pyim).** `F2` toggles a Lisp input method instead of the macOS IME, so no keypress waits for a system round trip; candidates appear at point through posframe.
- **Shared asynchronous layer.** One composable process API (`tuned-async`) runs Git, dired listings and previews, Org `:async` source blocks, and background Lisp evaluation.
- **Unified spinner animation UI for all async tasks.** One loader design (`tuned-spinner`) shows every asynchronous task: the mode-line spinner, the centre loader, and the progress lines of background work.
- **Spacemacs-style keys and a home page.** `SPC` trees, `,` major-mode leader, a Tuned Emacs home screen with its own logo and the recent files, and a restart command that returns to the same file and point.
- **One CLI for three systems.** `bin/tuned` installs, syncs, checks and runs the profile on macOS, Linux and Windows, and works without `just`.
- **Tested without a GUI.** ERT suites for each module run in `emacs --batch`.

The personal layer is small. Its design is in the documents under [doc/](doc/).

`tuned-` is the namespace of this project (`tuned-key-bindings`, `tuned-git-status`, `tuned-spinner`). The version is in `VERSION`; `M-x tuned-version` shows it. The configuration lives in the `tuned-emacs/` folder of the dotfiles repository.

## Background compilation

`tuned-eln-opt` compiles, in a child Emacs, the source of what your
history uses. The sources are the commands in savehist, the minor modes
that were enabled, the major modes of your recent files, and the recent
Emacs Lisp files. Each file is native-compiled only if it lies under a
trusted folder (Emacs's library, the package folder, the profile, or
this distribution), and only if it does not opt out with
`no-native-compile` or `no-byte-compile`. Files over 1 MB are skipped.
A failed file is not retried until its source changes.

- The child starts once per session, after five seconds of idle time;
  it compiles only what is left, and exits at once when nothing is.
  `tuned-eln-opt-mode` and `tuned-eln-opt-idle` change this.
- `tuned-eln-opt-scope` chooses what is compiled by itself: commands,
  minor modes and recent files from your history, and the code from
  packages or from the distro. All are on by default.
- `M-x tuned-eln-opt-report` shows each source, its size, its status and
  the command that forces a compile. Nothing is compiled by the report.
- `M-x tuned-eln-opt-start` runs the compile now.
  `M-x tuned-eln-opt-compile-file` and `M-x tuned-eln-opt-compile` force
  one file or one function.
- `M-x tuned-eln-opt-clean` deletes the compiled files and keeps the
  subr trampolines.
- While `tuned-perf-trace-start` records, the child's run is
  recorded too, into `tuned-eln-opt-trace-*.json` next to the main
  trace.

Everything is described in [doc/tuned-eln-opt.md](doc/tuned-eln-opt.md).

## Requirements

- Emacs 30 or later (developed and tested on Emacs 31.1).
- `git`, `curl` and `gpg` for package installation. `gpg` is needed for signature checks on GNU ELPA.
- `aspell`, `hunspell` or `ispell` for `tuned-consult-spell`; Emacs has no built-in dictionary.
- `just` is optional. Every `just emacs-*` recipe is a thin wrapper around `bin/tuned`.

## What is guaranteed, and what is not

| Claim | How it is checked |
| --- | --- |
| The personal layer is small | Line counts of `init.el`, `early-init.el`, `lisp/tuned-key-bindings.el` and `personal.el` (see the table in `doc/startup.md`). |
| Git does not block the UI | Reads and writes all run through `make-process`. The ERT tests assert that the loader is shown while the content has not arrived. |

Not yet asynchronous, stated plainly: package installation and removal from `list-packages` (package transactions) still run synchronously. Archive refresh is asynchronous. Installation from the CLI is parallel; see `doc/startup.md`.

## Layout

```
tuned-emacs/
├── VERSION                  Version, shown by M-x tuned-version
├── CHANGELOG.md             One section per version
├── README.md                What it is and how to install it
├── early-init.el            Startup settings: GC, frame chrome, package archives
├── init.el                  The manifest: Crafted modules, packages, layers
├── personal.el              Your own settings; starts empty, with comments
├── example/                 The author's opinionated configuration (not loaded by default)
│   ├── xueliang.el              The configuration itself
│   └── xueliang-utils.el        Helpers behind the example's bindings
├── assets/                  tuned-emacs-logo.svg, the home screen logo
├── lisp/                    Modules loaded with `require'
│   ├── tuned-key-bindings.el       Spacemacs-style key bindings (SPC tree, M-m, ...)
│   ├── tuned-home.el               The home screen: logo, recent files 1-9, links, startup time
│   ├── tuned-bootstrap.el          Package installation: refresh, fetch, retry
│   ├── tuned-async.el              Asynchronous processes: ownership, slots, queues
│   ├── tuned-spinner.el            Shared loader: kernel and mode-line slot
│   ├── tuned-async-evaluate.el     Child Emacs command lines and evaluation
│   ├── tuned-git-core.el           Git processes and parsing
│   ├── tuned-git-show.el           Diff and commit views, faces
│   ├── tuned-git-status.el         Status rendering, mode and auto refresh
│   ├── tuned-git-commands.el       Gstatus, Gblame, Gwrite, Gcommit
│   ├── tuned-consult-spinner.el    Spinner for asynchronous consult lists
│   ├── tuned-consult-spell.el      Spell checks in a consult list
│   ├── tuned-tagbar.el             Tagbar-style imenu sidebar (F8)
│   ├── tuned-org-babel-async.el    `:async yes' for Org source blocks
│   ├── tuned-dired.el              Asynchronous dired listings and previews
│   ├── tuned-perf-trace.el         Chrome JSON timelines of Lisp work
│   ├── tuned-perf-probes.el        Its probes for tuned-async, vertico, corfu, pyim
│   ├── tuned-package-fetch.el      Parallel download and signature check of packages
│   ├── tuned-native-compile.el     Native-compile queue and its monitor
│   ├── tuned-package-install.el    Asynchronous package install
│   ├── tuned-eln-opt.el            Background native compilation of what you use
│   ├── tuned-doctor.el             Health checks behind M-x tuned-doctor
│   └── tuned-autoloads.el          Generated autoloads (do not edit)
├── bin/
│   └── tuned                     The CLI: install, sync, doctor, test, upgrade, run
├── install/
│   └── emacs-setup.sh            Compatibility shim for the old command names
├── tests/                   ERT suites (see "Testing")
├── doc/                     Design notes and reference documents
├── crafted.lock             Crafted Emacs repository URL and pinned commit
└── LICENSE                  License
```

## Installation

### macOS

```bash
just emacs-mac-setup                       # with just
bash tuned-emacs/bin/tuned install         # without just
```

The setup runs eight steps: copy the dotfiles, clone Crafted at the pinned commit and write the profile stubs, install the packages, generate the autoloads, byte-compile the personal Lisp, build the advice trampolines, write `packages.lock`, and point `~/.emacs.d` at the Crafted profile. The last step needs Emacs to be closed. If it is running, rerun `just emacs-make-default` after quitting it.

Start Emacs with `just emacs-try`, `bash tuned-emacs/bin/tuned run`, or the Dock icon.

### Linux

```bash
just emacs-linux-setup                     # with just
bash tuned-emacs/bin/tuned install         # without just
```

### Windows

See [doc/windows.md](doc/windows.md).

### Setup output

Each setup step prints a `[i/N]` title. Package installation reports the archive refresh, the parallel fetch, and any packages that fell back to the serial path. `NO_COLOR=1` disables colors. Output from inside a step is hidden unless the step fails; `TUNED_VERBOSE=1` shows everything.

## Commands

Every `just` recipe has an equivalent in `bin/tuned`:

| `just` recipe | `tuned` command | What it does |
| --- | --- | --- |
| `emacs-mac-setup` | `install` | Full setup on this platform |
| `emacs-linux-setup` | `install` | Full setup on this platform |
| `emacs-windows-setup` | `install` | Full setup on this platform (Git Bash) |
| `emacs-crafted-install` | `crafted-install` | Clone Crafted and write the profile stubs |
| `emacs-smoke` | `smoke` | Load the whole profile in batch mode |
| `emacs-test` | `test` | Run all ERT suites |
| `emacs-check` | `sync`, then `test` | Sync and run all suites |
| `emacs-trampolines-cache-setup` | `trampolines` | Build advice trampolines by hand |
| `emacs-make-default` | `default` | Point `~/.emacs.d` at the Crafted profile |
| `emacs-restore-spacemacs` | `restore-spacemacs` | Switch back to the old Spacemacs profile |
| `emacs-try` | `run` | Start Emacs; arguments pass through |

Other commands: `sync` (packages, autoloads, byte code, trampolines,
`packages.lock`), `doctor` (what is wrong on this machine), `test NAME` (one
suite) and `upgrade` (pull the distro, move Crafted to the new pin, sync).
The old `install/emacs-setup.sh` names still work through a compatibility
shim.

Environment variables: `EMACS`, `PROFILE`, `PROFILES`, `PROFILE_DIR`, `DOTFILES`, `TUNED_SKIP_DOTFILES=1` (skip `env.sh` in setup), `TUNED_FORCE_WINDOWS=1` (exercise the Windows flow on another OS).

## Testing

```bash
just emacs-check                                      # sync + all ERT suites
bash tuned-emacs/bin/tuned test                       # without just
bash tuned-emacs/bin/tuned test async                 # one suite
```

The suites run in `emacs --batch`, without a graphical frame:

| File | Covers |
| --- | --- |
| `tests/smoke-tests.el` | Profile structure, key bindings, startup settings, Org, Magit, dired, completion, restart, and more |
| `tests/tuned-async-tests.el`, `tests/tuned-async-compat-tests.el`, `tests/tuned-async-evaluate-tests.el` | Asynchronous process layer, compatibility API and child Emacs |
| `tests/tuned-spinner-tests.el`, `tests/tuned-consult-spinner-tests.el` | Loader frames and mode-line slot, consult indicator |
| `tests/tuned-git-status-tests.el`, `tests/tuned-git-commands-tests.el` | Git status, auto refresh, the write queue, GBlame |
| `tests/tuned-org-babel-async-tests.el` | `:async yes' Org blocks |
| `tests/tuned-home-tests.el` | Home screen branding, text logo, recent files and digit keys, redraw |
| `tests/tuned-dired-tests.el` | Asynchronous dired listings and previews |
| `tests/tuned-tagbar-tests.el` | Tagbar sections, jumping, folding, refresh and the Embark target |
| `tests/tuned-perf-trace-tests.el` | Trace recording and JSON output |
| `tests/tuned-package-fetch-tests.el` | Parallel job runner, dependency plan, unsigned-archive policy, gpg check |
| `tests/tuned-consult-spell-tests.el` | Checker output parsing, word positions, region widening, program choice |
| `tests/tuned-doctor-tests.el` | Doctor formatting, version lookup, stale byte code, autoloads and `packages.lock` |

Every `tests/tuned-*-tests.el` runs on its own, without the profile, and every
suite is loaded by a glob so a new module needs no script change.

## Common changes

| Goal | Where |
| --- | --- |
| Add or remove a package | `tuned-packages` in `init.el` (do not list packages that Crafted already provides) |
| Change completion (vertico, corfu, ...) | The completion section in `example/xueliang.el`; the Crafted switch is in `init.el` |
| Change a personal key binding or command | `personal.el` (examples in `example/xueliang.el`) |
| Change the Spacemacs-style default bindings | `lisp/tuned-key-bindings.el` |
| Add or remove a Crafted module | The `require` lines in `init.el` |
| Upgrade Crafted | Change the commit in `crafted.lock`, then run `emacs-crafted-install` |
| Change startup settings (GC, native-compilation settings, package directories) | `early-init.el` |
| After upgrading Emacs or rebuilding a profile | Run `tuned sync` to rebuild the trampolines |

After a change, run `just emacs-check` (or `bin/tuned sync` then
`bin/tuned test`).

## Documentation

| Document | Topic |
| --- | --- |
| [doc/startup.md](doc/startup.md) | Startup sequence, package installation and the parallel fetch, the load order |
| [doc/lisp-modules.md](doc/lisp-modules.md) | The personal Lisp modules: Git status, G commands, key bindings, spinners, Org babel, dired, personal settings |
| [doc/tuned-consult-spell.md](doc/tuned-consult-spell.md) | Async buffer and region spell checks in a consult list |
| [doc/tuned-tagbar.md](doc/tuned-tagbar.md) | The Tagbar-style imenu sidebar: keys, refresh, folding, Embark |
| [doc/tuned-git-write-queue.md](doc/tuned-git-write-queue.md) | One serial Git write queue per repository: Gwrite and Gcommit with pull |
| [doc/windows.md](doc/windows.md) | Windows setup, platform differences, line endings |
| [doc/tuned-async.md](doc/tuned-async.md) | The asynchronous process layer |
| [doc/tuned-eln-opt.md](doc/tuned-eln-opt.md) | Background native compilation: sources, verdicts, the child, reports |
| [doc/tuned-perf-trace.md](doc/tuned-perf-trace.md) | Chrome JSON timelines of Lisp work |
| [doc/tuned-perf-trace-prompt.md](doc/tuned-perf-trace-prompt.md) | Prompt for generating trace-based investigations |
| [doc/refactoring-plan.md](doc/refactoring-plan.md) | Architecture refactoring: target layers and the eight steps, all landed |
| [doc/distro-design.md](doc/distro-design.md) | Proposed distribution packaging with a flat lisp/: user folder, CLI, doctor, versioning, migration phases |

New documents go in `doc/`. Documents are written in English.

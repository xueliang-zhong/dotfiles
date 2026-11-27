# tuned-emacs

A thin personal layer on top of a pinned [Crafted Emacs](https://github.com/SystemCrafters/crafted-emacs). It is a distribution in its own right, not just a Git tweak.

**Key feature: eln-opt, a lightweight background async compilation.** Hot
functions come from a reachability analysis of public data such as savehist and
recentf. eln-opt aims to replace Emacs native compilation's default
compile-everything policy with one that only compiles hot functions based on
reachability analysis. Its startup cost is small. When Emacs is idle it only
compares a fingerprint, and work happens only when the fingerprint changes.

**Main features:**

- **Background compilation (eln-opt).** The key feature described above. It is safe to interrupt, retry and switch off. See [doc/eln-opt.md](doc/eln-opt.md).
- **Asynchronous rewrite of popular packages/commands** like magit-status, package-install, git blame, etc. Status, staging, commit, blame and the commit view run as asynchronous processes, with a spinner instead of a frozen interface. See [doc/lisp-modules.md](doc/lisp-modules.md).
- **Perfetto trace profiling support.** `xl-perfetto-trace` records commands, timers, process callbacks, GC and font-lock as a Perfetto timeline, with zero cost when off. See [doc/xl-perfetto-trace.md](doc/xl-perfetto-trace.md).
- **Async extras.** Asynchronous dired, Org babel with live output and cancellation, persistent scratch, a completion stack (vertico, consult, corfu, orderless, embark), and a macOS UK keyboard fix for `#`.
- **Shared asynchronous layer.** One composable process API (`xl-async`) runs Git, dired listings and previews, Org `:async` source blocks, and background Lisp evaluation.
- **Unified spinner animation UI for all async tasks.** One loader design (`xl-spinner`) shows every asynchronous task: the mode-line spinner, the centre loader, and the progress lines of background work.
- **Spacemacs-style keys and a home page.** `SPC` trees, `,` major-mode leader, a recent-files home buffer, and a restart command that returns to the same file and point.
- **One setup for three systems.** A single script installs and checks the profile on macOS, Linux and Windows, and works without `just`.
- **Tested without a GUI.** ERT suites for each module run in `emacs --batch`.

The personal layer is small. Its design is in the documents under [doc/](doc/).

`xl-` is the namespace of this project (`xl-key-bindings`, `xl-git-status`, `xl-spinner`). The configuration lives in the `tuned-emacs/` folder of the dotfiles repository.

## Background compilation: no all-or-nothing choice

**eln-opt is a lightweight background async compilation.** Hot functions come
from a reachability analysis of public data such as savehist and recentf.
eln-opt aims to replace Emacs native compilation's default compile-everything
policy with one that only compiles hot functions based on reachability analysis.
Its startup cost is small. When Emacs is idle it only compares a fingerprint,
and work happens only when the fingerprint changes.

Emacs 31 gives two ways to use native compilation, and both are awkward:

- **All or nothing.** Emacs's native-compilation settings either compile every
  file that loads, or none. Compiling every file spends CPU and disk on code
  that is rarely run; compiling none leaves the code you use most running
  without native compilation.
- **Trampolines on the main thread.** When a function is advised or a C
  primitive is redefined, Emacs compiles a trampoline for it synchronously.
  That takes about a second and freezes the interface, and it happens in the
  middle of work, the first time the advice is used.

eln-opt removes the choice. You do not turn native compilation on or off, and
you do not manage the compiled files:

- **Native-compiles only the hot code**, ranked by how the code is reached from
  the commands you use (not by file size). Only files whose expected gain
  justifies the cost are compiled natively.
- **Handles trampolines.** It predicts which C primitives your configuration
  advises and builds their trampolines in the background worker, never in your
  running session, so the first use does not freeze. `trampolines` rebuilds
  them by hand.
- **Works only when Emacs is idle**, one file at a time, at low priority, and
  spaced out so it never competes with your work. Its results are used from the next start.
- **Stays safe.** It compiles only files under trusted roots, skips a file after
  it fails until its source changes, resumes after an interruption, and can be
  switched off with `ELN_OPT=0`.

You can see what it is doing and why with `M-x eln-opt-report`: the status of
each file, its score, its size, and the command that forces a native compile.
The design is in [doc/eln-opt.md](doc/eln-opt.md).

## Requirements

- Emacs 30 or later (developed and tested on Emacs 31.1).
- `git`, `curl` and `gpg` for package installation. `gpg` is needed for signature checks on GNU ELPA.
- `just` is optional. Every `just emacs-*` recipe is a thin wrapper around `install/emacs-setup.sh`.

## What is guaranteed, and what is not

| Claim | How it is checked |
| --- | --- |
| The personal layer is small | Line counts of `init.el`, `early-init.el`, `lisp/xl-key-bindings.el` and `xueliang.el` (see the table in `doc/startup.md`). |
| Git does not block the UI | Reads and writes all run through `make-process`. The ERT tests assert that the loader is shown while the content has not arrived. |

Not yet asynchronous, stated plainly: package installation and removal from `list-packages` (package transactions) still run synchronously. Archive refresh is asynchronous. Installation from the setup script is parallel; see `doc/startup.md`.

## Layout

```
tuned-emacs/
├── early-init.el            Startup settings: GC, frame chrome, package archives
├── init.el                  Selects Crafted modules, installs packages, loads the layers
├── xueliang.el              Personal settings (not published)
├── lisp/                    Modules loaded with `require'
│   ├── xl-key-bindings.el       Spacemacs-style key bindings (SPC tree, M-m, ...)
│   ├── xl-async.el              Asynchronous processes: ownership, slots, queues
│   ├── xl-spinner.el            Shared loader: kernel and mode-line slot
│   ├── xl-git-status.el         Asynchronous Git status buffer, commit view
│   ├── xl-git-commands.el       Gstatus, Gblame, Gwrite, Gcommit
│   ├── xl-consult-spinner.el    Spinner for asynchronous consult lists
│   ├── xl-org-babel-async.el    `:async yes' for Org source blocks
│   ├── xl-dired.el              Asynchronous dired listings and previews
│   ├── xl-perfetto-trace.el     Chrome JSON timelines of Lisp work
│   ├── xl-package-fetch.el      Parallel download and signature check of packages
│   ├── eln-opt.el               Background byte and native compilation
│   └── xl-utils.el              Small shared helpers
├── install/emacs-setup.sh   Install, check and run script (no `just' needed)
├── tests/                   ERT suites (see "Testing")
├── doc/                     Design notes and reference documents
├── data/                    Generated data (eln-opt baseline)
└── frameworks.lock          Crafted Emacs repository URL and pinned commit
```

## Installation

### macOS

```bash
just emacs-mac-setup                                  # with just
bash tuned-emacs/install/emacs-setup.sh setup-macos   # without just
```

The setup runs five steps: copy the dotfiles, clone Crafted at the pinned commit and write the profile stubs, install the packages, byte-compile the personal Lisp, and point `~/.emacs.d` at the Crafted profile. Step 5 needs Emacs to be closed. If it is running, rerun `just emacs-make-default` after quitting it.

Start Emacs with `just emacs-try`, `bash tuned-emacs/install/emacs-setup.sh try`, or the Dock icon.

### Linux

```bash
just emacs-linux-setup                                # with just
bash tuned-emacs/install/emacs-setup.sh setup-linux   # without just
```

### Windows

See [doc/windows.md](doc/windows.md).

### Setup output

Each setup step prints a `[i/N]` title. Package installation reports the archive refresh, the parallel fetch, and any packages that fell back to the serial path. `NO_COLOR=1` disables colors. Output from inside a step is hidden unless the step fails; `XL_VERBOSE=1` shows everything.

## Commands

Every `just` recipe has an equivalent in `install/emacs-setup.sh`:

| `just` recipe | Script command | What it does |
| --- | --- | --- |
| `emacs-mac-setup` | `setup-macos` | Full macOS setup |
| `emacs-linux-setup` | `setup-linux` | Full Linux setup |
| `emacs-windows-setup` | `setup-windows` | Full Windows setup (Git Bash) |
| `emacs-crafted-install` | `crafted-install` | Clone Crafted and write the profile stubs |
| `emacs-smoke` | `smoke` | Load the whole profile in batch mode |
| `emacs-test` | `test` | Run all ERT suites |
| `emacs-check` | `check` | `smoke` and `test` together |
| `emacs-trampolines-cache-setup` | `trampolines` | Build advice trampolines by hand |
| `emacs-make-default` | `make-default` | Point `~/.emacs.d` at the Crafted profile |
| `emacs-restore-spacemacs` | `restore-spacemacs` | Switch back to the old Spacemacs profile |
| `emacs-try` | `try` | Start Emacs; arguments pass through |

Other script commands: `eln-opt`, `eln-opt-compile`, `eln-opt-report`, `eln-opt-status`, `eln-opt-reset`, `compile-lisp`. They are described in [doc/eln-opt.md](doc/eln-opt.md).

Environment variables: `EMACS`, `PROFILE`, `PROFILES`, `PROFILE_DIR`, `DOTFILES`, `XL_SKIP_DOTFILES=1` (skip `env.sh` in setup), `XL_FORCE_WINDOWS=1` (exercise the Windows flow on another OS).

## Testing

```bash
just emacs-check                                      # smoke + all ERT suites
bash tuned-emacs/install/emacs-setup.sh check         # without just
```

The suites run in `emacs --batch`, without a graphical frame:

| File | Covers |
| --- | --- |
| `tests/smoke-tests.el` | Profile structure, key bindings, startup settings, Org, Magit, GBlame, status queue, dired, completion, restart, and more |
| `tests/eln-opt-tests.el` | Background compilation: eligibility, plans, the worker, the quiet state |
| `tests/xl-async-tests.el`, `tests/xl-async-evaluate-tests.el` | Asynchronous process layer |
| `tests/xl-dired-tests.el` | Asynchronous dired listings and previews |
| `tests/xl-perfetto-trace-tests.el` | Trace recording and JSON output |
| `tests/xl-package-fetch-tests.el` | Parallel job runner, dependency plan, unsigned-archive policy, gpg check |

## Common changes

| Goal | Where |
| --- | --- |
| Add or remove a package | `xl-packages` in `init.el` (do not list packages that Crafted already provides) |
| Change completion (vertico, corfu, ...) | The completion section in `xueliang.el`; the Crafted switch is in `init.el` |
| Change a personal key binding or command | `xueliang.el` |
| Change the Spacemacs-style default bindings | `lisp/xl-key-bindings.el` |
| Add or remove a Crafted module | The `require` lines in `init.el` |
| Upgrade Crafted | Change the commit in `frameworks.lock`, then run `emacs-crafted-install` |
| Change startup settings (GC, native-compilation settings, package directories) | `early-init.el` |
| After upgrading Emacs or rebuilding a profile | Run `trampolines` to avoid a blank first frame |

After a change, run `just emacs-check` (or the script equivalent).

## Documentation

| Document | Topic |
| --- | --- |
| [doc/startup.md](doc/startup.md) | Startup sequence, package installation and the parallel fetch, the load order |
| [doc/lisp-modules.md](doc/lisp-modules.md) | The personal Lisp modules: Git status, G commands, key bindings, spinners, Org babel, dired, personal settings |
| [doc/windows.md](doc/windows.md) | Windows setup, platform differences, line endings |
| [doc/eln-opt.md](doc/eln-opt.md) | Background byte and native compilation |
| [doc/xl-async.md](doc/xl-async.md) | The asynchronous process layer |
| [doc/xl-perfetto-trace.md](doc/xl-perfetto-trace.md) | Chrome JSON timelines of Lisp work |
| [doc/xl-perfetto-trace-prompt.md](doc/xl-perfetto-trace-prompt.md) | Prompt for generating trace-based investigations |
| [doc/eln-opt-probes/README.md](doc/eln-opt-probes/README.md) | Benchmark probes for eln-opt |

New documents go in `doc/`. Documents are written in English.

# tuned-emacs

A thin personal layer on top of a pinned [Crafted Emacs](https://github.com/SystemCrafters/crafted-emacs). It is a distribution in its own right, not just a Git tweak.

**Key feature: xl-eln-opt, background native compilation of what you use.** Once per session, after Emacs has been idle for five seconds, a child Emacs native-compiles the files behind your commands, major modes, minor modes and recent files. Nothing is compiled while you work, and code you never use is never compiled. Details: [doc/xl-eln-opt.md](doc/xl-eln-opt.md).

**Main features:**

- **Background native compilation (xl-eln-opt).** The key feature described above. See [Background compilation](#background-compilation).
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

## Background compilation

`xl-eln-opt` compiles, in a child Emacs, the source of what your history uses.
The sources are the commands in savehist, the minor modes that were enabled,
the major modes of your recent files, and the recent Emacs Lisp files. Each file
is native-compiled only if it lies under a trusted folder (Emacs's library, the
package folder, the profile, or this distribution), and only if it does not opt
out with `no-native-compile` or `no-byte-compile`. Files over 1 MB are skipped.
A failed file is not retried until its source changes.

- The child starts once per session, after five seconds of idle time, and only
  when something is left to compile. `xl-eln-opt-mode` and `xl-eln-opt-idle`
  change this.
- `xl-eln-opt-scope` chooses what is compiled by itself: commands, minor modes
  and recent files from your history, and the code from packages or from the
  distro. All are on by default.
- `M-x xl-eln-opt-report` shows each source, its size, its status and the
  command that forces a compile. Nothing is compiled by the report.
- `M-x xl-eln-opt-start` runs the compile now. `M-x xl-eln-opt-compile-file` and
  `M-x xl-eln-opt-compile` force one file or one function.
- `M-x xl-eln-opt-clean` deletes the compiled files and keeps the subr
  trampolines.
- While `xl-perfetto-trace-start` records, the child's run is recorded too, into
  `xl-eln-opt-trace-*.json` next to the main trace.

Everything is described in [doc/xl-eln-opt.md](doc/xl-eln-opt.md).

## Requirements

- Emacs 30 or later (developed and tested on Emacs 31.1).
- `git`, `curl` and `gpg` for package installation. `gpg` is needed for signature checks on GNU ELPA.
- `just` is optional. Every `just emacs-*` recipe is a thin wrapper around `install/emacs-setup.sh`.

## What is guaranteed, and what is not

| Claim | How it is checked |
| --- | --- |
| The personal layer is small | Line counts of `init.el`, `early-init.el`, `lisp/xl-key-bindings.el` and `personal.el` (see the table in `doc/startup.md`). |
| Git does not block the UI | Reads and writes all run through `make-process`. The ERT tests assert that the loader is shown while the content has not arrived. |

Not yet asynchronous, stated plainly: package installation and removal from `list-packages` (package transactions) still run synchronously. Archive refresh is asynchronous. Installation from the setup script is parallel; see `doc/startup.md`.

## Layout

```
tuned-emacs/
├── early-init.el            Startup settings: GC, frame chrome, package archives
├── init.el                  Selects Crafted modules, installs packages, loads the layers
├── personal.el             Your own settings; starts empty, with comments
├── example/xueliang.el     The author's opinionated configuration (not loaded by default)
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
│   ├── xl-eln-opt.el            Background native compilation of what you use
│   └── xl-utils.el              Small shared helpers
├── install/emacs-setup.sh   Install, check and run script (no `just' needed)
├── tests/                   ERT suites (see "Testing")
├── doc/                     Design notes and reference documents
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

Other script command: `compile-lisp` (byte-compiles our own `lisp/` where stale).

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
| `tests/xl-async-tests.el`, `tests/xl-async-evaluate-tests.el` | Asynchronous process layer |
| `tests/xl-dired-tests.el` | Asynchronous dired listings and previews |
| `tests/xl-perfetto-trace-tests.el` | Trace recording and JSON output |
| `tests/xl-package-fetch-tests.el` | Parallel job runner, dependency plan, unsigned-archive policy, gpg check |

## Common changes

| Goal | Where |
| --- | --- |
| Add or remove a package | `xl-packages` in `init.el` (do not list packages that Crafted already provides) |
| Change completion (vertico, corfu, ...) | The completion section in `example/xueliang.el`; the Crafted switch is in `init.el` |
| Change a personal key binding or command | `personal.el` (examples in `example/xueliang.el`) |
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
| [doc/xl-async.md](doc/xl-async.md) | The asynchronous process layer |
| [doc/xl-eln-opt.md](doc/xl-eln-opt.md) | Background native compilation: sources, verdicts, the child, reports |
| [doc/xl-perfetto-trace.md](doc/xl-perfetto-trace.md) | Chrome JSON timelines of Lisp work |
| [doc/xl-perfetto-trace-prompt.md](doc/xl-perfetto-trace-prompt.md) | Prompt for generating trace-based investigations |

New documents go in `doc/`. Documents are written in English.

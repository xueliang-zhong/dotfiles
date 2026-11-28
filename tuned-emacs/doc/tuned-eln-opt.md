# tuned-eln-opt: background native compilation of what you use

Status: implemented and in use. Code: `lisp/tuned-eln-opt.el`. There is no ERT suite for this module yet.

Emacs can native-compile your Lisp in two ways, and both are poor fits for a personal setup. Compiling everything that loads spends time and disk on code you rarely run. Letting the JIT compile on first use blocks the main thread while it works. `tuned-eln-opt` takes a third path: a child Emacs compiles, while you are idle, only the files behind the commands, modes and minor modes you actually use.

## When it runs

- `tuned-eln-opt-arm` (called from `init.el`) starts one idle timer of `tuned-eln-opt-idle` seconds (default 5.0). It does nothing in batch, or when `tuned-eln-opt-mode` is 0.
- The timer fires once per session. If no child from an earlier run is alive, it starts a child with `tuned-eln-opt-start`. The main Emacs reads no file for this: the child works out what there is to compile, and exits at once when there is nothing. This costs one background batch Emacs per session.
- There is no polling and no hook on your commands. The main Emacs compiles nothing itself.

## Where the sources come from

| Source | Read from | Becomes |
| --- | --- | --- |
| Commands you ran | `extended-command-history` in the savehist file | The `symbol-file` of each command |
| Minor modes that were enabled | `tuned-eln-opt-minor-modes` in the savehist file | The `symbol-file` of each mode |
| Major modes of recent files | `auto-mode-alist`, applied to the names in `recentf-list` | The `symbol-file` of each mode |
| Recent Emacs Lisp files | `recentf-list` | The file itself (`.el` or `.el.gz`) |

- The savehist and recentf files are read as data, with `read-eval` bound to nil. Nothing in them is evaluated.
- Minor modes are recorded by `savehist-save-hook`, which lists the modes enabled globally or in some buffer at each save. The list keeps at most 200 names. A mode that is enabled during a session is therefore known to tuned-eln-opt only after the next save.
- A function's source is the `.el` (or `.el.gz`) next to the file that defines it. A C primitive has no Lisp source and is skipped.

## Which files are compiled

Each source gets one verdict. The first rule that applies decides:

1. Outside the trusted roots: skipped, "outside the trusted roots".
2. Opted out: skipped, "opts out with `no-native-compile`" (or `no-byte-compile`).
3. Larger than `tuned-eln-opt-max-source-bytes` (1 MB, uncompressed): skipped, with the size and the limit.
4. An `.eln` exists and is not older than the source, and for a `.el` the `.elc` is not older than the source and the `.eln` is newer than the `.elc`: "eln already". An `.eln` without its `.elc` is not current: Emacs does not load it. The `.eln` is looked for in the user's eln folder first, then in the folders of `native-comp-eln-load-path`, where Emacs keeps its own preloaded elns (`<version>/preloaded/`). A library that Emacs ships precompiled is therefore not compiled again.
5. Failed before, and the source has not changed since: "failed at <time>: <reason>".
6. Otherwise: "to compile".

Trusted roots are Emacs's `lisp-directory`, the package folder, the profile folder (`user-emacs-directory`), and `tuned-emacs/` when `tuned-dotfiles` is set. A file counts only when it is a regular file, not a symbolic link, and its true name lies under a root.

A file opts out by setting `no-native-compile` or `no-byte-compile` to `t`. The cookie counts on the `-*-` first line, or on a line of the local variables (a line that starts with the name, after optional spaces or `;`).

## The child process

`tuned-eln-opt-start` runs a batch Emacs with the profile's `early-init.el` and `init.el`, then calls `tuned-eln-opt-run`. The command line comes from `tuned-async-child-command`, and `tuned-eln-opt` requires `tuned-async-evaluate` only when it is about to start a child, so a normal startup does not load the evaluator:

- The child never writes the user's state. Before `init.el` loads, `savehist-file`, `recentf-save-file`, `save-place-file`, `persistent-scratch-save-file` and `project-list-file` point at the null device, and after it loads the modes that save at exit are turned off. (`init.el` writes recentf while it loads, so the redirect must come first.) The report child does the same.
- Planning caches, per source, the opt-out cookie and the uncompressed size, in `tuned-eln-opt/facts.eld`, keyed by path, modification time and size. A source that has not changed is not opened again. Only the sources asked for in the run are kept in the file.
- A pid file, `tuned-eln-opt/pid` in the profile, allows one run at a time. A zombie process (exited but not yet reaped) does not count as alive.
- For each file, in history order: follow the trace marker (see Tracing), make the `.elc` beside an `.el` if it is missing or older (Emacs uses an `.eln` only beside a `.elc` older than it, so a bare `.el` never picks the `.eln` up; `.elc` files are git-ignored), then `native-compile` to the `.eln` path given by `comp-el-to-eln-filename`. The file succeeds when the `.eln` exists afterwards. The `.eln` files of earlier contents of the same file are then deleted: the name is a hash of the path plus a hash of the content, so an edit would otherwise leave the old `.eln` behind.
- A failure (an error, or no `.eln` written) is recorded with the source, its modification time, the reason and the time. The record is kept in `tuned-eln-opt/failed.eld` until the source changes.
- Because the child loads the profile, advice added at load time makes Emacs build the trampolines of advised C primitives in the child, so the main Emacs does not build them on its first use.

The pid and failed-list folder is `tuned-eln-opt/` in the profile.

## Forcing a compile

- `tuned-eln-opt-compile-file FILE`: compiles FILE now, even if it is native already or failed before.
- `tuned-eln-opt-compile FUNCTION`: compiles the source of FUNCTION. An autoload is loaded first, so a command that is not loaded yet still works.

A forced compile ignores the failed list and the size limit. It does not ignore the trusted-root rule or an opt-out cookie.

## The report

`tuned-eln-opt-report` starts a child that prints the report, and the buffer `*tuned-eln-opt report*` shows a loader until the report arrives. Nothing is compiled.

- Status line: how many sources come from the history, how many entries have no source, and how many files the next run compiles.
- A table with one row per source: its name without `.el` or `.el.gz`, its size in KB, its status (what Emacs runs for it: `eln` native, `elc` bytecode, `lisp` interpreted; or `failed`, `skipped`), the history entries that use it, and the force command with its full path.
- "Not compiled, and why": the skipped and failed sources, with their reasons.
- "Not yet compiled": history entries with no source, with the reason. An autoloaded command gets a hint, `(tuned-eln-opt-compile 'name)`.

## Cleaning

`tuned-eln-opt-clean` deletes every `.eln` in the user's eln folder except the subr trampolines, which are costly to rebuild, and it deletes the failed list. It refuses while a child is running. Interactively it asks for a yes-or-no answer first.

## Tracing

While `tuned-perf-trace-start` records in the main Emacs, a child run follows the recording with `tuned-perf-follow-marker` (prefix `tuned-eln-opt-trace`). The child writes its own `tuned-eln-opt-trace-*.json` into the same folder. The run is the span `tuned-eln-opt run`, and each file is a span named `native <file>`. The follow happens between files, so a file that is being compiled is not checked for the marker until it finishes. See [tuned-perf-trace.md](tuned-perf-trace.md).

## Customization

| Variable | Default | Meaning |
| --- | --- | --- |
| `tuned-eln-opt-mode` | 1 | 0: only when you run a command; 1: also once per session, after the idle delay |
| `tuned-eln-opt-idle` | 5.0 | Seconds of idle time before the once-per-session check |
| `tuned-eln-opt-max-source-bytes` | 1048576 | Largest source, uncompressed, that is compiled without force |
| `tuned-eln-opt-scope` | `(savehist minor-modes recentf packages distro)` | Which sources to compile by itself (see Scope) |

## Scope

`tuned-eln-opt-scope` is a set of two kinds of choice.

- Where a source comes from: `savehist` (the commands you ran), `minor-modes` (the minor modes that were enabled), `recentf` (the major modes of recent files, and the recent Emacs Lisp files).
- Where the file lies: `packages` (Emacs's own libraries, installed packages, and the profile folder), or `distro` (`tuned-emacs/`). With `distro`, every Emacs Lisp file of the distro is a candidate, whether or not the history mentions it: `tuned-emacs/*.el` and `tuned-emacs/lisp/*.el`. Files under `example/`, `tests/` and `install/` are not candidates.

The default is all five, which is what tuned-eln-opt did before the option existed. A source from a kind that is out of scope is not read at all. A source that lies in a location out of scope has no source in the report, and the row says "outside tuned-eln-opt-scope (packages)" or "(distro)". The report prints the scope it used. The explicit commands `tuned-eln-opt-compile` and `tuned-eln-opt-compile-file` ignore the scope.

## Known limits

- Verified in batch: a fresh `require` of a distro file loads the native function once its `.elc` and `.eln` are in place. Not yet verified: the same after a GUI restart.
- No ERT suite covers this module yet.
- The minor-mode list changes only when savehist saves.
- The idle check runs once per session. A long session is not re-checked until the next start.

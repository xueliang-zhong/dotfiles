# Distribution design

How `tuned-emacs/` should be packaged so that it installs, upgrades and is
configured like a mature Emacs distribution, while the code stays flat in
`lisp/`. Also the steps to get there from today's tree.

Status: proposal. Phase 1, the rename in [Naming](#naming), has landed; the
rest is not started. It builds on [refactoring-plan.md](refactoring-plan.md),
whose eight steps and follow-up have landed.

## Where we are

The code is in good shape: one asynchronous process layer, one loader, an ERT
suite per module, generated autoloads, a manifest-style `init.el`. What still
reads as "a personal config folder" rather than "a distribution" is the
packaging around that code:

| Area | Today | What a user of a distribution expects |
| --- | --- | --- |
| User config | `personal.el` lives inside the distro folder and is tracked in git | Their files live in their own folder; upgrading the distro never touches them |
| Author's config | `example/xueliang.el`, switched on by `XL_EXAMPLE=1` in the profile stub | The author is a user like any other, with their own user folder |
| Packages | One list in `init.el` that includes the author's taste (`pyim`, `golden-ratio`, two theme packs, `lua-mode`, ...) | The distro installs what its own code needs; a user adds the rest |
| Base framework | Crafted calls are spread over `init.el` and `early-init.el` | One file knows about Crafted |
| CLI | `install/emacs-setup.sh` with 17 subcommands, wrapped by `just emacs-*` | One command (`bin/tuned`) with `install`, `sync`, `doctor`, `upgrade`, `test` |
| Health check | `check` runs the tests | A `doctor` that explains what is wrong on this machine |
| Versioning | none | `VERSION`, `CHANGELOG.md`, the version shown in Emacs |
| Docs | Design notes and user docs mixed in `doc/` | A user manual and design notes, apart |
| Leftovers | `doc/doomemacs.lock`, `doc/prelude.lock`, `doc/crafted-emacs.lock` are read by nothing; `personal.el` says `~/.emacs-profiles/local.el` is loaded before the modules, but only the example loads it | none |

## Principles

- **Flat `lisp/`.** Every Lisp file of the distribution sits directly in
  `lisp/`. No `core/`, `lib/` or `modules/` folders, no module system and no
  per-feature package files. A file's role shows in its header and in the
  README's file table, not in its path.
- **Thin layer over Crafted.** No macro DSL and no package manager of our own.
- **Startup cost is a feature.** Nothing here may add work at startup; the
  slow-start recorder and the smoke tests keep checking it.
- **One name.** Code, files, buffers, environment variables and the CLI all
  use `tuned` (`tuned-` for Lisp, `TUNED_` for the environment). The `xl-`
  prefix has been retired in phase 1; see [Naming](#naming).
- **Every step lands with the tests green** and keeps behaviour unchanged for
  the author's setup.

## Target layout

```
tuned-emacs/
├── README.md                 What it is, install in three commands, links
├── CHANGELOG.md              One section per version
├── VERSION                   e.g. 0.5.0
├── LICENSE
├── crafted.lock              Crafted URL and pinned commit (was frameworks.lock)
├── bin/
│   └── tuned                 The CLI (was install/emacs-setup.sh)
├── early-init.el             ~20 lines: paths, then lisp/tuned-early.el
├── init.el                   ~40 lines: the load order below
├── lisp/                     Every Lisp file of the distribution, flat
│   ├── tuned-core.el                New: version, paths, the user folder, `tuned-report'
│   ├── tuned-early.el               New: the body of today's early-init.el
│   ├── tuned-crafted.el             New: the only file that names Crafted
│   ├── tuned-doctor.el              New: the checks behind `tuned doctor'
│   ├── tuned-bootstrap.el  tuned-package-fetch.el  tuned-package-install.el
│   ├── tuned-async.el  tuned-async-evaluate.el  tuned-spinner.el  tuned-native-compile.el
│   ├── tuned-key-bindings.el
│   ├── tuned-git-core.el  tuned-git-show.el  tuned-git-status.el  tuned-git-commands.el
│   ├── tuned-dired.el  tuned-tagbar.el  tuned-org-babel-async.el
│   ├── tuned-consult-spinner.el  tuned-consult-spell.el
│   ├── tuned-eln-opt.el
│   └── tuned-perf-trace.el  tuned-perf-probes.el
├── templates/                What a new user starts from
│   ├── config.el                 Empty, with the rules personal.el has today
│   └── packages.el               Empty list, commented
├── users/
│   └── xueliang/                 The author's user folder (was example/)
│       ├── config.el                 was example/xueliang.el
│       ├── packages.el               the author's extra packages
│       └── xueliang-utils.el         was example/xl-utils.el
├── doc/
│   ├── manual/                   install.md, keys.md, windows.md, upgrading.md, faq.md
│   └── design/                   tuned-async.md, tuned-git-write-queue.md, tuned-eln-opt.md, startup.md, ...
└── tests/                    Flat, as today: smoke-tests.el and one tuned-*-tests.el per file
```

What changes in `lisp/` is four new files. Every existing file keeps its name
and place.

### The user folder

Looked up in this order, the first that exists wins:

1. `$TUNED_EMACS_USER_DIR`
2. `$XDG_CONFIG_HOME/tuned-emacs/` (usually `~/.config/tuned-emacs/`)
3. `~/.tuned-emacs.d/`

It holds:

- `config.el`, the user's own settings (today's `personal.el`), loaded last.
- `packages.el`, optional: `(setq tuned-user-packages '(lua-mode doom-themes))`,
  added to the selected packages.
- `local.el`, optional, machine-specific, loaded before everything else in
  `init.el`. That makes what `personal.el` says about `local.el` true for
  everyone.

The user folder goes on `load-path`, so helpers such as `xueliang-utils.el` sit next
to the config that uses them.

`bin/tuned install` copies `templates/` into the user folder when it does not
exist and never overwrites it. The author's machines set
`TUNED_EMACS_USER_DIR` to `users/xueliang/` in the dotfiles, which replaces
`TUNED_EXAMPLE` and `tuned-load-example`.

### Load order

```
early-init.el  → lisp/tuned-early.el            (unchanged behaviour)
init.el        → lisp/tuned-core.el             (paths, user folder, version)
               → USER/local.el               (optional)
               → USER/packages.el            (optional)
               → lisp/tuned-crafted.el          (Crafted packages and modules, overrides)
               → lisp/tuned-bootstrap.el        (installs what is missing)
               → generated autoloads
               → the features init.el requires today, in the same order
               → USER/config.el
```

`init.el` keeps listing the features it requires, as today; that list is the
manifest. A user who does not want one turns it off in `config.el` (for
example `(tuned-eln-opt-mode -1)`), the same way they would in Emacs itself.

### Packages

- `tuned-packages` in `init.el` keeps only what the distribution's own code
  needs: evil-escape, evil-surround, winum, avy, magit, diff-hl, and the like.
- The author's taste moves to `users/xueliang/packages.el`: themes,
  `lua-mode`, `just-mode`, `graphviz-dot-mode`, `markdown-mode`,
  `golden-ratio`, `org-superstar`, `persistent-scratch`, pyim and its
  dictionaries.
- `tuned-vc-packages` (today in `tuned-bootstrap.el`) follows the same split: the
  pyim dictionary is the author's.
- ELPA version pinning is out of scope: package.el cannot install an old
  version. `tuned sync` writes the installed versions to `packages.lock` in the
  profile, and `tuned doctor` reports drift against it.

### Generated files

`tuned sync` generates the autoloads of `lisp/` into the profile, under
`user-emacs-directory`'s `tuned/` folder. The repository then holds no
generated files and `lisp/tuned-autoloads.el` leaves git. `init.el` regenerates
the file when it is missing or older than a file in `lisp/`, so a fresh clone
still starts.

### The CLI

`bin/tuned` replaces `install/emacs-setup.sh`. `install/emacs-setup.sh` stays
for one version as a shim that prints the new command and runs it.

| Command | Does | Today |
| --- | --- | --- |
| `tuned install` | Clone Crafted at the pin, write profile stubs, create the user folder, `sync` | `setup-macos`, `setup-linux`, `setup-windows`, `crafted-install` |
| `tuned sync` | Install missing packages, generate autoloads, byte-compile, build trampolines | `autoloads`, `compile-lisp`, `trampolines`, `smoke` |
| `tuned doctor` | Explain what is wrong (below) | none |
| `tuned test [name]` | Run every suite, or `tests/tuned-NAME-tests.el` | `test`, `check` |
| `tuned upgrade` | Pull the distro, move Crafted to the new pin, `sync` | none |
| `tuned run` | Start Emacs with this profile | `try` |
| `tuned default` / `tuned restore-spacemacs` | Make it the default Emacs, or undo | `make-default`, `restore-spacemacs` |

The platform is detected; `install` runs the macOS, Linux or Windows steps
itself. The `just emacs-*` recipes stay as wrappers.

`tuned doctor` checks: the Emacs version and native compilation; `git`,
`curl`, `gpg` and the ELPA keyring; profile stubs pointing at this folder;
the user folder; stale `.elc` and autoloads; missing trampolines; package
drift against `packages.lock`; the last `startup-slow.log`. Each failure
prints one line saying what to run. `M-x tuned-doctor` shows the same report in
a buffer.

### Version

`VERSION` holds the version; `tuned-core.el` reads it into `tuned-version`. The
home buffer and `M-x tuned-version` show it, and `tuned doctor` prints it first.
`CHANGELOG.md` gets one section per version, written when the version
changes.

### Docs

- `README.md`: what it is, requirements, install in three commands, a table of
  the files in `lisp/` with one line each, links. Design detail moves out.
- `doc/manual/`: for users.
- `doc/design/`: today's design notes, unchanged in content.
- `doc/lisp-modules.md` stays as the one reference for the `lisp/` files.

## Naming

The `xl-` prefix was the author's initials, from when this was a personal
config. A distribution should carry its own name, so everything moves to
`tuned`:

| Kind | Today | After |
| --- | --- | --- |
| Lisp symbols (about 1250 distinct, 6450 uses) | `xl-git-status`, `xl-async-run` | `tuned-git-status`, `tuned-async-run` |
| Files in `lisp/` and `tests/` (42) | `xl-async.el`, `xl-async-tests.el` | `tuned-async.el`, `tuned-async-tests.el` |
| Features and `Package-Requires` | `(xl-async "0.1.0")` | `(tuned-async "0.1.0")` |
| Customize groups, options, faces (55) | `xl-dired`, `xl-git-status-heading` | `tuned-dired`, `tuned-git-status-heading` |
| Buffers | `*xl-native-compile*` | `*tuned-native-compile*` |
| State files | `xl-restart-place.eld`, `xl-eln-opt/` in the profile | `tuned-restart-place.eld`, `tuned-eln-opt/` |
| Environment | `XL_PROGRESS`, `XL_VERBOSE`, `XL_EXAMPLE`, ... | `TUNED_PROGRESS`, `TUNED_VERBOSE`, ... |
| Profile stub variables | `xl-dotfiles`, `xl-crafted-home` | `tuned-dotfiles`, `tuned-crafted-home` |
| Docs | `doc/xl-async.md` | `doc/tuned-async.md` |

Not renamed:

- The author's own functions use `xueliang-` and stay that way: they belong to
  a user folder, not to the distribution. The `xl-` symbols that `example/`
  owns (`xl-dotfiles-dir`, `xl-links-file`, `xl-macism`, `xl-idle-require`,
  `xl-git-status-window`) become `xueliang-`, and `example/xl-utils.el`
  becomes `example/xueliang-utils.el`.
- `archive/` outside `tuned-emacs/` is not touched.
- Ex commands (`:Gstatus`, `:Gwrite`, ...) and key bindings do not change.

How:

- Two commits. The first is only a script: `git mv` for the files, then a
  replacement of `xl-` and `XL_` wherever they are not preceded by a letter,
  digit or underscore, across `tuned-emacs/`, the `justfile` and `env.sh`.
  The script lives in the commit message so the commit can be checked by
  rerunning it. The second holds the hand-written changes (rewrapping,
  comment alignment, regenerated autoloads), so the scripted one stays
  reviewable. The profile stubs are then rewritten by `write_profile_stubs`.
- No obsolete aliases: the distribution has one user, whose config is renamed
  in the same commit. Aliases for 1250 symbols would be noise.
- No migration code. State that outlives a restart (`xl-eln-opt/` and
  `xl-restart-place.eld` in the profile, `xl-` names in `custom.el` and the
  savehist history) is renamed by hand on the author's machine, and
  `CHANGELOG.md` tells any other user what to rename. `tuned doctor`
  (phase 2) reports `xl-` names left in the profile.
- Delete every `.elc` under `lisp/` before the first start, or the old
  `xl-*.elc` files stay on `load-path` next to the new sources.
- Docstrings and comments get longer by three characters per name. Rewrap only
  lines that pass 80 columns; byte-compile warns about docstrings wider than
  80, which makes those easy to find.
- Check: `grep -rnP '(?<![A-Za-z0-9_])(xl-|XL_)' tuned-emacs justfile env.sh`
  prints nothing outside `CHANGELOG.md` and this document.

## Migration

Each phase is a few commits, each passing the tests. The author's daily setup
works after every commit.

1. **Rename `xl-` to `tuned-` (done).** As described in [Naming](#naming),
   before any other phase, so every new file is born with its final name and
   no later commit mixes a rename with a change.
2. **Packaging basics.** Add `VERSION`, `CHANGELOG.md`, `tuned-version`. Rename
   `frameworks.lock` to `crafted.lock`. Delete the three unused `doc/*.lock`.
   Add `bin/tuned` with today's subcommands under the new names, keeping
   `install/emacs-setup.sh` as a shim. Add `tuned doctor` and
   `lisp/tuned-doctor.el`.
3. **Thin entry files.** Move the body of `early-init.el` to `lisp/tuned-early.el`,
   and every Crafted call and override from `init.el` to `lisp/tuned-crafted.el`.
   Add `lisp/tuned-core.el`. `init.el` shrinks to the load order above.
4. **User folder.** Look the user folder up, add `templates/`, move
   `example/` to `users/xueliang/`, split the package list. `tuned install`
   migrates an existing `personal.el` with content into the new user folder
   and says so. Remove `TUNED_EXAMPLE` and `tuned-load-example`. The profile folder
   becomes `~/.emacs-profiles/tuned-emacs`, with the old `crafted` path kept
   as a symlink for one version.
5. **Generated files out of the repo.** Autoloads move into the profile;
   `lisp/tuned-autoloads.el` leaves git.
6. **Docs.** Split into `doc/manual/` and `doc/design/`, slim the README,
   write `CHANGELOG.md` for 1.0.
7. **Optional: CI.** A GitHub Actions job running `tuned test` on macOS and
   Linux with Emacs 30 and 31.

## Decisions to confirm

- **User folder location.** Proposed: XDG first, `~/.tuned-emacs.d/` second.
  The alternative, keeping `personal.el` inside the repo, is simpler but means
  every user has a dirty checkout.
- **Generated autoloads in the profile.** Proposed: yes. The alternative is to
  keep committing `lisp/tuned-autoloads.el`, which avoids the regeneration check.

## Risks

- Paths are written in many places: profile stubs (`write_profile_stubs`),
  the compile step, the test runner, the eln-opt trusted roots, and a few
  docs. Phase 4 changes the example's path and the profile folder, so they
  must be updated together, and `tuned doctor` should catch a stub that still
  points at an old path.
- The rename touches all 53 files that mention `xl-`. Do it when no other
  branch or agent has work in flight on `tuned-emacs/`, or every open change
  conflicts.
- Windows: the user folder lookup and the symlink in phase 4 need the
  Windows branch of the script (`doc/windows.md`).
- `lisp/` keeps growing flat. The README's file table, grouped by role
  (startup, async base, Git, editing features, profiling), is what keeps it
  readable; each new file gets a row in the same commit.

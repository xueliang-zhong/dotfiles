# Startup

This document covers what happens when Emacs starts: the files involved, the order they load in, how packages are installed, and the startup settings that were chosen and why.

## Size of the personal layer

| File | Lines | Role |
| --- | ---: | --- |
| `early-init.el` | 141 | Startup settings, package archives |
| `init.el` | 234 | Crafted modules, package installation, load order |
| `xueliang.el` | 493 | Personal settings |
| `lisp/xl-key-bindings.el` | 423 | Spacemacs-style key bindings |

Counts are from `wc -l`.

## The profile stubs

Emacs starts from `~/.emacs-profiles/crafted`, which holds two stub files written by `install/emacs-setup.sh`. Each stub defines where the dotfiles and Crafted live, then loads the real file:

```
~/.emacs-profiles/crafted/early-init.el  ->  tuned-emacs/early-init.el
~/.emacs-profiles/crafted/init.el        ->  tuned-emacs/init.el
```

`~/.emacs.d` is a symbolic link to `~/.emacs-profiles/crafted` on macOS and Linux. On Windows it is a directory holding copies of the two stubs, because Git Bash's `ln -s` copies instead of linking.

## Load order

1. Emacs reads the stub `early-init.el`, which loads `tuned-emacs/early-init.el`.
2. `tuned-emacs/early-init.el` turns off the GC during startup, hides the menu, tool and scroll bars, sets the package archives and priorities, and loads nothing heavy.
3. Emacs runs `before-init-hook`. Crafted checks whether its package list is empty and refreshes if it is.
4. Emacs reads the stub `init.el`, which loads `tuned-emacs/init.el`.
5. `init.el` checks the Emacs version, loads `custom.el` (where Customize writes), and loads `crafted-init-config`, which puts the Crafted module folder on the load path.
6. `init.el` collects the packages to install: Crafted's completion, evil and Org packages, plus `xl-packages`. Missing packages are installed (see "Package installation").
7. Crafted's defaults, completion, evil, Org and startup modules load. Completion covers vertico, consult, marginalia, orderless, embark, corfu and cape.
8. `init.el` restores a few Crafted defaults that conflict with this setup: `C-h` stays the help prefix in insert state, spell checking is off globally, and `consult-preview-at-point-mode` is removed from `embark-collect-mode-hook`, where Consult 3.4 no longer defines it.
9. `tuned-emacs/lisp/` is added to the load path. `xl-key-bindings`, `xl-git-status`, `xl-git-commands` and the other modules load with `require`.
10. `xueliang.el` loads last, so its bindings win over the defaults.
11. Startup finishes and the Crafted home buffer appears. If the session started from `SPC q r`, the previous file or dired listing is restored instead.

Settings for Org, dired, magit and others go inside `with-eval-after-load`, so they run when the package is first used and do not slow startup.

## Package installation

`init.el` installs missing packages before any module needs them. The steps are:

1. If any selected package is missing, refresh the archives with `package-refresh-contents`. This is the only network access on a normal start, and it happens only when a package is missing. The configuration turns off automatic refresh at startup, and `package-install-selected-packages` refreshes only when the cache is empty, so an old cache could report a new dependency as unavailable. Refreshing whenever something is missing avoids that.
2. **Parallel fetch** (`lisp/xl-package-fetch.el`). The missing packages and their dependencies are planned with `package-compute-transaction`. Each archive file and its `.sig` are downloaded with `curl`, up to twice the CPU count at once. The signatures are then checked with `gpg --verify`, also in parallel. Each package that passes is unpacked from its local file with `package-install-file`, in dependency order.
3. Packages without a signature (HTTP 404) are accepted only when `package.el` would accept unsigned archives. A signature that fails, or a download that fails, leaves that package out.
4. **Serial fallback** (`xl-install-packages`). Anything still missing runs through `package-install`, one package at a time.
5. If packages are still missing, the keyring is rebuilt and the archives are refreshed once more. If that fails too, startup reports the missing packages and, if none of the archives offers them, says so. If gpg is unusable, signature checks are turned off for that one startup.

Packages are copied as source. `package.el` would byte-compile each package during installation; both install paths suppress that with a `cl-letf` around `package--compile`, because the compilation is slow and eln-opt does it in the background (see [eln-opt.md](eln-opt.md)). After a fresh install there are no `.elc` files in `elpa-31.1`; they appear as eln-opt works.

Built-in libraries such as `lua-mode` are not installed from the archive, because `package-installed-p` already reports them as installed. Emacs ships them with byte-compiled files, so there is nothing to copy.

The progress messages look like this:

```
refreshing package archives ... (this may take a few seconds)
fetching packages in parallel -j 16 ...
fetched 38 package(s) in parallel
```

### Measured install time

On an 8-core Mac, starting from empty `elpa` directories and a warm network:

| Path | Wall time |
| --- | ---: |
| Serial (`package-install` per package, no byte-compile) | 42.0 s |
| Parallel fetch | 15.5 to 17.7 s |

Both runs installed the same 38 packages with the same `.signed` markers. The archive refresh takes about 6.5 s and is serial in both. These numbers are from one machine and one network; rerun the measurement on your own machine before relying on them.

Each gpg check takes roughly 0.3 to 0.8 s and is the largest per-package cost. Signature checks are not disabled for speed.

## Startup settings

These are in `early-init.el`:

- **GC**: the threshold is raised during startup and set back to 128 MB on `emacs-startup-hook`.
- **Frame chrome**: the menu, tool and scroll bars are off before the first frame is drawn.
- **Native compilation**: JIT compilation and package native compilation are off. Emacs's own core Lisp is native. Packages run as byte code and, after eln-opt runs, as native code. Advice on C primitives needs subr trampolines; `trampolines` builds them by hand.
- **Package directories** are per Emacs version (`elpa-31.1`), so byte or native output never mixes between versions.
- **evil** and its companions come from MELPA. The NonGNU release 1.15.0 fails on Emacs 30 and later.
- **`crafted-early-init-config`** is not loaded. It loads 37 more files (`url`, `auth-source`, `json`, `eieio`, `cus-edit` and others) on every start. The package archives and priorities it sets are written out directly in `early-init.el` instead.
- **ffap** does not run the compiler at startup. The C header paths that ffap computes at load time run `gcc`, `clang` and `g++`; on macOS these are Xcode shims that occasionally stall. Two variables, `ffap-c-path` and `ffap-c++-path`, are predefined to `/usr/include` and `/usr/local/include`, so the `defvar` initialisers are skipped. The cost is that `#include <...>` lookups search only those two directories.
- **Slow-start record.** Each finished load is timed. If startup takes longer than `xl-slow-start-seconds` (3 s), the slowest stretches, recent `.eln` files, our own files loaded from source and the full load order are written to `~/.emacs.d/startup-slow.log`. Fast starts write nothing.

## Why the order matters

- Packages install before any module or personal file loads, because they need them.
- Crafted modules load before `xl-key-bindings.el`, because key bindings need evil to be loaded.
- `xl-key-bindings.el` loads before `xueliang.el`, which adds personal bindings to the leader map. When two bindings collide, the later file wins.
- `evil-collection-key-blacklist` is set before the Crafted modules load, so evil-collection does not take `SPC` or `,`.

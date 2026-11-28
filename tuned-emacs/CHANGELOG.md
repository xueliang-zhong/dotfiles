# Changelog

## Unreleased

- The home screen is now tuned-emacs's own, `lisp/tuned-home.el`, in place of
  Crafted's splash screen. It keeps the layout (logo, greeting, recent files
  opened with `1` to `9`, quick links, startup time) and shows the new logo,
  `assets/tuned-emacs-logo.svg`: hexagram 35, Jin, the sun rising over the
  earth. The buffer is `*Tuned Emacs*`; `SPC b h` and `<f10>` call
  `tuned-home`, and `crafted-startup-config` is no longer loaded.

## 0.1.0

Packaging basics:

- `VERSION` holds `0.1.0`; `M-x tuned-version` shows it.
- `bin/tuned` is the CLI: `install`, `sync`, `doctor`, `test`, `upgrade`,
  `run`, `default` and `restore-spacemacs`. `install/emacs-setup.sh` remains
  as a compatibility shim for one version.
- `tuned doctor` and `M-x tuned-doctor` report the Emacs build, programs, the
  ELPA keyring, profile stubs, stale byte code and autoloads, trampolines,
  `packages.lock` drift and the last slow start.
- `tuned sync` writes `packages.lock` in the profile.
- `frameworks.lock` is now `crafted.lock`; the three unused `doc/*.lock` files
  are gone.
- The Perfetto modules shortened their prefix: `tuned-perfetto-` is now
  `tuned-perf-`, with `tuned-perf-trace.el` and `tuned-perf-probes.el`.

Renamed the `xl-` code prefix to `tuned-` and the `XL_` environment prefix to
`TUNED_`. Symbols, features, files, buffers and state files follow the new
name; behaviour, key bindings and Ex commands are unchanged. The author's own
names in `example/` stay `xueliang-`, and `example/xl-utils.el` is now
`example/xueliang-utils.el`.

Environment variables:

| Old name | New name |
| --- | --- |
| `XL_PROGRESS` | `TUNED_PROGRESS` |
| `XL_VERBOSE` | `TUNED_VERBOSE` |
| `XL_EXAMPLE` | `TUNED_EXAMPLE` |
| `XL_ENV` | `TUNED_ENV` |
| `XL_SKIP_DOTFILES` | `TUNED_SKIP_DOTFILES` |
| `XL_FORCE_WINDOWS` | `TUNED_FORCE_WINDOWS` |

Profile variables:

| Old name | New name |
| --- | --- |
| `xl-dotfiles` | `tuned-dotfiles` |
| `xl-crafted-home` | `tuned-crafted-home` |
| `xl-load-example` | `tuned-load-example` |

State in the profile follows the same rename: `xl-eln-opt/` is now
`tuned-eln-opt/`, and `xl-restart-place.eld` is now `tuned-restart-place.eld`.
Rename them by hand before the first start.

Delete every `.elc` under `tuned-emacs/` after upgrading. The old `xl-*.elc`
files would otherwise stay on `load-path` next to the new sources.

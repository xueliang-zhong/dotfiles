# Changelog

## Unreleased

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

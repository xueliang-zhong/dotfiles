# Design: Switch minibuffer completion to ivy & counsel

Date: 2026-11-15
Scope: `emacs/crafted/init.el`, `emacs/xueliang.el`

## Goal

Replace the vertico/consult/marginalia/orderless/embark/cape/corfu stack with the
ivy family: `ivy`, `counsel`, `ivy-rich`, `ivy-prescient`. The old packages are
no longer installed at all.

## Changes

1. **`emacs/xueliang.el`** (framework-neutral, stays that way):
   - Add `ivy counsel ivy-rich ivy-prescient` to `xl-packages`.
   - Replace the vertico config block with an ivy block: `(require 'counsel)`,
     turn off `fido-mode`/`icomplete-mode` (previously disabled by
     `crafted-completion-config`, which is going away), enable
     `ivy-mode`, `counsel-mode`, `ivy-rich-mode`, `ivy-prescient-mode` +
     `prescient-persist-mode`, keep the `C-k` = `kill-line` minibuffer habit.
   - Add `xl-M-x` intent alias (`counsel-M-x` when present) for `SPC x` and `s-x`,
     following the existing `xl-pick` pattern.
2. **`emacs/crafted/init.el`**:
   - Drop `(require 'crafted-completion-packages)` and
     `(require 'crafted-completion-config)`.

## What stays untouched

All `xl-pick` fallback aliases already resolve to counsel automatically once
consult is absent (`xl-recent-file` → `counsel-recentf`, `xl-history` →
`counsel-esh-history`, `SPC /` → `swiper`, `<f10>` inserts the `counsel-`
prefix). The `justfile` install/try recipes are unchanged.

## Local cleanup (one-off)

Crafted runs `package-initialize` in `before-init-hook`, which activates every
package directory under `~/.emacs-profiles/crafted/elpa-*` regardless of
`package-selected-packages` — stale vertico-family autoloads would win the
`xl-pick` fallback. The stale `vertico consult marginalia orderless embark
embark-consult corfu corfu-terminal cape` directories were removed from both
`elpa-29.4` and `elpa-31.1`. Fresh machines never install them in the first
place, so no repo-side cleanup logic is needed.

## Verification

`just emacs-try crafted`, then: package install completes without
vertico/consult; `SPC SPC` (ivy switch buffer), `SPC f r` (counsel-recentf with
rich annotations), `SPC /` (swiper), `SPC x` (counsel-M-x).

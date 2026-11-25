;;; eln-opt-baseline.eld --- default eln-opt roots for a fresh machine -*- lexical-binding: t; -*-
;; Function names only: no code, no paths, no personal data.  Each name is a
;; root at weight 1.0, and the weight falls as the user's own savehist and
;; recentf data grows (see eln-opt.el, `eln-opt-rank-baseline-users').
;; Chosen from functions on the per-keystroke and redisplay paths of the
;; packages this profile uses.
(:functions
 (evil--jump-hook
  winum--update
  winner-save-old-configurations
  evil-normalize-repeat-info
  evil-escape-pre-command-hook
  corfu--post-command
  cape--nonessential-table
  consult--async-refresh
  vertico--exhibit
  orderless-filter))

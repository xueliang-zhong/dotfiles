;; under dotspacemacs/layers ()
dotspacemacs-additional-packages
'(
  magit
  evil-magit
  fiplr
  ivy-rich
)

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
   This function is called at the very end of Spacemacs initialization after layers configuration.
   This is the place where most of your configurations should be done.
   Unless it is explicitly specified that a variable should be set before a package is loaded,you should place your code here."
  (define-key evil-normal-state-map (kbd "<f4>") 'kill-buffer-and-window)
  (set-default 'truncate-lines t)
  (when (string-equal system-type "gnu/linux") (set-default-font "Monospace"))
  (set-face-attribute 'default nil :height 130)

  ;; Evil Mode everywhere.
  (setq evil-emacs-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)
  (setq evil-shift-width 2)
  (setq evil-mode-line-format 'before)

  ;; leader key settings
  (spacemacs/set-leader-keys "gg" 'magit-status)
  (spacemacs/set-leader-keys "gd" 'magit-diff-working-tree)

  ;; ivy config
  ;; number of result lines to display
  (setq ivy-fixed-height-minibuffer t)
  (setq ivy-height 20)
  ;; enable more stuff
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers nil)
  ;; TAB behaves as ivy-partial-or-next-line
  (define-key ivy-mode-map (kbd "TAB") '(lambda() (interactive) (ivy-partial) (ivy-next-line)))
  ;; I don't like the default "^" for M-x command.
  (add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . ""))
  ;; Make sure C-a C-k work in ivy mode as well.
  (define-key ivy-mode-map (kbd "C-k") 'evil-delete-line)
  ;; Make counsel-grep case insensitive.
  (setq counsel-grep-base-command "grep -inE '%s' %s")
  ;; Better ivy switch buffer.
  (ivy-rich-mode 1)

  ;; global settings

  ;; modern style 'paste' in evil insert mode.
  (define-key evil-insert-state-map (kbd "C-v") 'yank)

  ;; F1..F12 key settings.
  ;; <f1> .. <f4> :
  (global-set-key (kbd "<f4>")  'kill-buffer-and-window)
  ;; <f5> .. <f8> :
  ;; code development related: debug/test, program structure, build.
  (global-set-key (kbd "<f5>")   'xueliang-linaro-gdb)
  (global-set-key (kbd "<f6>")   'xueliang-eshell-pwd)
  (global-set-key (kbd "C-<f6>") 'xueliang-terminal-shell)
  (global-set-key (kbd "<f7>")   'xueliang-linaro-art-gtest-host)
  (global-set-key (kbd "C-<f7>") 'xueliang-make-android-system-image)
  ;; Magic key <f8>
  ;; <f8> shows program/output/content structure in various languages.
  (add-hook 'c-mode-hook '(lambda () (define-key
                                       evil-normal-state-local-map (kbd "<f8>")
                                       '(lambda() (interactive) (swiper "void visit ([a-z]* ")))))
  (add-hook 'c++-mode-hook '(lambda () (define-key
                                         evil-normal-state-local-map (kbd "<f8>")
                                         '(lambda() (interactive) (swiper "void visit ([a-z]* ")))))
  (add-hook 'emacs-lisp-mode-hook '(lambda () (define-key
                                                evil-normal-state-local-map (kbd "<f8>")
                                                '(lambda() (interactive) (swiper "(defun =[=]* ")))))
  (add-hook 'magit-diff-mode-hook '(lambda () (define-key evil-normal-state-local-map
                                                (kbd "<f8>")
                                                '(lambda() (interactive) (swiper "modified ")))))
  (add-hook 'diff-mode-hook '(lambda () (define-key evil-normal-state-local-map
                                          (kbd "<f8>")
                                          '(lambda() (interactive) (swiper "^diff ")))))
  ;; for my repo-sync terminal
  (add-hook 'term-mode-hook '(lambda () (define-key evil-normal-state-local-map
                                          (kbd "<f8>")
                                          '(lambda() (interactive) (swiper "Fetching projects ")))))
  (add-hook 'markdown-mode-hook '(lambda () (define-key evil-normal-state-local-map
                                              (kbd "<f8>")
                                              '(lambda() (interactive) (swiper "^#")))))
  ;; for selecting date easily in my daily.org
  (add-hook 'org-mode-hook '(lambda () (define-key evil-normal-state-local-map
                                         (kbd "<f8>")
                                         '(lambda() (interactive)
                                            (org-shifttab)
                                            (evil-goto-first-line)
                                            ;;(helm-swoop :$query
                                            (swiper
                                             (format-time-string "<%Y-%m-%d" (current-time)))
                                            (org-sort-entries t ?o)
                                            (org-cycle) (org-cycle)
                                            ))))
                                        ; <f9> .. <f12>:
  (global-set-key (kbd "<f9>")  '(lambda() (interactive) (xueliang-cd-current-buffer-directory) (counsel-find-file)))
  (global-set-key (kbd "<C-f9>")  'xueliang-find-file-similar)
  (global-set-key (kbd "<f10>") 'ace-window)
  (global-set-key (kbd "<f11>") 'helm-google)
  (global-set-key (kbd "<C-f11>") '(lambda() (interactive) (org-open-link-from-string "https://google.co.uk")))
  (global-set-key (kbd "<f12>") 'xueliang-open-link)
)

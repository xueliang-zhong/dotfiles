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
  (linum-mode 1)
  (ivy-rich-mode 1)
  (when (string-equal system-type "gnu/linux") (set-default-font "Monospace"))
  (set-face-attribute 'default nil :height 130)
)

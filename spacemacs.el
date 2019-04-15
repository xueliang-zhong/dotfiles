;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are 25.2.2 (spacemacs) `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'nil
   ;; If non-nil then Spacemacs will~/.spacemacs ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     emacs-lisp
     git
     ivy
     markdown
     nlinum
     org
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     ;; smex
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     anti-zenburn-theme
     fiplr
     graphviz-dot-mode
     helm-google
     ivy-rich
     json-mode
    )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(yasnippet)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 10)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         spacemacs-dark
                         spacemacs-light  ;; works great with redshift.
                         anti-zenburn
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Monospace"
                               :size 17
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 5
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.3
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (when (string-equal system-type "gnu/linux") (setq xueliang-home "~"))
  (when (string-equal system-type "windows-nt")
    (when (file-exists-p "c:/Users/xuezho01") (setq xueliang-home "c:/Users/xuezho01"))
    (when (file-exists-p "c:/Users/xueliang") (setq xueliang-home "c:/Users/xueliang")))
  (setq dot-files               (concat xueliang-home "/workspace/dotfiles"))
  (setq dropbox-home            (concat xueliang-home "/Dropbox"))
  (setq android-root            (concat xueliang-home "/workspace/linaro"))
  (setq android-xueliang_test   (concat android-root "/xueliang_test"))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
   This function is called at the very end of Spacemacs initialization after layers configuration.
   This is the place where most of your configurations should be done.
   Unless it is explicitly specified that a variable should be set before a package is loaded,you should place your code here."

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; evil settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq evil-emacs-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)
  (setq evil-shift-width 2)
  (setq evil-mode-line-format 'before)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; leader key settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (spacemacs/set-leader-keys "SPC" 'ivy-switch-buffer)
  (spacemacs/set-leader-keys "*" 'xueliang-search-in-project)
  (spacemacs/set-leader-keys "#" 'xueliang-search-in-project)
  (spacemacs/set-leader-keys "gg" 'magit-status)
  (spacemacs/set-leader-keys "pf" 'xueliang-projectile-find-file)
  (spacemacs/set-leader-keys "ff" 'xueliang-projectile-find-file)
  (spacemacs/set-leader-keys "wg" 'golden-ratio)  ;; leader -> windows -> golden-ration.

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ivy config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; number of result lines to display
  (setq ivy-fixed-height-minibuffer t)
  (setq ivy-height 24)
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Helm config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (require 'helm)
  (setq helm-show-completion-display-function #'helm-show-completion-default-display-function)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Modeline config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq display-time-day-and-date 1)
  (display-time-mode 1)

  ;; Org-mode configs/settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq org-todo-keywords '((sequence "TODO" "FOCUS" "PROG" "DONE")))
  ;; Don't add a time stamp line to the 'DONE' task.
  (setq org-log-done nil)
  (add-hook 'org-mode-hook (lambda ()
    (set-face-attribute 'org-level-1 nil :bold nil :height 1.0)
    (set-face-attribute 'org-level-2 nil :bold nil :height 1.0)
    (set-face-attribute 'org-level-3 nil :bold nil :height 1.0)
    (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("◉" "○" "✿" "✼"))
  (add-hook 'org-mode-hook '(lambda () (define-key evil-normal-state-map (kbd "C-c C-o") 'org-open-at-point)))
  (add-hook 'org-mode-hook '(lambda () (define-key evil-insert-state-map (kbd "M-RET M-RET") 'org-ctrl-c-ret)))
  (add-hook 'org-mode-hook '(lambda () (define-key evil-normal-state-map (kbd "M-RET M-RET") 'org-ctrl-c-ret)))
  ;; Make sure org mode opens link with Google Chrome on Linux.
  (when (string-equal system-type "gnu/linux")
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "google-chrome"))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; magit settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; For small screens: keep magit status window always on the rigth side.
  (when (< (display-pixel-height) 1080)
    (setq split-height-threshold nil
          split-width-threshold  0))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; projectile settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq projectile-require-project-root nil)
  (setq projectile-enable-caching nil)
  (setq projectile-project-search-path '("~/workspace/linaro"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; auto completion / company settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq company-backends
        '((company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf
                        company-files company-dabbrev-code company-gtags company-etags company-keywords company-oddmuse company-dabbrev)))
  (setq company-idle-delay 0)
  (global-company-mode)
  (add-hook 'eshell-mode-hook '(lambda () (setq-local company-idle-delay 60)))
  (add-hook 'gdb-mode-hook '(lambda () (setq-local company-idle-delay 60)))
  (setq company-tooltip-minimum 9)
  (setq company-tooltip-limit 9)
  (setq company-tooltip-minimum-width 33)
  (setq company-minimum-prefix-length 2)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; eshell settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "C-a") 'eshell-bol)))
  (add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "C-r") 'helm-eshell-history)))
  ;; eshell will run a term session to support following complex commands
  (add-hook 'eshell-mode-hook '(lambda () (add-to-list 'eshell-visual-commands "htop")))
  (add-hook 'eshell-mode-hook '(lambda () (add-to-list 'eshell-visual-commands "top")))
  (add-hook 'eshell-mode-hook '(lambda () (add-to-list 'eshell-visual-commands "watch")))
  ;; TAB to complete command in eshell
  (add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "TAB")   'completion-at-point)))
  ;; Ctrl-d to quit the shell, just like other terminals
  (add-hook 'eshell-mode-hook '(lambda () (define-key evil-insert-state-local-map (kbd "C-d")   'kill-buffer-and-window)))
  ;; Helm eshell history colours
  (require 'helm-elisp)
  (set-face-background 'helm-lisp-show-completion "LightSteelBlue1")
  (set-face-foreground 'helm-lisp-show-completion "black")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; my functions.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (when (file-exists-p dropbox-home)
     (add-to-list 'load-path (concat dropbox-home "/emacs")) (require 'xzhong) (require 'xzhong-trader))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; theme settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (when (string-equal system-type "windows-nt") (load-theme 'spacemacs-dark t) (set-default-font "Consolas") (set-face-attribute 'default nil :height 130))
  ;; workstation
  ;; (when (> (display-pixel-height) 1080) (load-theme 'anti-zenburn t) (xueliang-anti-zenburn-theme-colors))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; global settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq counsel-grep-swiper-limit 7000000)
  (define-key evil-normal-state-map (kbd "/") 'counsel-grep-or-swiper)
  (define-key evil-normal-state-map (kbd "*") '(lambda() (interactive) (swiper (thing-at-point 'symbol))))
  ;; modern style 'paste' in evil insert mode.
  (define-key evil-insert-state-map (kbd "C-v") 'yank)
  ;; nowrap
  (set-default 'truncate-lines t)
  ;; line numbers
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (setq google-translate-default-target-language "zh-CN")
  ;; scratch buffer to be elisp mode by default.
  (setq dotspacemacs-scratch-mode 'emacs-lisp-mode)
  ;; start screen
  (kill-buffer "*spacemacs*")
  (spacemacs/toggle-highlight-current-line-globally-off)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; F1..F12 key settings.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <f1> .. <f4> :
  (global-set-key (kbd "<f3>")      'xueliang-faster-fundamental-mode)
  (global-set-key (kbd "<f4>")      'spacemacs/delete-window)  ;; avoid accidentally stopping some important task
  (global-set-key (kbd "C-<f4>")    'kill-buffer-and-window)
  (global-set-key (kbd "S-<f4>")    'xueliang-select-close-window-and-kill-buffer)
  (global-set-key (kbd "C-S-<f4>")  'xueliang-select-close-window-and-kill-buffer)

  ;; <f5> .. <f8> :
  ;; code development related: debug/test, shell commands, program structure, build.
  (global-set-key (kbd "<f5>")   'xueliang-eshell-pwd)
  (global-set-key (kbd "C-<f5>") 'xueliang-terminal-shell)

  (global-set-key (kbd "<f6>")      'counsel-yank-pop)

  (global-set-key (kbd "<f7>")   'xueliang-art-test-target-optimizing)
  (global-set-key (kbd "C-<f7>") 'xueliang-make-android-system-image)

  ;; Magic key <f8>
  ;; <f8> shows insights: program/output/content structure in various languages.
  (global-set-key (kbd "<f8>")   'counsel-imenu)

  ;; for my repo-sync terminal
  (add-hook 'eshell-mode-hook '(lambda () (define-key evil-normal-state-local-map
                                          (kbd "<f8>")
                                          '(lambda() (interactive) (swiper "Fetching projects ")))))

  ;; for jumping to current day tasks easily in my daily.org
  (defun xueliang-org-find-today () (interactive)
         (org-shifttab)
         (evil-goto-first-line)
         (swiper (format-time-string "<%Y-%m-%d" (current-time)))
         (org-cycle))
  (defun xueliang-set-org-f8-key () (interactive)
         (if (and (stringp buffer-file-name) (string-match "daily.org" buffer-file-name))
             (progn
               (define-key evil-normal-state-local-map (kbd "<f8>") 'xueliang-org-find-today)
               (define-key evil-normal-state-local-map (kbd "<C-f8>") '(lambda() (interactive) (org-cycle) (org-cycle))))
          ;; else, for all other org files, imenu is very useful.
          (define-key evil-normal-state-local-map (kbd "<f8>") 'counsel-imenu)))
  (add-hook 'find-file-hook 'xueliang-set-org-f8-key)

  ;; <f9> .. <f12>:
  (global-set-key (kbd "<f9>")  '(lambda() (interactive) (xueliang-cd-current-buffer-directory) (counsel-find-file)))
  (global-set-key (kbd "<C-f9>")  'xueliang-find-file-similar)
  (global-set-key (kbd "<f10>") 'xueliang-file-manager)
  (global-set-key (kbd "<f11>") 'helm-google)
  (global-set-key (kbd "<C-f11>") '(lambda() (interactive) (org-open-link-from-string "https://google.co.uk")))
  (global-set-key (kbd "<f12>") 'xueliang-open-link)
  (global-set-key (kbd "<C-f12>") 'xueliang-f12-trade-function)
)

(defun =============xueliang-functions=============())

(defun xueliang-google-translate-region () (interactive)
       (org-open-link-from-string (concat "https://translate.google.co.uk/#en/zh-CN/"
                                          (replace-regexp-in-string "[/ \n()]" "%20"            ;; replace some special character with space.
                                                                    (buffer-substring (region-beginning) (region-end)) ;; translate current selection/region in buffer.
                                                                    ))))

(defun xueliang-sum-numbers-in-region (start end)
  (interactive "r")
  (message "Sum: %s" (cl-reduce #'+ (split-string (buffer-substring start end)) :key #'string-to-number)))

(defun xueliang-projectile-find-file ()
   "projectile-find-file if it's in a project, find-file otherwise" (interactive)
   (if (projectile-project-root) (counsel-projectile-find-file)  ;; use projectile-find-file by default
         (if (string-equal system-type "windows-nt")             ;; otherwise
             (counsel-find-file)                                 ;;   on windows, use less powerful find-file.
             (require 'fiplr) (xueliang/find-file (fiplr-root) "")))              ;;   on linux, use my own find file implementation.
)

(defun xueliang-search-in-project (argument)
  "search in project using ag; use fiplr to goto the root dir of the project"
  (interactive "P")
  (xueliang-cd-current-buffer-directory)
  (require 'fiplr) (cd (fiplr-root))
  (counsel-ag (thing-at-point 'symbol)) ;; counsel-ag allows initial input to play with.
)

(defun xueliang-flush-blank-lines (start end)
  "Remove blank lines in selected lines." (interactive "r")
  (flush-lines "^\\s-*$" start end nil))

(defun xueliang-open-scratch-buffer ()
  "open switch buffer quickly" (interactive)
  (switch-to-buffer-other-window "*scratch*")
  (evil-goto-line) (evil-append-line 1)
  (when (> (buffer-size) 1) (insert "\n"))
  (insert ";; This buffer is for text that is not saved, and for Lisp evaluation.")
  (insert "\n\n")
  )

(defun xueliang-untabify-replaced-all-TAB() (interactive)
   "easily replace all TAB in current buffer with spaces."
   (untabify (point-min) (point-max)))

(defun xueliang-what-face (pos)
    (interactive "d")
        (let ((face (or (get-char-property (point) 'read-face-name)
                        (get-char-property (point) 'face))
                    ))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun xueliang-switch-project()
  "switch among my projects" (interactive)
  (find-file (ivy-read "Project: " xueliang-project-list))
  (counsel-projectile-find-file)
)

(defun xueliang-find-file ()
  "my fast find file in project" (interactive)
  (counsel-projectile-find-file)
)

(defun xueliang-find-file-from-pwd ()
  (xueliang/find-file "." ""))

(defun xueliang-find-file-similar ()
  "my fast find file with similar file names, e.g. switch between .cc, .h, _test.cc files" (interactive)
  (xueliang-cd-current-buffer-directory)
  (xueliang/find-file "." (string-remove-suffix "_test"  ;; remove the test suffix for _test.cc files.
                                                (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))

(defun xueliang/find-file (path init-input)
  "my fast find file in project"
  (find-file (ivy-read (concat "Find File " path ": ")
                             (split-string (shell-command-to-string
                                            (concat "ag " path " -l --nocolor -g \"\" "))  ;; faster than find command.
                                           "\n")
                             :initial-input init-input)))

(defun xueliang-file-manager () "Open file manager (Thunar/Nemo) on current folder" (interactive)
   (start-process "my-file-manager" shell-output-buffer-name "nemo" "."))

;;(defun xueliang-top() "my top command in emacs" (interactive) (ivy-read "Top: " (split-string (shell-command-to-string "top -b -n 1 | tail -n +6") "\n")))
(defalias 'xueliang-top 'helm-top)

;; avoid company-complete being annoying in gdb mode.
(add-hook 'gdb-mode-hook '(lambda () (setq-local company-idle-delay 60)))

(defalias 'xueliang-comment-code 'comment-box)

(defun xueliang-cnext-compilation-error ()
  "get compilation error easily in current buffer" (interactive)
  (compilation-mode)
  (compilation-next-error 1))

(defun xueliang-cprev-compilation-error ()
  "get compilation error easily in current buffer" (interactive)
  (compilation-mode)
  (compilation-previous-error 1))

; Help to make code reviews easier; requires cpplint.py in $PATH.
(defun xueliang-art-cpplint ()
  "invokes AOSP/art/tools/cpplint.py on current buffer" (interactive)
  (setq-local cpplint-cmd-and-options
              (concat android-root
                      "/external/google-styleguide/cpplint/cpplint.py "
                      "--filter=-whitespace/line_length,-build/include "))
  (shell-command (concat cpplint-cmd-and-options (buffer-file-name)))
  (switch-to-buffer-other-window "*Shell Command Output*")
  (evil-window-move-very-bottom) (compilation-mode))

(defun xueliang-art-weekly-report ()
  "Help me write ART weekly report easier." (interactive)
  (find-file (concat dropbox-home "/weekly/"
                     (format-time-string "art_weekly_%d_%m_%Y.md" (current-time))))
)

; Helper to cd to directory of current buffer/file.
(defun xueliang-cd-current-buffer-directory ()
  "cd to directory of current buffer/file." (interactive)
  (when buffer-file-name
    (cd (file-name-directory buffer-file-name))
    (message "pwd: %s" (file-name-directory buffer-file-name))))

;; this is for easier code reading and help drawing flowgraph of the coode (dot graph)
(defun xueliang-send-current-line-to-scratch ()
   "send current line to scratch for further edit." (interactive)
   (kill-ring-save (point) (line-end-position))
   (switch-to-buffer-other-window "*scratch*") (yank) (insert "\n") (evil-window-next 1))

; invoke e-shell
(defun xueliang-create-eshell-or-switch-to-existing ()
   (when (buffer-file-name)
     (xueliang-cd-current-buffer-directory)
     (if (get-buffer-window "*eshell*")
       (switch-to-buffer-other-window "*eshell*")
       ;; else
       (split-window-below) (evil-window-move-very-bottom) (eshell)))
   (evil-goto-line) (evil-append-line 1))

(defun xueliang-eshell-current-line ()
   "invokes eshell in a split window, send current line to eshell." (interactive)
   (kill-ring-save (point) (line-end-position))
   (xueliang-create-eshell-or-switch-to-existing) (yank))

(defun xueliang-eshell ()
   "invokes eshell in a split window." (interactive)
   (xueliang-create-eshell-or-switch-to-existing))

(setq-default eshell-buffer-number 0)
(defun xueliang-eshell-pwd ()
   "invokes a new eshell in a split window, shell starts in the root of current project." (interactive)
   (xueliang-cd-current-buffer-directory)
   (setq eshell-buffer-number (% (+ eshell-buffer-number 1) 1000))  ;; eshell number 0-99.
   (split-window-below) (evil-window-move-very-bottom) (eshell eshell-buffer-number)
   (evil-goto-line) (evil-append-line 1)
   ;; Better eshell colours in dark themes.
   (set-face-foreground 'term-color-black "grey42")
)

(defun xueliang-htop-cpu () (interactive) (xueliang-eshell-quick-command "htop"))

;; make it easier for me to remember & type some commands.
(defalias 'xueliang-spell-check-on-the-fly-check-mode 'flyspell-mode)
(defalias 'xueliang-eval-region 'eval-region)

(defun xueliang-highlight-current-word ()
  "makes highlight-regexp easier" (interactive)
  (highlight-regexp (thing-at-point 'word)))

(defun xueliang-pwd-string ()
  "get pwd string of current file-buffer easily."
  (interactive) (kill-new (ivy-read "PWD: " (list (buffer-file-name)))))

(defun xueliang-terminal-shell ()
  "start my terminal, e.g. gnome-terminal." (interactive)
  (start-process "my-shell" shell-output-buffer-name "~/bin/terminal" "--drop-down")
)

(defun xueliang-eshell-quick-command (str &optional exit-eshell-after-command)
  (xueliang-eshell-pwd)
  (insert str) (eshell-send-input)
  (when exit-eshell-after-command (eshell/x))
)

(setq xueliang-switch-mode-counter 0)
(defun xueliang-faster-fundamental-mode()
  "Quickly swich to fundamental mode, useful when editing huge size buffers."
  (interactive)
  (if (= (% xueliang-switch-mode-counter 2) 0) (fundamental-mode) (set-auto-mode))
  (setq xueliang-switch-mode-counter (+ xueliang-switch-mode-counter 1)))

(defun xueliang-dev-machine-temperature ()
  "check temperature" (interactive)
  (xueliang-eshell-quick-command "cat /sys/class/thermal/thermal_zone*/temp"))

(defun xueliang-dropbox-status ()
  "check dropbox status quickly" (interactive)
  (xueliang-eshell-quick-command "watch -n 0.3 dropbox status"))

(defun xueliang-open-link-from-string (str)
  (if (string-equal system-type "windows-nt")
      (org-open-link-from-string xueliang-weblink-str)
    (require 'browse-url) (browse-url-chrome str))
)

(defun xueliang-open-link ()
   "" (interactive)
   (setq-local xueliang-weblink-str (nth 1 (split-string (ivy-read "Link: " xueliang-private-weblink-list))))
   (xueliang-open-link-from-string xueliang-weblink-str)
)

(defun xueliang-art-proj-monitor () (interactive)
  (when (string-equal system-type "gnu/linux")
    ;; regenerate the svg file everytime, just in case it is deleted.
    (xueliang-eshell-quick-command (message "cd %s; sleep 0.3; dot -Tsvg art_arch.dot -o art_proj_monitor.svg; sleep 0.3" dropbox-home) t)
    (start-process "my-art-proj-monitor" nil "google-chrome" (concat dropbox-home "/art_proj_monitor.svg")))
)

(defun xueliang-markdown-to-PDF-slides ()
  "Use odpdown/soffice to convert current markdown .md file to PDF slides."
  (interactive)
  (when (string-equal "md" (file-name-extension (buffer-file-name)))
     (setq-local xueliang-file-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
     (shell-command (message "odpdown %s -p0 ~/workspace/dropbox/arm.otp %s.odp --break-master Title --content-master Normal" (buffer-file-name) xueliang-file-name))
     (shell-command (message "soffice --convert-to pdf %s.odp && evince %s.pdf" xueliang-file-name xueliang-file-name))
  )
)

(defun xueliang-rm-clean-home-rubish () (interactive)
   (xueliang-eshell-quick-command
    (concat "rm -rf ~/rubish/out." (format-time-string "%Y-%m-*" (current-time))) t)
)

(defun xueliang-df () (interactive)
   (if (string-equal (string-trim (shell-command-to-string "hostname")) xueliang-workstation-name)
      (xueliang-eshell-quick-command "df -h /data")
      (xueliang-eshell-quick-command "df -h /"))
)

(defun xueliang-smaller-window () (interactive)
       (evil-window-decrease-height 3)
       (evil-window-decrease-width 3))

(defun xueliang-bigger-window () (interactive)
       (evil-window-increase-height 3)
       (evil-window-increase-width 3))

(defun xueliang-select-close-window-and-kill-buffer() (interactive)
       (setq xueliang-window-num
             (string-to-number
              (nth 1 (split-string
                      (ivy-read "Select and Close Window/Buffer: "
                                (list "Window 1"
                                      "Window 2"
                                      "Window 3"
                                      "Window 4"
                                      "Window 5"
                                      "Window 6"
                                      "Window 7"
                                      "Window 8"
                                      "Window 9"))))))
       (winum-select-window-by-number xueliang-window-num)
       (kill-buffer-and-window))

(defun xueliang-kill-all-eshell-buffers() (interactive)
       (kill-matching-buffers "eshell"))

;; Use Ctrl + mouse wheel scroll to adjust window sizes.
(global-set-key (kbd "<C-mouse-4>") 'xueliang-bigger-window)
(global-set-key (kbd "<C-mouse-5>") 'xueliang-smaller-window)

(defun xueliang-anti-zenburn-theme-colors()
  "anti-zenburn theme is the best for my workstation in office."
  (interactive)  ;; make interactive so that it is easier for command line ssh shell to call.

  ;; Useful commands: list-faces-display, counsel-colors-emacs.
  (set-face-foreground 'font-lock-comment-face "DarkGreen")
  (set-face-foreground 'font-lock-doc-face "DarkGreen")
  (set-face-foreground 'font-lock-comment-delimiter-face "DarkGreen")

  (set-face-foreground 'linum "DarkSlateBlue")
  (set-face-background 'cursor "DimGrey")

  ;; Org mode colors
  (require 'org-faces)
  (setq org-src-fontify-natively t)
  (set-face-foreground 'org-done "DarkSlateGrey")
  (set-face-foreground 'org-todo "RoyalBlue")

  ;; ivy colors
  (set-face-attribute  'ivy-current-match nil :underline t)
  (set-face-background 'ivy-minibuffer-match-face-1 "grey67") ;; LemonChiffon2
  (set-face-background 'ivy-minibuffer-match-face-2 "grey67")
  (set-face-background 'ivy-minibuffer-match-face-3 "grey67")
  (set-face-background 'ivy-minibuffer-match-face-4 "grey67")
  (set-face-background 'ivy-match-required-face     "grey67")

  ;; cursor and line highlight
  (spacemacs/toggle-highlight-current-line-globally-off)

  ;; diff
  ;;(set-face-background 'diff-added "#93cccc")
  ;;(set-face-background 'diff-refine-added "#93cccc")
  ;;(set-face-background 'diff-removed "#d0b0d0")
  ;;(set-face-background 'diff-refine-removed "#d0b0d0")

  ;; flyspell
  (require 'flyspell)
  (set-face-foreground 'flyspell-incorrect "DarkRed")
  (set-face-foreground 'flyspell-duplicate "DarkRed")
)

(defun =============xueliang-linaro-development=============())

(defun xueliang-linaro-art-gtest-host ()
  (interactive)
  (xueliang-eshell-pwd) ;; have to use eshell here, which provides better/stable output searching functionality.
  (rename-buffer (concat "*eshell-linaro-make-art-gtest-host" (format-time-string "-%H:%M:%S" (current-time)) "*"))
  (setq-local ncpu (string-trim (shell-command-to-string "cat /proc/cpuinfo | grep processor | wc -l")))
  (insert "cd $android-root") (eshell-send-input)
  (insert (message "art/test/testrunner/run_build_test_target.py -j%s art-test-javac" ncpu)) (eshell-send-input)
  ;; (insert (message "art/test/testrunner/run_build_test_target.py -j%s art-gtest-asan" ncpu)) (eshell-send-input)
  ;; (insert "echo y | scripts/tests/test_art_host.sh") (eshell-send-input)
)

(defun xueliang-art-test-target-optimizing() (interactive)
  (xueliang-eshell-pwd) ;; have to use eshell here, which provides better/stable output searching functionality.
  (rename-buffer (concat "*eshell-linaro-make-art-test" (format-time-string "-%H:%M:%S" (current-time)) "*"))
  (insert "cd $android-root") (eshell-send-input)
  (setq-local ncpu (string-trim (shell-command-to-string "cat /proc/cpuinfo | grep processor | wc -l")))
  (insert (message "art/test/testrunner/run_build_test_target.py -j%s art-test-javac" ncpu)) (eshell-send-input)
  ;; (insert "scripts/tests/test_art_target.sh --64bit --optimizing") (eshell-send-input)
  ;; (insert "scripts/tests/test_art_target.sh --64bit --keep-failures --single-test test-art-target-run-test-debug-prebuild-optimizing-no-relocate-ntrace-cms-checkjni-picimage-ndebuggable-no-jvmti-cdex-fast-580-fp1664") (eshell-send-input)
)

(defun xueliang/make-android-system-image (lunch-target)
  "invoke build android system image from andriod-root source tree"
  (setq ncpu (string-trim (shell-command-to-string "cat /proc/cpuinfo | grep processor | wc -l")))
  (split-window-below) (evil-window-move-very-bottom)
  (term "bash") (rename-buffer (concat "*make-android-" (format-time-string "%H:%M:%S" (current-time)) "*"))
  (insert (message "cd %s" android-root)) (term-send-input)
  (insert "source build/envsetup.sh") (term-send-input)
  (insert (concat "lunch " lunch-target)) (term-send-input)
  (insert (message "time make dx -j%s" ncpu)) (term-send-input)
  (insert (message "time make -j%s" ncpu)) (term-send-input)
)

(defun xueliang-make-android-system-image ()
  "Choose from build targets" (interactive)
  (xueliang/make-android-system-image
   (ivy-read "Lunch Targets: " (list
                                      "arm_krait-eng"
                                      "aosp_arm-eng"
                                      "aosp_arm64-eng"
                                      "aosp_walleye-userdebug"
                                      ))))

(defun xueliang-linaro-gdb ()
  "invoke gdb linaro tree" (interactive)
  (require 'gdb-mi)
  ;; build and regenerate the test case before starting gdb.
  (tool-bar-mode -1)
  (cd android-xueliang_test) (shell-command (concat android-xueliang_test "/run.sh"))
  (cd android-root) (gdb-many-windows) (gdb "gdb -i=mi -x gdb.init"))

(defun xueliang-linaro-repo-sync-PINNED-MANIFEST ()
  "repo sync linaro tree with pinned manifest under $android-root/pinned-manifest.xml" (interactive)
  (when (file-exists-p "~/Downloads/pinned-manifest.xml")
    (xueliang-eshell-pwd) (insert "cd $android-root") (eshell-send-input)
    (insert (message "cat ~/Downloads/pinned-manifest.xml | sed \"s/git@dev-private-git.linaro.org/xueliang.zhong@dev-private-git.linaro.org:29418/\" > %s/pinned-manifest.xml" android-root))
    (eshell-send-input) (sleep-for 0 300)
    (setq-local ncpu (string-trim (shell-command-to-string "cat /proc/cpuinfo | grep processor | wc -l")))
    (insert (message "repo sync -d -m ~/workspace/linaro/pinned-manifest.xml -j%s" ncpu)) (eshell-send-input)
    (insert "rm -f ~/Downloads/pinned-manifest.xml") (eshell-send-input)
  )
)

(defun xueliang-cfg-analyze-c1visualizer-irhydra ()
  "analyze ART generated .cfg file" (interactive)
  ;;(browse-url-chrome "http://mrale.ph/irhydra/2.bak/")
  (start-process "cfg-analysis" nil "~/workspace/c1visualizer/bin/c1visualizer"))

(defun xueliang-clean-android-linaro-mv-rm-out-files () (interactive)
  (xueliang-eshell-pwd) ;; have to use eshell here, which provides better/stable output searching functionality.
  (rename-buffer (concat "*mv-rm-out-" (format-time-string "%H:%M:%S*" (current-time)) "*"))
  (insert "cd $android-root") (eshell-send-input)
  (insert (concat "mv out ~/rubish/out.") (format-time-string "%Y-%m-%d-%H_%M_%S" (current-time))) (eshell-send-input)
  (eshell/x)
)

(defun xueliang-coffee-break () (interactive)
  ;; rm ~/rubish/*
  (xueliang-rm-clean-home-rubish)
  ;; repo sync minimal_aosp_art
  (setq-local ncpu (string-trim (shell-command-to-string "cat /proc/cpuinfo | grep processor | wc -l")))
  (xueliang-eshell-quick-command (message "cd ~/workspace/minimal_aosp_art; repo sync -j%s" ncpu))
)

(defun =============xueliang-git-config/functions=============())

(setq-default shell-output-buffer-name "*Shell Command Output*")

(defalias 'xueliang-gdiff                 'magit-status)
(defalias 'xueliang-glog                  'magit-log-all)
(defalias 'xueliang-grebase               'magit-rebase-interactive)
(defalias 'xueliang-gread-current-buffer  'magit-status)
(defalias 'xueliang-gwrite-current-buffer 'magit-status)
(defalias 'xueliang-gpull                 'magit-git-pull)
(defalias 'xueliang-gpush                 'magit-git-push)

(defun xueliang-gcommit ()
  "run git commit on current buffer" (interactive)
   (xueliang-cd-current-buffer-directory)
   (shell-command (message "git commit -m \"%s\""
                           (ivy-read "COMMIT MSG: " (list
                                                     (message "Improve code in %s." (file-name-nondirectory buffer-file-name))
                                                     (message "Introduce %s." (file-name-nondirectory buffer-file-name))
                                                     (message "Address review comments to %s." (file-name-nondirectory buffer-file-name))
                                                     )))))

(defun xueliang/glog (&optional xueliang-cword)
  "git log with ivy"
  (require 'fiplr)
  (xueliang-cd-current-buffer-directory) (cd (fiplr-root))
  (car (split-string
        (ivy-read "Git Log: "
                        (split-string (shell-command-to-string "git log -n 100 --pretty=\"%h * %<(70)%s | %<(16)%an | %cr\"") "\n")
                        :preselect "|"  ;; this makes sure that the first candidate in the log is pre-selected.
                        :initial-input (if xueliang-cword                                      ;; if current word at point is a string
                                           (if (= 0 (string-match "[a-f0-9]+" xueliang-cword)) ;; and it is a git version string
                                               xueliang-cword                                  ;; then use it as initial-input;
                                             "") "")))))                                       ;; otherwise, avoid giving any initial-input.

(defun xueliang-gdiff-revision-at-point ()
   "run 'git diff' using the revision number from ivy glog" (interactive)
   (when (buffer-file-name) (require 'fiplr) (xueliang-cd-current-buffer-directory) (cd (fiplr-root)))
   (magit-diff (xueliang/glog)) (evil-next-line 7)
)

(defun xueliang-gshow ()
  "run 'git show' using the ivy glog" (interactive)
  (magit-show-commit (xueliang/glog)) (evil-next-line 17)
)

(defun xueliang-gshow-revision-at-point-OPEN-GERRIT-REVIEW ()
  "run 'git show' using the ivy glog" (interactive)
  (setq git-revision-string (thing-at-point 'word))
  (when git-revision-string
    (magit-show-commit git-revision-string) (evil-next-line 17)
    (xueliang-open-link-from-string (concat
                                     (ivy-read "Select Gerrit: "
                                               (list
                                                "about:blank"
                                                "https://android-review.googlesource.com/#/q/")
                                               :preselect "blank")
                                     git-revision-string))))

(defun xueliang-gblame-current-buffer ()
  "run git blame on current buffer, esp. current line" (interactive)
  (xueliang-cd-current-buffer-directory)
  (setq-local gblame-line (line-number-at-pos))
  (shell-command (concat "git blame " (buffer-file-name)))
  (goto-line gblame-line (switch-to-buffer-other-window shell-output-buffer-name))
  (evil-window-move-very-bottom) (toggle-truncate-lines 1))

(defun =============xueliang-eshell-functions=============())

(defalias 'eshell/vi 'eshell/e)
(defalias 'eshell/vim 'eshell/e)

(defun eshell/x ()
  "exit eshell and close the window."
  (insert "exit") (eshell-send-input)
  (delete-window))

(defun eshell/cdroot-git-root ()
  "cd to project root"
  (require 'fiplr)
  (cd (fiplr-root)))

(defun eshell/cdandroid-root ()
  "cd to project root"
  (cd android-root))

(defun eshell/e (file-name)
  "handle large files better"
  (if (> (string-to-number (car (split-string (shell-command-to-string (message "du -sm %s" file-name)))))
         30)
      (shell-command (message "gnome-terminal -e \"vim %s \"" file-name))
      (find-file-other-window file-name)))

(defun eshell/vimdiff (file1 file2)
  (shell-command (message "gnome-terminal -e \"vimdiff %s %s\"" file1 file2)))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (graphviz-dot-mode json-mode json-snatcher json-reformat projectile nlinum-relative zenburn-theme zen-and-art-theme xterm-color white-sand-theme unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle shell-pop seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme orgit organic-green-theme org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mwim mustang-theme multi-term monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme magit-gitflow madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme htmlize heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gandalf-theme flyspell-correct-ivy flyspell-correct flycheck-pos-tip pos-tip flycheck flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme eshell-z eshell-prompt-extras esh-help dracula-theme django-theme diff-hl darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-dictionary apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme mmm-mode markdown-toc markdown-mode gh-md grizzl magit magit-popup git-commit ghub treepy graphql with-editor counsel swiper ivy company yasnippet auto-complete helm-google nlinum helm-themes helm-swoop helm-projectile helm-mode-manager helm-flx helm-descbinds helm-ag ace-jump-helm-line ws-butler winum which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-rich ivy-hydra indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-make google-translate golden-ratio fuzzy flx-ido fiplr fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word counsel-projectile company-statistics column-enforce-mode clean-aindent-mode auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

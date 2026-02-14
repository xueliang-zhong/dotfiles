;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation nil

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Core layers - keeping your existing setup
     ;; ----------------------------------------------------------------
     auto-completion
     emacs-lisp
     git
     ivy
     org

     ;; ----------------------------------------------------------------
     ;; New layers for modern workflow (non-intrusive)
     ;; ----------------------------------------------------------------
     better-defaults
     syntax-checking
     treemacs
     )


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(just-mode doom-themes ellama
                                                ;; Modern enhancements
                                                org-superstar evil-collection)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   ;; Optimized GC settings for better performance
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light). A theme from external
   ;; package can be defined with `:package', or a theme can be defined with
   ;; `:location' to download the theme package, refer the themes section in
   ;; DOCUMENTATION.org for the full theme specifications.
   dotspacemacs-themes '(spacemacs-dark doom-tokyo-night doom-spacegrey)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("JetBrains Mono"
                               :size 18
                               :weight regular
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

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

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.3

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

   ;; If nil, no load-hints enabled. If t, enable the `load-hints' which will
   ;; put the most likely path on the top of `load-path' to reduce walking
   ;; through the whole `load-path'. It's an experimental feature to speedup
   ;; Spacemacs on Windows. Refer the FAQ.org "load-hints" session for details.
   dotspacemacs-enable-load-hints nil

   ;; If t, enable the `package-quickstart' feature to avoid full package
   ;; loading, otherwise no `package-quickstart' attemption (default nil).
   ;; Refer the FAQ.org "package-quickstart" section for details.
   dotspacemacs-enable-package-quickstart nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-fu', `undo-redo' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system. The default is currently `undo-fu' as `undo-tree'
   ;; is not maintained anymore and `undo-redo' is very basic."
   dotspacemacs-undo-system 'undo-redo

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y t

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )


(defun dotspacemacs/user-config ()
  "Configuration for user code: This function is called at the very end of
Spacemacs startup, after layer configuration. Put your configuration code here,
except for variables that should be set before packages are loaded."
  ;; ============================================================
  ;; Global Settings - Preserving your existing habits
  ;; ============================================================
  (when (spacemacs/system-is-mac) ;; make sure M-x works on MacOS
    (global-set-key (kbd "s-x") 'execute-extended-command))
  (spacemacs/toggle-highlight-current-line-globally-off)

  ;; keep editing state persistent and predictable
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  (save-place-mode 1)
  (savehist-mode 1)
  (global-auto-revert-mode 1)

  ;; Performance: Disable bidirectional text for speed
  (setq-default bidi-display-reordering nil)
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq-default scroll-margin 0)

  ;; Keep indentation and tab behaviour aligned with Neovim
  (setq-default indent-tabs-mode nil
                tab-width 2
                standard-indent 2)
  (add-hook 'before-save-hook #'xueliang-create-missing-directories-h)

  ;; avoid long line wrap
  (add-hook 'dired-mode-hook (lambda () (setq truncate-lines t)))
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines t)))
  (add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

  ;; Enable visual-line-mode only where it makes sense
  (add-hook 'text-mode-hook #'visual-line-mode)

  ;; ============================================================
  ;; Evil Mode Settings - Preserving your vim habits
  ;; ============================================================
  ;; modern style 'paste' in evil insert mode.
  (define-key evil-insert-state-map (kbd "C-v") #'yank)
  (define-key evil-insert-state-map (kbd "C-g") #'evil-normal-state)
  (define-key evil-insert-state-map (kbd "TAB") #'(lambda() (interactive) (insert "  ")))
  ;; better evil behaviour
  (setq evil-emacs-state-modes nil
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-shift-width 2)

  ;; Additional evil improvements (non-intrusive)
  (setq evil-want-fine-undo t)
  (setq evil-want-Y-yank-to-eol t)

  ;; ============================================================
  ;; Ivy/Counsel Settings - Enhanced completion
  ;; ============================================================
  (setq counsel-grep-swiper-limit 30000000)
  (define-key ivy-mode-map (kbd "C-k") 'evil-delete-line)
  (setq ivy-initial-inputs-alist (remove '(counsel-M-x . "^") ivy-initial-inputs-alist))

  ;; Performance: Faster ivy settings
  (setq ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-virtual-abbreviate 'full
        ivy-use-selectable-prompt t
        ivy-wrap t)

  ;; ============================================================
  ;; Dired Settings
  ;; ============================================================
  (setq-default dired-listing-switches "-alh") ; List file details in human-readable format
  (evil-define-key 'normal dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file-other-window)
  (define-key dired-mode-map (kbd "TAB") 'dired-display-file)
  (define-key dired-mode-map (kbd "RET") 'dired-find-file)
  (define-key dired-mode-map (kbd "<return>") 'dired-find-file)

  ;; Enable dired-x for extra features
  (require 'dired-x)

  ;; ============================================================
  ;; Company - Better completion experience
  ;; ============================================================
  (setq-default
   company-dabbrev-char-regexp "[\\0-9a-zA-Z-_'/]"
   company-idle-delay 0
   company-minimum-prefix-length 1
   company-tooltip-align-annotations t
   company-require-match 'never
   company-selection-wrap-around t
   company-global-modes '(not eshell-mode comint-mode ielm-mode)
   company-backends '(company-capf company-dabbrev company-files))
  (setq company-transformers '(company-sort-by-occurrence))

  ;; Better company keybindings (vim-friendly)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous))

  ;; vim style completions
  (define-key evil-insert-state-map (kbd "C-x C-f") #'company-files)
  (define-key evil-insert-state-map (kbd "C-x C-l") #'xueliang-complete-line)
  (define-key evil-normal-state-map (kbd "C-x C-l") #'xueliang-complete-line)

  ;; eshell settings
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local company-idle-delay 600)
              (define-key evil-insert-state-local-map (kbd "C-a") #'eshell-bol)
              (define-key evil-insert-state-local-map (kbd "C-k") #'kill-line)
              (define-key evil-insert-state-local-map (kbd "C-r") #'counsel-esh-history)
              (define-key evil-insert-state-local-map (kbd "TAB") #'completion-at-point)
              (define-key evil-insert-state-local-map (kbd "C-d") #'kill-buffer-and-window)
              (define-key evil-insert-state-local-map (kbd "<f1>") #'evil-force-normal-state) ;; just like tmux
              (evil-insert-state)))

  ;; ============================================================
  ;; Projectile - Better project handling
  ;; ============================================================
  (setq projectile-switch-project-action 'projectile-find-file
        projectile-track-known-projects-automatically nil)

  ;; leader keys
  (spacemacs/set-leader-keys "SPC" 'counsel-switch-buffer)
  (spacemacs/set-leader-keys "?"   'which-key-show-top-level)
  (spacemacs/set-leader-keys "bs"  'xueliang-open-scratch-buffer-window)
  (spacemacs/set-leader-keys "bw"  'read-only-mode)
  (spacemacs/set-leader-keys "cc"  'xueliang-just-make)
  (spacemacs/set-leader-keys "ff"  'projectile-find-file)
  (spacemacs/set-leader-keys "fr"  'counsel-recentf)
  (spacemacs/set-leader-keys "fT"  'treemacs-find-tag)
  (spacemacs/set-leader-keys "gf"  'counsel-git)
  (spacemacs/set-leader-keys "gg"  'xueliang-magit-status-window)
  (spacemacs/set-leader-keys "si"  'counsel-imenu)
  (spacemacs/set-leader-keys "wc"  'spacemacs/delete-window) ;; window close
  (spacemacs/set-leader-keys "wg"  'golden-ratio)
  (spacemacs/set-leader-keys "/"   'counsel-grep-or-swiper)
  (spacemacs/set-leader-keys "x"   'counsel-M-x)
  (spacemacs/set-leader-keys "fp"  'xueliang-find-file-in-dotfiles)
  (spacemacs/set-leader-keys "RET" 'counsel-recentf)

  ;; function keys
  (global-set-key (kbd "<f2>")  #'xueliang-T-open-T-in-browser)
  (global-set-key (kbd "<f3>")  #'xueliang-dired-sidebar)
  (global-set-key (kbd "<f4>")  #'evil-window-delete)
  (global-set-key (kbd "<f5>")  #'xueliang-eshell-popup)
  (global-set-key (kbd "<f6>")  #'counsel-yank-pop)
  (global-set-key (kbd "<f7>")  #'xueliang-just-make)
  (global-set-key (kbd "<f8>")  #'xueliang-imenu-or-org-today) ;; counsel-imenu
  (global-set-key (kbd "<f9>")  #'counsel-find-file)
  (global-set-key (kbd "<f10>") #'xueliang-telescope-counsel)
  (global-set-key (kbd "<f11>") #'xueliang-org-sort-upheading)
  (global-set-key (kbd "<f12>") #'xueliang-open-knowledge-links)
  (global-set-key (kbd "C-<f4>") #'kill-buffer-and-window)

  ;; ============================================================
  ;; Recent Files & Session Management
  ;; ============================================================
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 50
        recentf-auto-cleanup 'never)

  ;; ============================================================
  ;; Backup Settings
  ;; ============================================================
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
        backup-by-copying t
        version-control t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2)

  ;; reload this function when enter org-mode
  (add-hook 'org-mode-hook 'xueliang-reload-spacemacs-config)

  ;; ============================================================
  ;; Git settings - Improve magit experience
  ;; ============================================================
  (setq magit-display-buffer-function 'display-buffer
        magit-diff-refine-hunk 'all)

  ;; ============================================================
  ;; Better help and discoverability
  ;; ============================================================
  (setq which-key-sort-order 'which-key-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1)
  )

;;
;; My own functions
;;
(defun xueliang-reload-spacemacs-config ()
  "reload my spacemacs config" (interactive)
  ;; ============================================================
  ;; Org Mode Settings - Enhanced appearance
  ;; ============================================================
  (org-indent-mode 1)
  (org-superstar-mode 1)
  (org-set-frame-title "YI")
  (setq-default split-width-threshold 160) ;; works better for org-timestamp
  (setq
   org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
   org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?➜) (?- . ?✓)) ; changes +/- symbols in item lists
   org-ellipsis " ▶"
   org-log-done nil
   org-link-abbrev-alist '(("SC"    . "https://stockcharts.com/h-sc/ui?s=%s")
                           ("piano" . "https://www.scales-chords.com/chord/piano/%s"))
   org-todo-keywords '((sequence "FOCUS(f)" "TODO(t)" "PROG(p)" "|" "DONE(d)")))

  ;; Enhanced org settings
  (setq org-startup-folded 'content
        org-cycle-separator-lines 2
        org-src-fontify-natively nil
        org-src-tab-acts-natively t
        org-pretty-entities nil)
  ;; org-babel improvements
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))
  (setq org-confirm-babel-evaluate nil)
  ;; key bindings
  (evil-define-key 'normal evil-org-mode-map
    (kbd "<return>")  #'xueliang-org-open-at-point
    (kbd "RET")       #'xueliang-org-open-at-point)
  ;; better org-mode CMD key behaviour on MacOS
  (define-key org-mode-map (kbd "s-<right>")  'org-metaright)
  (define-key org-mode-map (kbd "s-<left>")   'org-metaleft)
  (define-key org-mode-map (kbd "s-<return>") 'org-meta-return)
  ;; Face settings
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5 org-level-6 org-link org-table))
    (set-face-attribute face nil :bold nil :height 1.0 :weight 'regular))
  )

(defun xueliang-create-missing-directories-h ()
  "Create missing parent directories before saving current file."
  (when (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (let ((parent-dir (file-name-directory buffer-file-name)))
      (when (and parent-dir (not (file-exists-p parent-dir)))
        (make-directory parent-dir t)))))

(defun xueliang-complete-line ()
  "Like vim's Blines completion" (interactive)
  (setq-local comp-line
              (ivy-read "Complete Line > " (split-string (buffer-string) "\n" t) :initial-input (thing-at-point 'line)))
  (delete-region (line-beginning-position) (line-end-position))
  (insert comp-line))

(defun xueliang-dired-sidebar ()
  "Toggle dired sidebar. Open or close the sidebar window." (interactive)
  (let ((dired-sidebar-window nil))
    ;; Check if there's already a dired sidebar window
    (dolist (win (window-list))
      (when (with-current-buffer (window-buffer win)
              (eq major-mode 'dired-mode))
        (setq dired-sidebar-window win)))
    ;; If sidebar exists, close it; otherwise open it
    (if dired-sidebar-window
        (delete-window dired-sidebar-window)
      (xueliang-cd-current-dir)
      (evil-window-vsplit) (evil-window-move-far-left)
      (dired-jump) (dired-hide-details-mode)
      (hl-line-mode 1) (evil-window-decrease-width (floor (* (window-width) 0.382))))))

(defun xueliang-imenu-or-org-today ()
  "" (interactive)
  (if (derived-mode-p 'org-mode)
      (progn (swiper (format-time-string "<%Y-%m-%d")) (evil-ex-nohighlight))
    (counsel-imenu)))

(defun xueliang-cd-current-dir ()
  "cd to directory of current buffer/file." (interactive)
  (when buffer-file-name
    (cd (file-name-directory buffer-file-name))
    (message "pwd: %s" (file-name-directory buffer-file-name))))

(defun xueliang-eshell-popup ()
  "Invokes a new eshell in a popup window and ready for command" (interactive)
  (xueliang-cd-current-dir) (split-window-below-and-focus) (eshell) (evil-append-line 1))

(defun xueliang-open-knowledge-links ()
  "My knowledge links quick open" (interactive)
  ;; fast search in pre-compiled URL list
  (setq-local url-list
              (with-temp-buffer (insert-file-contents "~/workspace/xzhong-links.txt")
                                (split-string (buffer-string) "\n" t)))
  ;; ivy workflow
  (setq-local selected-str (ivy-read "Knowledge Link: " url-list))
  ;; find the URL substr (Knowledge Link) within the selected line
  (when (string-match "\\(https?://[^\s]+\\)" selected-str)
    (setq-local xueliang-url-str (match-string 1 selected-str)))
  (when (string-match "\\(file:[^\s]+\\)" selected-str)
    (setq-local xueliang-url-str (match-string 1 selected-str)))
  (org-link-open-from-string xueliang-url-str))

(defun xueliang-org-open-at-point()
  "Open links in org-mode headings, otherwise just behave like dwim-at-point." (interactive)
  (when (string-equal major-mode "org-mode")
    (if (string-match "^\*[\*]* " (thing-at-point 'line))
        (org-open-at-point) (evil-ret))))

(defun xueliang-sum-numbers-in-region (start end)
  (interactive "r")
  (message "Sum: %s" (cl-reduce #'+ (split-string (buffer-substring start end)) :key #'string-to-number)))

(defun xueliang-open-scratch-buffer-window ()
  "Open scratch buffer window" (interactive)
  (split-window-right-and-focus) (spacemacs/switch-to-scratch-buffer))

(defun xueliang-magit-status-window ()
  "Open magit status on the right split" (interactive)
  (setq-local split-width-threshold 10) ;; prefer vsplit
  (xueliang-cd-current-dir) (magit-status))

(defun xueliang-org-sort-upheading ()
  "Sort org tree entries by priority." (interactive)
  (when (derived-mode-p 'org-mode)
    (evil-next-line 1) (outline-up-heading 1 t) (org-sort-entries nil ?o)))

(defun Gwrite () (interactive) (evil-ex-execute "!git add %"))

(defun Gcommit ()
  "Support :Gcommit similar to vim." (interactive)
  (xueliang-cd-current-dir)
  (evil-ex-execute (message "!git commit -m \"Update %s\"" (file-name-nondirectory buffer-file-name))))

;;
;; My one liner commands
;;
(defun xueliang-replace-tab-trailing-spaces() (interactive) (untabify (point-min) (point-max)) (delete-trailing-whitespace))
(defun xueliang-T-open-T-in-browser () (interactive) (org-link-open-from-string "https://stockcharts.com/sc3/ui/?s=VWRL.L"))
(defun xueliang-telescope-counsel () (interactive) (counsel-M-x "counsel "))
(defun xueliang-dired-jump () (interactive) (dired-jump) (dired-hide-details-mode -1) (hl-line-mode 1))
(defun xueliang-find-file-in-dotfiles () (interactive) (counsel-find-file nil "~/workspace/dotfiles/"))
(defun xueliang-just-make () (interactive) (evil-ex-execute "!just all &"))
(defun xueliang-daily-websites() (interactive) (evil-ex-execute "!just daily-websites &"))

;;
;; Some useful alias
;;
(defalias 'xueliang-what-face  'describe-face)
(defalias 'xueliang-cap-region 'capitalize-region)
(defalias 'xueliang-org-sort   'org-sort)
(defalias 'eshell/e   'find-file-other-window)
(defalias 'eshell/vi  'eshell/e)
(defalias 'eshell/vim 'eshell/e)
(defalias 'eshell/fzf 'counsel-fzf)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

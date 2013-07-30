; enable Common Lisp support
;(require 'cl)

; some modes need to call stuff on the exec-path
(push "/usr/local/bin" exec-path)

; Start in server mode
(server-start)

; add directories to the load path
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/utilities")
; (add-to-list 'load-path "~/.emacs.d/vendor")

; handy function to load all elisp files in a directory
(load-file "~/.emacs.d/utilities/utilities.elc")

(add-hook 'emacs-lisp-mode-hook '(lambda ()
  (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))
)

;;;;;;;;;;;;;;;;;;;;;;; ELPA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("Tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;; Buffer Tweaks ;;;;;;;;;;;;;;;;;;;;;;;

; recent files
(require 'recentf)
(setq recentf-max-saved-items 100)

; disable auto-save files (#foo#)
(setq auto-save-default nil)

; disable backup files (foo~)
(setq backup-inhibited t)

; save cursor position within files
(require 'saveplace)
(setq save-place-file "~/.emacs.d/.saveplace")
(setq-default save-place t)

; save minibuffer history across sessions
(setq savehist-file "~/.emacs.d/.savehist")
(savehist-mode 1)

; nicer naming of buffers with identical names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

; Interactively Do Things
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ; case insensitive matching
(add-to-list 'ido-ignore-files "\\.DS_Store")

; automatically clean up old buffers
(require 'midnight)

; use default Mac browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

; delete files by moving them to the OS X trash
(setq delete-by-moving-to-trash t)

; pick up changes to files on disk automatically (ie, after git pull)
(global-auto-revert-mode 1)


;;;;;;;;;;;;;;;;;;;; Editing Preferences ;;;;;;;;;;;;;;;;;;;;;;;

; tabs
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

; mousing
(setq mac-emulate-three-button-mouse nil)

; encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

; whitespace
;(global-whitespace-mode t)
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; line numbering
(require 'linum)
(global-linum-mode)
(setq linum-format " %d ") ; space after line number

; show column number in bar
(column-number-mode t)
; highlight URLs in comments/strings
(add-hook 'find-file-hooks 'goto-address-prog-mode)

; selection
(delete-selection-mode t)

; show marks as selections
(setq transient-mark-mode t)

; highlight matching parens
(show-paren-mode t)

; highlight incremental search
(defconst search-highlight t)

; no newlines past EOF
(setq next-line-add-newlines nil)

; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;;;;;;;;;;;;;;;;;;;; UI Preferences ;;;;;;;;;;;;;;;;;;;;;;;

(set-frame-font "Source Code Pro Light-12")

; don't display startup message
(setq inhibit-startup-message t)

; no scrollbar
(scroll-bar-mode -1)

; no toolbar
(tool-bar-mode -1)

; blink cursor
(blink-cursor-mode t)

; highlight current line
(global-hl-line-mode t)

; force new frames into existing window
(setq ns-pop-up-frames nil)

; no bell
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;; Keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-input-method "MacOSX")

; option/alt is meta key
(setq mac-command-key-is-meta nil)

; Enable hyper key!
(setq ns-function-modifier 'hyper)

; Make yes-or-no questions answerable with 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)

; To be able to M-x without meta - yes, this overwrites exiting but
; I don't care because I quit Apple style with s-q
(global-set-key (kbd "C-x C-c") 'execute-extended-command)
(global-set-key (kbd "C-x c") 'execute-extended-command)
(global-set-key (kbd "C-x m") 'execute-extended-command)


; search with ag
(setq ag-highlight-search t)
(global-set-key (kbd "s-F") 'ag-project-at-point)
(global-set-key (kbd "H-f") 'ag-regexp-project-at-point)

;; goto line
(global-set-key (kbd "C-l") 'goto-line)

; open file
(global-set-key [(super o)] 'find-file)

; buffer switching
(global-set-key [(super {)] 'previous-buffer)
(global-set-key [(super })] 'next-buffer)

; window switching
(global-set-key (kbd "s-`") 'other-window)

; close window
(global-set-key [(super w)] (lambda ()
  (interactive)
  (kill-buffer (current-buffer)
)))

; navigating through errors
(global-set-key [(meta n)] 'next-error)
(global-set-key [(meta p)] 'previous-error)

;; XMPFilter
(global-set-key [(hyper x)] 'xmp)

; run Ruby tests, TextMate-style
(add-hook 'rinari-minor-mode-hook
  (lambda ()
    (define-key rinari-minor-mode-map (kbd "s-r") 'rinari-test)))

; magit
(global-set-key (kbd "C-c i") 'magit-status)
(global-set-key (kbd "<f5>") 'magit-status)
(global-set-key (kbd "<f6>") 'magit-blame-mode)

;; This below is from my previous bindigs file

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Duplicate line
;; (global-set-key (kbd "M-d") 'defunkt-duplicate-line)
(global-set-key (kbd "M-d") 'duplicate-current-line-or-region)

;; Newline and indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; use ibuffer instead of the built in buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Undo and Redo
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)

;; Move line up and down
(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

;; Join multiple lines
(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

;; Block editing mode
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-.") 'set-rectangular-region-anchor)
(global-set-key (kbd "s-<mouse-1>") 'mc/add-cursor-on-click)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Quickly jump to init file
(global-set-key (kbd "C-c I") 'find-user-init-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Shell Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; sane path
;; (setq path "/usr/local/ruby-1.9.3-p194/bin:/Users/mranallo/Code/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin:/usr/X11/bin:/Users/mranallo/Code/nodejs/bin")
;; (setenv "PATH" path)

;; more bash-like autocomplete
;; (setq eshell-cmpl-cycle-completions nil)

; automatically save history
;; (setq eshell-save-history-on-exit t)

; ignore version control directories when autocompleting
;; (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

; can't write over prompt, that would be weird
;; (setq comint-prompt-read-only)

; scroll to bottom on output, more like a terminal
;; (setq eshell-scroll-to-bottom-on-output t)
;; (setq eshell-scroll-show-maximum-output t)

;; Setting the correct $PATH
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "GEM_HOME")
(exec-path-from-shell-copy-env "GEM_PATH")
(exec-path-from-shell-copy-env "GEM_ROOT")
(exec-path-from-shell-copy-env "RUBY_ROOT")
(exec-path-from-shell-copy-env "RUBY_ENGINE")

; colorful shell
(require 'ansi-color)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; escape the shell
;; (add-hook 'eshell-mode-hook
  ;; '(lambda nil (local-set-key "\C-u" 'eshell-kill-input)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ack-and-a-half     20130103.... installed  Yet another front-end for ack [source: github]
;; anti-zenburn-theme 20130129.... installed  Low-contrast Zenburn-inverted theme [source: github]
;; anything           1.287        installed  open anything / QuickSilver-like candidate-selection framework
;; anything-complete  1.86         installed  completion with anything
;; anything-exuber... 0.1.2        installed  Exuberant ctags anything.el interface
;; anything-git       1.1.1        installed  Show git files in anything
;; anything-git-goto  0.1.0        installed  Quick listing of:
;; anything-match-... 1.27         installed  Humane match plug-in for anything
;; anything-obsolete  0.1          installed  obsolete functions of anything
;; browse-kill-ring   20130122.... installed  interactively insert items from kill-ring [source: github]
;; centered-cursor... 20100310.... installed  cursor stays vertically centered [source: wiki]
;; coffee-mode        20130125.... installed  Major mode to edit CoffeeScript files in Emacs [source: github]
;; ctags-update       20121219.... installed  (auto) update TAGS in parent directory using exuberant-ctags [source: github]
;; deft               20130129.907 installed  quickly browse, filter, and edit plain text notes [source: git]
;; dired+             20130128.... installed  Extensions to Dired. [source: wiki]
;; eieio              1.4          installed  Enhanced Implememntation of Emacs Interpreted Objects
;; exec-path-from-... 20121108.945 installed  Make Emacs use the $PATH set up by the user's shell [source: github]
;; findr              20130127.... installed  Breadth-first file-finding facility for (X)Emacs [source: wiki]
;; flymake-coffee     20121107.... installed  A flymake handler for coffee script [source: github]
;; flymake-easy       20130106.... installed  Helpers for easily building flymake checkers [source: github]
;; flymake-haml       20121104.... installed  A flymake handler for haml files [source: github]
;; flymake-ruby       20121104.... installed  A flymake handler for ruby-mode files [source: github]
;; flymake-sass       20121218.812 installed  Flymake handler for sass and scss files [source: github]
;; gh                 20121231.208 installed  A GitHub library for Emacs [source: github]
;; gist               20121231.212 installed  Emacs integration for gist.github.com [source: github]
;; github-theme       0.0.3        installed  Github color theme for GNU Emacs 24
;; gitignore-mode     20121012.... installed  Major mode for editing .gitignore files [source: github]
;; hackernews         0.1          installed  Access the hackernews aggregator from Emacs
;; haml-mode          20130121.837 installed  Major mode for editing Haml files [source: github]
;; httpcode           0.1          installed  explains the meaning of an HTTP status code
;; inf-ruby           20121215.... installed  Run a ruby process in a buffer [source: github]
;; inflections        20121016.957 installed  convert english words between singular and plural [source: github]
;; jump               20121016.... installed  build functions which contextually jump between files [source: github]
;; logito             20120225.... installed  logging library for Emacs [source: github]
;; magit              20130123.... installed  Control Git from Emacs. [source: github]
;; minimap            20110427.... installed  Minimap sidebar for Emacs [source: git]
;; monokai-theme      0.0.8        installed  REQUIRES EMACS 24: Monokai Color Theme for Emacs.
;; pcache             20120408.... installed  persistent caching for Emacs [source: github]
;; pivotal-tracker    20120403.... installed  Interact with Pivotal Tracker through its API [source: github]
;; rinari             20130107.... installed  Rinari Is Not A Rails IDE [source: github]
;; rspec-mode         20130129.... installed  Enhance ruby-mode for RSpec [source: github]
;; ruby-block         20111101.... installed  highlight matching block [source: wiki]
;; ruby-compilation   20121209.... installed  run a ruby process in a compilation buffer [source: github]
;; ruby-electric      20130127.... installed  Minor mode with electric editing commands for Ruby files [source: github]
;; ruby-end           20121008.... installed  Automatic insertion of end blocks for Ruby. [source: github]
;; ruby-interpolation 20120326.... installed  Ruby string interpolation helpers [source: github]
;; ruby-tools         20121008.... installed  Collection of handy functions for ruby-mode. [source: github]
;; sass-mode          20101019.30  installed  Major mode for editing Sass files [source: github]
;; scala-mode         20121205.... installed  No description available. [source: github]
;; smart-tab          20120409.940 installed  Intelligent tab completion and indentation. [source: github]
;; smex               20120915.... installed  M-x interface with Ido-style fuzzy matching. [source: github]
;; solarized-theme    20130108.651 installed  The Solarized color theme, ported to Emacs. [source: github]
;; sr-speedbar        20090723.435 installed  Same frame speedbar [source: wiki]
;; tango-2-theme      20120312.... installed  Tango 2 color theme for GNU Emacs 24 [source: github]
;; twilight-anti-b... 20120713.... installed  A soothing Emacs 24 light-on-dark theme [source: github]
;; twilight-bright... 20120630.... installed  A Emacs 24 faces port of the TextMate theme [source: github]
;; twilight-theme     20120412.803 installed  Twilight theme for GNU Emacs 24 (deftheme) [source: github]
;; undo-tree          20130119.926 installed  Treat undo history as a tree [source: git]
;; unfill             20120529.... installed  The inverse of fill-paragraph and fill-region [source: github]
;; yaml-mode          20120901.... installed  Major mode for editing YAML files [source: github]
;; yas-jit            0.8.3        installed  Loads Yasnippets on demand (makes start up faster)
;; yasnippet          20130112.... installed  Yet another snippet extension for Emacs. [source: github]
;; zen-and-art-theme  20120622.937 installed  zen and art color theme for GNU Emacs 24 [source: github]
;; zenburn-theme      20130129.... installed  A low contrast color theme for Emacs. [source: github]


;; Magit settings
;; full screen magit-status
(require 'magit)
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; Smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x m") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Rinari for rails
(require 'rinari)
(setq rinari-tags-file-name "tags")

;; Haml because they make me
(require 'haml-mode)
(add-hook 'haml-mode-hook 'flymake-haml-load)

;; Minimap like SublimeText2
(require 'minimap)

;; Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Keyboard Shortcuts
(vendor 'textmate)
(textmate-mode)

;; Winner mode
(winner-mode 1)

;; Centered Cursor mode
(require 'centered-cursor-mode)
(global-centered-cursor-mode t)

;; kill ring browsing
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; dired
(require 'dired+)
(setq dired-recursive-deletes 'top)
(put 'dired-find-alternate-file 'disabled nil)

;; sr-speedbar
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; rcodetools
(vendor 'rcodetools)

;; RHTML mode
;; (add-to-list 'load-path "~/.emacs.d/require/rhtml-mode")
;; (require 'rhtml-mode)
;; (vendor 'rhtml-mode)
;; (add-hook 'rhtml-mode-hook
;;      	  (lambda () (rinari-launch)))
;; (add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))

;; Ruby
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(setq auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Gemfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.gemspec" . ruby-mode) auto-mode-alist))
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-electric-mode-hook 'ruby-interpolation-mode)

;; smart-tab
(require 'smart-tab)
(add-to-list 'hippie-expand-try-functions-list
             'yas/hippie-try-expand) ;put yasnippet in hippie-expansion list
(global-smart-tab-mode 1)
(setq smart-tab-using-hippie-expand nil)

;; YA Snippets
(require 'yasnippet)
(yas-global-mode 1)
;; (yas-load-directory "~/.emacs.d/vendor/snippets")
(setq yas-prompt-functions '(yas/ido-prompt
                             yas/dropdown-prompt
                             yas/completing-prompt))


;; Coffeescript mode
(vendor 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

;; Sass mode
(vendor 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-hook 'sass-mode-hook 'flymake-sass-load)


;; Ruby End
(require 'ruby-end)

;; Deft (for notes)
(require 'deft)
(setq deft-extension "txt")
(setq deft-directory "~/Dropbox/notes")

;; Powerline https://github.com/jonathanchu/emacs-powerline
(vendor 'powerline)
(setq powerline-arrow-shape 'arrow)
;; (setq powerline-color1 "#212D24")
;; (setq powerline-color2 "#181d23")


;; Anything Modes
(require 'anything-complete)
(require 'anything-exuberant-ctags)
(global-set-key (kbd "s-T") 'anything-exuberant-ctags-select)
(require 'anything-git-goto)
(global-set-key (kbd "s-t") 'anything-git-goto)

;; YAML hooks
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; Go Mode
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))

;; Git Gutter Mode Fringe
(git-gutter+-mode 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(ansi-term-color-vector [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(background-color "#fcf4dc")
 '(background-mode light)
 '(column-number-mode t)
 '(cursor-color "#52676f")
 '(custom-enabled-themes (quote (twilight-bright)))
 '(custom-safe-themes (quote ("5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "1f4e6cf4cb3bdba8afdb9244e037698238080eeecb209084602f7d717225f102" "a5a1e3cd5f790846f4eec5fcff52935e5ef6d713a0f9342fef12eccfd9e9eff0" "1cf3f29294c5a3509b7eb3ff9e96f8e8db9d2d08322620a04d862e40dc201fe2" "764777857ef24b4ef1041be725960172ac40964b9f23a75894a578759ba6652f" "c377a5f3548df908d58364ec7a0ee401ee7235e5e475c86952dc8ed7c4345d8e" "8c5ffc9848db0f9ad4e296fa3cba7f6ea3b0e4e00e8981a59592c99d21f99471" "68769179097d800e415631967544f8b2001dae07972939446e21438b1010748c" "e4eaeb23c81fd6c6b1796b823dbec0129d828e13da89a222901a758348db57fd" "50ceca952b37826e860867d939f879921fac3f2032d8767d646dd4139564c68a" "e9a1226ffed627ec58294d77c62aa9561ec5f42309a1f7a2423c6227e34e3581" "d24e10524bb50385f7631400950ba488fa45560afcadd21e6e03c2f5d0fad194" "84c93dd294de8d877259fe2a4ab6540aaadbba3fbeb466692187f7a265c41203" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "39211540c554d4d11d505a96c6baa04d43b03c04c8c3bf55f6409192936bd754" "06f5145c01ec774a0abb49eeffa3980743ce2f997112b537effeb188b7c51caf" "47d2a01f2cbd853ccd1eddcb0e9e4fdfdabcc97ddad1d1a5218304294889f731" "085b401decc10018d8ed2572f65c5ba96864486062c0a2391372223294f89460" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "5cb805901c33a175f7505c8a8b83c43c39fb84fbae4e14cfb4d1a6c83dabbfba" "e9680c4d70f1d81afadd35647e818913da5ad34917f2c663d12e737cdecd2a77" "159bb8f86836ea30261ece64ac695dc490e871d57107016c09f286146f0dae64" "fca8ce385e5424064320d2790297f735ecfde494674193b061b9ac371526d059" "e60c82f43f96935aaff6387fc270b2011d40543c5fc2ba70c2c3038e0d8a6e81" "81805c86e126018f339211bb3f03e1c9eae30adfbe72832bd02f89ca0cbe5885" "f03970e52d0b3072e39439456ef3279ca71b88847a0992d517afaee83fc01488" "7511ae742ae5e87bc096db346ab4694c1042a4a6035d7d15f4b86b4f2213c8d8" "9f5fe6191b981ce29a2b4f8e4dbcefef7dd33b292d80c620f754be174efa9d58" "5debeb813b180bd1c3756306cd8c83ac60fda55f85fb27249a0f2d55817e3cab" "117284df029007a8012cae1f01c3156d54a0de4b9f2f381feab47809b8a1caef" "e6fca0aa3f94451ed1fc06b1f022ded9f4a20ad5bd64e14fc568cd73b7cd1e49" "86adc18aa6fb3ea0a801831f7b0bc88ed5999386" "0174d99a8f1fdc506fa54403317072982656f127" "5600dc0bb4a2b72a613175da54edb4ad770105aa" "83653b68e5a1c1184e90b3433dd1ffc0da65f517" default)))
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9")
 '(foreground-color "#52676f")
 '(fringe-mode 4 nil (fringe))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#F2F2F2" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#6DA8D2" . 50) ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#F2F2F2" . 100))))
 '(main-line-color1 "#1e1e1e")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(show-paren-mode t)
 '(syslog-debug-face (quote ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face (quote ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face (quote ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face (quote ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(tool-bar-mode nil)
 '(transient-mark-mode (quote (only . t)))
 '(vc-annotate-background "#d4d4d4")
 '(vc-annotate-color-map (quote ((20 . "#437c7c") (40 . "#336c6c") (60 . "#205070") (80 . "#2f4070") (100 . "#1f3060") (120 . "#0f2050") (140 . "#a080a0") (160 . "#806080") (180 . "#704d70") (200 . "#603a60") (220 . "#502750") (240 . "#401440") (260 . "#6c1f1c") (280 . "#935f5c") (300 . "#834744") (320 . "#732f2c") (340 . "#6b400c") (360 . "#23733c"))))
 '(vc-annotate-very-old-color "#23733c")
 '(weechat-color-list (quote (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((((class color) (min-colors 89)) (:foreground "#657b83" :background "#eee8d5" :box (:line-width -1 :style released-button))))))

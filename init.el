; enable Common Lisp support
;(require 'cl)

; some modes need to call stuff on the exec-path
(push "/usr/local/bin" exec-path)

; Start in server mode
(server-start)

; add directories to the load path
;; (add-to-list 'load-path "~/.emacs.d")
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

;; set-up scratch
(setq initial-major-mode 'ruby-mode)
(setq initial-scratch-message "# This buffer is for notes you don't want to save, and for Ruby evaluation.
# If you want to create a file, visit that file with C-x C-f,
# then enter the text in that file's own buffer.")

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

;; Set modes to turn off linum
(require 'linum-off)

; show column number in bar
(column-number-mode t)
; highlight URLs in comments/strings
(add-hook 'find-file-hooks 'goto-address-prog-mode)

; selection
(delete-selection-mode t)

; show marks as selections
(setq transient-mark-mode t)

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
(global-set-key (kbd "<f13>") 'xmp)

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
(global-set-key (kbd "s-T") 'anything-exuberant-ctags-select)
(global-set-key (kbd "s-t") 'anything-git-goto)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)

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

;; Dired edit file at point
;;(define-key dired-mode-map [f2] 'dired-efap)
;;(define-key dired-mode-map [down-mouse-1] 'dired-efap-click)


(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "jk" 'ace-jump-char-mode)
(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-define-global "xx" 'execute-extended-command)
(key-chord-define-global "yy" 'popup-kill-ring)
(key-chord-define-global "bb" 'psw-switch-buffer)

(key-chord-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Shell Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; more bash-like autocomplete
;; (setq eshell-cmpl-cycle-completions nil)

; automatically save history
;; (setq eshell-save-history-on-exit t)

; ignore version control directories when autocompleting
;; (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

; can't write over prompt, that would be weird
(setq comint-prompt-read-only)

;; scroll to bottom on output, more like a terminal
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

;; Haml because they make me
(require 'haml-mode)
(add-hook 'haml-mode-hook 'flymake-haml-load)

;; Minimap like SublimeText2
(require 'minimap)
(global-set-key (kbd "C-c m") 'minimap-toggle)

;; Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Keyboard Shortcuts
(vendor 'textmate)
(textmate-mode)

;; Winner mode
(winner-mode 1)

;; popup-switcher
(setq psw-in-window-center t)

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
;; (require 'sr-speedbar)
;; (global-set-key (kbd "s-s") 'sr-speedbar-toggle)
;; ;; show all files
;; (setq speedbar-show-unknown-files t)

;; turn off the ugly icons
(setq speedbar-use-images nil)

;; left-side pane
(setq sr-speedbar-right-side nil)

;; don't refresh on buffer changes
(setq sr-speedbar-auto-refresh t)

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
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'rubocop-mode)

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

;; Smartparens highlight
(show-smartparens-global-mode +1)
(require 'smartparens-ruby)


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

;; flyspell
;; (setq flyspell-issue-message-flg nil)
;; (add-hook 'ruby-mode-hook
;;           (lambda () (flyspell-prog-mode)))

;; Deft (for notes)
(require 'deft)
(setq deft-extension "txt")
(setq deft-directory "~/Dropbox/notes")

;; Anything Modes
(require 'anything-complete)
(require 'anything-exuberant-ctags)

;; YAML hooks
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; Shell mode
(add-to-list 'auto-mode-alist '("aliases" . shell-script-mode))
(add-to-list 'auto-mode-alist '("config" . shell-script-mode))
(add-to-list 'auto-mode-alist '("env" . shell-script-mode))
(add-to-list 'auto-mode-alist '("specific" . shell-script-mode))

;; Go Mode
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))

;; Git Gutter Mode Fringe
(global-git-gutter+-mode 1)


;; some github themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/base16")

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
 '(compilation-message-face (quote default))
 '(cursor-color "#52676f")
 '(custom-enabled-themes (quote (twilight-bright)))
 '(custom-safe-themes t)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9")
 '(foreground-color "#52676f")
 '(fringe-mode 4 nil (fringe))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#F2F2F2" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#6DA8D2" . 50) ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#F2F2F2" . 100))))
 '(magit-use-overlays nil)
 '(main-line-color1 "#1e1e1e")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111")
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
 )
(put 'upcase-region 'disabled nil)

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

(set-frame-font "PT Mono-12")

; don't display startup message
(setq inhibit-startup-message t)

; no scrollbar
;; (scroll-bar-mode -1)
(scroll-bar-mode)

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


; search with ack
(global-set-key (kbd "s-F") 'ack)

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

; run Ruby tests, TextMate-style
(add-hook 'rinari-minor-mode-hook
  (lambda ()
    (define-key rinari-minor-mode-map (kbd "s-r") 'rinari-test)))

; magit
(global-set-key (kbd "C-c i") 'magit-status)
(global-set-key (kbd "<f5>") 'magit-status)

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

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))
(setenv "GEM_HOME" "/Users/mranallo/.gem/ruby/1.9.3")

; colorful shell
(require 'ansi-color)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; escape the shell
;; (add-hook 'eshell-mode-hook
  ;; '(lambda nil (local-set-key "\C-u" 'eshell-kill-input)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ack-and-a-half     20130103.... installed  Yet another front-end for ack [source: github]
;; anti-zenburn-theme 20130110.... installed  Low-contrast Zenburn-inverted theme [source: github]
;; anything           1.287        installed  open anything / QuickSilver-like candidate-selection framework
;; anything-complete  1.86         installed  completion with anything
;; anything-exuber... 0.1.2        installed  Exuberant ctags anything.el interface
;; anything-git       1.1.1        installed  Show git files in anything
;; anything-git-goto  0.1.0        installed  Quick listing of:
;; anything-match-... 1.27         installed  Humane match plug-in for anything
;; anything-obsolete  0.1          installed  obsolete functions of anything
;; browse-kill-ring   20130122.... installed  interactively insert items from kill-ring [source: github]
;; coffee-mode        20130125.... installed  Major mode to edit CoffeeScript files in Emacs [source: github]
;; ctags-update       20121219.... installed  (auto) update TAGS in parent directory using exuberant-ctags [source: github]
;; eieio              1.4          installed  Enhanced Implememntation of Emacs Interpreted Objects
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
;; logito             20120225.... installed  logging library for Emacs [source: github]
;; magit              20130123.... installed  Control Git from Emacs. [source: github]
;; monokai-theme      0.0.8        installed  REQUIRES EMACS 24: Monokai Color Theme for Emacs.
;; pcache             20120408.... installed  persistent caching for Emacs [source: github]
;; pivotal-tracker    20120403.... installed  Interact with Pivotal Tracker through its API [source: github]
;; rspec-mode         20121224.7   installed  Enhance ruby-mode for RSpec [source: github]
;; ruby-mode          20121202.... installed  Major mode for editing Ruby files [source: svn]
;; ruby-tools         20121008.... installed  Collection of handy functions for ruby-mode. [source: github]
;; scala-mode         20121205.... installed  No description available. [source: github]
;; smart-tab          20120409.940 installed  Intelligent tab completion and indentation. [source: github]
;; smex               20120915.... installed  M-x interface with Ido-style fuzzy matching. [source: github]
;; solarized-theme    20130108.651 installed  The Solarized color theme, ported to Emacs. [source: github]
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
;; zenburn-theme      20130128.... installed  A low contrast color theme for Emacs. [source: github]


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
(vendor 'rinari)
(setq rinari-tags-file-name "tags")

;; Haml because they make me
(vendor 'haml-mode)
(add-hook 'haml-mode-hook 'flymake-haml-load)

;; Minimap like SublimeText2
(vendor 'minimap)

;; Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Keyboard Shortcuts
(vendor 'textmate)
(textmate-mode)

;; Winner mode
(winner-mode 1)

;; Centered Cursor mode
(vendor 'centered-cursor-mode)
(global-centered-cursor-mode t)

;; kill ring browsing
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; dired
(vendor 'dired+)
(setq dired-recursive-deletes 'top)
(put 'dired-find-alternate-file 'disabled nil)

;; sr-speedbar
(vendor 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; rcodetools
(vendor 'rcodetools)

;; RHTML mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/rhtml-mode")
;; (require 'rhtml-mode)
(vendor 'rhtml-mode)
(add-hook 'rhtml-mode-hook
     	  (lambda () (rinari-launch)))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))

;; Ack
(require 'ack-and-a-half)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

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

;; smart-tab
(require 'smart-tab)
(add-to-list 'hippie-expand-try-functions-list
             'yas/hippie-try-expand) ;put yasnippet in hippie-expansion list
(global-smart-tab-mode 1)
(setq smart-tab-using-hippie-expand nil)

;; YA Snippets
(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/vendor/snippets")
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
(vendor 'ruby-end)

;; Deft (for notes)
(vendor 'deft)
(setq deft-extension "txt")
(setq deft-directory "~/Dropbox/notes")

;; Powerline https://github.com/jonathanchu/emacs-powerline
(vendor 'powerline)
(setq powerline-arrow-shape 'arrow)
(setq powerline-color1 "grey22")
(setq powerline-color2 "honeydew4")

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(ansi-term-color-vector [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"])
 '(background-color "#fcf4dc")
 '(background-mode light)
 '(cursor-color "#52676f")
 '(custom-enabled-themes (quote (twilight-bright)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "5cb805901c33a175f7505c8a8b83c43c39fb84fbae4e14cfb4d1a6c83dabbfba" "e9680c4d70f1d81afadd35647e818913da5ad34917f2c663d12e737cdecd2a77" "159bb8f86836ea30261ece64ac695dc490e871d57107016c09f286146f0dae64" "fca8ce385e5424064320d2790297f735ecfde494674193b061b9ac371526d059" "e60c82f43f96935aaff6387fc270b2011d40543c5fc2ba70c2c3038e0d8a6e81" "81805c86e126018f339211bb3f03e1c9eae30adfbe72832bd02f89ca0cbe5885" "f03970e52d0b3072e39439456ef3279ca71b88847a0992d517afaee83fc01488" "7511ae742ae5e87bc096db346ab4694c1042a4a6035d7d15f4b86b4f2213c8d8" "9f5fe6191b981ce29a2b4f8e4dbcefef7dd33b292d80c620f754be174efa9d58" "5debeb813b180bd1c3756306cd8c83ac60fda55f85fb27249a0f2d55817e3cab" "117284df029007a8012cae1f01c3156d54a0de4b9f2f381feab47809b8a1caef" "e6fca0aa3f94451ed1fc06b1f022ded9f4a20ad5bd64e14fc568cd73b7cd1e49" "86adc18aa6fb3ea0a801831f7b0bc88ed5999386" "0174d99a8f1fdc506fa54403317072982656f127" "5600dc0bb4a2b72a613175da54edb4ad770105aa" "83653b68e5a1c1184e90b3433dd1ffc0da65f517" default)))
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9")
 '(foreground-color "#52676f"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "grey75" :foreground "black" :box nil :family "PT Mono")))))

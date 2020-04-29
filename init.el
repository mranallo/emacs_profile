;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

; Start in server mode
(server-start)

; add directories to the load path
;; (add-to-list 'load-path "~/.emacs.d")
;; (add-to-list 'load-path "~/.emacs.d/utilities")
(add-to-list 'load-path "~/.emacs.d/vendor")

; handy function to load all elisp files in a directory
(load-file "~/.emacs.d/utilities/utilities.elc")

(add-hook 'emacs-lisp-mode-hook '(lambda ()
  (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))
)


;; Title bar
(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

;;;;;;;;;;;;;;;;;;;;;;; Cask ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;;;;;;;;;;;;;;;;;;;;;;; ELPA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

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
(setq initial-major-mode 'text-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(setq initial-scratch-message "# This buffer is for notes you don't want to save, and for Ruby evaluation.
# If you want to create a file, visit that file with C-x C-f,
# then enter the text in that file's own buffer.")

; tabs
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

; mousing
(setq mac-emulate-three-button-mouse t)

; encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

; whitespace
;; (global-whitespace-mode t)
;; (setq show-trailing-whitespace t)
;; (add-hook 'before-save-hook 'whitespace-cleanup)

;; line numbering
;; (global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

; show column number in bar
(column-number-mode t)

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

; intelligently clean up whitespace
(global-whitespace-cleanup-mode t)

;;;;;;;;;;;;;;;;;;;; UI Preferences ;;;;;;;;;;;;;;;;;;;;;;;

; don't display startup message
(setq inhibit-startup-message t)

; no scrollbar
(scroll-bar-mode -1)

; no toolbar
(tool-bar-mode -1)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

; blink cursor
(blink-cursor-mode t)

; highlight current line
(global-hl-line-mode t)

; force new frames into existing window
(setq ns-pop-up-frames nil)

; no bell
(setq ring-bell-function 'ignore)

;; Cursor type
(setq-default cursor-type 'box)

;; Titlebar 
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

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

;; (setq ag-highlight-search t)
(global-set-key (kbd "s-F") 'counsel-projectile-ag)

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
(global-set-key (kbd "<f6>") 'magit-blame)

;; This below is from my previous bindigs file

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'counsel-find-file)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "s-t") 'counsel-projectile)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-f") 'swiper)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-x n") (lambda() (interactive) (find-file "/Users/mranallo/Library/Mobile Documents/iCloud~co~noteplan~NotePlan/Documents/Calendar")))

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Duplicate line
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

;; Sane Term
(global-set-key (kbd "C-x t") 'sane-term)
(global-set-key (kbd "C-x T") 'sane-term-create)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Quickly jump to init file
(global-set-key (kbd "C-c I") 'find-user-init-file)

;; Dired edit file at point
;;(define-key dired-mode-map [f2] 'dired-efap)
;;(define-key dired-mode-map [down-mouse-1] 'dired-efap-click)


(key-chord-define-global "jj" 'avy-goto-word-0)
(key-chord-define-global "jl" 'avy-goto-line)
(key-chord-define-global "jk" 'avy-goto-char)
(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-define-global "xx" 'execute-extended-command)
(key-chord-define-global "yy" 'counsel-yank-pop)
(key-chord-define-global "bb" 'counsel-switch-buffer)
(key-chord-define-global "rr" 'counsel-buffer-or-recentf)
(key-chord-define-global "ww" 'ace-window)
(key-chord-define-global "dd" 'dumb-jump-go)
(key-chord-define-global "``" 'eshell-toggle)

(key-chord-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Shell Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; more bash-like autocomplete
(setq eshell-cmpl-cycle-completions nil)

; automatically save history
(setq eshell-save-history-on-exit t)

; ignore version control directories when autocompleting
;; (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

; can't write over prompt, that would be weird
(setq comint-prompt-read-only t)

;; scroll to bottom on output, more like a terminal
;; (setq eshell-scroll-to-bottom-on-output t)
;; (setq eshell-scroll-show-maximum-output t)

;; Emoji support
(if window-system
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; Setting the correct $PATH
(setenv "PATH"
  (concat
   "/Users/mranallo/Code/gocode/bin" ":"
   "/usr/local/opt/go/libexec/bin" ":"
   "/usr/local/bin" ":"
   "/Users/mranallo/.rbenv/shims" ":"
   (getenv "PATH")
  )
  )

(setenv "PAGER" "cat")

(exec-path-from-shell-copy-env "GEM_HOME")
(exec-path-from-shell-copy-env "GEM_PATH")
(exec-path-from-shell-copy-env "GEM_ROOT")
(exec-path-from-shell-copy-env "RUBY_ROOT")
(exec-path-from-shell-copy-env "RUBY_ENGINE")
(exec-path-from-shell-copy-env "GOPATH")
(exec-path-from-shell-initialize)

;; Choose which eshell-funcs to enable
;; (setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))

; escape the shell
;; (add-hook 'eshell-mode-hook
;; '(lambda nil (local-set-key "\C-u" 'eshell-kill-input)))

(add-hook 'eshell-mode-hook (lambda ()
                              (add-to-list 'eshell-visual-commands "ssh")
                              (add-to-list 'eshell-visual-commands "tail")
                              (add-to-list 'eshell-visual-commands "top")))
;; eshell toggle
(global-set-key (kbd "s-`") 'eshell-toggle)

;; (load-file "~/.emacs.d/vendor/esh-custom/esh-custom.elc")
;; Enable the new eshell prompt
;; (setq eshell-prompt-function 'esh-prompt-func)
(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
(setq magit-last-seen-setup-instructions "1.4.0")

;; Ivy config
(all-the-icons-ivy-rich-mode 1)
(ivy-rich-mode 1)
(define-key ivy-minibuffer-map (kbd "<return>") #'ivy-alt-done)


(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x m") 'counsel-M-x)
(global-set-key (kbd "C-x C-m") 'counsel-M-x)
(global-set-key (kbd "M-X") 'counsel-M-x)

; search with ag
(setq counsel-ag-base-command "ag --nocolor --nogroup --ignore-case")

;; Persistant Scratch
(persistent-scratch-setup-default)

;; Haml because they make me
(require 'haml-mode)

;; Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)

;; pianobar
(autoload 'pianobar "pianobar" nil t)

;; Winner mode
(winner-mode 1)

;; popup-switcher
(setq psw-in-window-center t)

;; Centered Cursor mode
;; (require 'centered-cursor-mode)
;; (global-centered-cursor-mode t)

;; kill ring browsing
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; dired
(setq dired-recursive-deletes 'top)
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; rcodetools
(vendor 'rcodetools)

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(chef-foodcritic ruby-rubocop))
(setq flycheck-ruby-rubocop-executable "/Users/mranallo/.rbenv/shims/rubocop")
(setq flycheck-ruby-executable "/Users/mranallo/.rbenv/shims/ruby")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; Use json load flymake
(add-hook 'json-mode 'flymake-json-load)

;; change indent level for JSON
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; Ruby stuff
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; smart-tab
(require 'smart-tab)
;; (add-to-list 'hippie-expand-try-functions-list
;;              'yas/hippie-try-expand) ;put yasnippet in hippie-expansion list
(global-smart-tab-mode 1)
(setq smart-tab-using-hippie-expand t)

;; Smartparens highlight
(show-smartparens-global-mode +1)
(require 'smartparens-ruby)

;; flyspell
(setq flyspell-issue-message-flg nil)
(add-hook 'enh-ruby-mode-hook
          (lambda () (flyspell-prog-mode)))
(add-hook 'slim-mode-hook
          (lambda () (flyspell-prog-mode)))
(add-hook 'cfn-mode-hook
          (lambda () (flyspell-prog-mode)))
(global-set-key (kbd "<mouse-3>") 'flyspell-correct-word)

;; YAML hooks
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook #'display-line-numbers-mode)


;; Shell mode
(add-to-list 'auto-mode-alist '("aliases" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\config$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("env" . shell-script-mode))
(add-to-list 'auto-mode-alist '("specific" . shell-script-mode))

;; Dumb Jump
(use-package dumb-jump
  :config (setq dumb-jump-selector 'ivy)
  :ensure)

(defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))

(global-set-key (kbd "M-j") 'dumb-jump-hydra/body)

;; Go Mode
(require 'go-guru)
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))

;; Popwin
(use-package popwin
  :config
  (progn
    (setq popwin:special-display-config nil)
    (push '("*Backtrace*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.2)
          popwin:special-display-config)
    (push '("*compilation*"
            :dedicated t :position bottom :stick t :noselect t   :height 0.2)
          popwin:special-display-config)
    (push '("*Compile-Log*"
            :dedicated t :position bottom :stick t :noselect t   :height 0.2)
          popwin:special-display-config)
    (push '("*Flycheck errors*"
            :dedicated t :position bottom :stick t :noselect t   :height 0.2)
          popwin:special-display-config)
    (push '("^\\*docker-build-output:.*\\*$"
            :regexp t :dedicated t :position bottom :stick t :noselect t   :height 0.2  :tail t)
          popwin:special-display-config)
    (push '("*Help*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.2)
          popwin:special-display-config)
    (push '("*Shell Command Output*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.2)
          popwin:special-display-config)
    (push '(" *undo-tree*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.2)
          popwin:special-display-config)
    (push '("*Warnings*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.2)
          popwin:special-display-config)
    (push '("^\\*Man .*\\*$"
            :regexp t    :position bottom :stick t :noselect nil :height 0.2)
            popwin:special-display-config)
    (popwin-mode 1)))
(global-set-key (kbd "C-z") popwin:keymap)

;; Neotree
(global-set-key (kbd "s-\\") 'neotree-project-dir)
(doom-themes-neotree-config)

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;; SolaireMode
(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

;; DoomModeline
(doom-modeline-mode)

;; Git Gutter Mode Fringe
(global-git-gutter+-mode 1)

;; Projectile Settings
(projectile-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
;; (setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)
(setq projectile-switch-project-action 'neotree-projectile-action)


;; Expand Region http://emacsrocks.com/e09.html
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; show the cursor when moving after big movements in the window
(require 'beacon)
(beacon-mode +1)

;; show available keybindings after you start typing
(require 'which-key)
(which-key-mode +1)

;; Company Mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)

(require 'company-web-html)                          ; load company mode html backend
(require 'company-web-slim)                          ; load company mode slim backend
(require 'company-go)                                ; load company mode go backend
(require 'company-tabnine)                           ; Company tab-nine

(add-to-list 'company-backends 'company-go)
(add-to-list 'company-backends 'company-tabnine)

(add-hook 'eshell-mode-hook
      (lambda ()
        (set (make-local-variable 'company-backends)
             '((company-eshell-history)))))

(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
(setq company-idle-delay 0)                          ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(global-set-key (kbd "C-c /") 'company-files)        ; Force complete file names on "C-c /" key




(require 'compile)
;; Find root directory by searching for Gemfile
(defun* get-closest-gemfile-root (&optional (file "Gemfile"))
  (let ((root (expand-file-name "/")))
    (loop
     for d = default-directory then (expand-file-name ".." d)
     if (file-exists-p (expand-file-name file d))
     return d
     if (equal d root)
     return nil)))

;; Deft Mode
(global-set-key (kbd "C-x n") 'deft)
(setq deft-extensions '("txt"))
(setq deft-directory "/Users/mranallo/Library/Mobile Documents/iCloud~co~noteplan~NotePlan/Documents/Notes/")

;; Custom validate for CloudFormation
(defun cfn-validate-file ()
  ;; This calls the aws-cli to compile the current cloudformation template
  (interactive)
  (compile (format "cfn-lint -t %s"
                   (buffer-file-name))
           t))

(define-derived-mode cfn-mode yaml-mode
  "Cloudformation"
  "Cloudformation template mode.")

(add-to-list 'auto-mode-alist '("infrastructure/.*\\.yml$" . cfn-mode))
(after! flycheck
  (flycheck-define-checker cfn-lint
    "A Cloudformation linter using cfn-python-lint.

See URL 'https://github.com/awslabs/cfn-python-lint'."
    :command ("cfn-lint" "-f" "parseable" source)
    :error-patterns (
                     (warning line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "W" (one-or-more digit)) ":" (message) line-end)
                     (error line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "E" (one-or-more digit)) ":" (message) line-end)
                     )
    :modes (cfn-mode)
    )
    (add-to-list 'flycheck-checkers 'cfn-lint)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#D8DEE9" "#99324B" "#4F894C" "#9A7500" "#3B6EA8" "#97365B" "#398EAC" "#3B4252"])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(background-color "#fcf4dc")
 '(background-mode light)
 '(beacon-color "#d33682")
 '(column-number-mode t)
 '(company-quickhelp-use-propertized-text t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#52676f")
 '(cursor-type (quote box))
 '(custom-enabled-themes (quote (doom-nord-light)))
 '(custom-safe-themes t)
 '(deft-auto-save-interval 0.0)
 '(eshell-history-file-name "/Users/mranallo/.emacs.d/eshell/history")
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#AEBACF")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(foreground-color "#52676f")
 '(frame-background-mode (quote dark))
 '(fringe-mode 4 nil (fringe))
 '(global-centered-cursor-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#F2F2F2" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#6DA8D2" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#F2F2F2" . 100))))
 '(history-delete-duplicates t)
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-sexp-background-color "#efebe9")
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(json-reformat:indent-width 2)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(main-line-color1 "#1e1e1e")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(neo-theme (quote icons))
 '(neo-window-fixed-size t)
 '(neo-window-width 40)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (counsel-tramp tramp friendly-tramp-path whitespace-cleanup-mode counsel-dash ivy-emoji eshell-prompt-extras emojify company-shell crystal-mode textmate eshell-toggle flycheck-pos-tip company-quickhelp hydra dumb-jump smex sane-term counsel-projectile all-the-icons-ivy-rich all-the-icons-ivy ivy-rich counsel company-posframe flycheck-posframe popwin ag sass-mode solaire-mode doom-modeline doom-themes qsimpleq-theme color-theme-sanityinc-solarized atom-one-dark-theme enh-ruby-mode awscli-capf spacemacs-theme company-tabnine js2-mode prettier-js forge company-emoji dired-sidebar deft bury-successful-compilation unicode-fonts flyspell-lazy ess-smart-underscore rbenv presentation magit-popup package-build projectile s spaceline beacon which-key use-package use-package-chords all-the-icons-dired spaceline-all-the-icons go-eldoc company company-go groovy-mode nginx-mode markdown-mode dockerfile-mode color-theme-solarized web-mode undo-tree twilight-bright-theme twilight-anti-bright-theme smartparens rspec-mode pos-tip pcache pallet multiple-cursors magit lua-mode linum-off key-chord indent-guide ido-better-flex github-browse-file git-gutter-fringe+ free-keys flymake-sass flymake-ruby flymake-go flycheck expand-region es-lib editorconfig dired-efap dired+ company-web centered-cursor-mode browse-kill-ring blank-mode ace-jump-mode ace-jump-buffer)))
 '(pdf-view-midnight-colors (cons "#3B4252" "#E5E9F0"))
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111")
 '(rm-blacklist
   (quote
    (" hl-p" " mate" " MRev" " Undo-Tree" " GitGutter" " Helm" " Smrt")))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(spaceline-all-the-icons-clock-always-visible nil)
 '(spaceline-all-the-icons-eyebrowse-display-name t)
 '(spaceline-all-the-icons-hide-long-buffer-path t)
 '(spaceline-all-the-icons-icon-set-git-ahead (quote commit))
 '(spaceline-all-the-icons-icon-set-modified (quote chain))
 '(spaceline-all-the-icons-icon-set-vc-icon-git (quote git-name))
 '(spaceline-all-the-icons-icon-set-window-numbering (quote square))
 '(spaceline-all-the-icons-primary-separator "|")
 '(spaceline-all-the-icons-separator-type (quote arrow))
 '(spaceline-all-the-icons-window-number-always-visible t)
 '(spaceline-buffer-id-max-length 25)
 '(spaceline-info-mode t)
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(vc-annotate-background "#E5E9F0")
 '(vc-annotate-color-map
   (list
    (cons 20 "#4F894C")
    (cons 40 "#688232")
    (cons 60 "#817b19")
    (cons 80 "#9A7500")
    (cons 100 "#a0640c")
    (cons 120 "#a65419")
    (cons 140 "#AC4426")
    (cons 160 "#a53f37")
    (cons 180 "#9e3a49")
    (cons 200 "#97365B")
    (cons 220 "#973455")
    (cons 240 "#983350")
    (cons 260 "#99324B")
    (cons 280 "#a0566f")
    (cons 300 "#a87b93")
    (cons 320 "#b0a0b6")
    (cons 340 "#AEBACF")
    (cons 360 "#AEBACF")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "nil" :slant normal :weight thin :height 130 :width normal))))
 '(helm-header ((t (:inherit header-line))))
 '(helm-source-header ((t (:background "gray66" :foreground "black" :weight bold :height 1.0))))
 '(spaceline-all-the-icons-sunrise-face ((t (:inherit powerline-active2 :foreground "#f6c175"))))
 '(spaceline-highlight-face ((t (:background "light blue" :foreground "#3E3D31" :inherit (quote mode-line)))))
 '(spaceline-unmodified ((t (:inherit (quote mode-line) :background "light blue" :foreground "#3E3D31"))))
 '(term-color-blue ((t (:background "SteelBlue2" :foreground "SteelBlue2"))))
 '(term-color-cyan ((t (:background "PaleTurquoise3" :foreground "PaleTurquoise3"))))
 '(term-color-green ((t (:background "light green" :foreground "light green"))))
 '(term-color-magenta ((t (:background "pink1" :foreground "pink1"))))
 '(term-color-red ((t (:background "indian red" :foreground "indian red"))))
 '(term-color-yellow ((t (:background "LightYellow3" :foreground "LightYellow3"))))
 '(tooltip ((t (:background "#C2D0E7" :foreground "#3B4252" :height 130 :family "JetBrains Mono")))))
(put 'upcase-region 'disabled nil)

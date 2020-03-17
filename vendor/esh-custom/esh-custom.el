(require 'dash)
(require 's)

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

(defmacro esh-section (NAME ICON FORM &rest PROPS)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  `(setq ,NAME
         (lambda () (when ,FORM
                 (-> ,ICON
                    (concat esh-section-delim ,FORM)
                    (with-face ,@PROPS))))))

(defun esh-acc (acc x)
  "Accumulator for evaluating and concatenating esh-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (concat acc esh-sep it))
    acc))

(defun esh-prompt-func ()
  "Build `eshell-prompt-function'"
  (concat esh-header
          (-reduce-from 'esh-acc "" eshell-funcs)
          ""
          eshell-prompt-string))

(esh-section esh-dir
             "\xf07c"  ;  (faicon folder)
             (abbreviate-file-name (eshell/pwd))
             '(:foreground "SteelBlue4" :underline t))

(esh-section esh-git
             "\xe907"  ;  (git icon)
             (magit-get-current-branch)
             '(:foreground "SlateGray4"))

(esh-section esh-clock
             "\xf017"  ;  (clock icon)
             (format-time-string "%H:%M" (current-time))
             '(:foreground "forest green"))

;; Below I implement a "prompt number" section
;; (setq esh-prompt-num 0)
;; (add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
;; (advice-add 'eshell-send-input :before
;;             (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))

;; (esh-section esh-num
;;              "\xf0c9"  ;  (list icon)
;;              (number-to-string esh-prompt-num)
;;              '(:foreground "brown"))

;; Separator between esh-sections
(setq esh-sep " ")  ; or " | "

;; Separator between an esh-section icon and form
(setq esh-section-delim " ")

;; Eshell prompt header
(setq esh-header " ")  ; or "\n┌─"

;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
;; your login, these can be the same.
(setq eshell-prompt-regexp " ⚡️ ")   ; or "└─> "
(setq eshell-prompt-string " ⚡️ ")   ; or "└─> "

;; Choose which eshell-funcs to enable
(setq eshell-funcs (list esh-dir esh-git))

;; Turn off the default prompt.
(setq eshell-highlight-prompt nil)

;; Make C-a go to beginning of command instead of line
(add-hook 'eshell-mode-hook '(lambda ()
                               (local-set-key (kbd "C-a")
                                              '(lambda ()
                                                 (interactive)
                                                 (beginning-of-line)
                                                 (search-forward-regexp eshell-prompt-regexp)))))

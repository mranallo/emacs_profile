;;; dired-efap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (dired-efap dired-efap-click) "dired-efap" "dired-efap.el"
;;;;;;  (21216 203 0 0))
;;; Generated autoloads from dired-efap.el

(autoload 'dired-efap-click "dired-efap" "\
Move to the point and, if needed, edit filename at point.

Depending of the value of `dired-efap-use-mouse', if EVENT is a
doubleclick, and the previous position of the point, edit
filename at point.

See `dired-efap-use-mouse' and `dired-efap'

\(fn EVENT)" t nil)

(autoload 'dired-efap "dired-efap" "\
Make the filename at point editable by user.
Press RET to actually rename the file or directory in disk, and
C-g to abort.

If FROM-MOUSE is not nil, the mode is being set because of a mouse event.

\(fn &optional FROM-MOUSE)" t nil)

;;;***

;;;### (autoloads nil nil ("dired-efap-pkg.el") (21216 203 500762
;;;;;;  0))

;;;***

(provide 'dired-efap-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-efap-autoloads.el ends here

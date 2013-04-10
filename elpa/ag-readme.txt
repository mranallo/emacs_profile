This file is heavily based on the excellent ack-and-a-half.el.

Usage:

Add you to your .emacs.d:

(add-to-list 'load-path "/path/to/ag.el") ;; optional
(require 'ag)

Alternatively, just install the package from MELPA.

If you're using ag 0.14+, which supports --color-match, then you
can add highlighting with:

(setq ag-highlight-search t)

I like to bind the *-at-point commands to F5 and F6:

(global-set-key (kbd "<f5>") 'ag-project-at-point)
(global-set-key (kbd "<f6>") 'ag-regexp-project-at-point)

;;; bm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "bm" "bm.el" (23046 44974 857361 613000))
;;; Generated autoloads from bm.el

(autoload 'bm-toggle "bm" "\
Toggle bookmark at point.

\(fn)" t nil)

(autoload 'bm-toggle-mouse "bm" "\
Toggle a bookmark with a mouse click.
EV is the mouse event.

\(fn EV)" t nil)

(autoload 'bm-lifo-previous "bm" "\
Goto previous bookmark in LIFO order . (that is, most
recently set ones come first, oldest ones come last)

\(fn)" t nil)

(autoload 'bm-lifo-next "bm" "\
Goto next bookmark in LIFO order .(that is, most
recently set ones come first, oldest ones come last)

\(fn)" t nil)

(autoload 'bm-next "bm" "\


\(fn)" t nil)

(autoload 'bm-common-next "bm" "\
Goto next bookmark.

\(fn)" t nil)

(autoload 'bm-next-mouse "bm" "\
Go to the next bookmark with the scroll wheel.
EV is the mouse event.

\(fn EV)" t nil)

(autoload 'bm-previous "bm" "\


\(fn)" t nil)

(autoload 'bm-common-previous "bm" "\
Goto previous bookmark.

\(fn)" t nil)

(autoload 'bm-previous-mouse "bm" "\
Go to the previous bookmark with the scroll wheel.
EV is the mouse event.

\(fn EV)" t nil)

;;;***

;;;### (autoloads nil nil ("bm-pkg.el" "bm-sync.el") (23046 44974
;;;;;;  857361 613000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; bm-autoloads.el ends here

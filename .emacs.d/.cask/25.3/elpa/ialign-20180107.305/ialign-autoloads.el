;;; ialign-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ialign" "ialign.el" (23126 22356 86669 834000))
;;; Generated autoloads from ialign.el

(autoload 'ialign "ialign" "\
Interactively align region BEG END using regexp read from minibuffer.
As characters are typed in the minibuffer, the region is aligned
using `align-regexp' and the result is presented to the user.
\\<ialign-minibuffer-keymap>
If the custom option `ialign-auto-update' is not set to t, manual update is
possible with command `ialign-update' bound to \\[ialign-update].

Additional arguments passed to `align-regexp' are displayed in
the minibuffer prompt and all of them can be interactively
specified.  The parenthesis group argument can be changed using
\\[ialign-decrement-group], \\[ialign-increment-group] and \\[ialign-set-group], the spacing can be modified using
\\[ialign-decrement-spacing], \\[ialign-increment-spacing] and \\[ialign-set-spacing].

The keymap used in minibuffer is `ialign-minibuffer-keymap':
\\{ialign-minibuffer-keymap}

\(fn BEG END)" t nil)

(define-obsolete-function-alias 'ialign-interactive-align 'ialign "0.1.0")

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ialign-autoloads.el ends here

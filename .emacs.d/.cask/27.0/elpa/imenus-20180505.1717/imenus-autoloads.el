;;; imenus-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "imenus" "imenus.el" (0 0 0 0))
;;; Generated autoloads from imenus.el

(autoload 'imenus "imenus" "\
Prompt for a place from a list of BUFFERS and jump to it.
Interactively, use the current buffer.  With a prefix argument,
prompt for multiple buffers.

In a minibuffer prompt you may use the following commands:
\\{imenus-minibuffer-map}

\(fn BUFFERS)" t nil)

(autoload 'imenus-mode-buffers "imenus" "\
Perform `imenus' on all buffers with MODE.
Interactively, use the major mode of the current buffer.

\(fn MODE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "imenus" '("imenus-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; imenus-autoloads.el ends here

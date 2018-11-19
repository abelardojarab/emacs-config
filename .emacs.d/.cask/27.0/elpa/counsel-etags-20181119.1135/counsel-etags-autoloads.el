;;; counsel-etags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "counsel-etags" "counsel-etags.el" (0 0 0 0))
;;; Generated autoloads from counsel-etags.el

(autoload 'counsel-etags-guess-program "counsel-etags" "\
Guess path from its EXECUTABLE-NAME on Windows.
Return nil if it's not found.

\(fn EXECUTABLE-NAME)" nil nil)

(autoload 'counsel-etags-version "counsel-etags" "\


\(fn)" nil nil)

(autoload 'counsel-etags-get-hostname "counsel-etags" "\
Reliable way to get current hostname.
`(getenv \"HOSTNAME\")' won't work because $HOSTNAME is NOT an
 environment variable.
`system-name' won't work because /etc/hosts could be modified

\(fn)" nil nil)

(autoload 'counsel-etags-directory-p "counsel-etags" "\
Does directory of current file match REGEX?

\(fn REGEX)" nil nil)

(autoload 'counsel-etags-filename-p "counsel-etags" "\
Does current file match REGEX?

\(fn REGEX)" nil nil)

(autoload 'counsel-etags-update-tags-force "counsel-etags" "\
Update tags file now using default implementation.

\(fn)" t nil)

(autoload 'counsel-etags-scan-code "counsel-etags" "\
Use Ctags to scan code at DIR.

\(fn &optional DIR)" t nil)

(autoload 'counsel-etags-list-tag "counsel-etags" "\
List all tags.

\(fn)" t nil)

(autoload 'counsel-etags-find-tag "counsel-etags" "\
Find tag in two step.
Step 1, user need input regex to fuzzy match tag.
Any tag whose sub-string matches regex will be listed.

Step 2, user could filter tags.

\(fn)" t nil)

(autoload 'counsel-etags-find-tag-at-point "counsel-etags" "\
Find tag using tagname at point.

\(fn)" t nil)

(autoload 'counsel-etags-recent-tag "counsel-etags" "\
Find tag using tagname from `counsel-etags-tag-history'.

\(fn)" t nil)

(autoload 'counsel-etags-virtual-update-tags "counsel-etags" "\
Scan code and create tags file again.
It's the interface used by other hooks or commands.
the tags updating might not happen.

\(fn)" t nil)

(autoload 'counsel-etags-grep "counsel-etags" "\
Grep at project root directory or current directory.
Try to find best grep program (ripgrep, grep...) automatically.
Extended regex like (pattern1|pattern2) is used.
If DEFAULT-KEYWORD is not nil, it's used as grep keyword.
If HINT is not nil, it's used as grep hint.

\(fn &optional DEFAULT-KEYWORD HINT)" t nil)

(autoload 'counsel-etags-grep-symbol-at-point "counsel-etags" "\
Similar to `counsel-etags-grep' but grep symbol at point.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "counsel-etags" '("counsel-etags-")))

;;;***

;;;### (autoloads nil "counsel-etags-javascript" "counsel-etags-javascript.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from counsel-etags-javascript.el

(autoload 'counsel-etags-javascript-collect "counsel-etags-javascript" "\
Get CONTEXT before finding tag definition.

\(fn)" nil nil)

(autoload 'counsel-etags-javascript-predicate "counsel-etags-javascript" "\
Use CONTEXT to test CANDIDATE.  If return nil, the CANDIDATE is excluded.

\(fn CONTEXT CANDIDATE)" nil nil)

;;;***

;;;### (autoloads nil "counsel-etags-sdk" "counsel-etags-sdk.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from counsel-etags-sdk.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "counsel-etags-sdk" '("counsel-etags-sdk-")))

;;;***

;;;### (autoloads nil nil ("counsel-etags-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; counsel-etags-autoloads.el ends here

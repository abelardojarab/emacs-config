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
Return version." nil nil)

(autoload 'counsel-etags-get-hostname "counsel-etags" "\
Reliable way to get current hostname.
`(getenv \"HOSTNAME\")' won't work because $HOSTNAME is NOT an
 environment variable.
`system-name' won't work because /etc/hosts could be modified" nil nil)

(autoload 'counsel-etags-async-shell-command "counsel-etags" "\
Execute string COMMAND and create TAGS-FILE asynchronously.

\(fn COMMAND TAGS-FILE)" nil nil)

(autoload 'counsel-etags-scan-dir-internal "counsel-etags" "\
Create tags file from SRC-DIR.

\(fn SRC-DIR)" nil nil)

(autoload 'counsel-etags-directory-p "counsel-etags" "\
Does directory of current file match REGEX?

\(fn REGEX)" nil nil)

(autoload 'counsel-etags-filename-p "counsel-etags" "\
Does current file match REGEX?

\(fn REGEX)" nil nil)

(autoload 'counsel-etags-find-tag-name-default "counsel-etags" "\
Find tag at point." nil nil)

(autoload 'counsel-etags-word-at-point "counsel-etags" "\
Get word at point.  PREDICATE should return t on testing word character.

For example, get a word when dot character is part of word,

   (counsel-etags-word-at-point (lambda (c)
                                  (or (= c ?.)
                                      (and (>= c ?0) (<= c ?9))
                                      (and (>= c ?A) (<= c ?Z))
                                      (and (>= c ?a) (<= c ?z)))))

\(fn PREDICATE)" nil nil)

(autoload 'counsel-etags-scan-code "counsel-etags" "\
Use Ctags to scan code at DIR.

\(fn &optional DIR)" t nil)

(autoload 'counsel-etags-list-tag "counsel-etags" "\
List all tags.  Tag is fuzzy and case insensitively matched." t nil)

(autoload 'counsel-etags-imenu-default-create-index-function "counsel-etags" "\
Create an index alist for the definitions in the current buffer." nil nil)

(autoload 'counsel-etags-list-tag-in-current-file "counsel-etags" "\
List tags in current file." t nil)

(autoload 'counsel-etags-find-tag "counsel-etags" "\
Find tag in two step.
Step 1, user need input regex to fuzzy and case insensitively match tag.
Any tag whose sub-string matches regex will be listed.

Step 2, user keeps filtering tags." t nil)

(autoload 'counsel-etags-find-tag-at-point "counsel-etags" "\
Find tag using tagname at point.
Please note parsing tags file containing line with 2K characters could be slow.
That's the known issue of Emacs Lisp.  The program itself is perfectly fine." t nil)

(autoload 'counsel-etags-recent-tag "counsel-etags" "\
Find tag using tagname from `counsel-etags-tag-history'." t nil)

(autoload 'counsel-etags-virtual-update-tags "counsel-etags" "\
Scan code and create tags file again.
It's the interface used by other hooks or commands.
The tags updating might not happen." t nil)

(autoload 'counsel-etags-grep "counsel-etags" "\
Grep at project root directory or current directory.
Try to find best grep program (ripgrep, grep...) automatically.
Extended regex like (pattern1|pattern2) is used.
If DEFAULT-KEYWORD is not nil, it's used as grep keyword.
If HINT is not nil, it's used as grep hint.
ROOT is root directory to grep.

\(fn &optional DEFAULT-KEYWORD HINT ROOT)" t nil)

(autoload 'counsel-etags-grep-current-directory "counsel-etags" "\
Grep current directory or LEVEL up parent directory.

\(fn &optional LEVEL)" t nil)

(autoload 'counsel-etags-update-tags-force "counsel-etags" "\
Update current tags file using default implementation.
If FORCED-TAGS-FILE is nil, the updating process might now happen.

\(fn &optional FORCED-TAGS-FILE)" t nil)

(autoload 'counsel-etags-tag-line "counsel-etags" "\
One line in tag file using CODE, TAG-NAME, LINE-NUMBER, and BYTE-OFFSET.

\(fn CODE TAG-NAME LINE-NUMBER &optional BYTE-OFFSET)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "counsel-etags" '("counsel-etags-")))

;;;***

;;;### (autoloads nil "counsel-etags-javascript" "counsel-etags-javascript.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from counsel-etags-javascript.el

(autoload 'counsel-etags-javascript-collect "counsel-etags-javascript" "\
Get CONTEXT before finding tag definition." nil nil)

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

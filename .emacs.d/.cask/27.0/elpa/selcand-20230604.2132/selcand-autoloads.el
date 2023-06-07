;;; selcand-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "selcand" "selcand.el" (0 0 0 0))
;;; Generated autoloads from selcand.el

(autoload 'selcand-select "selcand" "\
Use PROMPT to prompt for a selection from CANDIDATES.

STRINGIFY-FN is an optional function to represent a candidate as a string.
If AUTOSELECT-IF-SINGLE is non-nil and there is exactly one candidate,
prompting the user is skipped.
INITIAL-INPUT, if non-nil, is used as the initial input in a
COMPLETING-READ call.
If READ-CHAR is non-nil, a single character key press is read
and mapped to the corresponding single-char candidate.

\(fn CANDIDATES &key PROMPT STRINGIFY-FN AUTOSELECT-IF-SINGLE INITIAL-INPUT READ-CHAR CHARS)" nil nil)

(register-definition-prefixes "selcand" '("selcand-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; selcand-autoloads.el ends here

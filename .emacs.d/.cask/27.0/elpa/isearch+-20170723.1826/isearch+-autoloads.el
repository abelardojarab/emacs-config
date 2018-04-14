;;; isearch+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "isearch+" "isearch+.el" (23049 22152 944796
;;;;;;  343000))
;;; Generated autoloads from isearch+.el

(defvar isearchp-case-fold nil "\
*Whether incremental search is case sensitive.
nil   means search is always case sensitive
t     means search is never  case sensitive
`yes' means search case-sensitivity follows option `search-upper-case'")

(custom-autoload 'isearchp-case-fold "isearch+" t)

(defvar isearchp-drop-mismatch nil "\
*Non-nil means remove or replace a search-string mismatch.
There are three possible values:

`replace-last' - Replace the last mismatch in the search string with
                 the latest input (e.g., replace the last typed char
                 or last yanked text).
nil            - Never remove mismatched text from the search string.
anything else  - Always remove mismatched text from the search string.

* Vanilla Isearch has the behavior of a nil value.

* Non-nil, non-`replace-last' means the search string never contains
  mismatched characters.

* `replace-last' means you see only the latest mismatched input, and
  it is available for editing, using \\<isearch-mode-map>`\\[isearch-edit-string]'.

You can cycle among the three possible values using `\\[isearchp-cycle-mismatch-removal]'.

See also option `isearchp-drop-mismatch-regexp-flag'.  It controls
whether regexp search respects or ignores `isearchp-drop-mismatch'.
If `nil' (the default value) then regexp search acts as if
`isearchp-drop-mismatch' were nil.  This is because typing a regexp
such as `[a-w]' can be problematic when mismatches are automatically
replaced.")

(custom-autoload 'isearchp-drop-mismatch "isearch+" nil)

(defvar isearchp-drop-mismatch-regexp-flag nil "\
*Non-nil means respect `isearchp-drop-mismatch' for regexp search too.
Otherwise (nil), regexp search ignores `isearchp-drop-mismatch',
acting as if it were nil.

Turning off automatic mismatch replacement can help during regexp
search when you type a pattern such as `[a-z]', because there likely
is no match when you type `[' and if not turned off then your typing
is automatically replaced by `a'.

There is no problem for many regexp patterns however, so you might
prefer customizing `isearchp-drop-mismatch-regexp-flag' to non-`nil'
and just using `M-k' to turn `isearchp-drop-mismatch' off temporarily
when needed.")

(custom-autoload 'isearchp-drop-mismatch-regexp-flag "isearch+" t)

(defvar isearchp-mouse-2-flag t "\
*Non-nil means clicking `mouse-2' during Isearch yanks the selection.
In that case, you can select text with the mouse, then hit `C-s' to
search for it.

If the value is nil, yank only if the `mouse-2' click is in the echo
area.  If not in the echo area, invoke whatever `mouse-2' is bound to
outside of Isearch.")

(custom-autoload 'isearchp-mouse-2-flag "isearch+" t)

(defvar isearchp-regexp-quote-yank-flag t "\
*Non-nil means escape special chars in text yanked for a regexp isearch.
You can toggle this using `isearchp-toggle-regexp-quote-yank', bound
to `C-`' during Isearch.")

(custom-autoload 'isearchp-regexp-quote-yank-flag "isearch+" t)

(defvar isearchp-resume-with-last-when-empty-flag t "\
If non-nil, newly empty search string means resume with last one.
This applies to resumption of search after `with-isearch-suspended'.

If the search string is empty after suspending search (because you set
`isearch-new-string' = \"\"), and it was not empty before suspending,
then a non-nil value of this option means resume searching with the
last search string.  Otherwise, resume searching with the empty search
string.

This option has no effect for Emacs releases prior to Emacs 22.")

(custom-autoload 'isearchp-resume-with-last-when-empty-flag "isearch+" t)

(defvar isearchp-ring-bell-function #'ignore "\
*Function that Isearch+ uses to ring the bell during search, or nil.
This does not affect the use of `C-g'.
If nil then use the value of `ring-bell-function'.

Possible functions you can use:
 `echo-bell' - Indication shown in echo area (requires `echo-bell.el')
 `ignore'    - Do nothing - no sound or visible indication")

(custom-autoload 'isearchp-ring-bell-function "isearch+" t)

(defvar isearchp-set-region-flag nil "\
*Non-nil means set region around search target.
This is used only for Transient Mark mode.
You can toggle this using `isearchp-toggle-set-region', bound to
`M-s M-SPC' during Isearch.")

(custom-autoload 'isearchp-set-region-flag "isearch+" t)

(defvar isearchp-toggle-option-flag nil "\
*Non-nil means Isearch toggling commands can affect option values.
If nil, the option value remains unchanged - the effect is temporary.

Applies to toggle commands for behavior that has an associated user
option.  Currently this means `M-s i' (`isearch-toggle-invisible') and
`M-c' (`isearch-toggle-case-fold').")

(custom-autoload 'isearchp-toggle-option-flag "isearch+" t)

(autoload 'isearchp-cycle-mismatch-removal "isearch+" "\
Cycle option `isearchp-drop-mismatch'.
See also option `isearchp-drop-mismatch-regexp-flag'.

\(fn)" t nil)

(autoload 'isearch-toggle-case-fold "isearch+" "\
Toggle case sensitivity on or off during incremental searching.
The minor-mode lighter shows `ISEARCH' for case-insensitive, `Isearch'
for case-sensitive.

If `isearchp-toggle-option-flag' is non-nil then toggle the value of
option `isearchp-case-fold'.  If it is nil then toggle the behavior
only temporarily, so that the option value is unchanged for subsequent
searches.

A prefix argument flips the sense of the last paragraph, so that the
option is updated only if `isearchp-toggle-option-flag' is nil instead
of non-nil.

To use a prefix argument you must set either `isearch-allow-scroll' or
`isearch-allow-prefix' (if available) to non-nil.  Otherwise, a prefix
arg during Isearch exits Isearch.

When toggling case-sensitive searching on, restores the last behavior
according to option `isearchp-case-fold': t or `yes'.

\(fn FLIP)" t nil)

(autoload 'isearch-toggle-invisible "isearch+" "\
Toggle searching in invisible text on or off.
If `isearchp-toggle-option-flag' is non-nil then toggle the value of
option `search-invisible'.  If it is nil then toggle the behavior only
temporarily, so that the option value is unchanged for subsequent
searches.

A prefix argument flips the sense of the last paragraph, so that the
option is updated only if `isearchp-toggle-option-flag' is nil instead
of non-nil.

To use a prefix argument you must set either `isearch-allow-scroll' or
`isearch-allow-prefix' (if available) to non-nil.  Otherwise, a prefix
arg during Isearch exits Isearch.

When toggling invisible searching on, restores the last behavior
according to option `search-invisible': t or `open'.

\(fn FLIP)" t nil)

(autoload 'isearchp-toggle-search-invisible "isearch+" "\
Toggle the value of user option `search-invisible'.
Toggles between nil and the last non-nil value.

\(fn)" t nil)

(autoload 'isearchp-toggle-option-toggle "isearch+" "\
Toggle the value of option `isearchp-toggle-option-flag'.

\(fn)" t nil)

(autoload 'isearchp-toggle-regexp-quote-yank "isearch+" "\
Toggle `isearchp-regexp-quote-yank-flag'.

\(fn)" t nil)

(autoload 'isearchp-toggle-set-region "isearch+" "\
Toggle `isearchp-set-region-flag'.

\(fn)" t nil)

(autoload 'isearchp-set-region-around-search-target "isearch+" "\
Set the region around the last search or query-replace target.

\(fn)" t nil)

(autoload 'isearch-mode-help "isearch+" "\
Display information on interactive search in buffer *Help*.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; isearch+-autoloads.el ends here

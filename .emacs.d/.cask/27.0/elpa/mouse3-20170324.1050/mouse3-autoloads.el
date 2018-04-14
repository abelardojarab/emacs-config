;;; mouse3-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "mouse3" "mouse3.el" (23048 36441 535630 139000))
;;; Generated autoloads from mouse3.el

(let ((loads (get 'mouse3 'custom-loads))) (if (member '"mouse3" loads) nil (put 'mouse3 'custom-loads (cons '"mouse3" loads))))

(defvar mouse3-popup-include-global-menus-flag t "\
*Non-nil means `mouse-3' menu includes major-mode or menu-bar menus.
When non-nil:
 If the menu bar is visible then include the major-mode menus.
 Otherwise, include the menu-bar menus.

This option has no effect unless `mouse3-popup-x-popup-panes-flag' is
nil, and it has no effect before Emacs 23.")

(custom-autoload 'mouse3-popup-include-global-menus-flag "mouse3" t)

(defvar mouse3-popup-x-popup-panes-flag nil "\
*Non-nil means use `mouse3-(no)region-popup-x-popup-panes'.
If nil, or if both `mouse3-region-popup-x-popup-panes' and
`mouse3-noregion-popup-x-popup-panes' are nil, then use
`mouse3-region-popup-entries' instead.")

(custom-autoload 'mouse3-popup-x-popup-panes-flag "mouse3" t)

(defvar mouse3-region-popup-x-popup-panes `(("Remove/Replace" ,@`(("Kill" . kill-region) ("Delete" . delete-region) ("Yank (Replace)" lambda (start end) "Replace selected text by last text killed." (interactive "r") (when (string= (buffer-substring-no-properties (point) (mark)) (car kill-ring)) (current-kill 1)) (delete-region start end) (yank)) ,@(and (fboundp 'hlt-highlight-region) '(("Yank Copied Text Properties" . hlt-yank-props))) ("--") ("Kill Rectangle" . kill-rectangle) ("Delete Rectangle" . delete-rectangle) ("Yank Rectangle (Replace)" lambda (start end) "Replace the selected rectangle by the last rectangle killed." (interactive "r") (delete-rectangle start end) (exchange-point-and-mark) (yank-rectangle)) ("Clear Rectangle (Replace)" . clear-rectangle) ("String Rectangle (Replace)" . string-rectangle) ("Replace Rectangle from Register" lambda (start end) "Replace the selected rectangle by the contents of a register you name.\nNote that the rectangle currently selected is first killed.  You can\nrestore it by yanking." (interactive "r") (kill-rectangle start end) (exchange-point-and-mark) (condition-case nil (call-interactively #'insert-register) (error (exchange-point-and-mark) (yank-rectangle)))))) ("Copy" ("Copy as Kill" . kill-ring-save) ("Copy to Register" . copy-to-register) ,@(and (fboundp 'hlt-highlight-region) '(("Copy Text Properties" . hlt-copy-props))) ("--") ("Copy Rectangle to Register" . copy-rectangle-to-register)) ("Register" ("Copy to..." . copy-to-register) ("Delete to..." lambda (register start end) "Delete the selected text, and copy it to a register you name." (interactive "cDelete region to register: \nr") (copy-to-register register start end t)) ("Append to..." . append-to-register) ("Prepend to..." . prepend-to-register) ("--") ("Copy Rectangle to..." . copy-rectangle-to-register) ("Delete Rectangle to..." lambda (register start end) "Delete the selected rectangle, and copy it to a register you name." (interactive "cDelete rectangle to register: \nr") (copy-rectangle-to-register register start end t))) ("Rectangle" ("Kill" . kill-rectangle) ("Delete" . delete-rectangle) ("Open" . open-rectangle) ("Yank (Replace)" lambda (start end) "Replace the selected rectangle by the last rectangle killed." (interactive "r") (delete-rectangle start end) (exchange-point-and-mark) (yank-rectangle)) ("Clear (Replace)" . clear-rectangle) ("String (Replace)" . string-rectangle) ,@`,(and (fboundp 'string-insert-rectangle) '(("String (Insert)" . string-insert-rectangle))) ,@`,(and (fboundp 'rectangle-number-lines) '(("Numbers (Insert)" . rectangle-number-lines))) ,@`,(and (fboundp 'delimit-columns-rectangle) '(("Delimit Columns" . delimit-columns-rectangle))) ,@`,(and (fboundp 'rectangle-mark-mode) '(("Rectangular Region" . rectangle-mark-mode))) ("--") ("Delete to Register" lambda (register start end) "Delete the selected rectangle, and copy it to a register you name." (interactive "cDelete rectangle to register: \nr") (copy-rectangle-to-register register start end t)) ("Replace from Register" lambda (start end) "Replace the selected rectangle by the contents of a register you name.\nNote that the rectangle currently selected is first killed.  You can\nrestore it by yanking." (interactive "r") (kill-rectangle start end) (exchange-point-and-mark) (condition-case nil (call-interactively #'insert-register) (error (exchange-point-and-mark) (yank-rectangle)))) ("Copy to Register" . copy-rectangle-to-register)) ("Change Text" ,@`,(and (fboundp 'boxquote-region) '(("Boxquote" . boxquote-region))) ,@`,(and (fboundp 'boxquote-unbox-region) '(("Unboxquote" . boxquote-unbox-region))) ,@`,(and (fboundp 'delimit-columns-rectangle) '(("Delimit Columns" . delimit-columns-region))) ,@`,(if (fboundp 'comment-or-uncomment-region) '(("Comment/Uncomment" . comment-or-uncomment-region)) '(("Comment" . comment-region) ("Uncomment" . uncomment-region))) ("--") ("Fill" . fill-region) ("Fill as Paragraph" . fill-region-as-paragraph) ("Canonically Space" . canonically-space-region) ("Indent" . indent-region) ("--") ("Capitalize" . capitalize-region) ("Upcase" . upcase-region) ("Downcase" . downcase-region) ,@`,(and (fboundp 'unaccent-region) '(("Remove Accents" . unaccent-region))) ("--") ("Center" . center-region) ("Reverse Line Order" . reverse-region)) ("Check, Correct, Convert" ("Ispell" . ispell-region) ("Flyspell" . flyspell-region) ,@`,(and (fboundp 'whitespace-cleanup-region) '(("Check Whitespace" . whitespace-report-region) ("Clean Up Whitespace" . whitespace-cleanup-region))) ("Printify" . printify-region) ("PR Printify" . pr-printify-region) ("Compose Characters" . compose-region) ("Decompose Characters" . decompose-region) ("--") ("Encode using Coding System" . encode-coding-region) ("Decode using Coding System" . decode-coding-region) ("Encode using Format" . format-encode-region) ("Decode using Format" . format-decode-region) ,@`,(and (fboundp 'yenc-decode-region) '(("Decode Yenc" . yenc-decode-region))) ("--") ("EPA Encrypt" . epa-encrypt-region) ("EPA Decrypt" . epa-decrypt-region) ("PGG Encrypt" . pgg-encrypt-region) ("PGG Decrypt" . pgg-decrypt-region)) ,@(and (fboundp 'hlt-highlight-region) '(("Highlight" ("Highlight" . hlt-highlight-region) ("Highlight Regexp" . hlt-highlight-regexp-region) ("Unhighlight" . hlt-unhighlight-region) ("Unhighlight for Face" . hlt-unhighlight-region-for-face) ("--") ("Copy Text Properties" . hlt-copy-props) ("Yank Copied Text Properties" . hlt-yank-props)))) ("Print" ("PostScript Print" . ps-print-region) ("PostScript Print with Faces" . ps-print-region-with-faces) ("PostScript Preview" . pr-ps-region-preview) ("PostScript Number of Pages" . ps-nb-pages-region) ("--") ("Print to Text Printer" . pr-txt-region) ("Print to Text Printer (`lpr')" . lpr-region) ("Print with Paging (`pr')" . print-region) ,@`,(and (fboundp 'ebnf-print-region) '(("--") ("BNF PostScript Analyze" . ebnf-syntax-region))) ,@`,(and (fboundp 'ebnf-print-region) '(("BNF PostScript Print " . ebnf-print-region))) ,@`,(and (fboundp 'ebnf-print-region) '(("BNF PostScript Save" . ebnf-eps-region)))) ("Misc" ,@`,(and (fboundp 'count-words-region) '(("Count Lines, Words, Chars" lambda nil (interactive) (call-interactively #'count-words-region) (sleep-for 3)))) ,@`,(and (not (fboundp 'count-words-region)) '(("Count Lines and Chars" lambda nil (interactive) (call-interactively #'count-lines-region) (sleep-for 3)))) ("Narrow" . narrow-to-region) ("Eval" . eval-region) ("Key-Macro on Region Lines" . apply-macro-to-region-lines) ("Shell Command" . shell-command-on-region) ("Write to File" . write-region) ,@`,(and (fboundp 'bmkp-set-autonamed-regexp-region) '(("Create Bookmarks Matching" . bmkp-set-autonamed-regexp-region))) ,@`,(and (fboundp 'bmkp-light-bookmarks-in-region) '(("Highlight Bookmarks" . bmkp-light-bookmarks-in-region))) ,@`,(and (fboundp 'browse-url-of-region) '(("Render in Browser" . browse-url-of-region))))) "\
*Submenus of `mouse-3' `Region' popup menu.
Used only if `mouse3-popup-x-popup-panes-flag' is non-nil.

A list of `x-popup-menu' pane menus, where each has the form
 (TITLE ITEM1 ITEM2...), with each ITEM a string or a cons cell
 (STRING . VALUE).  See `x-popup-menu'.

If you want to use features offered by extended menu items, then do
not use this option.  Instead, set option
`mouse3-popup-x-popup-panes-flag' to nil and use option
`mouse3-region-popup-entries' to define the menu.")

(custom-autoload 'mouse3-region-popup-x-popup-panes "mouse3" t)

(autoload 'mouse3-kill/delete-region "mouse3" "\
Delete the active region.  Kill it if KILLP is non-nil.
Kill it anyway if `mouse-drag-copy-region' is non-nil.
For Emacs prior to Emacs 22, always kill region.

\(fn EVENT KILLP)" t nil)

(autoload 'mouse3-popup-menu "mouse3" "\
Pop up a `Region' menu of actions for the selected text.
See options `mouse3-region-popup-entries',
`mouse3-popup-x-popup-panes-flag', and
`mouse3-region-popup-x-popup-panes'.

You have two alternatives, which correspond to the possible values of
option `mouse3-popup-x-popup-panes-flag' (and to the possibilities for
the MENU argument of function `x-popup-menu'):

1. nil (recommended) - Define the menu as a keymap, using submenu
   keymaps and `menu-item' bindings.

2. non-nil - Define the menu using (non-keymap) `x-popup-menu' panes
   lists, `mouse3-region-popup-x-popup-panes' and
   `mouse3-noregion-popup-x-popup-panes'.

The first alternative lets you make use of keywords such as `:enable'
and `:visible', and it lets you reuse existing menu keymaps.

The second alternative allows easy customization of individual menu
items, but it does not let you use menu keywords or reuse existing
keymaps.

For the first alternative, in addition to the items and submenus
supplied by options `mouse3-region-popup-entries' and
`mouse3-noregion-popup-entries', a submenu is added at the beginning
of the popup menu for either (a) the menu-bar menus (if the menu bar
is not visible) or (b) the major-mode menus.  (This is only for Emacs
23+.)

\(fn EVENT IGNORED)" t nil)

(autoload 'mouse-save-then-kill "mouse3" "\
Like vanilla `mouse-save-then-kill', but uses `mouse3-second-click-command'.

\(fn CLICK &optional PREFIX)" t nil)

(autoload 'mouse3-dired-use-menu "mouse3" "\
Make a second `mouse-3' click at the same place pop up a menu in Dired.

\(fn)" t nil)

(defvar mouse3-dired-function 'mouse3-dired-use-menu "\
*Fuction to call to update `dired-after-readin-hook' for `mouse-3' behavior.")

(custom-autoload 'mouse3-dired-function "mouse3" nil)

(autoload 'mouse3-dired-use-toggle-marks "mouse3" "\
Make a second `mouse-3' click at the same place toggle marks in Dired.
If you use Dired+ (`dired+.el') then this is a no-op.

\(fn)" t nil)

(autoload 'mouse3-dired-toggle-marks-in-region-from-mouse "mouse3" "\
Toggle marked and unmarked files and directories in region.

\(fn IGNORE1 IGNORE2)" t nil)

(autoload 'mouse3-dired-toggle-marks-in-region "mouse3" "\
Toggle marks in the region.

\(fn START END)" t nil)

(autoload 'mouse3-dired-mark-region-files "mouse3" "\
Mark all of the files in the current region (if it is active).
With non-nil prefix arg, unmark them instead.

\(fn &optional UNMARK-P)" t nil)

(autoload 'mouse3-dired-unmark-region-files "mouse3" "\
Unmark all of the files in the current region (if it is active).
With non-nil prefix arg, mark them instead.

\(fn &optional MARK-P)" t nil)

(autoload 'mouse3-dired-flag-region-files-for-deletion "mouse3" "\
Flag all of the files in the current region (if it is active) for deletion.

\(fn)" t nil)

(defvar mouse3-picture-mode-x-popup-panes `,@`(("Clear Rectangle" . picture-clear-rectangle) ("Kill Rectangle" lambda (start end) "Kill the selected rectangle.\nYou can yank it using \\<picture-mode-map>`\\[picture-yank-rectangle]'." (interactive "r") (picture-clear-rectangle start end 'KILLP)) ("Clear Rectangle to Register" . picture-clear-rectangle-to-register) ("Draw Rectangle" . picture-draw-rectangle) ("Yank Picture Rectangle (Replace)" . picture-yank-rectangle) ("Yank Rectangle from Register (Replace)" lambda nil "Replace the selected rectangle by the contents of a register you name." (interactive) (exchange-point-and-mark) (call-interactively #'picture-yank-rectangle-from-register))) "\
*Picture mode submenu of popup menu for `mouse-3'.")

(custom-autoload 'mouse3-picture-mode-x-popup-panes "mouse3" t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; mouse3-autoloads.el ends here

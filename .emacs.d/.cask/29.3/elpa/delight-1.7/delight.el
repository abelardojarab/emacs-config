;;; delight.el --- A dimmer switch for your lighter text  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

;; Author: Phil Sainty <psainty@orcon.net.nz>
;; Maintainer: Phil Sainty <psainty@orcon.net.nz>
;; URL: https://savannah.nongnu.org/projects/delight
;; Package-Requires: ((cl-lib "0.5") (nadvice "0.3"))
;; Keywords: convenience
;; Created: 25 Jun 2013
;; Version: 1.7

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Enables you to customise the mode names displayed in the mode line.
;;
;; For major modes, the buffer-local `mode-name' variable is modified.
;; For minor modes, the associated value in `minor-mode-alist' is set.
;;
;; Example usage:
;;
;; (require 'delight)
;;
;; (delight 'abbrev-mode " Abv" "abbrev")
;;
;; (delight '((abbrev-mode " Abv" "abbrev")
;;            (smart-tab-mode " \\t" "smart-tab")
;;            (eldoc-mode nil "eldoc")
;;            (rainbow-mode)
;;            (overwrite-mode " Ov" t)
;;            (emacs-lisp-mode "Elisp" :major)))
;;
;; The first argument is the mode symbol.
;;
;; The second argument is the replacement name to use in the mode line
;; (or nil to hide it).
;;
;; The third argument is either the keyword :major for major modes or,
;; for minor modes, the library which defines the mode.  This is passed
;; to ‘eval-after-load’ and so should be either the name (as a string)
;; of the library file which defines the mode, or the feature (symbol)
;; provided by that library.  If this argument is nil, the mode symbol
;; will be passed as the feature.  If this argument is either t or 'emacs
;; then it is assumed that the mode is already loaded (you can use this
;; with standard minor modes that are pre-loaded by default when Emacs
;; starts).
;;
;; To determine which library defines a mode, use e.g.: C-h f
;; eldoc-mode RET.  The name of the library is displayed in the first
;; paragraph, with an “.el” suffix (in this example it displays
;; “eldoc.el”, and therefore we could use the value “eldoc” for the
;; library).
;;
;; Important note:
;;
;; Although strings are common, any mode-line construct is permitted
;; as the value (for both minor and major modes); so before you
;; override a value you should check the existing one, as you may
;; want to replicate any structural elements in your replacement
;; if it turns out not to be a simple string.
;;
;; For major modes, M-: mode-name
;; For minor modes, M-: (cadr (assq 'MODE minor-mode-alist))
;; for the minor MODE in question.
;;
;; Conversely, you may incorporate additional mode-line constructs in
;; your replacement values, if you so wish. e.g.:
;;
;; (delight 'emacs-lisp-mode
;;          '("Elisp" (lexical-binding ":Lex" ":Dyn"))
;;          :major)
;;
;; See `mode-line-format' for information about mode-line constructs,
;; and M-: (info "(elisp) Mode Line Format") for further details.
;;
;; Also bear in mind that some modes may dynamically update these
;; values themselves (for instance dired-mode updates mode-name if
;; you change the sorting criteria) in which cases this library may
;; prove inadequate.

;;; Change Log:
;;
;; 1.7 (2020-07-11)
;;   - Rename `delight--inhibit' to `delight-mode-name-inhibit', and
;;     document its uses.
;; 1.6 (2019-07-23)
;;   - Use cl-lib, nadvice, and lexical-binding.
;;   - Rename `inhibit-mode-name-delight' to `delight--inhibit'.
;; 1.5 (2016-03-01)
;;   - Support FILE value t, meaning that the minor MODE in question
;;     is guaranteed to already be loaded.
;; 1.4 (2016-02-28)
;;   - Respect `inhibit-mode-name-delight' when already set.
;; 1.3 (2014-05-30)
;;   - Add support for `mode-line-mode-menu'.
;; 1.2 (2014-05-04)
;;   - Bug fix for missing 'cl requirement for destructuring-bind macro.
;; 1.1 (2014-05-04)
;;   - Allow the keyword :major as the FILE argument for major modes,
;;     to avoid also processing them as minor modes.
;; 1.0 (2013-06-25)
;;   - Initial release.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'nadvice)

(defvar delighted-modes ()
  "List of specs for modifying the display of mode names in the mode line.

See `delight'.")

;;;###autoload
(defun delight (spec &optional value file)
  "Modify the lighter value displayed in the mode line for the given mode SPEC
if and when the mode is loaded.

SPEC can be either a mode symbol, or a list containing multiple elements of
the form (MODE VALUE FILE).  In the latter case the two optional arguments are
omitted, as they are instead specified for each element of the list.

For minor modes, VALUE is the replacement lighter value (or nil to disable)
to set in the `minor-mode-alist' variable.  For major modes VALUE is the
replacement buffer-local `mode-name' value to use when a buffer changes to
that mode.

In both cases VALUE is commonly a string, but may in fact contain any valid
mode-line construct.  For details see the `mode-line-format' variable, and
Info node `(elisp) Mode Line Format'.

The FILE argument is passed through to `eval-after-load'.  If FILE is nil then
the mode symbol is passed as the required feature.  If FILE is t then it is
assumed that the mode is already loaded.  (Note that you can also use \\='emacs
for this purpose).  These FILE options are relevant to minor modes only.

For major modes you should specify the keyword :major as the value of FILE,
to prevent the mode being treated as a minor mode."
  (add-hook 'after-change-major-mode-hook #'delight-major-mode)
  (let ((glum (if (consp spec) spec (list (list spec value file)))))
    (while glum
      (cl-destructuring-bind (mode &optional value file) (pop glum)
        (assq-delete-all mode delighted-modes)
        (add-to-list 'delighted-modes (list mode value file))
        (unless (eq file :major)
          (eval-after-load (if (eq file t) 'emacs (or file mode))
            `(let ((minor-delight (assq ',mode minor-mode-alist)))
               (when minor-delight
                 (setcar (cdr minor-delight) ',value)
                 (delight-mode-line-mode-menu ',mode ',value)))))))))

(defun delight-mode-line-mode-menu (mode value)
  "Delight `mode-line-mode-menu' (the \"Toggle minor modes\" menu)
so that the Lighter text displayed in the menu matches that displayed in
the mode line (when such menu items exist).

The expected naming scheme for the menu items is: \"Friendly name (Lighter)\"
e.g.: \"Highlight changes (Chg)\".

We replace the \"Lighter\" portion of that with our delighted VALUE, for the
specified MODE, unless VALUE is empty/nil, in which case we remove the text
and parentheses altogether.

If the delighted VALUE is not a string and not nil, we do nothing."
  (when (string-or-null-p value)
    (let* ((menu-keymap mode-line-mode-menu)
           (menu-item (assq mode (cdr menu-keymap))))
      (when menu-item
        ;; Lighter text is typically prefixed with a space to separate
        ;; it from the preceding lighter.  We need to trim that space.
        (let* ((trimmed-value (if (and value (string-match "\\`\\s-+" value))
                                  (replace-match "" t t value)
                                value))
               (wrapped-value (if (> (length trimmed-value) 0)
                                  (concat " (" trimmed-value ")")
                                ""))
               (menu-def (cdr menu-item))
               (label (cadr menu-def))
               (new-label (and (stringp label)
                               (or (string-match "\\s-+(.+?)\\s-*\\'" label)
                                   (string-match "\\s-*\\'" label))
                               (replace-match wrapped-value t t label))))
          (when new-label
            ;; Pure storage is used for the default menu items, so we
            ;; cannot modify those objects directly.
            (setq menu-def (copy-sequence menu-def))
            (setf (cadr menu-def) new-label)
            (define-key menu-keymap (vector mode) menu-def)))))))

(defun delight-major-mode ()
  "Delight the 'pretty name' of the current buffer's major mode
when displayed in the mode-line.

When `mode-name' is displayed in other contexts (such as in the
`describe-mode' help buffer), its original value will be used,
unless `delight-mode-name-inhibit' is bound and nil."
  (let ((major-delight (assq major-mode delighted-modes)))
    (when major-delight
      (setq mode-name `(delight-mode-name-inhibit
                        ,mode-name ;; glum
                        ,(cadr major-delight)))))) ;; delighted

(define-obsolete-variable-alias 'inhibit-mode-name-delight
  'delight-mode-name-inhibit "delight-1.6")
(define-obsolete-variable-alias 'delight--inhibit
  'delight-mode-name-inhibit "delight-1.7")

(defvar delight-mode-name-inhibit)
;; This variable determines whether the `mode-name' set by `delight-major-mode'
;; will render as the original name or the delighted name.  For the purposes of
;; mode line formatting, void and nil are equivalent.  It is void by default so
;; that we are able to respect any binding made by external code, and only
;; let-bind it ourselves if no such external binding exists.
;;
;; Note that if this were bound to nil by default, `delight--format-mode-line'
;; would be unable to recognise a nil binding made by some other library; and
;; if it were bound to a non-nil value by default, then we would render the
;; wrong value in the mode line.

(put 'delight-mode-name-inhibit 'variable-documentation
     "Whether to display the original `mode-name' of a delighted major mode.

A non-nil value means that the original mode name will be displayed
instead of the delighted name.

If nil or void, then the delighted mode name will be displayed.

With the exception of Emacs' standard mode line rendering, anything
rendering a mode line construct (for instance the `describe-mode' help
buffer) will call `format-mode-line'.  Normally we want to display
delighted major mode names only in the mode line itself, and not in
other contexts, and so this variable is used to inhibit the delighted
names during `format-mode-line' calls.

However, certain libraries may call `format-mode-line' for the purpose
of replacing the standard mode line entirely, in which case we DO want
to see the delighted major mode names during those particular
`format-mode-line' calls.

This variable is normally void, and bound to t during calls to
`format-mode-line'.  If, however, it is already bound, then its value
will be respected; therefore binding `delight-mode-name-inhibit' to
nil around a call to `format-mode-line' will allow the delighted name
to be rendered.

See also `delight--format-mode-line'.")

(defun delight--format-mode-line (orig-fun &rest args)
  "Advice for `format-mode-line'.

Delighted major modes should exhibit their original `mode-name' when
`format-mode-line' is called.  See `delight-major-mode' as well as
`delight-mode-name-inhibit'."
  (let ((delight-mode-name-inhibit (if (boundp 'delight-mode-name-inhibit)
                                       delight-mode-name-inhibit
                                     t)))
    (apply orig-fun args)))

(advice-add 'format-mode-line :around #'delight--format-mode-line)

(provide 'delight)
;;; delight.el ends here

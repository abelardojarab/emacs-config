;;; ergoemacs-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ergoemacs-advice" "ergoemacs-advice.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ergoemacs-advice.el

(register-definition-prefixes "ergoemacs-advice" '("ergoemacs-"))

;;;***

;;;### (autoloads nil "ergoemacs-calculate-bindings" "ergoemacs-calculate-bindings.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ergoemacs-calculate-bindings.el

(register-definition-prefixes "ergoemacs-calculate-bindings" '("ergoemacs-"))

;;;***

;;;### (autoloads nil "ergoemacs-command-loop" "ergoemacs-command-loop.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ergoemacs-command-loop.el

(register-definition-prefixes "ergoemacs-command-loop" '("ergoemacs-" "erogemacs-command--echo-timer"))

;;;***

;;;### (autoloads nil "ergoemacs-cua" "ergoemacs-cua.el" (0 0 0 0))
;;; Generated autoloads from ergoemacs-cua.el

(register-definition-prefixes "ergoemacs-cua" '("cua--prefix-" "ergoemacs-"))

;;;***

;;;### (autoloads nil "ergoemacs-debug" "ergoemacs-debug.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ergoemacs-debug.el

(register-definition-prefixes "ergoemacs-debug" '("ergoemacs-debug"))

;;;***

;;;### (autoloads nil "ergoemacs-functions" "ergoemacs-functions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ergoemacs-functions.el

(autoload 'ergoemacs-undo "ergoemacs-functions" "\
Run `undo'. Prefer running undo-fo if present" t nil)

(autoload 'ergoemacs-isearch-forward "ergoemacs-functions" nil t nil)

(autoload 'ergoemacs-isearch-backward "ergoemacs-functions" nil t nil)

(autoload 'ergoemacs-paste-cycle "ergoemacs-functions" "\
Run `yank-pop' or`yank'.
This is `yank-pop' if `ergoemacs-smart-paste' is nil.
This is `yank' if `ergoemacs-smart-paste' is t.

If `browse-kill-ring' is enabled and the last command is not a
paste, this will start `browse-kill-ring'.

When in `browse-kill-ring-mode', cycle backward through the key ring.
" t nil)

(autoload 'ergoemacs-paste "ergoemacs-functions" "\
Run `yank' or `yank-pop' if this command is repeated.
This is `yank' if `ergoemacs-smart-paste' is nil.
This is `yank-pop' if `ergoemacs-smart-paste' is t and last command is a yank.
This is `browse-kill-ring' if `ergoemacs-smart-paste' equals 'browse-kill-ring and last command is a yank.

When in `browse-kill-ring-mode', cycle forward through the key ring.

This does the same thing in `isearch-mode' using `isearch-yank-pop' and  `isearch-yank-kill'

If in `term-mode', run `term-paste'.
" t nil)

(autoload 'ergoemacs-unaccent-word "ergoemacs-functions" "\
Move curseur forward NUM (prefix arg) words, removing accents.
Guillemet -> quote, degree -> @, s-zed -> ss, upside-down ?! -> ?!.

\(fn NUM)" t nil)

(autoload 'ergoemacs-unaccent-region "ergoemacs-functions" "\
Replace accented chars between START and END by unaccented chars.
Guillemet -> quote, degree -> @, s-zed -> ss, upside-down ?! -> ?!.
When called from a program, third arg DISPLAY-MSGS non-nil means to
display in-progress messages.

\(fn START END DISPLAY-MSGS)" t nil)

(autoload 'ergoemacs-unaccent-char "ergoemacs-functions" "\
Replace accented char at curser by corresponding unaccented char(s).
Guillemet -> quote, degree -> @, s-zed -> ss, upside-down ?! -> ?!." t nil)

(register-definition-prefixes "ergoemacs-functions" '("ergoemacs-"))

;;;***

;;;### (autoloads nil "ergoemacs-key-description" "ergoemacs-key-description.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ergoemacs-key-description.el

(register-definition-prefixes "ergoemacs-key-description" '("ergoemacs-"))

;;;***

;;;### (autoloads nil "ergoemacs-layouts" "ergoemacs-layouts.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ergoemacs-layouts.el

(register-definition-prefixes "ergoemacs-layouts" '("describe-ergoemacs-layout" "ergoemacs-layout"))

;;;***

;;;### (autoloads nil "ergoemacs-lib" "ergoemacs-lib.el" (0 0 0 0))
;;; Generated autoloads from ergoemacs-lib.el

(autoload 'ergoemacs-gen-ahk "ergoemacs-lib" "\
Generates autohotkey for all layouts and themes

\(fn &optional ALL)" t nil)

(register-definition-prefixes "ergoemacs-lib" '("ergoemacs-"))

;;;***

;;;### (autoloads nil "ergoemacs-macros" "ergoemacs-macros.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ergoemacs-macros.el

(autoload 'ergoemacs-keymapp "ergoemacs-macros" "\
Error free check of keymap by `keymapp'

\(fn KEYMAP)" nil t)

(autoload 'ergoemacs-sv "ergoemacs-macros" "\
Error free `symbol-value'.
If SYMBOL is void, return nil

\(fn SYMBOL &optional DEFAULT)" nil t)

(autoload 'ergoemacs-save-buffer-state "ergoemacs-macros" "\
Eval BODY,
then restore the buffer state under the assumption that no significant
modification has been made in BODY.  A change is considered
significant if it affects the buffer text in any way that isn't
completely restored again.  Changes in text properties like `face' or
`syntax-table' are considered insignificant.  This macro allows text
properties to be changed, even in a read-only buffer.

This macro should be placed around all calculations which set
\"insignificant\" text properties in a buffer, even when the buffer is
known to be writeable.  That way, these text properties remain set
even if the user undoes the command which set them.

This macro should ALWAYS be placed around \"temporary\" internal buffer
changes (like adding a newline to calculate a text-property then
deleting it again), so that the user never sees them on his
`buffer-undo-list'.  

However, any user-visible changes to the buffer (like auto-newlines)
must not be within a `ergoemacs-save-buffer-state', since the user then
wouldn't be able to undo them.

The return value is the value of the last form in BODY.

This was stole/modified from `c-save-buffer-state'

\(fn &rest BODY)" nil t)

(autoload 'ergoemacs "ergoemacs-macros" "\
Get/Set keymaps and `ergoemacs-mode' properties

When arg1 can be a property.  The following properties are supported:
- :layout - returns the current (or specified by PROPERTY) keyboard layout.
- :map-list,  :composed-p, :composed-list, :key-hash :empty-p calls ergoemacs-map-properties-- equivalent functions.

\(fn &rest ARGS)" nil t)

(autoload 'ergoemacs-translation "ergoemacs-macros" "\
Defines an `ergoemacs-mode' translation.
:text -- Text to display while completing this translation
:keymap -- Local Keymap for translation
:keymap-modal -- Modal keymap for overrides.
:modal-always -- If the modal state is always on, regardless of
                 the values of  `ergoemacs-modal-ignored-buffers',
                `ergoemacs-modal-emacs-state-modes' `minibufferp'
The following arguments allow the keyboard presses to be translated:
 - :meta
 - :control
 - :shift
 - :meta-control
 - :meta-shift
 - :control-shift
 - :meta-control-shift
 - :unchorded (no modifiers)
This also creates functions:
- ergoemacs-translate--NAME-universal-argument
- ergoemacs-translate--NAME-digit-argument
- ergoemacs-translate--NAME-negative-argument
- ergoemacs-translate--NAME-modal

\(fn &rest BODY-AND-PLIST)" nil t)

(function-put 'ergoemacs-translation 'doc-string-elt '2)

(function-put 'ergoemacs-translation 'lisp-indent-function '2)

(register-definition-prefixes "ergoemacs-macros" '("ergoemacs-"))

;;;***

;;;### (autoloads nil "ergoemacs-map" "ergoemacs-map.el" (0 0 0 0))
;;; Generated autoloads from ergoemacs-map.el

(register-definition-prefixes "ergoemacs-map" '("ergoemacs-"))

;;;***

;;;### (autoloads nil "ergoemacs-map-properties" "ergoemacs-map-properties.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ergoemacs-map-properties.el

(register-definition-prefixes "ergoemacs-map-properties" '("ergoemacs-map-properties--"))

;;;***

;;;### (autoloads nil "ergoemacs-mapkeymap" "ergoemacs-mapkeymap.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ergoemacs-mapkeymap.el

(register-definition-prefixes "ergoemacs-mapkeymap" '("ergoemacs-map-"))

;;;***

;;;### (autoloads nil "ergoemacs-mode" "ergoemacs-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ergoemacs-mode.el

(defvar ergoemacs-mode nil "\
Non-nil if ErgoEmacs mode is enabled.
See the `ergoemacs-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ergoemacs-mode'.")

(custom-autoload 'ergoemacs-mode "ergoemacs-mode" nil)

(autoload 'ergoemacs-mode "ergoemacs-mode" "\
Toggle ergoemacs keybinding minor mode.
This minor mode changes your emacs keybinding.

This is a minor mode.  If called interactively, toggle the
`ErgoEmacs mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='ergoemacs-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Without argument, toggles the minor mode.
If optional argument is 1, turn it on.
If optional argument is 0, turn it off.

Home page URL `http://ergoemacs.github.io/'

The `execute-extended-command' is now \\[execute-extended-command].

\(fn &optional ARG)" t nil)

(register-definition-prefixes "ergoemacs-mode" '("erg"))

;;;***

;;;### (autoloads nil "ergoemacs-test" "ergoemacs-test.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ergoemacs-test.el

(autoload 'ergoemacs-test "ergoemacs-test" "\
Test ergoemacs issues." t nil)

(register-definition-prefixes "ergoemacs-test" '("ergoemacs-"))

;;;***

;;;### (autoloads nil "ergoemacs-theme-engine" "ergoemacs-theme-engine.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ergoemacs-theme-engine.el

(autoload 'ergoemacs-theme-create-bash "ergoemacs-theme-engine" "\
Create bash ~/.inputrc for use with bash." t nil)

(defalias 'ergoemacs-bash 'ergoemacs-theme-create-bash)

(register-definition-prefixes "ergoemacs-theme-engine" '("ergoemacs-"))

;;;***

;;;### (autoloads nil "ergoemacs-themes" "ergoemacs-themes.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ergoemacs-themes.el

(register-definition-prefixes "ergoemacs-themes" '("ergoemacs-" "org-mode-map"))

;;;***

;;;### (autoloads nil "ergoemacs-translate" "ergoemacs-translate.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ergoemacs-translate.el

(register-definition-prefixes "ergoemacs-translate" '("ergoemacs-translate"))

;;;***

;;;### (autoloads nil nil ("ergoemacs-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ergoemacs-mode-autoloads.el ends here

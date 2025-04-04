;;; ctags-update-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from ctags-update.el

(autoload 'ctags-update "ctags-update" "\
ctags-update in parent directory using `exuberant-ctags'.
1. you can call this function directly,
2. enable `ctags-auto-update-mode',
3. with prefix `C-u' then you can generate a new TAGS file in selected directory,
4. with prefix `C-uC-u' save the command to kill-ring instead of execute it.

(fn &optional ARGS)" t)
(autoload 'ctags-auto-update-mode "ctags-update" "\
auto update TAGS using `exuberant-ctags' in parent directory.

This is a minor mode.  If called interactively, toggle the
`Ctags-Auto-Update mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `ctags-auto-update-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "\
turn on `ctags-auto-update-mode'." t)
(put 'ctags-global-auto-update-mode 'globalized-minor-mode t)
(defvar ctags-global-auto-update-mode nil "\
Non-nil if Ctags-Global-Auto-Update mode is enabled.
See the `ctags-global-auto-update-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ctags-global-auto-update-mode'.")
(custom-autoload 'ctags-global-auto-update-mode "ctags-update" nil)
(autoload 'ctags-global-auto-update-mode "ctags-update" "\
Toggle Ctags-Auto-Update mode in all buffers.
With prefix ARG, enable Ctags-Global-Auto-Update mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Ctags-Auto-Update mode is enabled in all buffers where
`turn-on-ctags-auto-update-mode' would do it.

See `ctags-auto-update-mode' for more information on Ctags-Auto-Update
mode.

(fn &optional ARG)" t)
(register-definition-prefixes "ctags-update" '("ctags-"))

;;; End of scraped data

(provide 'ctags-update-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; ctags-update-autoloads.el ends here

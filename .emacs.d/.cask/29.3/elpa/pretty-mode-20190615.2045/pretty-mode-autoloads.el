;;; pretty-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from pretty-mode.el

(autoload 'pretty-mode "pretty-mode" "\
Toggle Pretty minor mode.
With arg, turn Pretty minor mode on if arg is positive, off otherwise.

Pretty mode builds on `font-lock-mode'. Instead of highlighting
keywords, it replaces them with symbols. For example, lambda is
displayed as λ in lisp modes.

(fn &optional ARG)" t)
(put 'global-pretty-mode 'globalized-minor-mode t)
(defvar global-pretty-mode t "\
Non-nil if Global Pretty mode is enabled.
See the `global-pretty-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-pretty-mode'.")
(custom-autoload 'global-pretty-mode "pretty-mode" nil)
(autoload 'global-pretty-mode "pretty-mode" "\
Toggle Pretty mode in all buffers.
With prefix ARG, enable Global Pretty mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Pretty mode is enabled in all buffers where
`turn-on-pretty-if-desired' would do it.

See `pretty-mode' for more information on Pretty mode.

(fn &optional ARG)" t)
(autoload 'turn-off-pretty-mode "pretty-mode" nil t)
(autoload 'turn-on-pretty-mode "pretty-mode" nil t)
(register-definition-prefixes "pretty-mode" '("ensure-" "pretty-" "turn-on-pretty-if-desired"))

;;; End of scraped data

(provide 'pretty-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; pretty-mode-autoloads.el ends here

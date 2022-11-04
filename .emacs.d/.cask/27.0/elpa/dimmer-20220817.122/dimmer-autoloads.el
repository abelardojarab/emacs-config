;;; dimmer-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dimmer" "dimmer.el" (0 0 0 0))
;;; Generated autoloads from dimmer.el

(autoload 'dimmer-configure-company-box "dimmer" "\
Convenience setting for company-box users.
This predicate prevents dimming the buffer you are editing when
company-box pops up a list of completion." nil nil)

(autoload 'dimmer-configure-helm "dimmer" "\
Convenience settings for helm users." nil nil)

(autoload 'dimmer-configure-gnus "dimmer" "\
Convenience settings for gnus users." nil nil)

(autoload 'dimmer-configure-hydra "dimmer" "\
Convenience settings for hydra users." nil nil)

(autoload 'dimmer-configure-magit "dimmer" "\
Convenience settings for magit users." nil nil)

(autoload 'dimmer-configure-org "dimmer" "\
Convenience settings for org users." nil nil)

(autoload 'dimmer-configure-posframe "dimmer" "\
Convenience settings for packages depending on posframe.

Note, packages that use posframe aren't required to be consistent
about how they name their buffers, but many of them tend to
include the words \"posframe\" and \"buffer\" in the buffer's
name.  Examples include:

  - \" *ivy-posframe-buffer*\"
  - \" *company-posframe-buffer*\"
  - \" *flycheck-posframe-buffer*\"
  - \" *ddskk-posframe-buffer*\"

If this setting doesn't work for you, you still have the option
of adding another regular expression to catch more things, or
in some cases you can customize the other package and ensure it
uses a buffer name that fits this pattern." nil nil)

(autoload 'dimmer-configure-which-key "dimmer" "\
Convenience settings for which-key-users." nil nil)

(defvar dimmer-mode nil "\
Non-nil if Dimmer mode is enabled.
See the `dimmer-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dimmer-mode'.")

(custom-autoload 'dimmer-mode "dimmer" nil)

(autoload 'dimmer-mode "dimmer" "\
Visually highlight the selected buffer.

This is a minor mode.  If called interactively, toggle the
`Dimmer mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='dimmer-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'dimmer-activate 'dimmer-mode "0.2.0")

(register-definition-prefixes "dimmer" '("dimmer-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dimmer-autoloads.el ends here

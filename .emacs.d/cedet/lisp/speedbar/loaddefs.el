;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "rpm" "rpm.el" (0 0 0 0))
;;; Generated autoloads from rpm.el

(autoload 'rpm "rpm" "\
Red Hat Package Management in Emacs.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rpm" '("rpm-")))

;;;***

;;;### (autoloads nil "sb-ant" "sb-ant.el" (0 0 0 0))
;;; Generated autoloads from sb-ant.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sb-ant" '("speedbar-")))

;;;***

;;;### (autoloads nil "sb-html" "sb-html.el" (0 0 0 0))
;;; Generated autoloads from sb-html.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sb-html" '("speedbar-")))

;;;***

;;;### (autoloads nil "sb-info" "sb-info.el" (0 0 0 0))
;;; Generated autoloads from sb-info.el

(autoload 'Info-speedbar-browser "sb-info" "\
Initialize speedbar to display an info node browser.
This will add a speedbar major display mode.

\(fn)" t nil)

(autoload 'Info-speedbar-buttons "sb-info" "\
Create a speedbar display to help navigation in an Info file.
BUFFER is the buffer speedbar is requesting buttons for.

\(fn BUFFER)" nil nil)

(eval-after-load "info" '(require 'sb-info))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sb-info" '("Info-")))

;;;***

;;;### (autoloads nil "sb-rmail" "sb-rmail.el" (0 0 0 0))
;;; Generated autoloads from sb-rmail.el

(autoload 'rmail-speedbar-buttons "sb-rmail" "\
Create buttons for BUFFER containing rmail messages.
Click on the address under Reply to: to reply to this person.
Under Folders: Click a name to read it, or on the <M> to move the
current message into that RMAIL folder.

\(fn BUFFER)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sb-rmail" '("rmail-")))

;;;***

;;;### (autoloads nil "sb-texinfo" "sb-texinfo.el" (0 0 0 0))
;;; Generated autoloads from sb-texinfo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sb-texinfo" '("speedbar-")))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here

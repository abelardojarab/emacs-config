;;; restclient-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from restclient.el

(autoload 'restclient-http-send-current "restclient" "\
Sends current request.
Optional argument RAW don't reformat response if t.
Optional argument STAY-IN-WINDOW do not move focus to response buffer if t.

(fn &optional RAW STAY-IN-WINDOW SUPPRESS-RESPONSE-BUFFER)" t)
(autoload 'restclient-http-send-current-raw "restclient" "\
Sends current request and get raw result (no reformatting or syntax highlight of XML, JSON or images)." t)
(autoload 'restclient-http-send-current-stay-in-window "restclient" "\
Send current request and keep focus in request window." t)
(autoload 'restclient-http-send-current-suppress-response-buffer "restclient" "\
Send current request but don't show response buffer." t)
(autoload 'restclient-mode "restclient" "\
Turn on restclient mode.

(fn)" t)
(register-definition-prefixes "restclient" '("restclient-"))

;;; End of scraped data

(provide 'restclient-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; restclient-autoloads.el ends here
;;; dts-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dts-mode" "dts-mode.el" (0 0 0 0))
;;; Generated autoloads from dts-mode.el

(autoload 'dts-mode "dts-mode" "\
Major mode for editing Devicetrees

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dts\\'" . dts-mode))

(add-to-list 'auto-mode-alist '("\\.dtsi\\'" . dts-mode))

(register-definition-prefixes "dts-mode" '("dts-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dts-mode-autoloads.el ends here

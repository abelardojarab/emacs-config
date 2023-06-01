;;; consult-lsp-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "consult-lsp" "consult-lsp.el" (0 0 0 0))
;;; Generated autoloads from consult-lsp.el

(autoload 'consult-lsp-diagnostics "consult-lsp" "\
Query LSP-mode diagnostics.

When ARG is set through prefix, query all workspaces.

\(fn ARG)" t nil)

(autoload 'consult-lsp-symbols "consult-lsp" "\
Query workspace symbols. When ARG is set through prefix, query all workspaces.

\(fn ARG)" t nil)

(autoload 'consult-lsp-file-symbols "consult-lsp" "\
Search symbols defined in current file in a manner similar to `consult-line'.

If the prefix argument GROUP-RESULTS is specified, symbols are grouped by their
kind; otherwise they are returned in the order that they appear in the file.

\(fn GROUP-RESULTS)" t nil)

(register-definition-prefixes "consult-lsp" '("consult-lsp-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; consult-lsp-autoloads.el ends here

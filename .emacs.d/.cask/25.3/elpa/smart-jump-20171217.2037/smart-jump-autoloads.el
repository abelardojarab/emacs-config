;;; smart-jump-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "smart-jump" "smart-jump.el" (23095 24997 796658
;;;;;;  967000))
;;; Generated autoloads from smart-jump.el

(autoload 'smart-jump-setup-default-registers "smart-jump" "\
Register a default set of modes for `smart-jump'.

\(fn)" t nil)

(autoload 'smart-jump-go "smart-jump" "\
Go to the function/variable declartion for thing at point.

SMART-LIST will be set if this is a continuation of a previous jump.

\(fn &optional SMART-LIST)" t nil)

(autoload 'smart-jump-back "smart-jump" "\
Jump back to where the last jump was done.

\(fn)" t nil)

(autoload 'smart-jump-references "smart-jump" "\
Find references with fallback.
Optional argument SMART-LIST This will be non-nil of continuation of previous
call to `smart-jump-references'.

\(fn &optional SMART-LIST)" t nil)

(autoload 'smart-jump-peek "smart-jump" "\
Peek at definition.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("smart-jump-cc-mode.el" "smart-jump-clojure-mode.el"
;;;;;;  "smart-jump-csharp-mode.el" "smart-jump-elisp-mode.el" "smart-jump-elixir-mode.el"
;;;;;;  "smart-jump-elm-mode.el" "smart-jump-ensime.el" "smart-jump-erlang-mode.el"
;;;;;;  "smart-jump-go-mode.el" "smart-jump-intero.el" "smart-jump-js2-mode.el"
;;;;;;  "smart-jump-lisp-mode.el" "smart-jump-lsp-mode.el" "smart-jump-pkg.el"
;;;;;;  "smart-jump-python.el" "smart-jump-ruby-mode.el" "smart-jump-scheme.el"
;;;;;;  "smart-jump-typescript-mode.el" "smart-jump-web-mode.el")
;;;;;;  (23095 24997 800658 881000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; smart-jump-autoloads.el ends here

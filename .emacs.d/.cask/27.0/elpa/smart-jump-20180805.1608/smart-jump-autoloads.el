;;; smart-jump-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "smart-jump" "smart-jump.el" (0 0 0 0))
;;; Generated autoloads from smart-jump.el

(autoload 'smart-jump-setup-default-registers "smart-jump" "\
Register a default set of modes for `smart-jump'.

\(fn)" t nil)

(autoload 'smart-jump-peek "smart-jump" "\
Peek at definition.

\(fn)" t nil)

(autoload 'smart-jump-go "smart-jump" "\
Go to the function/variable declartion for thing at point.

SMART-LIST will be set (or nil) if this is a continuation of a previous jump.

CONTINUE will be non nil if this is a continuation of a previous jump.

\(fn &optional SMART-LIST CONTINUE)" t nil)

(autoload 'smart-jump-back "smart-jump" "\
Jump back to where the last jump was done.

\(fn)" t nil)

(autoload 'smart-jump-references "smart-jump" "\
Find references with fallback.
Optional argument SMART-LIST This will be non-nil of continuation of previous
call to `smart-jump-references'.

CONTINUE will be set if this is a continuation of a previous call to
`smart-jump-references'.

\(fn &optional SMART-LIST CONTINUE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump" '("smart-jump-")))

;;;***

;;;### (autoloads nil "smart-jump-cc-mode" "smart-jump-cc-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-cc-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-cc-mode" '("smart-jump-cc-mode-register")))

;;;***

;;;### (autoloads nil "smart-jump-clojure-mode" "smart-jump-clojure-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-clojure-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-clojure-mode" '("smart-jump-clojure-")))

;;;***

;;;### (autoloads nil "smart-jump-csharp-mode" "smart-jump-csharp-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-csharp-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-csharp-mode" '("smart-jump-csharp-")))

;;;***

;;;### (autoloads nil "smart-jump-eglot" "smart-jump-eglot.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from smart-jump-eglot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-eglot" '("smart-jump-eglot-")))

;;;***

;;;### (autoloads nil "smart-jump-elisp-mode" "smart-jump-elisp-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-elisp-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-elisp-mode" '("smart-jump-elisp-")))

;;;***

;;;### (autoloads nil "smart-jump-elixir-mode" "smart-jump-elixir-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-elixir-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-elixir-mode" '("smart-jump-elixir-")))

;;;***

;;;### (autoloads nil "smart-jump-elm-mode" "smart-jump-elm-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-elm-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-elm-mode" '("smart-jump-elm-mode-register")))

;;;***

;;;### (autoloads nil "smart-jump-erlang-mode" "smart-jump-erlang-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-erlang-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-erlang-mode" '("smart-jump-erlang-mode-register")))

;;;***

;;;### (autoloads nil "smart-jump-go-mode" "smart-jump-go-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-go-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-go-mode" '("smart-jump-go-mode-")))

;;;***

;;;### (autoloads nil "smart-jump-haskell-mode" "smart-jump-haskell-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-haskell-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-haskell-mode" '("smart-jump-haskell-mode-")))

;;;***

;;;### (autoloads nil "smart-jump-js2-mode" "smart-jump-js2-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-js2-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-js2-mode" '("smart-jump-js2-mode-register")))

;;;***

;;;### (autoloads nil "smart-jump-lisp-mode" "smart-jump-lisp-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-lisp-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-lisp-mode" '("smart-jump-lisp-")))

;;;***

;;;### (autoloads nil "smart-jump-lispy" "smart-jump-lispy.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from smart-jump-lispy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-lispy" '("smart-jump-lispy-")))

;;;***

;;;### (autoloads nil "smart-jump-lsp-mode" "smart-jump-lsp-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-lsp-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-lsp-mode" '("smart-jump-lsp-mode-")))

;;;***

;;;### (autoloads nil "smart-jump-lua-mode" "smart-jump-lua-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-lua-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-lua-mode" '("smart-jump-lua-mode-register")))

;;;***

;;;### (autoloads nil "smart-jump-python" "smart-jump-python.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-python.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-python" '("smart-jump-python-")))

;;;***

;;;### (autoloads nil "smart-jump-ruby-mode" "smart-jump-ruby-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-ruby-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-ruby-mode" '("smart-jump-ruby-")))

;;;***

;;;### (autoloads nil "smart-jump-rust-mode" "smart-jump-rust-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-rust-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-rust-mode" '("smart-jump-rust-mode-r")))

;;;***

;;;### (autoloads nil "smart-jump-scala-mode" "smart-jump-scala-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-scala-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-scala-mode" '("smart-jump-scala-mode-")))

;;;***

;;;### (autoloads nil "smart-jump-scheme" "smart-jump-scheme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-scheme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-scheme" '("smart-jump-scheme-")))

;;;***

;;;### (autoloads nil "smart-jump-swift-mode" "smart-jump-swift-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-swift-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-swift-mode" '("smart-jump-swift-mode-register")))

;;;***

;;;### (autoloads nil "smart-jump-typescript-mode" "smart-jump-typescript-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-typescript-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-typescript-mode" '("smart-jump-typescript-")))

;;;***

;;;### (autoloads nil "smart-jump-web-mode" "smart-jump-web-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-web-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-web-mode" '("smart-jump-web-mode-register")))

;;;***

;;;### (autoloads nil nil ("smart-jump-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smart-jump-autoloads.el ends here

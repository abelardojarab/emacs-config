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

(autoload 'smart-jump-diag "smart-jump" "\
Pop a buffer with information about `smart-jump'.

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

(autoload 'smart-jump-cc-mode-register "smart-jump-cc-mode" "\
Register `cc-mode' for `smart-jump'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "smart-jump-clojure-mode" "smart-jump-clojure-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-clojure-mode.el

(autoload 'smart-jump-clojure-mode-register "smart-jump-clojure-mode" "\
Register `clojure-mode' for `smart-jump'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-clojure-mode" '("smart-jump-clojure-cider-available-p")))

;;;***

;;;### (autoloads nil "smart-jump-csharp-mode" "smart-jump-csharp-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-csharp-mode.el

(autoload 'smart-jump-csharp-mode-register "smart-jump-csharp-mode" "\
Register `smart-jump' for `csharp-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-csharp-mode" '("smart-jump-csharp-omnisharp-available-p")))

;;;***

;;;### (autoloads nil "smart-jump-eglot" "smart-jump-eglot.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from smart-jump-eglot.el

(autoload 'smart-jump-eglot-register "smart-jump-eglot" "\
Register `eglot' for `smart-jump'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-eglot" '("smart-jump-eglot-order")))

;;;***

;;;### (autoloads nil "smart-jump-elisp-mode" "smart-jump-elisp-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-elisp-mode.el

(autoload 'smart-jump-elisp-mode-register "smart-jump-elisp-mode" "\
Register `smart-jump' for `elisp-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-elisp-mode" '("smart-jump-elisp-slime-nav-available-p")))

;;;***

;;;### (autoloads nil "smart-jump-elixir-mode" "smart-jump-elixir-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-elixir-mode.el

(autoload 'smart-jump-elixir-mode-register "smart-jump-elixir-mode" "\
Register `smart-jump' for `elixir-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-elixir-mode" '("smart-jump-elixir-alchemist-available-p")))

;;;***

;;;### (autoloads nil "smart-jump-elm-mode" "smart-jump-elm-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-elm-mode.el

(autoload 'smart-jump-elm-mode-register "smart-jump-elm-mode" "\
Register `elm-mode' for `smart-jump'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "smart-jump-erlang-mode" "smart-jump-erlang-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-erlang-mode.el

(autoload 'smart-jump-erlang-mode-register "smart-jump-erlang-mode" "\
Register `erlang-mode' for `smart-jump'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "smart-jump-go-mode" "smart-jump-go-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-go-mode.el

(autoload 'smart-jump-go-mode-register "smart-jump-go-mode" "\
Register `go-mode' for `smart-jump'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-go-mode" '("smart-jump-go-mode-go-guru-available-p")))

;;;***

;;;### (autoloads nil "smart-jump-haskell-mode" "smart-jump-haskell-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-haskell-mode.el

(autoload 'smart-jump-haskell-mode-register "smart-jump-haskell-mode" "\
Register `smart-jump' for `haskell-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-haskell-mode" '("smart-jump-haskell-mode-")))

;;;***

;;;### (autoloads nil "smart-jump-js2-mode" "smart-jump-js2-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-js2-mode.el

(autoload 'smart-jump-js2-mode-register "smart-jump-js2-mode" "\
Register `js2-mode' for `smart-jump'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "smart-jump-lisp-mode" "smart-jump-lisp-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-lisp-mode.el

(autoload 'smart-jump-lisp-mode-register "smart-jump-lisp-mode" "\
Register `smart-jump' for `lisp-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-lisp-mode" '("smart-jump-lisp-sl")))

;;;***

;;;### (autoloads nil "smart-jump-lispy" "smart-jump-lispy.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from smart-jump-lispy.el

(autoload 'smart-jump-lispy-register "smart-jump-lispy" "\
Register `smart-jump' for `lispy'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-lispy" '("smart-jump-lispy-order")))

;;;***

;;;### (autoloads nil "smart-jump-lsp-mode" "smart-jump-lsp-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-lsp-mode.el

(autoload 'smart-jump-lsp-mode-register "smart-jump-lsp-mode" "\
Register `lsp-mode' for `smart-jump'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-lsp-mode" '("smart-jump-lsp-mode-order")))

;;;***

;;;### (autoloads nil "smart-jump-lua-mode" "smart-jump-lua-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-lua-mode.el

(autoload 'smart-jump-lua-mode-register "smart-jump-lua-mode" "\
Register `smart-jump' for `lua-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "smart-jump-python" "smart-jump-python.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-python.el

(autoload 'smart-jump-python-register "smart-jump-python" "\
Register `smart-jump' for `python'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-python" '("smart-jump-python-")))

;;;***

;;;### (autoloads nil "smart-jump-ruby-mode" "smart-jump-ruby-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-ruby-mode.el

(autoload 'smart-jump-ruby-mode-register "smart-jump-ruby-mode" "\
Register `smart-jump' for `ruby-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-ruby-mode" '("smart-jump-ruby-robe-available-p")))

;;;***

;;;### (autoloads nil "smart-jump-rust-mode" "smart-jump-rust-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-rust-mode.el

(autoload 'smart-jump-rust-mode-register "smart-jump-rust-mode" "\
Register `smart-jump' for `rust-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-rust-mode" '("smart-jump-rust-mode-racer-available-p")))

;;;***

;;;### (autoloads nil "smart-jump-scala-mode" "smart-jump-scala-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-scala-mode.el

(autoload 'smart-jump-scala-mode-register "smart-jump-scala-mode" "\
Register `smart-jump' for `scala-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-scala-mode" '("smart-jump-scala-mode-ensime-available-p")))

;;;***

;;;### (autoloads nil "smart-jump-scheme" "smart-jump-scheme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-scheme.el

(autoload 'smart-jump-scheme-register "smart-jump-scheme" "\
Register `smart-jump' for `scheme'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-scheme" '("smart-jump-scheme-geiser-available-p")))

;;;***

;;;### (autoloads nil "smart-jump-swift-mode" "smart-jump-swift-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-swift-mode.el

(autoload 'smart-jump-swift-mode-register "smart-jump-swift-mode" "\
Register `smart-jump' for `swift-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "smart-jump-typescript-mode" "smart-jump-typescript-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-typescript-mode.el

(autoload 'smart-jump-typescript-mode-register "smart-jump-typescript-mode" "\
Register `smart-jump' for `typescript-mode'.

\(fn &optional MODE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-jump-typescript-mode" '("smart-jump-typescript-")))

;;;***

;;;### (autoloads nil "smart-jump-web-mode" "smart-jump-web-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-jump-web-mode.el

(autoload 'smart-jump-web-mode-register "smart-jump-web-mode" "\
Register `smart-jump' for `web-mode'.

\(fn)" nil nil)

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

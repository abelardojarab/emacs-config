;; -*-mode: Emacs-Lisp; -*-
;; Copyright (C) 1996-2015 Abelardo Jara-Berrocal
;; URL: http://pintucoperu.wordpress.com
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error t)
(defconst debian-emacs-flavor 'emacs24
  "A symbol representing the particular debian flavor of emacs running.
 Something like 'emacs20, 'xemacs20, etc.")
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/dadams")
(add-to-list 'load-path "~/.emacs.d/setup")
(add-to-list 'load-path "~/.emacs.d/elp")
(add-to-list 'load-path "~/.emacs.d/s")
(add-to-list 'load-path "~/.emacs.d/f")
(add-to-list 'load-path "~/.emacs.d/deferred")
(add-to-list 'load-path "~/.emacs.d/ctable")
(add-to-list 'load-path "~/.emacs.d/dash")
(add-to-list 'load-path "~/.emacs.d/tabbar")

;; cl-lib
(require 'cl-lib)
(defun cl--set-getf (plist tag val)
  (let ((p plist))
    (while (and p (not (eq (car p) tag))) (setq p (cdr (cdr p))))
    (if p (progn (setcar (cdr p) val) plist) (list* tag val plist))))

;; CEDET
(add-to-list 'load-path "~/.emacs.d/cedet")
(add-to-list 'load-path "~/.emacs/cedet/contrib")
(require 'cedet-remove-builtin)
(setq byte-compile-warnings nil)
(load-file "~/.emacs.d/cedet/cedet-devel-load.el")

(load-file "~/.emacs.d/cedet/contrib/cedet-contrib-load.el")
(global-ede-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "96ec5305ec9f275f61c25341363081df286d616a27a69904a35c9309cfa0fe1b" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "fb4bf07618eab33c89d72ddc238d3c30918a501cf7f086f2edf8f4edba9bd59f" default)))
 '(ecb-options-version "2.40")
 '(ede-locate-setup-options (quote (ede-locate-global ede-locate-locate)))
 '(ede-project-directories (quote ("~/workspace")))
 '(magit-use-overlays nil)
 '(protect-buffer-bury-p nil)
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote aggressive-indent-mode))
           (aggressive-indent-mode -1))
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (encoding . utf-8-unix)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jedi:highlight-function-argument ((t (:inherit eldoc-highlight-function-argument))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t)))))

;; Setup package
(require 'setup-package)

;; Setup general
(require 'setup-general)

;; Setup appearance
(require 'setup-appearance)

;; ;; Setup Cedet
;; (require 'setup-cedet)

;; Setup Org and LaTeX
(require 'setup-org)

;; Setup web support
(require 'setup-web)

;; Setup Etags and GTAGS
(require 'setup-tags)

;; Setup Spelling
(require 'setup-spell)

;; Setup Flycheck
(require 'setup-flycheck)

;; Setup compile
(require 'setup-compile)

;; Setup Ido and Flex
(require 'setup-ido)

;; Setup Autopair
(require 'setup-autopair)

;; Setup Eldoc
(require 'setup-eldoc)

;; Setup Yasnippet
(require 'setup-yasnippet)

;; Setup Auto-Complete
(require 'setup-auto-complete)

;; Setup markdown and Yaml
(require 'setup-markdown)

;; Setup Python
(require 'setup-python)

;; Setup Javascript mode
(require 'setup-js2-mode)

;; Setup Lisp mode
(require 'setup-lisp)

;; Setup VHDL/Verilog mode
(require 'setup-vhdl)

;; Setup bison/yacc/lex
(require 'setup-bison)

;; Setup bookmarks
(require 'setup-bookmarks)

;; Setup modeline and Smex
(require 'setup-modeline)

;; Setup recent
(require 'setup-recentf)

;; Setup tabbar
(require 'setup-tabbar)

;; Setup versioning
(require 'setup-versioning)

;; Setup helm
(require 'setup-helm)

;; Setup project
(require 'setup-project)

;; Setup dash
(require 'setup-dash)

;; Setup utilities
(require 'setup-utilities)

;; Setup ECB
(require 'setup-ecb)

;; Setup post
(require 'setup-post)

;; ;; Setup keys
(require 'setup-keys)

;; Setup server
(require 'setup-server)

(setq debug-on-error nil)

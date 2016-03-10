;; -*-mode: Emacs-Lisp; -*-
;; Copyright (C) 1996-2016 Abelardo Jara-Berrocal
;; URL: http://pintucoperu.wordpress.com
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-quit t)
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
(add-to-list 'load-path "~/.emacs.d/seq")
(add-to-list 'load-path "~/.emacs.d/emacs-buttercup")

;; Missing cl-lib function
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "96ec5305ec9f275f61c25341363081df286d616a27a69904a35c9309cfa0fe1b" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "fb4bf07618eab33c89d72ddc238d3c30918a501cf7f086f2edf8f4edba9bd59f" default)))
 '(ecb-options-version "2.40")
 '(ede-locate-setup-options (quote (ede-locate-global ede-locate-locate)))
 '(ede-project-directories (quote ("~/workspace")))
 '(magit-use-overlays t)
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

;; Setup utilities
(require 'setup-functions)

;; Setup package
(require 'setup-package)

;; Setup general
(require 'setup-general)

;; Setup tramp
(require 'setup-tramp)

;; Setup appearance
(require 'setup-appearance)

;; Setup Cedet
(require 'setup-cedet)

;; Setup Org and LaTeX
(require 'setup-org)

;; Setup web support
(require 'setup-web)

;; Setup tags (optional)
(require 'setup-tags)

;; Setup spelling (optional)
(require 'setup-spell)

;; Setup Flycheck
(require 'setup-flycheck)

;; Setup compile
(require 'setup-compile)

;; Setup Ido (optional, disabled GUI for open file)
(require 'setup-ido)

;; Setup Autopair
(require 'setup-autopair)

;; Setup Yasnippet
(require 'setup-yasnippet)

;; Setup Auto-Insert
(require 'setup-auto-insert)

;; Setup Auto-Complete
(require 'setup-auto-complete)

;; Setup bookmarks
(require 'setup-bookmarks)

;; Setup modeline and Smex
(require 'setup-modeline)

;; Setup recent
(require 'setup-recentf)

;; Setup tabbar (optional)
(require 'setup-tabbar)

;; Setup versioning (optional)
(require 'setup-versioning)

;; Setup helm
(require 'setup-helm)

;; Setup Lisp mode
(require 'setup-lisp)

;; Setup Python
(require 'setup-python)

;; Setup markdown and Yaml
(require 'setup-markdown)

;; Setup Javascript
(require 'setup-js2)

;; Setup VHDL/Verilog mode
(require 'setup-vhdl)

;; Setup VHDL/Verilog mode
(require 'setup-spice)

;; Setup bison/yacc/lex
(require 'setup-bison)

;; Setup R/ess
(require 'setup-ess)

;; Setup polymode
(require 'setup-polymode)

;; Setup shell
(require 'setup-eshell)

;; Setup ECB
(require 'setup-ecb)

;; Setup hideshow
(require 'setup-hideshow)

;; Setup post
(require 'setup-post)

;; Setup keys
(require 'setup-keys)

;; Setup server
(require 'setup-server)

(setq debug-on-quit nil)
(setq debug-on-error nil)

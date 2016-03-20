;; -*-mode: Emacs-Lisp; -*-
;; Copyright (C) 1996-2016 Abelardo Jara-Berrocal
;; URL: http://pintucoperu.wordpress.com
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq debug-on-quit t)
(setq debug-on-error t)
(defconst debian-emacs-flavor 'emacs24
  "A symbol representing the particular debian flavor of emacs running.
 Something like 'emacs20, 'xemacs20, etc.")
(add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "dadams/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "setup/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "use-package/" user-emacs-directory))

;; Setup package
(require 'setup-package)

;; Setup functions
(require 'setup-functions)

;; CEDET
(add-to-list 'load-path (expand-file-name "cedet/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "cedet/contrib/" user-emacs-directory))
(require 'cedet-remove-builtin)
(setq byte-compile-warnings nil)
(load-file (expand-file-name "cedet/cedet-devel-load.el" user-emacs-directory))
(load-file (expand-file-name "cedet/contrib/cedet-contrib-load.el" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-enable-cua-keys nil)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "96ec5305ec9f275f61c25341363081df286d616a27a69904a35c9309cfa0fe1b" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "fb4bf07618eab33c89d72ddc238d3c30918a501cf7f086f2edf8f4edba9bd59f" default)))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("/" "/"))))
 '(ede-locate-setup-options (quote (ede-locate-global ede-locate-locate)))
 '(ede-project-directories (quote ("~/workspace")))
 '(ergoemacs-ctl-c-or-ctl-x-delay 0.2)
 '(ergoemacs-handle-ctl-c-or-ctl-x (quote both))
 '(ergoemacs-ini-mode t)
 '(ergoemacs-keyboard-layout "us")
 '(ergoemacs-mode nil)
 '(ergoemacs-smart-paste nil)
 '(ergoemacs-theme "standard")
 '(ergoemacs-theme-options nil)
 '(ergoemacs-use-menus t)
 '(initial-scratch-message
   ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

")
 '(org-CUA-compatible t)
 '(org-special-ctrl-a/e nil)
 '(org-support-shift-select (quote always))
 '(recentf-menu-before "Open File...")
 '(scroll-error-top-bottom nil)
 '(set-mark-command-repeat-pop nil)
 '(shift-select-mode t)
 '(smex-prompt-string "M-x "))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jedi:highlight-function-argument ((t (:inherit eldoc-highlight-function-argument))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t)))))

;; Setup environment
(require 'setup-environment)

;; Setup general
(require 'setup-general)

;; Setup files
(require 'setup-file)

;; Setup search
(require 'setup-search)

;; Setup tramp
(require 'setup-tramp)

;; Setup appearance
(require 'setup-appearance)

;; Setup fonts
(require 'setup-fonts)

;; Setup cursor
(require 'setup-cursor)

;; Setup themes
(require 'setup-themes)

;; Setup parenthesis
(require 'setup-parenthesis)

;; Setup indentation
(require 'setup-indent)

;; Setup Ido (optional, disabled GUI for open file, caused crash too?)
(require 'setup-ido)

;; Setup highlights
(require 'setup-highlight)

;; Setup desktop
(require 'setup-desktop)

;; Setup CEDET
(require 'setup-cedet)

;; Setup spelling (optional)
(require 'setup-spell)

;; Setup Org
(require 'setup-org)

;; Setup Org (babel support)
(require 'setup-org-babel)

;; Setup Org (image supporg)
(require 'setup-org-image)

;; Setup Org (latex support)
(require 'setup-org-latex)

;; Setup Org (html support)
(require 'setup-org-html)

;; Setup modeline
(require 'setup-modeline)

;; Setup tabbar (optional)
(require 'setup-tabbar)

;; Setup smex
(require 'setup-smex)

;; Setup hydra
(require 'setup-hydra)

;; Setup web support
(require 'setup-web)

;; Setup pandoc
(require 'setup-pandoc)

;; Setup tags (optional)
(require 'setup-tags)

;; Setup flycheck
(require 'setup-flycheck)

;; Setup compile
(require 'setup-compile)

;; Setup Yasnippet
(require 'setup-yasnippet)

;; Setup Auto-Insert
(require 'setup-auto-insert)

;; Setup Auto-Complete
(require 'setup-auto-complete)

;; Setup bookmarks
(require 'setup-bookmarks)

;; Setup recentf (causes crash?)
(require 'setup-recentf)

;; Setup dash
(require 'setup-dash)

;; Setup versioning (optional)
(require 'setup-versioning)

;; Setup Lisp mode (cause crash)
(require 'setup-lisp)

;; Setup Python
(require 'setup-python)

;; Setup HTML
(require 'setup-html)

;; Setup Javascript
(require 'setup-js2)

;; Setup YAML
(require 'setup-yaml)

;; Setup Java
(require 'setup-java)

;; Setup VHDL mode
(require 'setup-vhdl)

;; Setup VHDL mode
(require 'setup-verilog)

;; Setup Spice mode
(require 'setup-spice)

;; Setup bison/yacc/lex
(require 'setup-bison)

;; Setup R/ess
(require 'setup-ess)

;; Setup markdown
(require 'setup-markdown)

;; Setup polymode
(require 'setup-polymode)

;; Setup shell
(require 'setup-eshell)

;; Setup hideshow
(require 'setup-hideshow)

;; Setup imenu
(require 'setup-imenu)

;; Setup helm
(require 'setup-helm)

;; Setup helm plugin
(require 'setup-helm-plugins)

;; Setup post
(require 'setup-post)

;; Setup ECB
(require 'setup-ecb)

;; Setup keys
(require 'setup-keys)

;; Setup ergoemacs
(require 'setup-ergoemacs)

;; Setup server
(require 'setup-server)

(setq debug-on-quit nil)
(setq debug-on-error nil)

;; -*-mode: Emacs-Lisp; -*-
;; Copyright (C) 1996-2017 Abelardo Jara-Berrocal
;; URL: http://pintucoperu.wordpress.com
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-quit t)
(setq debug-on-error t)
(defvar debian-aspell-only-dictionary-alist nil)

(defconst debian-emacs-flavor 'emacs25
  "A symbol representing the particular debian flavor of emacs running.
 Something like 'emacs20, 'xemacs20, etc.")

;; Setup the starting directories
(if (file-exists-p "~/workspace/emacs-config/.emacs.d")
    (setq user-emacs-directory "~/workspace/emacs-config/.emacs.d"))

;; Basics
(add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "dadams/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "setup/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "use-package/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "helm/" user-emacs-directory))

;; Setup package
(require 'setup-package)

;; Setup functions
(require 'setup-functions)

;; CEDET
(setq byte-compile-warnings nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("06b2849748590f7f991bf0aaaea96611bb3a6982cad8b1e3fc707055b96d64ca" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "fbcdb6b7890d0ec1708fa21ab08eb0cc16a8b7611bb6517b722eba3891dfc9dd" "e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" "cd8176f5688f9a0086d5bc2cacca3c7c2b56808bceafc9df2eb2c2f45f97cea6" "6c803f5992d797ab94a94004b459cc3af3f3cfbe59652f1fece297335d437c81" "a7b211f45891a56000b8f823ad9edb564d873a689c21d9fba114d7a703f9b322" "793d74f2269fd3c2df12900e725fbffca5aabe55ab3c9a5c9d5259e05f0c4de8" "0953e81e91886e57512ef653bf32890e829540484dc5e471c162adb797b9a86d" "95dd1eeafe55e8f94d9234ec5b4214d34bdeee930dd8f71044a0722a3878bcb2" "445b8fed4feed166c021f1102ee9e5233158b71eb5402182fc0a4c6b17d67ba8" "2ab5e12f7513825ba869ca3de0f6185a9deb82c6b1a222eb25b0e38a60c7a606" "a75137dcd452a69cd404b4d44f0e50986196bcd9cf92bae49f8aa01038228221" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "96ec5305ec9f275f61c25341363081df286d616a27a69904a35c9309cfa0fe1b" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "fb4bf07618eab33c89d72ddc238d3c30918a501cf7f086f2edf8f4edba9bd59f" default)))
 '(ecb-options-version "2.50")
 '(ecb-source-path
   (quote
    (("~/workspace/Documents" "Documents")
     ("~/Dropbox/Documents" "Documents")
     ("~/workspace/emacsfull" "Emacs")
     ("~/workspace" "Workspace")
     ("~/" "Home")
     ("/" "File System"))))
 '(ede-locate-setup-options (quote (ede-locate-global ede-locate-locate)))
 '(ede-project-directories (quote ("~/workspace")))
 '(initial-scratch-message
   ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with 【▤】【o】,
;; then enter the text in that file's own buffer."))

;; Setup customs
(require 'setup-customs)

;; Setup environment
(require 'setup-environment)

;; Setup general
(require 'setup-general)

;; Setup abbrev
(require 'setup-dabbrev)

;; Setup files
(require 'setup-file)

;; Setup ediff
(require 'setup-ediff)

;; Setup search
(require 'setup-search)

;; Setup tramp
(require 'setup-tramp)

;; Setup appearance
(require 'setup-appearance)

;; Setup fonts
(require 'setup-fonts)

;; Setup font lock
(require 'setup-font-lock)

;; Setup region
(require 'setup-region)

;; Setup cursor
(require 'setup-cursor)

;; Setup scrolling
(require 'setup-scroll)

;; Setup themes
(require 'setup-themes)

;; Setup parenthesis
(require 'setup-parenthesis)

;; Setup indentation
(require 'setup-indent)

;; Setup ido (optional, disables GUI for open file)
(require 'setup-ido)

;; Setup highlights
(require 'setup-highlight)

;; Setup CEDET
(require 'setup-cedet)

;; Setup C++
(require 'setup-c++)

;; Setup doxygen
(require 'setup-doxygen)

;; Setup cmake
(require 'setup-cmake)

;; Setup spelling (optional)
(require 'setup-spell)

;; Setup gnuplot
(require 'setup-gnuplot)

;; Setup calendar
(require 'setup-calendar)

;; Setup Org
(require 'setup-org)

;; Setup Org Agenda
(require 'setup-org-agenda)

;; Setup Org (babel support)
(require 'setup-org-babel)

;; Setup Org plugins
(require 'setup-org-plugins)

;; Setup Org (latex support)
(require 'setup-org-latex)

;; Setup Org (html support)
(require 'setup-org-html)

;; Setup LaTeX
(require 'setup-latex)

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

;; Setup gdb integration
(require 'setup-gdb)

;; Setup Yasnippet
(require 'setup-yasnippet)

;; Setup Auto-Insert
(require 'setup-auto-insert)

;; Setup Company
(require 'setup-company)

;; Setup bookmarks
(require 'setup-bookmarks)

;; Setup recentf (causes crash?)
(require 'setup-recentf)

;; Setup versioning
(require 'setup-versioning)

;; Setup eldoc
(require 'setup-eldoc)

;; Setup Lisp
(require 'setup-lisp)

;; Setup Python
(require 'setup-python)

;; Setup Python Jedi
(require 'setup-python-plugins)

;; Setup Perl
(require 'setup-perl)

;; Setup HTML
(require 'setup-html)

;; Setup Javascript
(require 'setup-javascript)

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

;; Setup sqlite
(require 'setup-sqlite)

;; Setup Markdown
(require 'setup-markdown)

;; Setup PlantUML
(require 'setup-plantuml)

;; Setup xUnit
(require 'setup-xunit)

;; Setup folding
(require 'setup-folding)

;; Setup imenu
(require 'setup-imenu)

;; Setup windows
(require 'setup-windows)

;; Setup shell
(require 'setup-eshell)

;; Setup elscreen
(require 'setup-elscreen)

;; Setup Imagemagick support
(require 'setup-iimage)

;; Setup ivy
(require 'setup-ivy)

;; Setup helm
(require 'setup-helm)

;; Setup helm plugins
(require 'setup-helm-plugins)

;; Setup projectile
(require 'setup-projectile)

;; Setup dired
(require 'setup-dired)

;; Setup dired plugins
(require 'setup-dired-plugins)

;; Setup keychain
(require 'setup-keychain)

;; Setup gnus
(require 'setup-gnus)

;; Setup email
(require 'setup-email)

;; Setup Org (blogging support)
(require 'setup-org-blog)

;; Setup post
(require 'setup-post)

;; Setup ECB
(require 'setup-ecb)

;; Setup mouse
(require 'setup-mouse)

;; Setup undo and redo
(require 'setup-undoandredo)

;; Setup write room
(require 'setup-writeroom)

;; Setup regular expressions
(require 'setup-regex)

;; Setup keys
(require 'setup-keys)

;; Setup OS-specific keys
(require 'setup-keys-extensions)

;; Setup tab key
(require 'setup-tabkey)

;; Setup ergoemacs
(require 'setup-ergoemacs)

;; Disable checks
(setq debug-on-quit nil)
(setq debug-on-error nil)

;; Setup server
(require 'setup-server)

;; Setup network-related tools
(require 'setup-nettools)

;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-settings-dir)
  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))

;; Setup desktop
(require 'setup-desktop)

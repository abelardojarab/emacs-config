;; -*-mode: Emacs-Lisp; -*-
;; Copyright (C) 1996-2017 Abelardo Jara-Berrocal
;; URL: http://jaraberrocal.readmyblog.org
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

;; Only utilize local files
(let ((file-name-handler-alist nil))

  ;; Basics
  (add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "dadams/" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "setup/" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "use-package/" user-emacs-directory))

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

  ;; Setup keychain
  (require 'setup-keychain)

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

  ;; Setup projectile
  (require 'setup-projectile)

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

  ;; Setup dired
  (require 'setup-dired)

  ;; Setup dired plugins
  (require 'setup-dired-plugins)

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

  ) ;; let

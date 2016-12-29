;; -*-mode: Emacs-Lisp; -*-
;; Copyright (C) 1996-2016 Abelardo Jara-Berrocal
;; URL: http://pintucoperu.wordpress.com
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Added by package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq debug-on-quit t)
(setq debug-on-error t)
(defvar debian-aspell-only-dictionary-alist nil)

(defconst debian-emacs-flavor 'emacs24
  "A symbol representing the particular debian flavor of emacs running.
 Something like 'emacs20, 'xemacs20, etc.")

;; Setup the starting directories
(if (file-exists-p "~/workspace/emacs-config/.emacs.d")
    (setq user-emacs-directory "~/workspace/emacs-config/.emacs.d"))
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
(setq vc-follow-symlinks t)
;;(add-to-list 'load-path (expand-file-name "cedet/" user-emacs-directory))
;;(add-to-list 'load-path (expand-file-name "cedet/contrib/" user-emacs-directory))
;;(require 'cedet-remove-builtin)
;;(load-file (expand-file-name "cedet/cedet-devel-load.el" user-emacs-directory))
;;(load-file (expand-file-name "cedet/contrib/cedet-contrib-load.el" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(bmkp-last-as-first-bookmark-file "~/.emacs.cache/bookmarks")
 '(compilation-message-face (quote default))
 '(cua-enable-cua-keys nil)
 '(custom-safe-themes
   (quote
    ("06b2849748590f7f991bf0aaaea96611bb3a6982cad8b1e3fc707055b96d64ca" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "fbcdb6b7890d0ec1708fa21ab08eb0cc16a8b7611bb6517b722eba3891dfc9dd" "e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" "cd8176f5688f9a0086d5bc2cacca3c7c2b56808bceafc9df2eb2c2f45f97cea6" "6c803f5992d797ab94a94004b459cc3af3f3cfbe59652f1fece297335d437c81" "a7b211f45891a56000b8f823ad9edb564d873a689c21d9fba114d7a703f9b322" "793d74f2269fd3c2df12900e725fbffca5aabe55ab3c9a5c9d5259e05f0c4de8" "0953e81e91886e57512ef653bf32890e829540484dc5e471c162adb797b9a86d" "95dd1eeafe55e8f94d9234ec5b4214d34bdeee930dd8f71044a0722a3878bcb2" "445b8fed4feed166c021f1102ee9e5233158b71eb5402182fc0a4c6b17d67ba8" "2ab5e12f7513825ba869ca3de0f6185a9deb82c6b1a222eb25b0e38a60c7a606" "a75137dcd452a69cd404b4d44f0e50986196bcd9cf92bae49f8aa01038228221" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "96ec5305ec9f275f61c25341363081df286d616a27a69904a35c9309cfa0fe1b" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "fb4bf07618eab33c89d72ddc238d3c30918a501cf7f086f2edf8f4edba9bd59f" default)))
 '(ecb-options-version "2.40")
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
 '(ergoemacs-ctl-c-or-ctl-x-delay 0.2)
 '(ergoemacs-handle-ctl-c-or-ctl-x (quote both))
 '(ergoemacs-ini-mode t)
 '(ergoemacs-keyboard-layout "us" t)
 '(ergoemacs-mode nil)
 '(ergoemacs-smart-paste nil)
 '(ergoemacs-theme "standard" t)
 '(ergoemacs-theme-options nil)
 '(ergoemacs-use-menus t)
 '(fci-rule-color "#49483E")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#49483E" . 100))))
 '(initial-scratch-message
   ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with 【▤】【o】,
;; then enter the text in that file's own buffer.")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-CUA-compatible t)
 '(org-emphasis-alist
   (quote
    (("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t)))))
 '(org-special-ctrl-a/e t)
 '(org-support-shift-select (quote always))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#A6E22E" t)
 '(pos-tip-foreground-color "#272822" t)
 '(recentf-menu-before "Close")
 '(safe-local-variable-values
   (quote
    ((hl-sexp-mode)
     (rainbow-mode . t)
     (encoding . utf-8-unix)
     (eval c-set-offset
           (quote innamespace)
           0)
     (eval when
           (fboundp
            (quote aggressive-indent-mode))
           (aggressive-indent-mode -1))
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1)))))
 '(scroll-error-top-bottom t)
 '(set-mark-command-repeat-pop t)
 '(shift-select-mode t)
 '(smex-prompt-string "M-x ")
 '(transient-mark-mode t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(fa-face-hint ((t (:background "#3f3f3f" :foreground "#ffffff"))))
 '(fa-face-hint-bold ((t (:background "#3f3f3f" :weight bold))))
 '(fa-face-semi ((t (:background "#3f3f3f" :foreground "#ffffff" :weight bold))))
 '(fa-face-type ((t (:inherit (quote font-lock-type-face) :background "#3f3f3f"))))
 '(fa-face-type-bold ((t (:inherit (quote font-lock-type-face) :background "#999999" :bold t))))
 '(fringe ((t (:background "#1B1E1C"))))
 '(jedi:highlight-function-argument ((t (:inherit eldoc-highlight-function-argument))))
 '(tabbar-button ((t (:inherit tabbar-default :foreground "#272822"))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default))))
 '(tabbar-default ((t (:inherit fixed-pitch :background "#47493e" :foreground "#F8F8F2"))))
 '(tabbar-highlight ((t (:underline t))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "#272822"))))
 '(tabbar-separator ((t (:inherit tabbar-default :background "#47493e"))))
 '(tabbar-unselected ((t (:inherit tabbar-default)))))

;; Setup environment
(require 'setup-environment)

;; Setup general
(require 'setup-general)

;; Setup abbrev
(require 'setup-dabbrev)

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

;; Setup themes (setup)
(require 'setup-themes-setup)

;; Setup parenthesis
(require 'setup-parenthesis)

;; Setup indentation
(require 'setup-indent)

;; Setup Ido (optional, disabled GUI for open file, caused crash too?)
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

;; Setup gnuplot (so org-babel accepts)
(require 'setup-gnuplot)

;; Setup Org
(require 'setup-org)

;; Setup Org Agenda
(require 'setup-org-agenda)

;; Setup Org (babel support)
(require 'setup-org-babel)

;; Setup Org plugins
(require 'setup-org-plugins)

;; Setup Org (image supporg)
(require 'setup-org-image)

;; Setup Org (latex support)
(require 'setup-org-latex)

;; Setup Org (html support)
(require 'setup-org-html)

;; Setup Org (blogging support)
(require 'setup-org-blog)

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

;; Setup Auto-Complete
;; (require 'setup-auto-complete)

;; Setup Company
(require 'setup-company)

;; Setup bookmarks
(require 'setup-bookmarks)

;; Setup recentf (causes crash?)
(require 'setup-recentf)

;; Setup dash
(require 'setup-dash)

;; Setup versioning (optional)
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

;; Setup swiper
(require 'setup-swiper)

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

;; Setup polymode
(require 'setup-polymode)

;; Setup gnus
(require 'setup-gnus)

;; Setup email
(require 'setup-email)

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

;; Setup desktop
(require 'setup-desktop)

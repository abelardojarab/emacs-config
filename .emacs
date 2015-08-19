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
(add-to-list 'load-path "~/.emacs.d/setup")
(add-to-list 'load-path "~/.emacs.d/elp")
(add-to-list 'load-path "~/.emacs.d/s")
(add-to-list 'load-path "~/.emacs.d/f")
(add-to-list 'load-path "~/.emacs.d/dash")
(add-to-list 'load-path "~/.emacs.d/tabbar")
(add-to-list 'load-path "~/.emacs.d/python-mode")

;; cl-lib
(require 'cl-lib)
(defun cl--set-getf (plist tag val)
  (let ((p plist))
    (while (and p (not (eq (car p) tag))) (setq p (cdr (cdr p))))
    (if p (progn (setcar (cdr p) val) plist) (list* tag val plist))))

;; Backported function
(unless (fboundp 'pop-to-buffer)
  (defun pop-to-buffer (buffer-or-name &optional other-window norecord)
    "Select buffer BUFFER-OR-NAME in some window, preferably a different one...."
    (let ((buffer
           ;; FIXME: This behavior is carried over from the previous C version
           ;; of pop-to-buffer, but really we should use just
           ;; `get-buffer' here.
           (if (null buffer-or-name) (other-buffer (current-buffer))
             (or (get-buffer buffer-or-name)
                (let ((buf (get-buffer-create buffer-or-name)))
                  (set-buffer-major-mode buf)
                  buf))))
          (old-window (selected-window))
          (old-frame (selected-frame))
          new-window new-frame)
      (set-buffer buffer)
      (setq new-window (display-buffer buffer other-window) norecord)
      (unless (eq new-window old-window)
        ;; `display-buffer' has chosen another window.
        (setq new-frame (window-frame new-window))
        (unless (eq new-frame old-frame)
          ;; `display-buffer' has chosen another frame, make sure it gets
          ;; input focus and is risen.
          (select-frame-set-input-focus new-frame))
        ;; Make sure the window chosen by `display-buffer' gets selected.
        (select-window new-window))
      buffer)))

;; Backported function
(unless (fboundp 'define-error)
  (defun define-error (name message &optional parent)
    "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
    (unless parent (setq parent 'error))
    (let ((conditions
           (if (consp parent)
               (apply #'nconc
                      (mapcar (lambda (parent)
                                (cons parent
                                      (or (get parent 'error-conditions)
                                         (error "Unknown signal `%s'" parent))))
                              parent))
             (cons parent (get parent 'error-conditions)))))
      (put name 'error-conditions
           (delete-dups (copy-sequence (cons name conditions))))
      (when message (put name 'error-message message)))))

;; CEDET
(add-to-list 'load-path "~/.emacs.d/cedet")
(add-to-list 'load-path "~/.emacs/cedet/contrib")
(require 'cedet-remove-builtin)
(setq byte-compile-warnings nil)
(load-file "~/.emacs.d/cedet/cedet-devel-load.el")
(load-file "~/.emacs.d/cedet/contrib/cedet-contrib-load.el")
(global-ede-mode 1)
;; (ede-enable-generic-projects)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-fuzzy-limit 2)
 '(ac-ispell-requires 4)
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(cua-enable-cua-keys nil)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "96ec5305ec9f275f61c25341363081df286d616a27a69904a35c9309cfa0fe1b" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "fb4bf07618eab33c89d72ddc238d3c30918a501cf7f086f2edf8f4edba9bd59f" default)))
 '(delete-selection-mode t)
 '(ecb-display-image-icons-for-semantic-tags t)
 '(ecb-grep-find-function (quote if))
 '(ecb-grep-recursive-function (quote rgrep))
 '(ecb-highlight-tag-with-point (quote highlight-scroll))
 '(ecb-kill-buffer-clears-history (quote auto))
 '(ecb-methods-menu-sorter
   (lambda
     (entries)
     (let
         ((sorted
           (copy-list entries)))
       (sort sorted
             (quote string-lessp)))))
 '(ecb-options-version "2.40")
 '(ecb-prescan-directories-for-emptyness t)
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(ecb-process-non-semantic-files t)
 '(ecb-redraw-layout-quickly t)
 '(ecb-source-path (quote (("/" "/"))))
 '(ecb-sources-perform-read-only-check t)
 '(ecb-sources-sort-method (quote extension))
 '(ecb-tree-buffer-style (quote image))
 '(ecb-use-speedbar-instead-native-tree-buffer nil)
 '(ecb-vc-enable-support t)
 '(ede-locate-setup-options (quote (ede-locate-global ede-locate-locate)))
 '(ede-project-directories (quote ("~/workspace/frametools")))
 '(ergoemacs-mode t)
 '(initial-scratch-message ";; scratch buffer created -- start typing...")
 '(org-CUA-compatible nil)
 '(org-support-shift-select (quote always))
 '(protect-buffer-bury-p nil)
 '(recentf-mode t)
 '(safe-local-variable-values (quote ((encoding . utf-8-unix))))
 '(shift-select-mode nil))

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

;; Setup Cedet
(require 'setup-cedet)

;; Setup Org and LaTeX
(require 'setup-org)

;; Setup regular expressions
(require 'setup-regexp)

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

;; Setup Hideshow
(require 'setup-hideshow)

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

;; Setup versioning control
(require 'setup-versioning)

;; Setup modeline and Smex
(require 'setup-modeline)

;; Setup recent
(require 'setup-recentf)

;; Setup tabbar
(require 'setup-tabbar)

;; Setup project support
(require 'setup-project)

;; Setup utilities
(require 'setup-utilities)

;; Setup elnode
(require 'setup-elnode)

;; Setup eshell
(require 'setup-eshell)

;; Setup ECB
(require 'setup-ecb)

;; Setup post
(require 'setup-post)

;; Setup keys
(require 'setup-keys)

;; Setup gnus and Newsticker
;; (require 'setup-gnus)

;; Setup server
(unless (string-equal system-type "windows-nt")
  (require 'setup-server))

(setq debug-on-error nil)

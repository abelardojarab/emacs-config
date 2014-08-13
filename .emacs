;; -*-mode: Emacs-Lisp; -*-
;; Copyright (C) 1996-2014 Abelardo Jara-Berrocal
;; URL: http://pintucoperu.wordpress.com
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/elp")
(add-to-list 'load-path "~/.emacs.d/pkg-info")
(add-to-list 'load-path "~/.emacs.d/s")
(add-to-list 'load-path "~/.emacs.d/f")
(add-to-list 'load-path "~/.emacs.d/dash")
(add-to-list 'load-path "~/.emacs.d/use-package")
(add-to-list 'load-path "~/.emacs.d/fringe-helper")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-indent-delete-trailing-whitespace-on-save-file t)
 '(auto-indent-delete-trailing-whitespace-on-visit-file t)
 '(cua-enable-cua-keys nil)
 '(custom-safe-themes (quote ("96ec5305ec9f275f61c25341363081df286d616a27a69904a35c9309cfa0fe1b" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "fb4bf07618eab33c89d72ddc238d3c30918a501cf7f086f2edf8f4edba9bd59f" default)))
 '(ecb-auto-activate t)
 '(ecb-display-image-icons-for-semantic-tags t)
 '(ecb-grep-find-function (quote if))
 '(ecb-highlight-tag-with-point (quote highlight-scroll))
 '(ecb-kill-buffer-clears-history (quote auto))
 '(ecb-layout-name "left9")
 '(ecb-layout-window-sizes (quote (("left9" (ecb-methods-buffer-name 0.17391304347826086 . 0.9833333333333333)))))
 '(ecb-methods-menu-sorter (lambda (entries) (let ((sorted (copy-list entries))) (sort sorted (quote string-lessp)))))
 '(ecb-options-version "2.40")
 '(ecb-prescan-directories-for-emptyness t)
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(ecb-process-non-semantic-files t)
 '(ecb-redraw-layout-quickly t)
 '(ecb-sources-perform-read-only-check t)
 '(ecb-sources-sort-method (quote extension))
 '(ecb-tree-buffer-style (quote image))
 '(ecb-use-speedbar-instead-native-tree-buffer nil)
 '(ecb-vc-enable-support t)
 '(ede-locate-setup-options (quote (ede-locate-global ede-locate-locate)))
 '(safe-local-variable-values (quote ((encoding . utf-8-unix))))
 '(semantic-self-insert-show-completion-function (lambda nil (semantic-ia-complete-symbol-menu (point)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jedi:highlight-function-argument ((t (:inherit eldoc-highlight-function-argument)))))

;; Set path environment depending on OS.
(cond
 ;; Add Homebrew path in Darwin
 ((equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path))

 ;; Add Cygwin path in Windows
 ((equal system-type 'windows-nt)
  (when (file-directory-p "c:/cygwin/bin")
    (setenv "PATH" (concat "c:/cygwin/bin:" (getenv "PATH")))
    (add-to-list 'exec-path "c:/cygwin/bin"))
  (if (file-directory-p "c:/cygwin64/bin")
      (setenv "PATH" (concat "c:/cygwin64/bin:" (getenv "PATH")))
    (add-to-list 'exec-path "c:/cygwin64/bin"))))

;; Improve Emacs display engine
(setq redisplay-dont-pause t)

;; Define preferred shell
(setq shell-file-name "bash")
(setq explicit-shell-file-name shell-file-name)

;; avoid garbage collection up to 20M (default is only 400k)
(setq gc-cons-threshold 20000000)

;; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

;; turn-off info about of pointee for symlinks
(setq dired-listing-switches "-alLF")

;; Disable tool-bar
(tool-bar-mode -1)

;; Disable scroll bar
(set-scroll-bar-mode nil)

;; Make ?, ? and such work
(set-language-environment 'spanish)

;; Inhibit startup window, very annoying
(setq inhibit-startup-message t)

;; Make shell scrips executable on save. Good!
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Auto compile *.elc-files on save
(defun auto-byte-recompile ()
  "If the current buffer is in emacs-lisp-mode and there already exists an .elc file corresponding to the current buffer file, then recompile the file on save."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook 'auto-byte-recompile)

;; Zenburn theme
(add-to-list 'load-path "~/.emacs.d/zenburn-emacs")
(require 'zenburn-theme)
(load-theme 'zenburn t)

;; Extra color tweaks
(set-face-foreground 'variable-pitch "#ffffff")
(set-face-foreground 'font-lock-comment-face "#90f0ff")
(set-face-foreground 'font-lock-comment-delimiter-face "#90f0ff")
(set-face-foreground 'font-lock-string-face "yellow")
(set-face-foreground 'font-lock-doc-face "orange")

;; Syntax coloring
(global-font-lock-mode 1)

;; Use 10-pt Consolas as default font
(if (find-font (font-spec :name "Consolas"))
    (set-face-attribute 'default nil :font "Consolas-12"))

(if (find-font (font-spec :name "Calibri"))
    (set-face-attribute 'variable-pitch nil :font "Calibri-14" :weight 'normal))
(add-hook 'text-mode-hook 'variable-pitch-mode)

(if (find-font (font-spec :name "Consolas"))
    (set-face-attribute 'fixed-pitch nil :font "Consolas-12"))

;; Fallback for Unicode symbols
(if (find-font (font-spec :name "Symbola"))
    (set-fontset-font "fontset-default" nil
                      (font-spec :size 18 :name "Symbola")))

;; Change form/shape of emacs cursor
(setq djcb-read-only-color "gray")
(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color "gray")
(setq djcb-normal-cursor-type 'bar)
(defun djcb-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."
  (cond
   (buffer-read-only
    (set-cursor-color djcb-read-only-color)
    (setq cursor-type djcb-read-only-cursor-type))
   (overwrite-mode
    (set-cursor-color djcb-overwrite-color)
    (setq cursor-type djcb-overwrite-cursor-type))
   (t
    (set-cursor-color djcb-normal-color)
    (setq cursor-type djcb-normal-cursor-type))))
(add-hook 'post-command-hook
          (lambda () (interactive)
            (unless (member
                     major-mode '(pdf-docs doc-view-mode))
              (djcb-set-cursor-according-to-mode))))

;; Garantee utf8 as input-method
(setq locale-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(set-input-method nil)
(setq read-quoted-char-radix 10)
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

;; no extra whitespace after lines
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Emacs is a text editor, make sure your text files end in a newline
(setq require-final-newline 'query)

;; Put a nice title to the window, including filename
(add-hook 'window-configuration-change-hook
          (lambda ()
            (setq frame-title-format
                  (concat
                   invocation-name "@" system-name ": "
                   (replace-regexp-in-string
                    (concat "/home/" user-login-name) "~"
                    (or buffer-file-name "%b"))))))

;; Next code work with Emacs 21.4, 22.3, 23.1.
(when window-system
  (add-hook 'window-setup-hook
            (let ((px (display-pixel-width))
                  (py (display-pixel-height))
                  (fx (frame-char-width))
                  (fy (frame-char-height))
                  tx ty)
              ;; Next formulas discovered empiric on Windows host with default font.
              (setq tx (- (/ px fx) 3))
              (setq ty (- (/ py fy) 8))
              (setq initial-frame-alist '((top . 2) (left . 2)))
              (add-to-list 'initial-frame-alist (cons 'width tx))
              (add-to-list 'initial-frame-alist (cons 'height ty))
              t)))

;; Adjust Emacs size according to resolution
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        ;; use 130 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 140))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 150) (frame-char-height)))))))

;; cl-lib
(require 'cl-lib)
(defun cl--set-getf (plist tag val)
  (let ((p plist))
    (while (and p (not (eq (car p) tag))) (setq p (cdr (cdr p))))
    (if p (progn (setcar (cdr p) val) plist) (list* tag val plist))))

;; Popup, used by auto-complete and other tools
(add-to-list 'load-path "~/.emacs.d/popup")
(require 'popup)

;; Server configuration
(load "server")
(unless (server-running-p) (server-start))

;; Make Emacs ignore the "-e (make-frame-visible)"
;; that it gets passed when started by emacsclientw.
(add-to-list 'command-switch-alist '("(make-frame-visible)" .
                                     (lambda (s))))

;; Automatically kill all spawned processes on exit
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; Try not to split windows
(setq same-window-regexps '("."))

;; Assure window is splitted horizontally (for compilation buffer)
(setq split-width-threshold nil)

;; Avoid to make a separate frame
(setq display-buffer nil)
(setq display-buffer-reuse-frames t)
(setq pop-up-frames nil)

;; Avoid popup windows too
(add-to-list 'load-path "~/.emacs.d/popwin")
(require 'popwin)

;; CEDET
(add-to-list 'load-path "~/.emacs.d/cedet-bzr")
(add-to-list 'load-path "~/projects/cedet-bzr/contrib")
(require 'cedet-remove-builtin)
(setq byte-compile-warnings nil)
(load-file "~/.emacs.d/cedet-bzr/cedet-devel-load.el")
(load-file "~/.emacs.d/cedet-bzr/contrib/cedet-contrib-load.el")
(global-ede-mode 1)

;; Enable Semantic
(require 'semantic/ia)
(semantic-mode 1)
(global-semantic-idle-completions-mode)
(set-default 'semantic-case-fold t)
(global-set-key [?\C- ] 'semantic-ia-complete-symbol-menu)
(semantic-load-enable-code-helpers) ;; Enable prototype help and smart completion
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-highlight-func-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode t)

;; Enable code folding
(global-semantic-tag-folding-mode)

;; Don't reparse really big buffers.
(setq semantic-idle-scheduler-max-buffer-size 100000)

;; Small workloads
(setq semantic-idle-scheduler-idle-time 1)

;; Big workloads
(setq semantic-idle-scheduler-work-idle-time 60)

;; Default directory
(setq semanticdb-default-save-directory
      (expand-file-name "~/.emacs.cache/semanticdb"))
(setq ede-project-placeholder-cache-file "~/.emacs.cache/ede-projects.el")

;; what to cache
(setq semanticdb-persistent-path '(never))

;; ctags
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)
(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))

(require 'semantic/analyze/refs)
(defun ac-complete-semantic-self-insert (arg)
  (interactive "p")
  (self-insert-command arg)
  (ac-complete-semantic))

(defun cc-mode-ac-key-bindings ()
  (local-set-key "." 'ac-complete-semantic-self-insert)
  (local-set-key ">" 'ac-complete-semantic-self-insert)
  (local-set-key ":" 'ac-complete-semantic-self-insert))

(add-hook 'c-mode-common-hook 'cc-mode-ac-key-bindings)
(global-set-key (kbd "M-n") 'ac-complete-semantic-self-insert)

;; smart completions
(require 'semantic/ia)
(setq-mode-local emacs-lisp-mode semanticdb-find-default-throttle
                 '(unloaded system))
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(unloaded system))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(unloaded system))
(setq-mode-local lisp-mode semanticdb-find-default-throttle
                 '(unloaded system))
(setq-mode-local python-mode semanticdb-find-default-throttle
                 '(unloaded system))

;; Include settings
(require 'semantic/bovine/c)
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/clang)
(require 'semantic/bovine/scm-by)

(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public" "."
        "../.." "../../include" "../../inc" "../../common" "../../public"))
(setq cedet-sys-include-dirs (list
                              "/usr/include"
                              "/usr/include/c++"))
(let ((include-dirs cedet-user-include-dirs))
  (setq include-dirs (append include-dirs cedet-sys-include-dirs))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))
(setq semantic-c-dependency-system-include-path "/usr/include/")

;; Fast jump
(defun semantic-jump-hook ()
  (define-key c-mode-base-map [f1] 'semantic-ia-fast-jump)
  (define-key c-mode-base-map [S-f1]
    (lambda ()
      (interactive)
      (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
          (error "Semantic bookmark ring is currently empty"))
      (let* ((ring (oref semantic-mru-bookmark-ring ring))
             (alist (semantic-mrub-ring-to-assoc-list ring))
             (first (cdr (car alist))))
        (if (semantic-equivalent-tag-p (oref first tag)
                                       (semantic-current-tag))
            (setq first (cdr (car (cdr alist)))))
        (semantic-mrub-switch-tags first)))))
(add-hook 'c-mode-common-hook 'semantic-jump-hook)
(add-hook 'lisp-mode-common-hook 'semantic-jump-hook)
(add-hook 'python-mode-common-hook 'semantic-jump-hook)

;; Fast switch between header and implementation for C/C++
(defun dts-switch-between-header-and-source ()
  "Switch between a c/c++ header (.h) and its corresponding source (.c/.cpp)."
  (interactive)
  (setq bse (file-name-sans-extension buffer-file-name))
  (setq ext (downcase (file-name-extension buffer-file-name)))
  (cond
   ((or (equal ext "h") (equal ext "hpp"))
    ;; first, look for bse.c
    (setq nfn (concat bse ".c"))
    (if (file-exists-p nfn)
        (find-file nfn)
      (progn
        (setq nfn (concat bse ".cpp"))
        (find-file nfn))))
   ;; second condition - the extension is "cpp"
   ((equal ext "cpp")
    (setq nfn (concat bse ".h"))
    (if (file-exists-p nfn)
        (find-file nfn)
      (progn
        (setq nfn (concat bse ".hpp"))
        (find-file nfn))))

   ((equal ext "c")
    (setq nfn (concat bse ".h"))
    (find-file nfn))

   ((equal ext "hxx")
    (setq nfn (concat bse ".cxx"))
    (find-file nfn))

   ((equal ext "cxx")
    (setq nfn (concat bse ".hxx"))
    (find-file nfn))

   ((equal ext "hx")
    (setq nfn (concat bse ".cx"))
    (find-file nfn))

   ((equal ext "cx")
    (setq nfn (concat bse ".hx"))
    (find-file nfn))

   ) ;; cond
  ) ;; defun

(require 'eassist nil 'noerror)
(define-key c-mode-base-map [f12] 'eassist-switch-h-cpp)
(define-key c-mode-base-map [C-f12] 'dts-switch-between-header-and-source)

;; Eassist header switches
(setq eassist-header-switches
      '(("h" . ("cpp" "cxx" "c++" "CC" "cc" "C" "c" "mm" "m"))
        ("hh" . ("cc" "CC" "cpp" "cxx" "c++" "C"))
        ("hpp" . ("cpp" "cxx" "c++" "cc" "CC" "C"))
        ("hxx" . ("cxx" "cpp" "c++" "cc" "CC" "C"))
        ("h++" . ("c++" "cpp" "cxx" "cc" "CC" "C"))
        ("H" . ("C" "CC" "cc" "cpp" "cxx" "c++" "mm" "m"))
        ("HH" . ("CC" "cc" "C" "cpp" "cxx" "c++"))
        ("cpp" . ("hpp" "hxx" "h++" "HH" "hh" "H" "h"))
        ("cxx" . ("hxx" "hpp" "h++" "HH" "hh" "H" "h"))
        ("c++" . ("h++" "hpp" "hxx" "HH" "hh" "H" "h"))
        ("CC" . ("HH" "hh" "hpp" "hxx" "h++" "H" "h"))
        ("cc" . ("hh" "HH" "hpp" "hxx" "h++" "H" "h"))
        ("C" . ("hpp" "hxx" "h++" "HH" "hh" "H" "h"))
        ("c" . ("h"))
        ("m" . ("h"))
        ("mm" . ("h"))))

;; when skipping to errors, show a few lines above
(setq compilation-context-lines 1)

;; scroll compilation buffer
(setq compilation-scroll-output t)

;; make sure ant's output is in a format emacs likes
(setenv "ANT_ARGS" "-emacs")

;; gdb should use many windows, to make it look like an IDE
(setq gdb-many-windows t
      gdb-max-frames 120)

;; Tags table
(setq tag-table-alist
      '(("\\.il$" . "~/workspace/frametools/TAGS")
        ("\\.ils$" . "~/workspace/frametools/TAGS")))
(setq tags-always-build-completion-table t)

;; working with tags
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)
(semanticdb-enable-gnu-global-databases 'lisp-mode)
(semanticdb-enable-gnu-global-databases 'python-mode)

;; better searching of tags
(add-to-list 'load-path "~/.emacs.d/ggtags")
(add-to-list 'load-path "~/.emacs.d/etags-select")
(require 'etags-select)
(require 'gtags)
(require 'ggtags)

;; Get the path of gtags root directory.
(defun gtags-update ()
  (interactive)
  (let (buffer)
    (save-excursion
      (setq buffer (generate-new-buffer (generate-new-buffer-name "*rootdir*")))
      (set-buffer buffer)
      (call-process "global" nil t nil "-u")
      (kill-buffer buffer))))

;; Use GNU global instead of normal find-tag, fall back to etags-select
(global-set-key (kbd "M-.") (if (and (fboundp 'ggtags-find-tag-dwim)
                                     (executable-find "global"))
                                'ggtags-find-tag-dwim
                              'etags-select-find-tag))

;; Use ggtags instead of gtags
(add-hook 'c-mode-common-hook
          (lambda () (ggtags-mode t)))
(add-hook 'python-mode-common-hook
          (lambda () (ggtags-mode t)))
(add-hook 'lisp-mode-common-hook
          (lambda () (ggtags-mode t)))
(add-hook 'skill-mode-common-hook
          (lambda () (ggtags-mode t)))
(add-hook 'scheme-mode-common-hook
          (lambda () (ggtags-mode t)))

;; Function arguments
(add-to-list 'load-path "~/.emacs.d/functions-args")
(require 'function-args)
(fa-config-default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Use ido to list tags, but then select via etags-select (best of both worlds!)
(defun my-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (find-tag (replace-regexp-in-string "\\\\" "" (ido-completing-read "Tag: " tag-names)))))

;; Use GNU global if available
(global-set-key (kbd "M-.") (if (and (fboundp 'my-ido-find-tag) t)
                                'my-ido-find-tag
                                'ggtags-find-tag-dwim))

;; Flycheck
(add-to-list 'load-path "~/.emacs.d/flycheck")
(add-to-list 'load-path "~/.emacs.d/flycheck-tip")
(require 'f)
(require 'flycheck)
(require 'flycheck-tip)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
(add-hook 'lisp-mode-hook
          (lambda () (flycheck-mode t)))
(add-hook 'python-mode-hook
          (lambda () (flycheck-mode t)))

;; Highlight whole line with error
(setq flycheck-highlighting-mode 'lines)

;; Define a poor c/c++ checker (it fails when errors affect other files,
;; not the one being being checked actually)
(defmacro flycheck-define-clike-checker (name command modes)
  `(flycheck-define-checker ,(intern (format "%s" name))
     ,(format "A %s checker using %s" name (car command))
     :command (,@command source-inplace)
     :error-patterns
     ((warning line-start (file-name) ":" line ":" column
               ": warning: " (message) line-end)
      (error line-start (file-name) ":" line ":" column
             ": error: " (message) line-end))
     :modes ',modes))
(flycheck-define-clike-checker c-gcc
                               ("gcc" "-fsyntax-only" "-Wall" "-Wextra")
                               c-mode)
(add-to-list 'flycheck-checkers 'c-gcc)
(flycheck-define-clike-checker c++-g++
                               ("g++" "-fsyntax-only" "-Wall" "-Wextra" "-std=c++10")
                               c++-mode)
(add-to-list 'flycheck-checkers 'c++-g++)

;; Display error messages on one line in minibuffer and by new lines
;; separated in `flycheck-error-message-buffer'.
(defun flycheck-diplay-error-messages-one-line (errors)
  (-when-let (messages (-keep #'flycheck-error-message errors))
    (when (flycheck-may-use-echo-area-p)
      (message (s-join " | " messages))
      (with-current-buffer (get-buffer-create flycheck-error-message-buffer)
        (erase-buffer)
        (insert (s-join "\n\n" messages))))))
(setq flycheck-display-errors-function
      'flycheck-diplay-error-messages-one-line)

;; Keep session
(require 'desktop)
(desktop-save-mode 1)
(setq desktop-load-locked-desktop t)
(setq-default desktop-missing-file-warning nil)
(setq desktop-path '("~/"))
(setq desktop-dirname "~/")

;; Activate highlight in search and replace
(setq search-highlight t)
(setq query-replace-highlight t)

;; highlight indentation using vertical lines
(add-hook 'c-mode-common-hook 'indent-vline)
(add-hook 'lisp-mode-hook 'indent-vline)
(add-hook 'python-mode-hook 'indent-vline)
(load "~/.emacs.d/elisp/00_func.el")
(require 'aux-line)

;; show-paren-mode: subtle blinking of matching paren (defaults are ugly)
(show-paren-mode t)

;; Disable tooltips
(tooltip-mode nil)

;; deleting files goes to OS's trash folder
(setq delete-by-moving-to-trash t)

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Moving cursor down at bottom scrolls only a single line, not half page
(setq
 scroll-step 1
 scroll-margin 0                ;; start scrolling when marker at top/bottom
 scroll-conservatively 100000   ;; marker distance from center (don't jump to center)
 scroll-preserve-screen-position 1) ;; try to keep screen position when PgDn/PgUp

;; These ones are buffer local and thus have to be set up by setq-default
(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)

;; Moving cursor down at bottom scrolls only a single line, not half page
(setq scroll-step 1)
(setq auto-window-vscroll nil)

;; Put something different in the scratch buffer
(setq initial-scratch-message
      ";; scratch buffer created -- start typing...\n")

;; Automatically reload files after they've been modified
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; Bell instead of annoying beep
(setq visible-bell t)

;; Turn off the bell http://www.emacswiki.org/cgi-bin/wiki?AlarmBell
(setq ring-bell-function 'ignore)

;; Do not add empty lines at the end of our file if we press down key
(setq next-line-add-newlines nil)

;; Makes final line always be a return
(setq require-final-newline t)

;; When in text (or related mode) break the lines at 80 chars
(setq fill-column 160)
(require 'fill-column-indicator)

;; Marker if the line goes beyond the end of the screen (arrows)
(global-visual-line-mode 1)

;; Save bookmark when you save a file
(setq bookmark-save-flag 1)

;; In every buffer, the line which contains the cursor will be fully highlighted
(global-hl-line-mode 1)
(defun local-hl-line-mode-off ()
  (interactive)
  (make-local-variable 'global-hl-line-mode)
  (setq global-hl-line-mode nil))
(add-hook 'org-mode-hook 'local-hl-line-mode-off)

;; Set indent to 4 instead of 2
(setq standard-indent 4)

;; Use spaces instead of tab
(setq-default indent-tabs-mode nil)

;; Set tab width
(setq-default tab-width 4)

;; Auto-indent mode
(add-to-list 'load-path "~/.emacs.d/auto-indent-mode")
(setq auto-indent-on-visit-file t)
(require 'auto-indent-mode)
(auto-indent-global-mode)

;; auto-indent pasted code
(defadvice yank (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
                                objc-mode latex-mode plain-tex-mode python-mode))
      (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
                                objc-mode latex-mode plain-tex-mode python-mode))
      (indent-region (region-beginning) (region-end) nil)))

;; Mouse wheel scroll support
(mouse-wheel-mode t)

;; Autosave
(setq auto-save-list-file-prefix "~/.emacs.cache/auto-save-list/.saves-")

;; Backups
(setq make-backup-files t
      backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.cache/backups"))
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t)

;; Preserve the owner and group of the file you're editing
(setq backup-by-copying-when-mismatch t)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; Remember the position where we closed a file
(setq save-place-file "~/.emacs.cache/emacs.saveplace")
(setq-default save-place t) ;; activate it for all buffers
(require 'saveplace)

;; Savehist: save some history
(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 120
      savehist-file "~/.emacs.cache/savehist")
(savehist-mode t)

;; filecache: http://www.emacswiki.org/cgi-bin/wiki/FileNameCache
(eval-after-load "filecache"
  '(progn (message "Loading file cache...")
          (file-cache-add-directory "~/")
          (file-cache-add-directory-list '("~/Desktop" "~/Documents" "~/workspace"))))

;; Ignore case when looking for a file
(setq read-file-name-completion-ignore-case t)

;; Insertion of Dates, bind to C-c i
(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; Brackets matching script
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; Abbrevs
(setq abbrev-file-name "~/.emacs.cache/abbrev_defs")
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(add-hook 'kill-emacs-hook
          'write-abbrev-file)

;; Activate template autocompletion
(abbrev-mode t)
(setq default-abbrev-mode t
      save-abbrevs t)

;; Close compile buffer if no errors nor warnings
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          **(goto-char 1)**
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; Helper for compilation. Close the compilation window if there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))

;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)
(setq compilation-scroll-output 1) ;; automatically scroll the compilation window
(setq compilation-window-height 7) ;; Set the compilation window height...

;; Better compile buffer
(require 'compile)
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq
             compilation-scroll-output 'first-error  ; scroll until first error
             ;; compilation-read-command nil          ; don't need enter
             compilation-window-height 11)

            (local-set-key (kbd "<M-up>")   'previous-error)
            (local-set-key (kbd "<M-down>") 'next-error)

            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -o %s %s %s"
                             (or (getenv "CC") "g++")
                             (file-name-sans-extension file)
                             ;;(or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") " -g -O2")
                             file)))))
          ;; (number of things in " " in format must match number of arg. in getenv.)

          ;; This will run Make if there is a Makefile in the same directory as the
          ;; source-file, or it will create a command for compiling a single
          ;; file and name the executable the same name as the file with the extension
          ;; stripped.
          )

;; Uniquify-buffers
(when (require 'uniquify nil 'noerror)  ;; make buffer names more unique
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"
   uniquify-after-kill-buffer-p t       ;; rename after killing uniquified
   uniquify-ignore-buffers-re "^\\*"))  ;; don't muck with special buffers

;; Tabbar mode
(add-to-list 'load-path "~/.emacs.d/tabbar")
(require 'tabbar)
(tabbar-mode)
(global-set-key [s-left] 'tabbar-backward)
(global-set-key [s-right] 'tabbar-forward)
(global-set-key [s-up] 'tabbar-forward-group)
(global-set-key [s-down] 'tabbar-backward-group)
(global-set-key "\C-c+" 'tabbar-forward)
(global-set-key "\C-c-" 'tabbar-backward)
(global-set-key "\C-c-" 'tabbar-backward)
(global-set-key "\C-c*" 'other-window)
(global-set-key [C-prior] 'tabbar-backward)
(global-set-key [C-next] 'tabbar-forward)
(global-set-key [M-prior] 'tabbar-backward-group)
(global-set-key [M-next] 'tabbar-forward-group)
(global-set-key [(control meta prior)] 'tabbar-forward-group)
(global-set-key [(control meta next)] 'tabbar-backward-group)

;; Tweaking the tabbar
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
                 (buffer-file-name (tabbar-tab-value tab)))
            (concat "+" (concat ad-return-value ""))
          (concat "" (concat ad-return-value "")))))

;; called each time the modification state of the buffer changed
(defun ztl-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

;; first-change-hook is called BEFORE the change is made
(defun ztl-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ztl-modification-state-change))
(add-hook 'after-save-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)

;; more tweaking to tabbar
(add-to-list 'load-path "~/.emacs.d/tabbar-ruler")
(setq tabbar-ruler-global-tabbar 't) ; If you want tabbar
(require 'tabbar-ruler)
(setq tabbar-separator '(0.5))

;; Define tabbar groups
(setq tabbar-buffer-groups-function
      (lambda ()
        (list (cond
               ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs")
               ((eq major-mode 'dired-mode) "Dired")
               ((eq major-mode 'compilation-mode) "Compilation")
               (t "User")))))

;; necessary support function for buffer burial
(defun crs-delete-these (delete-these from-this-list)
  "Delete DELETE-THESE FROM-THIS-LIST."
  (cond
   ((car delete-these)
    (if (member (car delete-these) from-this-list)
        (crs-delete-these (cdr delete-these) (delete (car delete-these)
                                                     from-this-list))
      (crs-delete-these (cdr delete-these) from-this-list)))
   (t from-this-list)))

;; this is the list of avoided buffers
(defvar crs-hated-buffers
  '("KILL" "*Compile-Log*"))
(add-to-list 'crs-hated-buffers "*Messages*")
(add-to-list 'crs-hated-buffers "*Completions*")
(add-to-list 'crs-hated-buffers "*scratch*")
(add-to-list 'crs-hated-buffers "*etags tmp*")
(add-to-list 'crs-hated-buffers "*Python*")
(add-to-list 'crs-hated-buffers "vc")

;; might as well use this for both
(setq iswitchb-buffer-ignore (append '("^ " "*Buffer") crs-hated-buffers))
(defun crs-hated-buffers ()
  "List of buffers I never want to see, converted from names to buffers."
  (delete nil
          (append
           (mapcar 'get-buffer crs-hated-buffers)
           (mapcar (lambda (this-buffer)
                     (if (string-match "^ " (buffer-name this-buffer))
                         this-buffer))
                   (buffer-list)))))

;; bury buffer function
(defun crs-bury-buffer (&optional n)
  (interactive)
  (unless n
    (setq n 1))
  (let ((my-buffer-list (crs-delete-these (crs-hated-buffers)
                                          (buffer-list (selected-frame)))))
    (switch-to-buffer
     (if (< n 0)
         (nth (+ (length my-buffer-list) n)
              my-buffer-list)
       (bury-buffer)
       (nth n my-buffer-list)))))
(global-set-key  [C-next] 'crs-bury-buffer)
(global-set-key  [C-prior] (lambda ()
                             (interactive)
                             (crs-bury-buffer -1)))

;; Edition of EMACS edition modes
(setq major-mode 'text-mode)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook (function
                           (lambda ()(ispell-minor-mode))))

;; Enter changes lines and auto-indents the new line
(add-hook 'java-mode-hook
          '(lambda ()
             (define-key java-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'c-mode-hook
          '(lambda ()
             (define-key c-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'c++-mode-hook
          '(lambda ()
             (define-key c++-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'vhdl-mode-hook
          '(lambda ()
             (define-key vhdl-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (define-key lisp-mode-map "\C-m" 'reindent-then-newline-and-indent)))

(add-hook 'skill-mode-hook
          '(lambda ()
             (define-key lisp-mode-map "\C-m" 'reindent-then-newline-and-indent)))

;; Cut the lines at 80 characters; I dont like it but it is a convention
(add-hook 'c++-mode-hook 'turn-on-auto-fill)
(add-hook 'c-mode-hook 'turn-on-auto-fill)
(add-hook 'vhdl-mode-hook 'turn-on-auto-fill)
(add-hook 'python-mode-hook 'turn-on-auto-fill)

;; Make a #define be left-aligned
(setq c-electric-pound-behavior (quote (alignleft)))

;; Rainbow delimiters
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(when (require 'rainbow-delimiters nil 'noerror)
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; End of line
(require 'eol-conversion)

;; Pretty diff mode
(autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files-remote "ediff"
  "Intelligent Emacs interface to diff")

;; Change type files
(setq auto-mode-alist
      (append '(("\\.cpp$" . c++-mode)
                ("\\.hpp$" . c++-mode)
                ("\\.lsp$" . lisp-mode)
                ("\\.il$" . lisp-mode)
                ("\\.ils$" . lisp-mode)
                ("\\.scm$" . scheme-mode)
                ("\\.pl$" . perl-mode)
                ) auto-mode-alist))

;; Put file name on clip board
(defun put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; ido makes competing buffers and finding files easier
(add-to-list 'load-path "~/.emacs.d/flx")
(require 'ido)
(require 'flx-ido)
(ido-mode 'both) ;; for buffers and files
(ido-everywhere 1)
(flx-ido-mode 1)

(setq
 ido-save-directory-list-file "~/.emacs.cache/ido.last"
 ido-ignore-buffers ;; ignore these guys
 '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
   "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
 ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
 ido-case-fold  t
 ido-enable-last-directory-history t
 ido-max-work-directory-list 30
 ido-max-work-file-list 50
 ido-use-filename-at-point nil
 ido-use-url-at-point nil
 ido-enable-flex-matching t
 ido-max-prospects 8
 ido-confirm-unique-completion t)

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

;; Helm
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)

;; Line numbers, vim style
(require 'linum)
(global-linum-mode 1)
(set-face-attribute 'linum nil :height 125)

(eval-after-load 'linum
  '(progn
     (defface linum-leading-zero
       `((t :inherit 'linum
            :foreground ,(face-attribute 'linum :background nil t)))
       "Face for displaying leading zeroes for line numbers in display margin."
       :group 'linum)
     (defun linum-format-func (line)
       (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
         (propertize (format (format " %%%dd " w) line) 'face 'linum)))
     (setq linum-format 'linum-format-func)))

;; Autopair
(add-to-list 'load-path "~/.emacs.d/autopair")
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
(setq autopair-autowrap t)
(put 'autopair-insert-opening 'delete-selection t)
(put 'autopair-skip-close-maybe 'delete-selection t)
(put 'autopair-insert-or-skip-quote 'delete-selection t)
(put 'autopair-extra-insert-opening 'delete-selection t)
(put 'autopair-extra-skip-close-maybe 'delete-selection t)
(put 'autopair-backspace 'delete-selection 'supersede)
(put 'autopair-newline 'delete-selection t)

;; only needed if you use autopair
(add-hook 'text-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))
(add-hook 'org-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))

;; Eldoc
(require 'eldoc)
(require 'eldoc-extension)
(setq eldoc-echo-area-use-multiline-p t)
(setq eldoc-idle-delay 0)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)))
(add-hook 'lisp-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)))
(add-hook 'skill-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)))
(add-hook 'scheme-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)))
(add-hook 'python-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)))

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)
(set-face-attribute 'eldoc-highlight-function-argument nil :underline "red")

;; Filladapt mode for text files
(require 'filladapt)
(add-hook 'text-mode-hook 'turn-on-filladapt-mode)

;; Pretty lambdas
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'lisp-mode-hook 'pretty-lambdas)
(add-hook 'scheme-mode-hook 'pretty-lambdas)
(add-to-list 'load-path "~/.emacs.d/pretty-symbols")
(require 'pretty-symbols)
(add-hook 'lisp-mode-hook 'pretty-symbols-mode)
(add-to-list 'load-path "~/.emacs.d/pretty-mode")
(require 'pretty-mode)
(global-pretty-mode t)

;; Yasnippet (should be invoked before auto-complete)
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; Remove Yasnippet's default tab key binding (avoid collision with auto-complete)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;; Select a snippet with popup library
(setq yas-prompt-functions '(yas-popup-prompt yas-no-prompt))

;; Tweaking Yasnippet for Org mode
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

;; Provide headers or templates for new files using Yasnippet
(defun yas/expand-by-uuid (mode uuid)
  "Exapnd snippet template in MODE by its UUID"
  (yas/expand-snippet
   (yas/template-content
    (yas/get-template-by-uuid mode uuid))))

;; Yasnippet templates used in auto-insert mode
(require 'autoinsert)
(auto-insert-mode)
(setq auto-insert-query nil)
(define-auto-insert "\.R"
  '(lambda () (yas/expand-by-uuid 'ess-mode "header")))

;; Get email from Magit if available
(defun yas--magit-email-or-default ()
  "Get email from GIT or use default"
  (if (magit-get-top-dir ".")
      (magit-get "user.email")
    user-mail-address))

;; Auto-Complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(require 'auto-complete)
(ac-config-default)
(setq-default ac-sources '(ac-source-semantic-raw))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
(global-auto-complete-mode t)
(auto-complete-mode 1)
(setq ac-show-menu-immediately-on-auto-complete t)
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; General settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 30
 ac-quick-help-height 30)

;; Make it a bit faster
(setq
 ac-delay 0.5 ;; same as Eclipse
 ac-auto-show-menu 0.5
 ac-quick-help-delay 0.5)

;; this is used for trigger ac actions from org-mode also
(add-to-list 'ac-trigger-commands 'org-self-insert-command)

;; create and add new words to the dictionary on the fly
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories "~/.emacs.cache/ac-dict")
  (setq ac-comphist-file  "~/.emacs.cache/ac-comphist.dat")
  (ac-config-default))

;; Autocomplete with TAGS
(add-to-list 'load-path "~/.emacs.d/auto-complete-etags")
(require 'auto-complete-etags)
(setq ac-etags-use-document t)

;; Let's have snippets and TAGS in the auto-complete dropdown
(defun ac-common-setup ()
  (setq ac-sources (append ac-sources '(ac-source-yasnippet ac-source-etags ac-source-gtags ac-source-semantic ac-source-semantic-raw))))
(add-hook 'auto-complete-mode-hook 'ac-common-setup)

;; We need tell emacs to use aspell, and where your custom dictionary is.
(require 'ispell)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(if (eq system-type 'darwin)
    (if (file-executable-p "/usr/local/bin/aspell")
        (progn
          (setq ispell-program-name "/usr/local/bin/aspell")
          (setq ispell-extra-args '("-d" "/Library/Application Support/cocoAspell/aspell6-en-6.0-0/en.multi")))))

;; change dictionary: "C-c e" = engelska, "C-c s"=spanish, "C-c w"=turn off flyspell
(add-hook 'text-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c s 2")
                            (lambda () (interactive)
                              (ispell-change-dictionary "american")
                              (flyspell-mode 1)
                              (flyspell-buffer)))
             (local-set-key (kbd "C-c s 1")
                            (lambda () (interactive)
                              (ispell-change-dictionary "spanish")
                              (flyspell-mode 1)
                              (flyspell-buffer)))
             (local-set-key (kbd "C-c s 0")
                            (lambda () (interactive)
                              (flyspell-mode -1)))))

;; flyspell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Markdown
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Yaml mode
(add-to-list 'load-path "~/.emacs.d/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Zoom in/out like feature, with mouse wheel
(global-unset-key (kbd "<C-wheel-up>")) ;; moved to <mode-line>
(global-unset-key (kbd "<C-wheel-down>"))
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase) ;; moved to <mode-line>
(global-set-key  (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Zoom in/out like feature, without mouse wheel
(global-set-key '[C-kp-add] 'text-scale-increase)
(global-set-key '[C-kp-subtract] 'text-scale-decrease)
(global-set-key '[C-+] 'text-scale-increase)

;; Quack
(require 'quack)

;; Fix tab problem in some modes that grab the tab key so auto-complete and yasnipet dont work
(defun iy-ac-tab-noconflict ()
  (let ((command (key-binding [tab]))) ; remember command
    (local-unset-key [tab]) ; unset from (kbd "<tab>")
    (local-set-key (kbd "TAB") command))) ; bind to (kbd "TAB")
(add-hook 'ruby-mode-hook 'iy-ac-tab-noconflict)
(add-hook 'markdown-mode-hook 'iy-ac-tab-noconflict)
(add-hook 'org-mode-hook 'iy-ac-tab-noconflict)

;; Time stamp
(setq
 time-stamp-active t          ;; do enable time-stamps
 time-stamp-line-limit 20     ;; check first 10 buffer lines for Time-stamp:
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ;; date format
(add-hook 'write-file-hooks 'time-stamp) ;; update when saving

;; Code folding
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)
(dolist (hook (list 'lisp-mode-hook
                    'scheme-mode-hook
                    'python-mode-hook
                    'emacs-lisp-mode-hook
                    'c++-mode-hook))
  (add-hook hook 'hideshowvis-enable))
(global-set-key (kbd "<f7>") 'hs-hide-block)
(global-set-key (kbd "S-<f7>") 'hs-show-block)
(global-set-key (kbd "C-c @ SPC") 'hs-show-block) ; second binding

;; enable `hs-minor-mode' at startup
(add-hook 'emacs-lisp-mode-hook
          (lambda () (hs-minor-mode 1)))

;; enable `hs-minor-mode' at startup
(add-hook 'lisp-mode-hook
          (lambda () (hs-minor-mode 1)))

;; enable `hs-minor-mode' at startup
(add-hook 'scheme-lisp-mode-hook
          (lambda () (hs-minor-mode 1)))

;; enable `hs-minor-mode' at startup
(add-hook 'python-mode-hook
          (lambda () (hs-minor-mode 1)))

;; Add the following to your .emacs and uncomment it in order to get a + symbol
(define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])

(defcustom hs-fringe-face 'hs-fringe-face
  "*Specify face used to highlight the fringe on hidden regions."
  :type 'face
  :group 'hideshow)

(defface hs-fringe-face
  '((t (:foreground "#999" :box (:line-width 2 :color "grey75" :style released-button))))
  "Face used to highlight the fringe on folded regions"
  :group 'hideshow)

(defcustom hs-face 'hs-face
  "*Specify the face to to use for the hidden region indicator"
  :type 'face
  :group 'hideshow)

(defface hs-face
  '((t (:background "#558" :box t)))
  "Face to hightlight the ... area of hidden regions"
  :group 'hideshow)

(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string))
           (display-string (format "(%d)..." (count-lines (overlay-start ov) (overlay-end ov)))))
      (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
      (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 0 (length display-string) 'face 'hs-face display-string)
      (overlay-put ov 'display display-string))))
(setq hs-set-up-overlay 'display-code-line-counts)

;; Trick emacs when opening a file through menu-find-file-existing
(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil))
    ad-do-it))

;; Windows-like mouse/arrow movement & selection
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)
(cua-mode 1)
(setq shift-select-mode t)

;; As in Windows, replace after typing a letter
(require 'delsel)
(delete-selection-mode 1)
(setq mouse-drag-copy-region nil)

;; Loads latex auto-complete
(add-to-list 'load-path "~/.emacs.d/ac-math")
(require 'ac-math)
(require 'auto-complete-latex)
(add-to-list 'ac-modes 'latex-mode)
(defun ac-latex-mode-setup ()
  (when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))
    (make-local-variable 'ac-sources)
    (setq ac-sources (append '(ac-source-words-in-same-mode-buffers
                               ac-source-dictionary
                               ac-source-math-unicode
                               ac-source-math-latex) ac-sources))))
(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

;; Org mode
(setq load-path (cons "~/.emacs.d/org/lisp" load-path))
(defvar org-list-allow-alphabetical t)
(defun org-element-bold-successor           (arg))
(defun org-element-code-successor           (arg))
(defun org-element-entity-successor         (arg))
(defun org-element-italic-successor         (arg))
(defun org-element-latex-fragment-successor (arg))
(defun org-element-strike-through-successor (arg))
(defun org-element-subscript-successor      (arg))
(defun org-element-superscript-successor    (arg))
(defun org-element-underline-successor      (arg))
(defun org-element-verbatim-successor       (arg))
(require 'org)
(require 'org-list)
(require 'ox-org)
(require 'ox-md)

(let ((todo "~/workspace/Documents/agenda.org"))
  (when (file-readable-p todo)
    (setq org-agenda-files '("~/workspace/Documents/agenda.org"))
    (setq initial-buffer-choice (lambda ()
                                  (org-agenda nil "n")
                                  (delete-other-windows)
                                  (current-buffer)))))
(add-hook 'org-mode-hook 'flyspell-mode)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'ac-modes 'org-mode)
(setq org-startup-folded 'nofold)
(setq org-startup-indented t)
(setq org-startup-with-inline-images t)
(setq org-startup-truncated t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-use-speed-commands t)
(setq org-default-notes-file "~/workspace/Documents/agenda.org")
(setq org-export-with-sub-superscripts nil)

;; Mouse in Org
(require 'org-mouse)

;; Fonts
(add-hook 'org-mode-hook (lambda () (variable-pitch-mode t)))
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit 'fixed-pitch)
(set-face-attribute 'org-block nil :inherit 'fixed-pitch)

(defface org-block-begin-line
  '((t (:inherit org-meta-line
                 :overline "light grey" :foreground "#008ED1")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-end-line
  '((t (:inherit org-meta-line
                 :underline "light grey" :foreground "#008ED1")))
  "Face used for the line delimiting the end of source blocks.")

;; Beamer support
(require 'ox-beamer)

;; Odt export
(require 'ox-odt)

;; Org Templates
(setq org-capture-templates
      '(("t" "Task" entry (file+headline "" "Tasks") "* TODO %?\n  %u\n  %a")
        ("s" "Simple Task" entry (file+headline "" "Tasks") "* TODO %?\n  %U\n")))

(add-to-list 'org-structure-template-alist '("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC\n"))
(add-to-list 'org-structure-template-alist '("S" "#+BEGIN_SRC shell-script\n?\n#+END_SRC\n"))

;; Fix shift problem in Org mode
(setq org-support-shift-select t)
(eval-after-load "org"
  '(progn
     (eval-after-load "cua-base"
       '(progn
          (defadvice org-call-for-shift-select (before org-call-for-shift-select-cua activate)
            (if (and cua-mode
                     org-support-shift-select
                     (not (use-region-p)))
                (cua-set-mark)))))))

;; Fix on the keys
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map [kp-enter] 'org-return-indent)
            (define-key org-mode-map [enter] 'org-return-indent)
            (define-key org-mode-map (kbd "RET") 'org-return-indent)))

;; define todo states: set time stamps one waiting, delegated and done
(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "IN PROGRESS(p!)"
         "HOLD(h!)"
         "WAITING(w)"
         "|"
         "DONE(d!)"
         "CANCELLED(c)")))
(setq org-todo-keyword-faces
      '(
        ("IN PROGRESS" . 'warning)
        ("HOLD" . 'font-lock-keyword-face)
        ("WAITING" . 'font-lock-builtin-face)
        ("CANCELLED" . 'font-lock-doc-face)))

;; PlantUML
(require 'iimage)
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
(add-to-list 'iimage-mode-image-regex-alist '("@startuml\s+\\(.+\\)" . 1))
(add-to-list 'iimage-mode-image-regex-alist (cons (concat "\[\[file:\(~?" iimage-mode-image-filename-regex "\)\]") 1))
(add-hook 'org-mode-hook '(lambda () (org-turn-on-iimage-in-org)))
(setq org-image-actual-width '(400))

;; Rendering plantuml
(setq org-plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar"))
(defun plantuml-render-buffer ()
  (interactive)
  (message "PLANTUML Start rendering")
  (shell-command (concat "java -jar ~/Downloads/plantuml.jar "
                         buffer-file-name))
  (message (concat "PLANTUML Rendered:  " (buffer-name))))

;; Image reloading
(defun reload-image-at-point ()
  (interactive)
  (message "reloading image at point in the current buffer...")
  (image-refresh (get-text-property (point) 'display)))

;; Image resizing and reloading
(defun resize-image-at-point ()
  (interactive)
  (message "resizing image at point in the current buffer...")
  (let* ((image-spec (get-text-property (point) 'display))
         (file (cadr (member :file image-spec))))
    (message (concat "resizing image..." file))
    (shell-command (format "convert -resize %d %s %s "
                           (* (window-width (selected-window)) (frame-char-width))
                           file file))
    (reload-image-at-point)))

;; Function to setup images for display on load
(defun org-turn-on-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (turn-on-iimage-mode)
  (set-face-underline-p 'org-link nil))

;; Function to toggle images in a org buffer
(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (call-interactively 'iimage-mode))

;; Preview LaTeX equations in buffers by showing images (C-c C-x C-l)
(setq org-latex-create-formula-image-program 'imagemagick)

;; Equations in Org
;; Use C-c C-x C-l to regenerate the images
(defvar text-scale-mode-hook nil
  "Hook run at end of command `text-scale-mode'.")

(defadvice text-scale-mode (after text-scale-mode-hooks nil activate)
  "Run `text-scale-mode-hook' at end of command `text-scale-mode'."
  (if (functionp text-scale-mode-hook)
      (funcall text-scale-mode-hook)
    (loop for hook in text-scale-mode-hook do
          (if (eq hook 't)
              (run-hooks (default-value text-scale-mode-hook))
            (run-hooks hook)))))

(defun org-text-scale-eye ()
  "Scale equation images according to text-scale-mode-amount."
  (when (boundp 'text-scale-mode-amount)
    (let ((relwidth (* (expt text-scale-mode-step text-scale-mode-amount))))
      (loop for ol in (overlays-in (point-min) (point-max)) do
            (when (eq (overlay-get ol 'org-overlay-type) 'org-latex-overlay)
              (unless (overlay-get ol 'org-image-original-width)
                (overlay-put ol 'org-image-original-width (car (image-size (overlay-get ol 'display) t))))
              (let ((ol-disp-plist (cdr (overlay-get ol 'display))))
                (setq ol-disp-plist (plist-put ol-disp-plist :type 'imagemagick))
                (setq ol-disp-plist (plist-put ol-disp-plist :width (round (* relwidth (overlay-get ol 'org-image-original-width)))))
                (overlay-put ol 'display (append '(image) ol-disp-plist))))))
    (force-window-update)))
(add-hook 'org-mode-hook '(lambda () (add-hook 'text-scale-mode-hook 'org-text-scale-eye)))

(defadvice org-format-latex (before set-scale activate)
  "Set :scale in `org-format-latex-options' to the scaling factor resulting from `text-scale-mode' and clear cache."
  (let ((relwidth (expt text-scale-mode-step text-scale-mode-amount)))
    (unless (= (plist-get org-format-latex-options :scale) relwidth)
      (plist-put org-format-latex-options :scale relwidth))))

;; Insert images from files #+BEGIN: image :file "~/Documents/personal/foo.png"
(defun org-dblock-write:image (params)
  (let ((file (plist-get params :file)))
    (clear-image-cache file)
    (insert-image (create-image file) )))

;; Insert screenshots into Org mode, very useful
(defun org-insert-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the same
directory as the org-buffer and insert
a link to this file."
  (interactive)
  (let ((case-fold-search nil))
    (setq tilde-buffer-filename
          (replace-regexp-in-string "/" "\\" (buffer-file-name) t t))
    (setq tilde-buffer-filename
          (replace-regexp-in-string ".org" "" tilde-buffer-filename t t))
    (setq filename
          (concat
           (make-temp-name
            (concat tilde-buffer-filename
                    "_"
                    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))

    (if (equal system-type 'windows-nt)
        ;; Windows: Irfanview
        (call-process "C:\\Program Files (x86)\\IrfanView\\i_view32.exe" nil nil nil (concat
                                                                                      "/clippaste /convert=" filename))

      ;; Linux: ImageMagick: (call-process "import" nil nil nil filename)
      (call-process "import" nil nil nil filename)
      ) ;; if
    (insert (concat "[[file:" filename "]]"))
    (org-display-inline-images)))

;; Insert images in Org files
(defun org-insert-clipboard-image ()
  "using xclip and perl to yank image file in link format for org, and save the
file named after the image's timestamp"
  (interactive)
  (let (clip-targets clip-text)
    (setq clip-targets (shell-command-to-string "xclip -o -selection clipboard -t TARGETS|perl -e '$ret=0;while(<>){if ($_ eq \"image/png\\n\"){$ret+=4 ;$result = `xclip -o -selection clipboard -t TIMESTAMP`;}$ret=+1 if ($_ eq \"STRING\\n\") or ($_ eq \"UTF8_STRING\\n\");$ret+=2 if $_ eq \"text/html\\n\";}if($ret eq 6){print \"webimg\"};if($ret eq 4){print \"clipboardimg\"};if($ret eq 1){print \"string\"};if($ret eq 0){print \"nothing\"};'"))
    (message (concat "type :" clip-targets))
    (when (string= "webimg"  clip-targets)
      (progn
        (setq clip-text (shell-command-to-string (concat "perl -e '$buffn=\""
                                                         (file-name-directory (buffer-file-name)) "\";$result = `xclip -o -selection clipboard -t TIMESTAMP`;chomp($result);$fn = `xclip -o -selection clipboard -t text/html`;$fn=~/src=\"(.*?)\"/i;system(\"xclip -o -selection clipboard -t image/png > $buffn$result.png\");print \"[[file:\".$result.\".png]\".$1.\"]\";'")))
        (insert clip-text)))
    (when (string= "clipboardimg"  clip-targets)
      (progn
        (setq clip-text (shell-command-to-string (concat "perl -e '$buffn=\""
                                                         (file-name-directory (buffer-file-name)) "\";$result = `xclip -o -selection clipboard -t TIMESTAMP`;chomp($result);system(\"xclip -o -selection clipboard -t image/png > $buffn$result.png\");print \"[[file:\".$result.\".png]\".$result.\".png]$buffn\";'")))
        (insert clip-text)))
    (org-display-inline-images)
    (or clip-text (x-selection-value))))

;; for Tikz image in Org
(setq org-babel-latex-htlatex "htlatex")
(defmacro by-backend (&rest body)
  `(case (if (boundp 'backend) (org-export-backend-name backend) nil) ,@body))

;; for Graphviz image in Org
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

;; for Gnuplot
(add-to-list 'load-path "~/.emacs.d/gnuplot")
(require 'gnuplot)

;; Make Yasnippet work here, but for Org
(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (and (fboundp 'yas-expand) (yas-expand))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-tab-first-hook
                         'yas-org-very-safe-expand)))

;; Configure org-mode so that when you edit source code in an indirect buffer (with C-c '), the buffer is opened in the current window. That way, your window organization isn't broken when switching.
(setq org-src-window-setup 'current-window)

;; Markdown exporter
(require 'ox-md)
(setq org-completion-use-ido t)

;; Tweaks for Latex exporting
(require 'ox-latex)

;; Choose either listings or minted for exporting source code blocks.
(setq org-latex-listings t)

;; Export " to csquotes macros
(setq org-export-latex-quotes
      '(("en" ("\\(\\s-\\|[[(]\\)\"" . "\\enquote{") ("\\(\\S-\\)\"" . "}") ("\\(\\s-\\|(\\)'" . "`"))))

;; Reftex
(require 'reftex-cite)
(require 'dash)
(setq reftex-default-bibliography '("~/workspace/Documents/Bibliography/biblio.bib")) ;; So that RefTeX in Org-mode knows bibliography
(defun org-mode-reftex-setup ()
  (interactive)
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
         ;; Reftex should use the org file as master file. See C-h v TeX-master for infos.
         (setq TeX-master t)
         (turn-on-reftex)
         ;; enable auto-revert-mode to update reftex when bibtex file changes on disk
         (global-auto-revert-mode t) ; careful: this can kill the undo
         ;; history when you change the file
         ;; on-disk.
         (reftex-parse-all)
         ;; add a custom reftex cite format to insert links
         ;; This also changes any call to org-citation!
         (reftex-set-cite-format
          '((?c . "\\citet{%l}") ; natbib inline text
            (?i . "\\citep{%l}") ; natbib with parens
            ))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; Org-Ref
(add-to-list 'load-path "~/.emacs.d/org-ref")
(require 'org-ref)
;; (org-babel-load-file "~/.emacs.d/org-ref/org-ref.org")
(setq org-ref-bibliography-notes "~/workspace/Documents/Bibliography/notes.org"
      org-ref-default-bibliography '("~/workspace/Documents/Bibliography/biblio.bib")
      org-ref-pdf-directory "~/workspace/Documents/Bibliography/bibtex-pdfs/")
(setq org-ref-insert-cite-key "C-c )")
(setq org-ref-default-citation-link "autocite")

;; Add defaults packages to include when exporting.
(add-to-list 'org-latex-packages-alist '("" "graphicx"))
(add-to-list 'org-latex-packages-alist '("" "geometry"))
(add-to-list 'org-latex-packages-alist '("" "hyperref"))
(add-to-list 'org-latex-packages-alist '("" "caption"))
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[10pt,article,oneside]{memoir}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("book"
               "\\documentclass[10pt]{memoir}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("xelatex"
               "\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec,lipsum}
\\usepackage{graphicx}
\\defaultfontfeatures{Mapping=tex-text}
\\setromanfont{Gentium}
\\setromanfont [BoldFont={Gentium Basic Bold},
                ItalicFont={Gentium Basic Italic}]{Gentium Basic}
\\setsansfont{Charis SIL}
\\setmonofont[Scale=0.8]{DejaVu Sans Mono}
\\defaultfontfeatures{Ligatures=TeX}
\\usepackage{geometry}
\\usepackage{listings}
\\usepackage{hyperref}
\\usepackage{caption}
\\usepackage{color}
\\usepackage{tikz}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("pdflatex"
               "\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{geometry}
\\usepackage{listings}
\\usepackage{hyperref}
\\usepackage{caption}
\\usepackage{color}
\\usepackage{mathptmx}
\\usepackage[section]{placeins}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\renewcommand{\\rmdefault}{ptm}
\\pagestyle{empty}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-default-class "pdflatex")

;; Let the exporter use the -shell-escape option to let latex execute external programs.
;; (setq org-latex-pdf-process
;;       '("xelatex -interaction nonstopmode %f"
;;         "bibtex $(basename %b)"
;;         "xelatex -interaction nonstopmode %f"
;;         "xelatex -interaction nonstopmode %f"
;;         )) ;; multipass

(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "bibtex $(basename %b)"
        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        )) ;; multipass

;; Tweak the PDF viewer
(eval-after-load "org"
  '(progn
     ;; .txt files aren't in the list initially, but in case that changes
     ;; in a future version of org, use if to avoid errors
     (if (assoc "\\.txt\\'" org-file-apps)
         (setcdr (assoc "\\.txt\\'" org-file-apps) "gedit %s")
       (add-to-list 'org-file-apps '("\\.txt\\'" . "gedit %s") t))
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "acroread %s")))

;; Automatically refresh inline images that are generated from Babel blocks
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Enable multiple languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (plantuml . t)
   (dot . t)
   (gnuplot . t)
   (sh . t)
   (R . t)
   (perl . t)
   (ruby . t)
   (python . t)
   (js . t)
   (C . t)
   (haskell . t)))

;; Make org do not open other frames
(setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                   (vm-imap . vm-visit-imap-folder-other-frame)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file)
                                   (wl . wl-other-frame))))

;; Add missing function
(defun org-reverse-string (string)
  (apply 'string (reverse (string-to-list string))))

;; The next block makes org-babel aware that a lower-case 'r' in a =src= block header should be processed as R.
(add-to-list 'org-src-lang-modes
             '("r" . ess-mode))

;; Don't ask for confirmation on every =C-c C-c= code-block compile.
(setq org-confirm-babel-evaluate nil)

;; Nice bulleted lists
(add-to-list 'load-path "~/.emacs.d/org-bullets")
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; CSS for the HTML
(setq org-html-style-include-scripts nil
      org-html-style-include-default nil)
(setq org-html-style
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://thomasf.github.io/solarized-css/solarized-light.min.css\" />")

;; Use Org bold, italics, code styles
(defun org-text-wrapper (txt &optional endtxt)
  "Wraps the region with the text passed in as an argument."
  (if (use-region-p)
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min))
        (insert txt)
        (goto-char (point-max))
        (if endtxt
            (insert endtxt)
          (insert txt)))
    (if (looking-at "[A-z]")
        (save-excursion
          (if (not (looking-back "[     ]"))
              (backward-word))
          (progn
            (mark-word)
            (org-text-wrapper txt endtxt)))
      (progn
        (insert txt)
        (let ((spot (point)))
          (insert txt)
          (goto-char spot))))))

(defun org-text-bold () "Wraps the region with asterisks."
  (interactive)
  (org-text-wrapper "*"))

(defun org-text-italics () "Wraps the region with slashes."
  (interactive)
  (org-text-wrapper "/"))

(defun org-text-code () "Wraps the region with equal signs."
  (interactive)
  (org-text-wrapper "="))

;; Gnus
(setq gnus-select-method
      '(nnimap "Gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

;; Gnus news
(setq gnus-summary-line-format "%U%R%z%d %I%(%[ %F %] %s %)\n")
(setq gnus-secondary-select-methods '((nntp "news.gmane.org")
                                      (nntp "news.gwene.org")))
(setq mm-text-html-renderer 'w3m)
(setq message-generate-headers-first t)
(add-hook 'message-mode-hook 'turn-on-auto-fill)

;; Newsticker and w3m
(when (executable-find "w3m")
  (add-to-list 'load-path "~/.emacs.d/newsticker")
  (add-to-list 'load-path "~/.emacs.d/w3m")
  (require 'newsticker)
  (require 'newsticker-notify)
  (require 'w3m)
  (setq newsticker-dir "~/.emacs.cache/Newsticker")
  (setq newsticker-url-list-defaults nil)
  (setq newsticker-automatically-mark-items-as-old t)
  (setq newsticker-automatically-mark-visited-items-as-old t)
  (setq newsticker-retrieval-interval 600)
  (setq newsticker-html-renderer 'w3m-region)
  (setq newsticker-retrieval-method 'extern)
  (setq newsticker-treeview-treewindow-width 40)
  (setq newsticker-treeview-listwindow-height 30)
  (setq newsticker-obsolete-item-max-age (* 30 (* 24 3600)))
  (setq newsticker-ticker-interval 4.3) ;;
  (setq newsticker-display-interval 3.3) ;; 0.3 for scroll-smooth, 15.3 otherwise
  (setq newsticker-scroll-smoothly nil) ;; dont make it t otherwise will start scrolling
  (setq newsticker-wget-arguments '("-q" "-O" "-"
                                    "--user-agent" "testing"))
  (setq newsticker-sort-method (quote sort-by-time))
  (setq newsticker-url-list
        (quote (("BBC News" "http://www.bbc.co.uk/syndication/feeds/news/ukfs_news/front_page/rss091.xml" nil nil nil)
                ("Phoronix" "http://www.phoronix.com/rss.php")
                ("Google News" "http://news.google.com/?output=rss"))))
  (setq newsticker-url-list-defaults
        (quote (("BBC News" "http://www.bbc.co.uk/syndication/feeds/news/ukfs_news/front_page/rss091.xml" nil nil nil)
                ("Phoronix" "http://www.phoronix.com/rss.php")
                ("Google News" "http://news.google.com/?output=rss"))))
  (newsticker-start)
  (newsticker-start-ticker))

;; Skill-mode
(load "skill-fn-info.el")

;; Figure out what the function name is
(defun skill-get-fnsym ()
  (let ((p (point))
        (ret nil))
    ;; Don't do anything if current word is inside a string.
    (if (= (or (char-after (1- (point))) 0) ?\")
        nil
      (progn
        (backward-up-list)
        (forward-word)
        (setq ret (thing-at-point 'symbol))))
    (goto-char p)
    ret))

(defun lispdoc-get-arg-index ()
  (save-excursion
    (let ((fn (eldoc-fnsym-in-current-sexp))
          (i 0))
      (unless (memq (char-syntax (char-before)) '(32 39))
        (condition-case err
            (backward-sexp) ;; for safety
          (error 1)))
      (condition-case err
          (while (not (equal fn (eldoc-current-symbol)))
            (setq i (1+ i))
            (backward-sexp))
        (error 1))
      (max 0 i))))

(defun lispdoc-highlight-nth-arg (doc n)
  (cond ((null doc) "")
        ((<= n 0) doc)
        (t
         (let ((i 0))
           (mapconcat
            (lambda (arg)
              (if (member arg '("&optional" "&rest" "@optional" "@key" "@rest"))
                  arg
                (prog2
                    (if (= i (1- n))
                        (put-text-property 0 (length arg) 'face '(:bold t :foreground "yellow") arg))
                    arg
                  (setq i (1+ i)))))
            (split-string doc) " ")))))

;; Function that looks up and return the docstring
(defun skill-eldoc-function ()
  "Returns a documentation string appropriate for the current context or nil."
  (condition-case err
      (let* ((current-fnsym  (skill-get-fnsym))
             (doc (skill-fn-info-get current-fnsym))
             (adviced (lispdoc-highlight-nth-arg doc
                                                 (lispdoc-get-arg-index))))
        adviced)
    ;; This is run from post-command-hook or some idle timer thing,
    ;; so we need to be careful that errors aren't ignored.
    (error (message "eldoc error: %s" err))))

;; Loads Lisp auto-complete
(add-to-list 'ac-modes 'lisp-mode)
(defun my-lisp-mode-common-hook-func ()
  (interactive)
  "Function to be called when entering into c-mode."
  (set (make-local-variable 'eldoc-documentation-function)
       'skill-eldoc-function)
  (when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))
    (auto-complete-mode t)
    (make-local-variable 'ac-sources)
    (setq ac-sources '(ac-source-semantic
                       ac-source-words-in-same-mode-buffers
                       ac-source-gtags
                       ac-source-etags
                       ac-source-dictionary))))
(add-hook 'lisp-mode-hook 'my-lisp-mode-common-hook-func)

;; Better undo
(require 'undo-tree)
(global-undo-tree-mode)

;; Line spacing
(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq-default line-spacing 0.5)
    (setq-default line-spacing nil))
  (redraw-display))

;; Redo
(require 'redo+)
(global-set-key (kbd "C-S-z") 'redo) ; Mac style
(global-set-key (kbd "C-y") 'redo) ; Microsoft Windows style
(setq undo-no-redo t)

;; Python tweaks
(when (featurep 'python) (unload-feature 'python t))
(add-to-list 'load-path "~/.emacs.d/python-mode")
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'auto-complete-mode)

;; Python Hook
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))
(setq-default python-indent 2)
(setq-default python-guess-indent nil)

;; Jedi settings
(add-to-list 'load-path "~/.emacs.d/ctable")
(add-to-list 'load-path "~/.emacs.d/deferred")
(add-to-list 'load-path "~/.emacs.d/epc")
(add-to-list 'load-path "~/.emacs.d/jedi")
(add-to-list 'load-path "~/.emacs.d/python-environment")
(require 'python-environment)
(require 'epc)
(require 'jedi)
(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)))
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(defun jedi:ac-direct-matches ()
  (mapcar
   (lambda (x)
     (destructuring-bind (&key word doc description symbol)
         x
       (popup-make-item word
                        :symbol symbol
                        :document (unless (equal doc "") doc))))
   jedi:complete-reply))

(eval-after-load 'jedi
  '(progn
     (custom-set-faces
      '(jedi:highlight-function-argument ((t (:inherit eldoc-highlight-function-argument)))))

     (setq jedi:tooltip-method nil)
     (defun jedi-eldoc-documentation-function ()
       (deferred:nextc
         (jedi:call-deferred 'get_in_function_call)
         #'jedi-eldoc-show)
       nil)

     (defun jedi-eldoc-show (args)
       (when args
         (let ((eldoc-documentation-function
                (lambda ()
                  (apply #'jedi:get-in-function-call--construct-call-signature args))))
           (eldoc-print-current-symbol-info))))))

;; Jedi Eldoc
(require 'jedi-eldoc)
(set-face-attribute 'jedi-eldoc:highlight-function-argument nil
                    :foreground "green")
(add-hook 'python-mode-hook 'jedi-eldoc-mode)

;; Javascript mode
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; Ergoemacs
(add-to-list 'load-path "~/.emacs.d/ergoemacs")
(add-to-list 'load-path "~/.emacs.d/ergoemacs-keybindings")
(setq ergoemacs-keyboard-layout "us")
(require 'ergoemacs-mode)
(ergoemacs-mode 1)

;; Remove the trailing spaces
(defun remove-trailing-spaces ()
  "Remove trailing spaces in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d Trailing spaces removed from buffer." remove-count))))))

;; Unfill
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the reverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the reverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

;; From Ergoemacs
(global-set-key (kbd "<C-f3>") 'ergoemacs-compact-uncompact-block)

;; Bookmarks
(add-to-list 'load-path "~/.emacs.d/bm")
(setq bm-restore-repository-on-load t)
(require 'bm)
(setq bm-highlight-style 'bm-highlight-line-and-fringe)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;; make bookmarks persistent as default
(setq-default bm-buffer-persistence t)

;; Loading the repository from file when on start up.
(add-hook 'after-init-hook 'bm-repository-load)

;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)

;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)

;; Saving the repository to file when on exit.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))

;; Designsync versioning control
(require 'vc-sync)
(defun dired-sync-symlink-filter ()
  (save-excursion
    ;; Goto the beginning of the buffer
    (goto-char (point-min))
    ;; For each matching symbolic link with sync_cache or sync/mirrors in the path name...
    (while (re-search-forward "\\(-> .*/sync\\(_cache\\|/mirrors\\)/.*$\\)" nil t)
      ;; Create an overlay that masks out everything between the -> and the end of line
      (let ((o (make-overlay (match-beginning 1) (progn (end-of-line) (point)))))
        (overlay-put o 'invisible t)
        (overlay-put o 'evaporate t)))))
(add-hook 'dired-after-readin-hook 'dired-sync-symlink-filter)

;; psvn
(add-to-list 'load-path "~/.emacs.d/diff-hl")
(require 'psvn)
(require 'diff-hl)
(defadvice svn-status-update-modeline (after svn-update-diff-hl activate)
  (diff-hl-update))

;; git
(add-to-list 'load-path "~/.emacs.d/git-modes")
(add-to-list 'load-path "~/.emacs.d/git-emacs")
(add-to-list 'load-path "~/.emacs.d/git-gutter-plus")
(add-to-list 'load-path "~/.emacs.d/git-gutter-fringe-plus")
(add-to-list 'load-path "~/.emacs.d/git-timemachine")
(add-to-list 'load-path "~/.emacs.d/magit")
(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/magit/")))

(require 'magit)
(require 'git-emacs)
(require 'git-gutter-fringe+)
(require 'git-timemachine)
(global-git-gutter+-mode t)
(set-face-foreground 'git-gutter-fr+-modified "white")
(set-face-foreground 'git-gutter-fr+-added    "green")
(set-face-foreground 'git-gutter-fr+-deleted  "orange")

;; Please adjust fringe width if your own sign is too big.
(setq-default left-fringe-width 20)

(fringe-helper-define 'git-gutter-fr+-added nil
  ".XXXXXX."
  "XXxxxxXX"
  "XX....XX"
  "XX....XX"
  "XXXXXXXX"
  "XXXXXXXX"
  "XX....XX"
  "XX....XX")

(fringe-helper-define 'git-gutter-fr+-deleted nil
  "XXXXXX.."
  "XXXXXXX."
  "XX...xXX"
  "XX....XX"
  "XX....XX"
  "XX...xXX"
  "XXXXXXX."
  "XXXXXX..")

(fringe-helper-define 'git-gutter-fr+-modified nil
  "XXXXXXXX"
  "XXXXXXXX"
  "Xx.XX.xX"
  "Xx.XX.xX"
  "Xx.XX.xX"
  "Xx.XX.xX"
  "Xx.XX.xX"
  "Xx.XX.xX")

;; Stop Org splitting window vertically
(setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                   (vm-imap . vm-visit-imap-folder-other-frame)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file)
                                   (wl . wl-other-frame))))

;; Code folding
(defun toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))
(global-set-key [f1] 'toggle-selective-display)

;; Commands to make my programming environment nice
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\C-l" 'goto-line)
(global-set-key (kbd "") 'other-window)
(global-set-key [f5] 'compile)
(global-set-key [f6] 'next-error)
(global-set-key [f11] 'djcb-full-screen-toggle)
(global-set-key [C-tab] 'comment-or-uncomment-region)
(global-set-key [kp-prior] 'scroll-down)
(global-set-key [prior] 'scroll-down)
(global-set-key [kp-next] 'scroll-up)
(global-set-key [next] 'scroll-up)
(global-set-key "\M-g" 'goto-line)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [(meta delete)] '(lambda () (interactive) (backward-or-forward-kill-word -1)))
(global-set-key [(alt delete)] '(lambda () (interactive) (backward-or-forward-kill-word -1)))
(global-set-key [M-up] 'enlarge-window)
(global-set-key [M-down] 'shrink-window)
(global-set-key [(control o)] 'find-file)
(global-set-key [(control n)] 'find-file-other-frame)
(global-set-key [(control s)] 'save-buffer)
(global-set-key [(meta s)] 'write-file)
(global-set-key [(control q)] 'save-buffers-kill-emacs)
(global-set-key [(meta q)] 'kill-this-buffer)
(global-set-key [(control t)] 'ispell-buffer)
(global-set-key [(control r)] 'replace-string)
(global-set-key [(control z)] 'undo)
(global-set-key "\C-a" 'mark-whole-buffer)
(global-set-key (kbd "\C-c \C-c") 'kill-ring-save)

;; search forward with Ctrl-f
(global-set-key [(control f)] 'isearch-forward)
(define-key isearch-mode-map [(control f)] (lookup-key isearch-mode-map "\C-f"))
(define-key minibuffer-local-isearch-map [(control f)]
  (lookup-key minibuffer-local-isearch-map "\C-s"))

;; search backward with Alt-f
(global-set-key [(meta f)] 'isearch-backward)
(define-key isearch-mode-map [(meta f)] (lookup-key isearch-mode-map "\C-r"))
(define-key minibuffer-local-isearch-map [(meta f)]
  (lookup-key minibuffer-local-isearch-map "\C-r"))

;; to get the scroll wheel work
(global-set-key [(button5)] '(lambda () (interactive) (scroll-up 3)))
(global-set-key [(button4)] '(lambda () (interactive) (scroll-down 3)))
(global-set-key [(shift button5)] '(lambda () (interactive) (scroll-up-command)))
(global-set-key [(shift button4)] '(lambda () (interactive) (scroll-down-command)))
(global-set-key [(control button5)] '(lambda () (interactive) (scroll-up-command)))
(global-set-key [(control button4)] '(lambda () (interactive) (scroll-down-command)))

(global-set-key [(mouse-5)] '(lambda () (interactive) (scroll-up 3)))
(global-set-key [(mouse-4)] '(lambda () (interactive) (scroll-down 3)))
(global-set-key [(shift mouse-5)] '(lambda () (interactive) (scroll-up)))
(global-set-key [(shift mouse-4)] '(lambda () (interactive) (scroll-down)))
(global-set-key [(control mouse-5)] '(lambda () (interactive) (scroll-up)))
(global-set-key [(control mouse-4)] '(lambda () (interactive) (scroll-down)))

;; higlight changes in documents
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)

;; toggle visibility
(global-set-key (kbd "<f6>") 'highlight-changes-visible-mode) ;; changes

;; remove the change-highlight in region
(global-set-key (kbd "S-<f6>") 'highlight-changes-remove-highlight)

;; if you're not already using it for something else...
(global-set-key (kbd "<M-next>") 'highlight-changes-next-change)
(global-set-key (kbd "<M-prior>")  'highlight-changes-previous-change)
(set-face-foreground 'highlight-changes nil)
(set-face-background 'highlight-changes "#882020")
(set-face-foreground 'highlight-changes-delete nil)
(set-face-background 'highlight-changes-delete "#916868")

;; toggle truncate lines
(global-set-key (kbd "<f8>") 'toggle-truncate-lines)

;; Refresh file on F9
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t))
(global-set-key [f9] 'refresh-file)

;; Show guide for shortcuts
(add-to-list 'load-path "~/.emacs.d/guide-key")
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(guide-key-mode 1) ;; Enable guide-key-mode

;; Recentf stuff
(require 'recentf)
(setq recentf-max-menu-items 25)
(setq recentf-exclude (append recentf-exclude '("/usr*")))
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(setq recentf-filename-handlers '(abbreviate-file-name))
(setq recentf-save-file "~/.emacs.cache/recentf")
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; iMenu
(add-hook 'scheme-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'imenu-example--create-lisp-index)
            (setq imenu-generic-expression scheme-imenu-generic-expression)))

(add-hook 'lisp-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'imenu-example--create-lisp-index)
            (setq imenu-generic-expression scheme-imenu-generic-expression)))

(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)
(add-hook 'lisp-mode-hook 'imenu-add-menubar-index)
(add-hook 'scheme-mode-hook 'imenu-add-menubar-index)
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
(add-hook 'latex-mode-hook 'imenu-add-menubar-index)
(add-hook 'org-mode-hook 'imenu-add-menubar-index)
(add-hook 'python-mode-hook 'imenu-add-menubar-index)

;; Enable which-function-mode for selected major modes
(setq which-func-modes '(ecmascript-mode python-mode emacs-lisp-mode lisp-mode scheme-mode skill-mode
                                         c-mode c++-mode makefile-mode sh-mode))
(which-function-mode t)
(add-hook 'python-mode-common-hook
          (lambda () (which-function-mode t)))
(add-hook 'c-mode-common-hook
          (lambda () (which-function-mode t)))
(add-hook 'lisp-mode-common-hook
          (lambda () (which-function-mode t)))
(add-hook 'scheme-mode-common-hook
          (lambda () (which-function-mode t)))
(add-hook 'emacs-lisp-mode-common-hook
          (lambda () (which-function-mode t)))

;; Project management
(add-to-list 'load-path "~/.emacs.d/ack-and-a-half")
(add-to-list 'load-path "~/.emacs.d/projectile")
(add-to-list 'load-path "~/.emacs.d/perspective")
(require 'ack-and-a-half)
;; (require 'perspective)
(require 'projectile)
(projectile-global-mode t)
(setq projectile-cache-file "~/.emacs.cache/projectile.cache")
(setq projectile-known-projects-file "~/.emacs.cache/projectile-bookmarks.eld")
(setq projectile-enable-caching t)
(setq projectile-keymap-prefix (kbd "C-c C-p"))

;; Code Browser
(setq stack-trace-on-error t)
(setq after-find-file-from-revert-buffer t)
(add-to-list 'load-path "~/.emacs.d/ecb")
(require 'ecb)
(setq ecb-show-sources-in-directories-buffer 'always)
(set-face-foreground 'ecb-default-general-face "#ffffff")
(setq ecb-tip-of-the-day nil)
(setq ecb-auto-compatibility-check nil)
(if (ecb--semantic-active-p)
    (ecb-update-methods-buffer--internal nil nil t)
  (ecb-rebuild-methods-buffer-for-non-semantic))

;; Fix the modeline
(add-to-list 'load-path "~/.emacs.d/smart-mode-line")
(require 'smart-mode-line)
(sml/setup)

(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 25)
(setq sml/mode-width 'full)

(add-to-list 'sml/hidden-modes " AC")
(add-to-list 'sml/hidden-modes " AI")
(add-to-list 'sml/hidden-modes " SP")
(add-to-list 'sml/hidden-modes " mate")
(add-to-list 'sml/hidden-modes " Plugged")
(add-to-list 'sml/hidden-modes " Gtags")
(add-to-list 'sml/hidden-modes " Abbrev")
(add-to-list 'sml/hidden-modes " Fill")
(add-to-list 'sml/hidden-modes " Guide")
(add-to-list 'sml/hidden-modes " hs")
(add-to-list 'sml/hidden-modes " yas")
(add-to-list 'sml/hidden-modes " pair")
(add-to-list 'sml/hidden-modes " GitGutter")
(add-to-list 'sml/hidden-modes " Undo-Tree")
(add-to-list 'sml/hidden-modes " MRev")
(add-to-list 'sml/hidden-modes " vl")
(add-to-list 'sml/hidden-modes " ElDoc")

;; Better Alt-x
(add-to-list 'load-path "~/.emacs.d/smex")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Right click mouse
(global-unset-key (kbd "<mouse-3>"))
(require 'mouse3)
(defalias 'mouse3-region-popup-menu 'mouse3-popup-menu)
(global-set-key (kbd "<mouse-3>") 'mouse3-region-popup-menu)

;; Enable the menu bar
(menu-bar-mode t)
;; End of customizations

;; Utilities

;; dos2unix
(defun dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))

;; Remove or add ending chars
(defun compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)
  (let (currentStateIsCompact (bigFillColumnVal 4333999) (deactivate-mark nil))
    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ) )

      (if (region-active-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))) )
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil)) ) )

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)) ) ) )

;; Beautify (poner bonito) tabulaciones en nuestro pograma en C/C++
(defun beautify-region (beg end)
  (interactive "r")
  (setq end (save-excursion (goto-char end) (point-marker)))
  (indent-region beg end nil))

(defun beautify-buffer ()
  "Beautify buffer by applying indentation, whitespace fixup, alignment, and
case fixing to entire buffer. Calls `vhdl-beautify-region' for the entire
buffer."
  (interactive)
  (beautify-region (point-min) (point-max))
  (when noninteractive (save-buffer)))
(global-unset-key "\C-b")
(global-set-key "\C-b" 'beautify-buffer)

;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                         (save-excursion
                           (untabify (point-min) (point-max)))) nil))

;;; setup-general.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(setq initial-scratch-message
      ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with 【▤】【o】,
;; then enter the text in that file's own buffer.")

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; We’re also going to use on.el to provide some of the same hooks Doom uses.
(use-package on
  :demand t)

;; Treat all themes as safe
(use-package custom
  :ensure nil
  :custom (custom-safe-themes t "Treat all themes as safe"))

;; Reporting Emacs bugs
(use-package emacsbug
  :custom (report-emacs-bug-no-explanations t))

(defvar org-mode-map (make-keymap))

;; Auto-revert buffers of changed files
(use-package autorevert
  :defer 15
  :if (not (equal system-type 'windows-nt))
  :commands (global-auto-revert-mode
             auto-revert-mode)
  :hook ((on-first-buffer . global-auto-revert-mode)
         (dired-mode      . auto-revert-mode))
  :diminish (auto-revert-mode . " Ⓐ")
  :custom ((auto-revert-verbose                 nil)
           (global-auto-revert-non-file-buffers t)
           (auto-revert-interval                2)
           (auto-revert-check-vc-info           t))
  :config (when (eq system-type 'darwin)
            ;; File notifications aren't supported on OS X
            (setq auto-revert-use-notify nil)))

;; Emacs startup profiler
(use-package esup
  :defer t
  :commands esup)

;; Minions
(use-package minions
  :demand t)

;; With-editor (emacsclient support)
(use-package with-editor
  :hook ((shell-mode eshell-mode term-exec) . with-editor-export-editor))

;; Pos-tip library
(use-package pos-tip
  :defer t
  :custom ((pos-tip-internal-border-width 6)
           (pos-tip-border-width          1))
  :config (defadvice popup-menu-show-quick-help
              (around pos-tip-popup-menu-show-quick-help () activate)
            "Show quick help using `pos-tip-show'."
            (if (display-graphic-p)
                (let ((doc (popup-menu-document
                            menu (or item
                                     (popup-selected-item menu)))))
                  (when (stringp doc)
                    (pos-tip-show doc nil
                                  (if (popup-hidden-p menu)
                                      (or (plist-get args :point)
                                          (point))
                                    (overlay-end (popup-line-overlay
                                                  menu (+ (popup-offset menu)
                                                          (popup-selected-line menu)))))
                                  nil 0) nil))
              ad-do-it)))

;; Turn on subword-mode for non-lispy languages
(use-package subword
  :defer t
  :commands subword-mode
  :init (mapc (lambda (mode)
                (add-hook mode #'subword-mode))
              my/subword-modes))

;; Choose wrap prefix automatically
(use-package adaptive-wrap
  :defer t
  :commands adaptive-wrap-prefix-mode
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

;; Uniquify-buffers
(use-package uniquify
  :defer 2
  :custom ((uniquify-buffer-name-style   'post-forward)
           (uniquify-separator           " • ")
           (uniquify-after-kill-buffer-p t)
           (uniquify-ignore-buffers-re   "^\\*")))

;; Unfill and fill
(use-package unfill
  :defer t
  :commands (unfill-region unfill-paragraph toggle-fill-unfill)
  :bind ([remap fill-paragraph] . toggle-fill-unfill))

;; Filladapt
(use-package filladapt
  :defer t
  :commands (filladapt-mode
             turn-off-filladapt-mode)
  :init (filladapt-mode t)
  :hook ((text-mode . filladapt-mode)
         (prog-mode . turn-off-filladapt-mode)))

;; Browse kill ring
(use-package browse-kill-ring
  :defer t
  :commands (browse-kill-ring
             browse-kill-ring-mode))

;; Unicode viewer (charmap)
(use-package charmap
  :defer t
  :commands charmap
  :custom (charmap-text-scale-adjust 2))

;; Page break lines
(use-package page-break-lines
  :commands page-break-lines-mode
  :defer t)

;; Naive implementation of RFC4122 Universally Unique IDentifier generation
(use-package uuidgen
  :commands (uuidgen
             insert-uuid-cid)
  :defer t)

;; This is a elisp library for websocket clients to talk to websocket servers,
;; and for websocket servers to accept connections from websocket clients.
;; This library is designed to be used by other library writers
(use-package websocket
  :defer t)

;; Simple HTTP requests
(use-package request
  :defer t)

;; Alert is a Growl-workalike for Emacs which uses a common notifications
(use-package alert
  :demand t
  :config (when (eq system-type 'gnu/linux)
            (setq alert-default-style 'notifications)))

;; Intelligent code search for Emacs lisp.
(use-package elisp-refs
  :defer t
  :after (list-utils loop))

;; Improved help system
(use-package helpful
  :defer t
  :after elisp-refs
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key]      . helpful-key)
         ("C-h f"                   . helpful-function)
         ("C-h c"                   . helpful-callable)
         ("C-h x"                   . helpful-command)
         ("C-h m"                   . helpful-macro)
         ("C-h k"                   . helpful-key)
         ("C-h v"                   . helpful-variable)))

;; Alignment
(use-package ialign
  :defer t
  :commands (ialign
             align-whitespace
             align-equals
             align-ampersand
             align-comma
             align-colon
             align-dot)
  :config (progn
            ;; Align functions
            (defun align-whitespace (start end)
              "Align columns by whitespace"
              (interactive "r")
              (align-regexp start end
                            "\\(\\s-*\\)\\s-" 1 0 t))

            (defun align-ampersand (start end)
              "Align columns by ampersand"
              (interactive "r")
              (align-regexp start end
                            "\\(\\s-*\\)&" 1 1 t))

            (defun align-equals (start end)
              "Align columns by equals sign"
              (interactive "r")
              (align-regexp start end
                            "\\(\\s-*\\)=" 1 0 t))

            (defun align-comma (start end)
              "Align columns by comma"
              (interactive "r")
              (align-regexp start end
                            "\\(\\s-*\\)," 1 1 t))

            (defun align-dot (start end)
              "Align columns by dot"
              (interactive "r")
              (align-regexp start end
                            "\\(\\s-*\\)\\\." 1 1 t))

            (defun align-colon (start end)
              "Align columns by equals sign"
              (interactive "r")
              (align-regexp start end
                            "\\(\\s-*\\):" 1 0 t))))

(use-package switch-buffer-functions
  :defer t
  :config (add-hook 'switch-buffer-functions
                    (lambda (prev cur)
                      (if (eval 'current-input-method)
                          (set-cursor-color "magenta")
                        (set-cursor-color "cyan")))))

;; Cross-editor style configuration
(use-package editorconfig
  :defer t
  :disabled t
  :hook ((prog-mode . editorconfig-mode)
         (text-mode . editorconfig-mode))
  :diminish editorconfig-mode
  :commands editorconfig-mode)

;; Keep .emacs.d clean
(use-package no-littering
  :demand t
  :init (setq no-littering-var-directory (expand-file-name "cache/var/" my/emacs-cache-dir)
              no-littering-etc-directory (expand-file-name "cache/etc/" my/emacs-cache-dir))
  :custom ((create-lockfiles                  nil)
           (auto-save-interval                500)
           (auto-save-default                 nil))
  :config (progn
            (setq
             backup-directory-alist
             `((".*" . ,(no-littering-expand-var-file-name "backup/")))
             auto-save-file-name-transforms
             `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

            (setq-default semanticdb-default-save-directory (concat no-littering-var-directory "semanticdb")
                          url-configuration-directory (concat no-littering-var-directory "url")
                          eshell-directory-name (concat no-littering-var-directory "eshell" ))))

(define-advice goto-line (:before (&rest _) preview-line-number)
  "Preview line number when prompting for goto-line."
  (interactive
   (lambda (spec)
     (if (and (boundp 'display-line-numbers)
              (not display-line-numbers))
         (unwind-protect
             (progn (display-line-numbers-mode)
                    (advice-eval-interactive-spec spec))
           (display-line-numbers-mode -1))
       (advice-eval-interactive-spec spec)))))

;; Clean stale buffers periodically
(use-package midnight
  :hook (on-first-buffer . midnight-mode))

;; Posframe
(use-package posframe
  :demand t)

;; set-mark-command-repeat-pop means we only need to hit C-u or C-x once before subsequent C-SPC, which makes it much nicer to navigate.
(setq set-mark-command-repeat-pop t)

;; Vertico is a little bit nicer version of the builtin icomplete-vertical.
(use-package vertico
  :delight vertico-mode
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle t)
  :hook (on-first-input . vertico-mode))

(use-package vertico-indexed
  :after vertico
  :config (vertico-indexed-mode))

(use-package vertico-multiform
  :after vertico
  :custom (vertico-multiform-commands '((git-related-find-file (vertico-sort-function . nil))))
  :config (vertico-multiform-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Marginalia annotates minibuffer completions with some useful info.
(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config (marginalia-mode))

;; For native-comp branch
(when (fboundp 'native-compile-async)
  (unless (fboundp 'subr-native-lambda-list)
    (defun subr-native-lambda-list (x)
      nil))
  (setq comp-async-jobs-number 4 ;; not using all cores
        comp-deferred-compilation t
        comp-deferred-compilation-black-list '()))

;; Bug workaround, 2023-02-26:
(if (boundp 'native-comp-enable-subr-trampolines)
    (if (not (boundp 'comp-enable-subr-trampolines))
        (setq comp-enable-subr-trampolines native-comp-enable-subr-trampolines)))

(defun has-fast-json ()
  "Return t if \"json-serialize\" is implemented as a C function.
This was done for Emacs 27 but not all builds include the C version,
which is a lot faster."
  (and
   (subrp (symbol-function 'json-serialize))
   ;; test that it works -- on Windows the DLL (or deps) may be missing
   (equal (json-serialize (json-parse-string "[123]")) "[123]")))

(unless (has-fast-json)
  (warn "This emacs is using older elisp json functions; maybe rebuild with libjansson?"))

(provide 'setup-general)
;;; setup-general.el ends here

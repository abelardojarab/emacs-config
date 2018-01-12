;;; setup-general.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017, 2018  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@ubuntu02>
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

;; Revert buffer
(use-package files
  :commands revert-buffer)

;; Auto-revert buffers of changed files
(use-package autorevert
  :demand t
  :if (not (equal system-type 'windows-nt))
  :commands global-auto-revert-mode
  :diminish (auto-revert-mode . " Ⓐ")
  :config (progn

            (setq auto-revert-verbose nil
                  global-auto-revert-non-file-buffers t)


            (when (eq system-type 'darwin)
              ;; File notifications aren't supported on OS X
              (setq auto-revert-use-notify nil))
            (global-auto-revert-mode)))

;; Emacs startup profiler
(use-package esup
  :defer t
  :load-path (lambda () (expand-file-name "esup/" user-emacs-directory))
  :commands esup)

;; With-editor (emacsclient support)
(use-package with-editor
  :load-path (lambda () (expand-file-name "with-editor/" user-emacs-directory))
  :init (progn
          (add-hook 'shell-mode-hook  #'with-editor-export-editor)
          (add-hook 'eshell-mode-hook #'with-editor-export-editor)))

;; Pos-tip library
(use-package pos-tip
  :defer t
  :load-path (lambda () (expand-file-name "pos-tip/" user-emacs-directory))
  :config (progn
            (defadvice popup-menu-show-quick-help
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
                ad-do-it))))

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
  :load-path (lambda () (expand-file-name "adaptive-wrap/" user-emacs-directory))
  :commands adaptive-wrap-prefix-mode
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

;; Uniquify-buffers
(use-package uniquify
  :config (setq
           uniquify-buffer-name-style 'post-forward
           uniquify-separator " • "
           ;; rename after killing uniquified
           uniquify-after-kill-buffer-p t
           ;; don't muck with special buffers
           uniquify-ignore-buffers-re "^\\*"))

;; Unfill and fill
(use-package unfill
  :defer t
  :commands (unfill-region unfill-paragraph toggle-fill-unfill)
  :bind ([remap fill-paragraph] . toggle-fill-unfill))

;; Browse kill ring
(use-package browse-kill-ring
  :load-path (lambda () (expand-file-name "browse-kill-ring/" user-emacs-directory)))

;; Unicode viewer (charmap)
(use-package charmap
  :defer t
  :commands charmap
  :load-path (lambda () (expand-file-name "charmap/" user-emacs-directory))
  :config (setq charmap-text-scale-adjust 2))

;; Page break lines
(use-package page-break-lines
  :defer t
  :load-path (lambda () (expand-file-name "page-break-lines/" user-emacs-directory)))

;; Naive implementation of RFC4122 Universally Unique IDentifier generation
(use-package uuidgen
  :defer t
  :load-path (lambda () (expand-file-name "uuidgen/" user-emacs-directory)))

;; This is a elisp library for websocket clients to talk to websocket servers,
;; and for websocket servers to accept connections from websocket clients.
;; This library is designed to be used by other library writers
(use-package websocket
  :defer t
  :load-path (lambda () (expand-file-name "websocket/" user-emacs-directory)))

;; Simple HTTP requests
(use-package request
  :defer t
  :load-path (lambda () (expand-file-name "request/" user-emacs-directory)))

;; alert is a Growl-workalike for Emacs which uses a common notifications
(use-package alert
  :demand t
  :load-path (lambda () (expand-file-name "alert/" user-emacs-directory))
  :config (when (eq system-type 'gnu/linux)
            (setq alert-default-style 'notifications)))

;; intelligent code search for Emacs lisp.
(use-package elisp-refs
  :defer t
  :after (list-utils loop)
  :load-path (lambda () (expand-file-name "elisp-refs/" user-emacs-directory)))

;; Improved help system
(use-package helpful
  :defer t
  :after elisp-refs
  :load-path (lambda () (expand-file-name "helpful/" user-emacs-directory))
  :bind (("C-h f" . helpful-function)
         ("C-h c" . helpful-callable)
         ("C-h x" . helpful-command)
         ("C-h m" . helpful-macro)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)))

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

(provide 'setup-general)
;;; setup-general.el ends here

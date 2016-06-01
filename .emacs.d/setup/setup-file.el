;;; setup-file.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2016, , , , ,   abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxcj9063.pdx.intel.com>
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

;; Help to determine who modifies buffer
(defvar my-debug-set-buffer-modified-p-buffers nil)
(defadvice set-buffer-modified-p
    (before my-debug-set-buffer-modified-p-advice)
  (when (memq (current-buffer) my-debug-set-buffer-modified-p-buffers)
    (debug)))
(ad-activate 'set-buffer-modified-p)

(defun my-debug-set-buffer-modified-p (buffer)
  (interactive (list (current-buffer)))
  (if (memq buffer my-debug-set-buffer-modified-p-buffers)
      (progn (setq my-debug-set-buffer-modified-p-buffers
                   (delq buffer my-debug-set-buffer-modified-p-buffers))
             (message "Disabled for %s" buffer))
    (add-to-list 'my-debug-set-buffer-modified-p-buffers buffer)
    (message "Enabled for %s" buffer)))

;; Trick emacs when opening a file through menu-find-file-existing
(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil)
        (use-dialog-box t))
    ad-do-it))

;; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

;; auto revert files if they are modified in an external editor
(global-auto-revert-mode t)

;; Garantee utf8 as input-method
(set-input-method nil)
(setq read-quoted-char-radix 10)
(if (equal system-type 'windows-nt)
    (progn
      (set-language-environment 'utf-8)
      ;; (setq locale-coding-system 'utf-16-le) ;; bad, it can potentially undefine all fonts
      (setq buffer-file-coding-system 'utf-8-unix)
      (set-default-coding-systems 'utf-8-unix)
      (set-terminal-coding-system 'utf-8)
      (set-keyboard-coding-system 'utf-8)
      (set-selection-coding-system 'utf-16-le)
      (prefer-coding-system 'utf-8-unix)) ;; progn
  (progn
    (set-locale-environment "en_US.UTF-8")
    (set-language-environment 'utf-8)
    (setq locale-coding-system 'utf-8-unix)
    (setq buffer-file-coding-system 'utf-8-unix)
    (set-default-coding-systems 'utf-8-unix)
    (set-terminal-coding-system 'utf-8-unix)
    (set-keyboard-coding-system 'utf-8-unix)
    (set-selection-coding-system 'utf-8-unix)
    (prefer-coding-system 'utf-8-unix)))

;; ansi-term doesn’t obey usage of utf-8-unix
(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

;; update the copyright when present
(add-hook 'before-save-hook 'copyright-update)

;; deleting files goes to OS's trash folder
(setq delete-by-moving-to-trash t)

;; Make URLs in comments/strings clickable, (emacs > v22)
(add-hook 'find-file-hooks 'goto-address-prog-mode)

;; Ignore case when looking for a file
(setq read-file-name-completion-ignore-case t)

;; Time stamp
(setq
 time-stamp-active t          ;; do enable time-stamps
 time-stamp-line-limit 20     ;; check first 10 buffer lines for Time-stamp:
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ;; date format
(add-hook 'write-file-hooks 'time-stamp) ;; update when saving

;; More exhaustive cleaning of white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; sudo edit
(use-package sudo-edit
  :load-path (lambda () (expand-file-name "sudo-edit/" user-emacs-directory)))

(provide 'setup-file)
;;; setup-file.el ends here

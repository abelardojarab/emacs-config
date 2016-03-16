;;; setup-file.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2016  abelardo.jara-berrocal

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

;; Trick emacs when opening a file through menu-find-file-existing
(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil)
        (use-dialog-box t))
    ad-do-it))

;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                         (save-excursion
                           (untabify (point-min) (point-max)))) nil))

;; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

;; Garantee utf8 as input-method
(set-input-method nil)
(setq read-quoted-char-radix 10)
(set-language-environment 'utf-8)
(set-locale-environment "en_US.UTF-8")
(setq locale-coding-system 'utf-8-unix)

;; Coding system
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Even so, ansi-term doesn’t obey:
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

;; Do not fontify large files
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 512 256))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;; More exhaustive cleaning of white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'setup-file)
;;; setup-file.el ends here

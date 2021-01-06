;;; setup-file.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2021  Abelardo Jara-Berrocal

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

;; A nice package to show the disk usage
(use-package disk-usage
  :disabled t
  :demand t)

;; Automatic filling setup
(use-package auto-fill
  :defer t
  :diminish (auto-fill-mode . " Ⓕ")
  :custom (fill-column 120)
  :hook (text-mode . auto-fill-mode)
  ;; set auto-fill to trigger on punctuation rather than only on whitespace.
  :config (mapc
           (lambda (c)
             (set-char-table-range auto-fill-chars c t))
           "!-=+]};:'\",.?"))

;; aggressive fill on paragraphs
(use-package aggressive-fill-paragraph
  :disabled t
  :defer t
  :commands aggressive-fill-paragraph-mode)

;; Backup and revert buffer
(use-package files
  :demand t
  :custom ((after-find-file-from-revert-buffer    t)
           (delete-old-versions                   t)
           (delete-by-moving-to-trash             t)
           (kept-new-versions                     6)
           (kept-old-versions                     2)
           (version-control                       t)
           (make-backup-files                     nil)
           (backup-by-copying                     t)
           (backup-by-copying-when-mismatch       t)
           (w32-get-true-file-attributes          nil)
           (w32-pipe-read-delay                   0)
           (read-file-name-completion-ignore-case t))
  :hook ((after-save-hook . executable-make-buffer-file-executable-if-script-p)
         (find-file-hooks . goto-address-prog-mode))
  :config (progn
            (setq confirm-kill-processes nil)
            (defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
              "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
              (let ((last-nonmenu-event nil)
                    (use-dialog-box t))
                ad-do-it))))

;; More exhaustive cleaning of white space
(use-package whitespace
  :defer t
  :commands delete-trailing-whitespace
  :hook (before-save . delete-trailing-whitespace)
  :config (setq whitespace-display-mappings
                '((space-mark 32 [183] [46]) ; normal space, ·
                  (space-mark 160 [164] [95])
                  (space-mark 2208 [2212] [95])
                  (space-mark 2336 [2340] [95])
                  (space-mark 3616 [3620] [95])
                  (space-mark 3872 [3876] [95])
                  (newline-mark 10 [182 10]) ; newline, ¶
                  (tab-mark 9 [9655 9] [92 9]) ; tab, ▷
                  )))

;; Whitespace-mode
(use-package whitespace-mode
  :defer t
  :commands whitespace-mode)

;; update the copyright whe present
(use-package copyright
  :defer t
  :hook (before-save . copyright-update)
  :custom (copyright-year-ranges t)
  :config (setq copyright-names-regexp (regexp-quote user-full-name)))

;; Time stamp
(use-package time-stamp
  :defer t
  :custom ((time-stamp-active        t)
           (time-stamp-line-limit    20)
           (time-stamp-warn-inactive t)
           (time-stamp-format        "%04y-%02m-%02d %02H:%02M:%02S (%u)"))
  :commands time-stamp
  :hook (before-save . time-stamp))

;; Guarantee utf8 as input-method
(use-package mule
  :demand t
  :custom (read-quoted-char-radix 10)
  :config (progn
            (set-input-method nil)
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
                (prefer-coding-system 'utf-8-unix)))))

;; sudo edit
(use-package sudo-edit
  :defer t
  :commands (sudo-edit sudo-edit-current-file))

;; Assure consistency when opening files
(use-package unify-opening
  :demand t)

(provide 'setup-file)
;;; setup-file.el ends here

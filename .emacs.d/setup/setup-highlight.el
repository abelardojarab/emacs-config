;;; setup-highlight.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

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

;; highlight phrases or regular expressions
(use-package hi-lock
  :defer t
  :commands global-hi-lock-mode)

;; higlight changes in documents
(use-package hilit-chg
  :defer t
  :if (display-graphic-p)
  :diminish highlight-changes-mode
  :init (global-highlight-changes-mode t)
  :commands global-highlight-changes-mode
  :config (progn
            (setq highlight-changes-visibility-initial-state nil)

            ;; Fix highlight bug of marking a file as modified
            (defadvice highlight-changes-rotate-faces (around around-rotate-faces)
              (let ((was-modified (buffer-modified-p))
                    (buffer-undo-list t))
                ad-do-it
                (unless was-modified
                  (set-buffer-modified-p nil))))
            (ad-activate 'highlight-changes-rotate-faces)))

;; Highlight the line
(use-package hl-line
  :commands (global-hl-line-mode
             hl-line-mode
             my/hl-line-mode-off)
  :hook ((prog-mode               . hl-line-mode)
         ((org-mode markdown-mode) . my/hl-line-mode-off))
  :custom ((hl-line-sticky-flag   t)
           (global-hl-sticky-flag t))
  :config (progn
            ;; https://stackoverflow.com/questions/20275596/how-to-use-hl-line-mode-to-highlight-just-one-1-line-when-visual-line-mode-is
            (defun visual-line-line-range ()
              (save-excursion (cons
                               (progn (vertical-motion 0) (point))
                               (progn (vertical-motion 1) (point)))))
            (setq hl-line-range-function 'visual-line-line-range)

            (defun my/hl-line-mode-off ()
              (interactive)
              (make-local-variable 'global-hl-line-mode)
              (setq global-hl-line-mode nil))

            ;; hl-line overrides the background of hi-lock’ed text, this will provide a fix
            (defadvice hi-lock-set-pattern (around use-overlays activate)
              (let ((font-lock-fontified nil))
                ad-do-it))))

;; Deferred highlighted line
(use-package hl-line+
  :config (progn
            (hl-line-when-idle-interval 0.3)
            (toggle-hl-line-when-idle 1)))

;; Highlight symbol
(use-package highlight-symbol
  :defer t
  :commands highlight-symbol-mode
  :diminish highlight-symbol-mode
  :custom ((highlight-symbol-idle-delay      0.1)
           (highlight-symbol-on-navigation-p t))
  :config (progn
            ;; http://emacs.stackexchange.com/questions/931
            (defun highlight-symbol-mode-post-command ()
              "After a command, change the temporary highlighting.
Remove the temporary symbol highlighting and, unless a timeout is specified,
create the new one."
              (if (eq this-command 'highlight-symbol-jump)
                  (when highlight-symbol-on-navigation-p
                    (highlight-symbol-temp-highlight))
                (highlight-symbol-update-timer highlight-symbol-idle-delay)))

            (defun highlight-symbol-update-timer (value)
              (when highlight-symbol-timer
                (cancel-timer highlight-symbol-timer))
              (setq highlight-symbol-timer
                    (run-with-timer value nil 'highlight-symbol-temp-highlight)))))

;; Highlight blocks
(use-package highlight-blocks
  :defer t
  :commands highlight-blocks-mode
  :diminish highlight-blocks-mode
  :config (mapc (lambda (mode)
                  (add-hook mode #'highlight-sexp-mode))
                my/highlight-blocks-modes))

;; Highlight s-exp (not available in melpa)
(use-package highlight-sexp
  :defer t
  :commands highlight-sexp-mode
  :load-path (lambda () (expand-file-name "highlight-sexp/" user-emacs-directory))
  :diminish highlight-sexp-mode
  :config (mapc (lambda (mode)
                  (add-hook mode #'highlight-sexp-mode))
                my/highlight-sexp-modes))

;; Highlight the latest changes in the buffer (like text inserted from: yank, undo, etc.)
(use-package volatile-highlights
  :defer t
  :commands volatile-highlights-mode
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

(provide 'setup-highlight)
;;; setup-highlight.el ends here

;;; setup-highlight.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@Abelardos-MacBook-Pro.local>
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
;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-highlight.el
(use-package hi-lock
  :bind (:map hi-lock-map
              ;; Unbind the "C-x w" bindings because "M-s h" bindings provide the same thing.
              ("C-x w" . nil)
              :map search-map
              ("C-h" . my/unhighlight-all-in-buffer))
  :commands (global-hi-lock-mode
             my/hi-lock-face-symbol-at-point-or-sel)
  :init (global-hi-lock-mode 1)
  :config (progn

            ;; Patch the `hi-lock-face-buffer' aka `highlight-regexp' to pick the
            ;; selected region to derive a regexp if a region is active.
            (defun hi-lock-face-buffer (regexp &optional face)
              "Set face of each match of REGEXP to FACE.
Interactively, prompt for REGEXP using `read-regexp', then FACE.
Use the global history list for FACE.
Use Font lock mode, if enabled, to highlight REGEXP.  Otherwise,
use overlays for highlighting.  If overlays are used, the
highlighting will not update as you type."
              (interactive
               (list
                (hi-lock-regexp-okay
                 (read-regexp "Regexp to highlight"
                              (if (use-region-p)
                                  ;; Use `rx' to generate regexp for selected text.
                                  ;; Example: regexp to find "a.b" text would be
                                  ;; "a\.b"
                                  (let ((str (buffer-substring-no-properties
                                              (region-beginning) (region-end))))
                                    (eval `(rx ,str)))
                                'regexp-history-last)))
                (hi-lock-read-face-name)))
              (or (facep face) (setq face 'hi-yellow))
              (unless hi-lock-mode (hi-lock-mode 1))
              (hi-lock-set-pattern regexp face))

            ;; Don't scan the file beyond 1000 characters to look for the hi-lock patterns.
            (setq hi-lock-file-patterns-range 1000)

            ;; Don't ask before highlighting any hi-lock: pattern found in a file
            ;; Below, (lambda (pattern) t) simply always returns `t' regardless of
            ;; what the `pattern' input is.
            (setq hi-lock-file-patterns-policy (lambda (pattern) t))

            ;; Mark the `hi-lock-file-patterns' variable as safe so that it can be
            ;; set in `.dir-locals.el' files.
            (put 'hi-lock-file-patterns 'safe-local-variable 'identity)

            ;; Automatically cycle through the highlighting faces listed in
            ;; `hi-lock-face-defaults' instead of bothering the user to pick a face
            ;; manually each time.
            (setq hi-lock-auto-select-face t)

            (defun my/hi-lock-face-symbol-at-point-or-sel ()
              "If a region is selected, highlight each instance of that.
Else highlight each instance of the symbol at point.
Uses the next face from `hi-lock-face-defaults' without prompting,
unless you use a prefix argument. Uses `find-tag-default-as-symbol-regexp' to
retrieve the symbol at point.
This uses Font lock mode if it is enabled; otherwise it uses overlays,
in which case the highlighting will not update as you type."
              (interactive)
              (let* ((regexp (hi-lock-regexp-okay
                              (cond ((use-region-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end)))
                                    (t
                                     (find-tag-default-as-symbol-regexp)))))
                     (hi-lock-auto-select-face t)
                     (face (hi-lock-read-face-name)))
                (or (facep face) (setq face 'hi-yellow))
                (unless hi-lock-mode (hi-lock-mode 1))
                (hi-lock-set-pattern regexp face)))

            ;; Enable `hi-lock-mode' in `text-mode' too
            ;; The hi-lock fontification will not be visible (the `font-lock-keywords'
            ;; variable will not be updated unless `font-lock-fontified' is already `t'.
            ;; This was derived by studying the definition of `hi-lock-font-lock-hook'
            ;; function.
            (defun my/hi-lock-enable-in-text-mode ()
              (setq-local font-lock-fontified t))
            (add-hook 'text-mode-hook #'my/hi-lock-enable-in-text-mode)

            (defun my/unhighlight-all-in-buffer ()
              "Remove all highlights made by `hi-lock' from the current buffer.
The same result can also be be achieved by \\[universal-argument] \\[unhighlight-regexp]."
              (interactive)
              ;; `unhighlight-regexp' is aliased to `hi-lock-unface-buffer'
              (hi-lock-unface-buffer t))))

;; higlight changes in documents
(use-package hilit-chg
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
  :config (progn
            (global-hl-line-mode t)

            ;; Highlight the line only in the active window
            (setq hl-line-sticky-flag nil)

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
                ad-do-it))
            (add-hook 'org-mode-hook 'my/hl-line-mode-off)))

;; Highlight symbol
(use-package highlight-symbol
  :defer t
  :commands highlight-symbol-mode
  :load-path (lambda () (expand-file-name "highlight-symbol/" user-emacs-directory))
  :diminish highlight-symbol-mode
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
                    (run-with-timer value nil 'highlight-symbol-temp-highlight)))

            (setq highlight-symbol-idle-delay       0.1
                  highlight-symbol-on-navigation-p  t)))

;; Highlight blocks
(use-package highlight-blocks
  :defer t
  :commands highlight-blocks-mode
  :load-path (lambda () (expand-file-name "highlight-blocks/" user-emacs-directory))
  :diminish highlight-blocks-mode
  :config (mapc (lambda (mode)
                  (add-hook mode 'highlight-sexp-mode))
                my/highlight-blocks-modes))

;; Highlight s-exp
(use-package highlight-sexp
  :defer t
  :commands highlight-sexp-mode
  :load-path (lambda () (expand-file-name "highlight-sexp/" user-emacs-directory))
  :diminish highlight-sexp-mode
  :config (mapc (lambda (mode)
                  (add-hook mode 'highlight-sexp-mode))
                my/highlight-sexp-modes))

;; Highlight the latest changes in the buffer (like text inserted from: yank, undo, etc.) until the next command is run
(use-package volatile-highlights
  :load-path (lambda () (expand-file-name "volatile-highlights/" user-emacs-directory))
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

(provide 'setup-highlight)
;;; setup-highlight.el ends here

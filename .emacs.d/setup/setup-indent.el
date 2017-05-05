;;; setup-indent.el ---                              -*- lexical-binding: t; -*-

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

;; Set indent to 4 instead of 2
(setq standard-indent my/tab-width)

;; My personal configurations, has to use setq-default
(setq-default indent-tabs-mode nil
              default-tab-width my/tab-width
              tab-width my/tab-width)

(defun tabtab/forward-char (n)
  (let ((space (- (line-end-position) (point))))
    (if (> space my/tab-width)
        (forward-char n)
      (move-end-of-line 1)
      (insert (make-string (- n space) ? )))))

(defun my/tab-jump ()
  (interactive)
  (let ((shift (mod (current-column) my/tab-width)))
    (tabtab/forward-char (- my/tab-width shift))))

;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                    (save-excursion
                      (untabify (point-min) (point-max)))) nil))

;; auto-indent pasted code
(defadvice yank (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode lisp-mode c-mode c++-mode
                                objc-mode latex-mode plain-tex-mode python-mode java-mode js2-mode))
      (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode lisp-mode c-mode c++-mode
                                objc-mode latex-mode plain-tex-mode python-mode java-mode js2-mode))
      (indent-region (region-beginning) (region-end) nil)))

;; Disable electric indent
(if (featurep 'electric-indent-mode)
    (add-hook 'prog-mode-hook (lambda () (electric-indent-local-mode -1))))

;; Auto-indent mode
(use-package auto-indent-mode
  :pin manual
  :defer t
  :commands auto-indent-mode
  :load-path (lambda () (expand-file-name "auto-indent-mode/" user-emacs-directory))
  :init (progn
          (setq auto-indent-indent-style 'conservative)
          (setq auto-indent-on-visit-file nil) ;; do not indent when a file is visit
          (setq auto-indent-blank-lines-on-move nil)
          (setq auto-indent-next-pair-timer-geo-mean (quote ((default 0.0005 0))))
          (setq auto-indent-disabled-modes-list (list (quote vhdl-mode)))))

;; Permanent indentation guide
(use-package indent-hint
  :defer t
  :commands indent-hint-mode
  :load-path (lambda () (expand-file-name "indent-hint/" user-emacs-directory))
  :init (progn
          (setq indent-hint-background-overlay t)
          (setq indent-hint-bg nil)))

;; Transient indentation guide
(use-package highlight-indent-guides
  :defer t
  :disabled t
  :commands highlight-indent-guides-mode
  :load-path (lambda () (expand-file-name "highlight-indent-guides/" user-emacs-directory))
  :config (progn

            ;; Fix indent guide issue with popup
            (defvar my/indent-guide-mode-suppressed nil)
            (defadvice popup-create (before highlight-indent-guides-mode activate)
              "Suspend highlight-indent-guides-mode while popups are visible"
              (let ((highlight-indent-guides-enabled (and (boundp 'highlight-indent-guides-mode) highlight-indent-guides-mode)))
                (set (make-local-variable 'my/highlight-indent-guides-mode-suppressed) highlight-indent-guides-mode)
                (when highlight-indent-guides-enabled
                  (highlight-indent-guides-mode -1))))
            (defadvice popup-delete (after highlight-indent-guides-mode activate)
              "Restore highlight-indent-guides-mode when all popups have closed"
              (let ((highlight-indent-guides-enabled (and (boundp 'highlight-indent-guides-mode) highlight-indent-guides-mode)))
                (when (and (not popup-instances) my/highlight-indent-guides-mode-suppressed)
                  (setq my/highlight-indent-guides-mode-suppressed nil)
                  (highlight-indent-guides-mode 1))))

            (setq highlight-indent-guides-method 'character)
            (if (display-graphic-p)
                (setq highlight-indent-guides-character ?┊))))

;; Highlight indentation levels
(use-package highlight-indentation
  :defer t
  :commands highlight-indentation-mode
  :load-path (lambda () (expand-file-name "highlight-indentation/" user-emacs-directory)))

(provide 'setup-indent)

;;; setup-indent.el ends here

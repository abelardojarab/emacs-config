;;; setup-indent.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

;; Author: Abelardo Jara <abelardojara@Abelardos-MacBook-Pro.local>
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
(use-package indent-guide
  :defer t
  :commands indent-guide-mode
  :load-path (lambda () (expand-file-name "indent-guide/" user-emacs-directory))
  :diminish indent-guide-mode
  :config (progn
            ;; Fix indent guide issue with popup
            (defvar my/indent-guide-mode-suppressed nil)
            (defadvice popup-create (before indent-guide-mode activate)
              "Suspend indent-guide-mode while popups are visible"
              (let ((indent-guide-enabled (and (boundp 'indent-guide-mode) indent-guide-mode)))
                (set (make-local-variable 'my/indent-guide-mode-suppressed) indent-guide-mode)
                (when indent-guide-enabled
                  (indent-guide-mode -1))))
            (defadvice popup-delete (after indent-guide-mode activate)
              "Restore indent-guide-mode when all popups have closed"
              (let ((indent-guide-enabled (and (boundp 'indent-guide-mode) indent-guide-mode)))
                (when (and (not popup-instances) my/indent-guide-mode-suppressed)
                  (setq my/indent-guide-mode-suppressed nil)
                  (indent-guide-mode 1))))

            (unless (or (equal system-type 'windows-nt)
                        (not (display-graphic-p)))
              (setq indent-guide-char "┊"))
            (setq indent-guide-recursive t)))

;; Highlight indentation levels
(use-package highlight-indentation
  :defer t
  :commands highlight-indentation-mode
  :load-path (lambda () (expand-file-name "highlight-indentation/" user-emacs-directory)))

(provide 'setup-indent)

;;; setup-indent.el ends here

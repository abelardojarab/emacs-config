;;; setup-scroll.el ---                              -*- lexical-binding: t; -*-

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

(setq-default scroll-step                     10
              scroll-margin                   3
              scroll-conservatively           101
              scroll-preserve-screen-position 't
              scroll-up-aggressively          0.01
              scroll-down-aggressively        0.01
              fast-but-imprecise-scrolling    t
              auto-window-vscroll             nil)

(defadvice line-move-to-column (around bar activate)
  (ignore-errors add-do-it))

;; Be careful it can ruin shift-select-mode
(use-package smooth-scrolling
  :disabled t
  :defer t
  :commands smooth-scrolling-mode
  :config (smooth-scrolling-mode 1))

;; Centered scrolling
(use-package centered-cursor-mode
  :defer t
  :commands centered-cursor-mode
  :config (centered-cursor-mode 1))

(use-package fast-scroll
  :defer t
  :load-path (lambda () (expand-file-name "fast-scroll/" user-emacs-directory))
  :custom (fast-scroll-throttle 0.5)
  :commands fast-scroll-mode
  :hook (prog-mode . fast-scroll-mode)
  :config (progn
            (defun my/flycheck-enabled-p () (symbol-value 'flycheck-mode))
            (defvar my/flycheck-mode-suppressed nil)
            (make-variable-buffer-local 'my/flycheck-mode-suppressed)
            (add-hook 'fast-scroll-start-hook (lambda ()
                                                (let ((flycheck-enabled (my/flycheck-enabled-p)))
                                                  (when flycheck-enabled
                                                    (flycheck-mode -1)
                                                    (setq my/flycheck-mode-suppressed flycheck-enabled)))))
            (add-hook 'fast-scroll-end-hook (lambda ()
                                              (when my/flycheck-mode-suppressed
                                                (flycheck-mode 1)
                                                (setq my/flycheck-mode-suppressed nil))))

            (defun fast-scroll-default-mode-line ()
              mode-line-format)
            (fast-scroll-config)))

(provide 'setup-scroll)
;;; setup-scroll.el ends here

;;; setup-region.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017  Abelardo Jara-Berrocal

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

;; Move text
(use-package move-text
  :defer t
  :commands (move-text-up move-text-down))

;; This is a fairly simple package that provides information about the active region.
(use-package region-state
  :defer t
  :commands region-state-mode
  :load-path (lambda () (expand-file-name "region-state/" user-emacs-directory))
  :config (region-state-mode))

;; Region bindings mode
(use-package region-bindings-mode
  :load-path (lambda () (expand-file-name "region-bindings-mode/" user-emacs-directory))
  :diminish region-bindings-mode
  :config (progn
            (region-bindings-mode-enable)

            ;; shift select bindings
            (define-key region-bindings-mode-map (kbd "<prior>") 'shift-mark-backward-page)
            (define-key region-bindings-mode-map (kbd "<next>")  'shift-mark-forward-page)
            (define-key region-bindings-mode-map (kbd "<left>")  'shift-mark-backward-char)
            (define-key region-bindings-mode-map (kbd "<right>") 'shift-mark-forward-char)

            ;; movement bindings
            (define-key region-bindings-mode-map (kbd "M-<up>")   'move-text-up)
            (define-key region-bindings-mode-map (kbd "M-<down>") 'move-text-down)
            (define-key region-bindings-mode-map (kbd "<")        'decrease-left-margin)
            (define-key region-bindings-mode-map (kbd ">")        'increase-right-margin)

            (define-key region-bindings-mode-map (kbd "C-x C-c")  'clipboard-kill-ring-save)
            (define-key region-bindings-mode-map (kbd "C-x C-x")  'clipboard-kill-region)
            (define-key region-bindings-mode-map (kbd "C-x C-k")  'kill-line)

            (define-key region-bindings-mode-map (kbd "c")        'kill-ring-save)
            (define-key region-bindings-mode-map (kbd "x")        'kill-region)))

(provide 'setup-region)
;;; setup-region.el ends here

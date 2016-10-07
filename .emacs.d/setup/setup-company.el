;;; setup-company.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author:  <abelardojara@abelardo-ubuntu>
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
(use-package company
  :defer 2
  :diminish company-mode
  :load-path (lambda () (expand-file-name "company-mode/" user-emacs-directory))
  :config (progn
            ;; (add-hook 'prog-mode-hook 'company-mode)
            (global-company-mode)

            ;; Use Company for completion
            (bind-key [remap completion-at-point] #'company-complete company-mode-map)
            (setq company-backends '(company-yasnippet
                                     company-semantic
                                     company-dabbrev
                                     company-gtags))
            (add-to-list 'company-backends 'company-dabbrev-code)

            (setq company-idle-delay 0.1
                  company-minimum-prefix-length 2
                  company-show-numbers t
                  company-tooltip-align-annotations t
                  company-dabbrev-downcase nil
                  company-dabbrev-ignore-case t)

            (define-key company-active-map (kbd "C-n") 'company-select-next)
            (define-key company-active-map (kbd "C-p") 'company-select-previous)
            (define-key company-active-map (kbd "TAB") 'company-complete-selection)
            (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
            (define-key company-active-map (kbd "RET") 'company-complete-selection)))

;; Documentation popups for Company
(use-package company-quickhelp
  :load-path (lambda () (expand-file-name "company-quickhelp/" user-emacs-directory))
  :config (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(provide 'setup-company)
;;; setup-company.el ends here

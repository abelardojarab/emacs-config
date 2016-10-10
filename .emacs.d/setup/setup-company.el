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
            (global-company-mode)

            ;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
            (setq tab-always-indent 'complete)  ;; use 'complete when auto-complete is disabled
            (add-to-list 'completion-styles 'initials t)

            ;; Use Company for completion
            (bind-key [remap completion-at-point] #'company-complete company-mode-map)
            (setq company-backends '(company-semantic
                                     company-dabbrev-code
                                     company-gtags))

            (setq company-idle-delay 0.1
                  company-minimum-prefix-length 2
                  company-show-numbers t
                  company-tooltip-align-annotations t
                  company-dabbrev-downcase nil
                  company-dabbrev-ignore-case t
                  company-semantic-insert-arguments t
                  company-gtags-insert-arguments t)

            (define-key company-active-map (kbd "C-n") 'company-select-next)
            (define-key company-active-map (kbd "C-p") 'company-select-previous)
            (define-key company-active-map (kbd "TAB") 'company-complete-selection)
            (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
            (define-key company-active-map (kbd "RET") 'company-complete-selection)))

;; Documentation popups for Company
(use-package company-quickhelp
  :after company
  :load-path (lambda () (expand-file-name "company-quickhelp/" user-emacs-directory))
  :if (display-graphic-p)
  :config (progn
            (setq company-quickhelp-delay 0.2)
            (add-hook 'global-company-mode-hook #'company-quickhelp-mode)

            ;; Update front-end tooltip
            (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))))

;; Company integration with irony
(use-package company-irony
  :after (company irony)
  :load-path (lambda () (expand-file-name "company-irony/" user-emacs-directory))
  :config (add-hook 'irony-mode-hook
                    (lambda ()
                      (setq company-backends '(
                                               ;; company-yasnippet
                                               company-semantic
                                               company-gtags
                                               company-irony
                                               ;; company-dabbrev
                                               company-dabbrev-code)))))

(provide 'setup-company)
;;; setup-company.el ends here

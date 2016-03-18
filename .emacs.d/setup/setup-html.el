;;; setup-html.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara

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

(use-package ac-html
  :load-path (lambda () (expand-file-name "ac-html/" user-emacs-directory))
  :config (progn
            (defun setup-ac-for-html ()
              ;; Require ac-html since we are setup html auto completion
              (require 'ac-html)
              ;; Require default data provider if you want to use
              (require 'ac-html-default-data-provider)
              ;; Enable data providers,
              ;; currently only default data provider available
              (ac-html-enable-data-provider 'ac-html-default-data-provider)
              ;; Let ac-html do some setup
              (ac-html-setup)
              ;; Set your ac-source
              (setq ac-sources '(ac-source-html-tag
                                 ac-source-html-attr
                                 ac-source-html-attrv))
              ;; Enable auto complete mode
              (auto-complete-mode))

            (add-hook 'html-mode-hook 'setup-ac-for-html)))

(provide 'setup-html)
;;; setup-org-html.el ends here

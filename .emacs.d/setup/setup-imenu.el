;;; setup-imenu.el ---                               -*- lexical-binding: t; -*-

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

;; iMenu
(use-package imenu-anywhere
  :load-path (lambda () (expand-file-name "imenu-anywhere/" user-emacs-directory))
  :config (progn
            (set-default 'imenu-auto-rescan t)
            (mapc (lambda (mode)
                    (add-hook mode 'imenu-add-menubar-index))
                  '(c-mode-common-hook
                    python-mode-hook
                    reftex-mode-hook
                    reftex-load-hook
                    org-mode-hook))
            (setq imenu-create-index-function
                  (lambda ()
                    (let ((end))
                      (beginning-of-buffer)
                      (re-search-forward "^%%")
                      (forward-line 1)
                      (setq end (save-excursion (re-search-forward "^%%") (point)))
                      (loop while (re-search-forward "^\\([a-z].*?\\)\\s-*\n?\\s-*:" end t)
                            collect (cons (match-string 1) (point))))))

            ;; Find definitions in current buffer using gtags
            (if (executable-find "global")
		(add-hook 'c-mode-common-hook
			  (lambda ()
			    (make-local-variable 'imenu-create-index-function)
			    (setq imenu-create-index-function #'ggtags-build-imenu-index))))))

;; iMenu list
(use-package imenu-list
  :load-path (lambda () (expand-file-name "imenu-list/" user-emacs-directory))
  :config (progn
            (setq imenu-list-size 0.2)
            (setq imenu-list-focus-after-activation t)
            (setq imenu-list-auto-resize t)
            (setq imenu-list-position 'right)))

;; iMenus
(use-package imenus
  :load-path (lambda () (expand-file-name "imenus/" user-emacs-directory)))


(provide 'setup-imenu)
;;; setup-imenu.el ends here

;;; setup-eldoc.el ---                               -*- lexical-binding: t; -*-

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

(use-package eldoc
  :diminish eldoc-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'c-mode-common-hook 'turn-on-eldoc-mode)
	  (setq eldoc-idle-delay 0)

	  ;; http://emacsredux.com/blog/2016/03/02/pimp-my-minibuffer/
	  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

	  ;; Use gtags to show documentation
	  (add-hook 'c-mode-common-hook
		    (lambda ()
		      (when (and (executable-find "global")
				 (projectile-project-p)
				 (file-exists-p (concat (projectile-project-root)
							"GTAGS")))
			(setq-local eldoc-documentation-function #'ggtags-eldoc-function))))))

(use-package inline-docs
  :defer t
  :load-path (lambda () (expand-file-name "inline-docs/" user-emacs-directory))
  :defines inline-docs-overlay)

(use-package eldoc-overlay-mode
  :defer t
  :commands eldoc-overlay-mode
  :diminish eldoc-overlay-mode
  :load-path (lambda () (expand-file-name "eldoc-overlay-mode/" user-emacs-directory)))

(provide 'setup-eldoc)
;;; setup-eldoc.el ends here

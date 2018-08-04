;;; setup-eldoc.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Abelardo Jara-Berrocal

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

(use-package eldoc
  :demand t
  :diminish eldoc-mode
  :hook (((prog-mode eval-expression-minibuffer-setup-hook) . eldoc-mode)
         ((emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook) . turn-on-eldoc-mode))
  :custom ((eldoc-idle-delay                0.8)
           (eldoc-echo-area-use-multiline-p t))
  :init (progn

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

(use-package quick-peek
  :defer t
  :load-path (lambda () (expand-file-name "quick-peek/" user-emacs-directory)))

(use-package eldoc-overlay-mode
  :defer t
  :after (eldoc quick-peek)
  :commands eldoc-overlay-mode
  :diminish eldoc-overlay-mode
  :load-path (lambda () (expand-file-name "eldoc-overlay-mode/" user-emacs-directory)))

(provide 'setup-eldoc)
;;; setup-eldoc.el ends here

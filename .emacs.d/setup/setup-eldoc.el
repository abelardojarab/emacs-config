;;; setup-eldoc.el ---                               -*- lexical-binding: t; -*-

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

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :commands (eldoc-mode
             turn-on-eldoc-mode
             my/eldoc-c-mode-init)
  :hook (((prog-mode c-mode c++-mode)                       . eldoc-mode)
         ((emacs-lisp-mode lisp-interaction-mode ielm-mode) . turn-on-eldoc-mode)
         (c-mode-common                                     . my/eldoc-c-mode-init))
  :custom ((eldoc-idle-delay                0.8)
           (eldoc-echo-area-use-multiline-p t))
  :preface (defun my/eldoc-c-mode-init ()
             (when (and (executable-find "global")
                        (projectile-project-p)
                        (file-exists-p (concat (projectile-project-root)
                                               "GTAGS")))
               (setq-local eldoc-documentation-function #'ggtags-eldoc-function))))

(use-package inline-docs
  :defer t
  :defines inline-docs-overlay)

(use-package quick-peek
  :demand t)

(use-package eldoc-overlay-mode
  :defer t
  :after (eldoc quick-peek)
  :commands eldoc-overlay-mode
  :diminish eldoc-overlay-mode)

(provide 'setup-eldoc)
;;; setup-eldoc.el ends here

;;; setup-hydra.el ---                               -*- lexical-binding: t; -*-

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

;; hydra package
(use-package hydra
  :demand t
  :custom (hydra-lv nil)
  :config (progn
            (setq hydra-hint-display-type 'my/posframe)
            (defun my/hydra-posframe-show (str)
              (posframe-show
               " *hydra-posframe*"
               :string str
               :point (point)
               :internal-border-color "gray50"
               :internal-border-width 2
               :poshandler #'posframe-poshandler-frame-bottom-center))
            (defun my/hydra-posframe-hide ()
              (posframe-hide " *hydra-posframe*"))
            (setq hydra-hint-display-alist
                  (list (list 'my/posframe #'my/hydra-posframe-show #'my/hydra-posframe-hide))
                  hydra--work-around-dedicated nil)))

;; hydra replacement using which-key
(use-package hercules
  :demand t
  :after (hydra which-key)
  :load-path (lambda () (expand-file-name "hercules/" user-emacs-directory)))

(provide 'setup-hydra)
;;; setup-hydra.el ends here

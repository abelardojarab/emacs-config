;;; setup-hydra.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016  Abelardo Jara-Berrocal

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

(use-package hydra
  :load-path (lambda () (expand-file-name "hydra/" user-emacs-directory))
  :config (progn
            (defhydra hydra-toggle-map nil
              "
^Toggle^
^^^^^^^^--------------------
_d_: debug-on-error
_D_: debug-on-quit
_f_: auto-fill-mode
_l_: toggle-truncate-lines
_h_: hl-line-mode
_r_: read-only-mode
_q_: quit
"
              ("d" toggle-debug-on-error :exit t)
              ("D" toggle-debug-on-quit :exit t)
              ("f" auto-fill-mode :exit t)
              ("l" toggle-truncate-lines :exit t)
              ("r" read-only-mode :exit t)
              ("h" hl-line-mode :exit t)
              ("q" nil :exit t))

            (global-set-key (kbd "C-x t") 'hydra-toggle-map/body))
  )

(provide 'setup-hydra)
;;; setup-hydra.el ends here

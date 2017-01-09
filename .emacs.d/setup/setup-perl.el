;;; setup-perl.el ---                                -*- lexical-binding: t; -*-

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

(use-package perl-mode
  :mode ("\\.pl\\'" . perl-mode)
  :init (defalias 'perl-mode 'cperl-mode)
  :config (progn
            ;; (use-package perl-find-library)
            (setq cperl-invalid-face nil
                  cperl-close-paren-offset -4
                  cperl-continued-statement-offset 0
                  cperl-indent-level 4
                  cperl-indent-parens-as-block t)))

(provide 'setup-perl)
;;; setup-perl.el ends here

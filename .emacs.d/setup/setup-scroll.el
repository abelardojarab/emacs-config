;;; setup-scroll.el ---                              -*- lexical-binding: t; -*-

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

;; Enable smooth scrolling package
;; Be careful it can ruin shift-select-mode
(use-package smooth-scrolling
  :load-path (lambda () (expand-file-name "smooth-scrolling/" user-emacs-directory))
  :config (progn
            (setq scroll-margin 7
                  scroll-step 1
                  scroll-conservatively 10000
                  scroll-preserve-screen-position 'always
                  scroll-up-aggressively 0.01
                  scroll-down-aggressively 0.01)

            (smooth-scrolling-mode 1)))

(provide 'setup-scroll)
;;; setup-scroll.el ends here

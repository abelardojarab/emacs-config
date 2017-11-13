;;; setup-themes.el ---                              -*- lexical-binding: t; -*-

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

;; Font-core library
(use-package font-core)

;; Choose different themes depending if we are using GUI or not
;; Console colors are enabled if "export TERM=xterm-256color" is added into .bashrc
(if (display-graphic-p)
    (load-theme my/emacs-theme)
  (load-theme my/emacs-theme-console))

;; Remember last theme
(use-package remember-last-theme
  :if (display-graphic-p)
  :load-path (lambda () (expand-file-name "remember-last-theme/" user-emacs-directory))
  :config (remember-last-theme-enable))

(provide 'setup-themes)
;;; setup-themes.el ends here

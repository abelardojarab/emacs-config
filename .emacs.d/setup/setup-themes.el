;;; setup-themes.el ---                              -*- lexical-binding: t; -*-

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

;; Let the themes deal with these things
(dolist (param '(background-mode tty-color-mode screen-gamma
                                 alpha font foreground-color background-color mouse-color
                                 cursor-color border-color scroll-bar-foreground
                                 scroll-bar-background))
  (add-to-list 'frameset-filter-alist `(,param . :never)))

;; Choose different themes depending if we are using GUI or not
;; Console colors are enabled if "export TERM=xterm-256color" is added into .bashrc
(add-hook 'after-init-hook
          (lambda ()
            (if (display-graphic-p)
                (load-theme my/emacs-theme t)
              (load-theme my/emacs-theme-console t))))

;; Remember last theme
(use-package remember-last-theme
  :disabled t
  :if (display-graphic-p)
  :config (remember-last-theme-enable))

;; (defadvice custom-theme-recalc-variable (around bar activate)
;;   (ignore-errors add-do-it))

;; (defadvice enable-theme (around bar activate)
;;   (ignore-errors add-do-it))

(provide 'setup-themes)
;;; setup-themes.el ends here

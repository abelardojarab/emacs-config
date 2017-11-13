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

;; Add themes
(dolist (my/theme
         (list
          "ample"
          "apropospriate"
          "atom-one-dark"
          "borland-blue"
          "creamsody"
          "danneskjold"
          "darcula"
          "darkokai"
          "darktooth"
          "dracula"
          "eink"
          "exotica"
          "eziam"
          "faff"
          "flatui"
          "goose"
          "grandshell"
          "gruber-darker"
          "intellij"
          "kaolin"
          "leuven"
          "madhat2r"
          "material"
          "minimal"
          "moe"
          "monochrome"
          "monokai"
          "monotropic"
          "plan9"
          "professional"
          "spacemacs"
          "spike"
          "tao"
          "white-sand"
          "zenburn"
          "zerodark"))
  (progn
    (add-to-list 'load-path (expand-file-name
                             (concat "themes/"
                                     my/theme
                                     "-theme/")
                             user-emacs-directory))
    (add-to-list 'custom-theme-load-path (expand-file-name
                                          (concat "themes/"
                                                  my/theme
                                                  "-theme/")
                                          user-emacs-directory))))

;; Override zenburn colors
(defvar zenburn-override-colors-alist
  '(("zenburn-bg-2"  . "#000000")
    ("zenburn-bg-1"  . "#101010")
    ("zenburn-bg-05" . "#282828")
    ("zenburn-bg"    . "#2F2F2F")
    ("zenburn-bg+05" . "#383838")
    ("zenburn-bg+1"  . "#3F3F3F")
    ("zenburn-bg+2"  . "#4F4F4F")
    ("zenburn-bg+3"  . "#5F5F5F")))

;; Font-core library
(use-package font-core)

;; Tomorrow theme
(add-to-list 'load-path (expand-file-name "themes/sanityinc-tomorrow-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes/sanityinc-tomorrow-theme/" user-emacs-directory))
(use-package color-theme-sanityinc-tomorrow)

;; Solarized theme
(add-to-list 'load-path (expand-file-name "themes/solarized-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes/solarized-theme" user-emacs-directory))
(use-package solarized
  :config (setq solarized-scale-org-headlines nil))

;; Choose different themes depending if we are using GUI or not
;; Console colors are enabled if "export TERM=xterm-256color" is added into .bashrc
(load-theme my/emacs-theme t)

;; Remember last theme
(use-package remember-last-theme
  :if (display-graphic-p)
  :load-path (lambda () (expand-file-name "remember-last-theme/" user-emacs-directory))
  :config (remember-last-theme-enable))

(provide 'setup-themes)
;;; setup-themes.el ends here

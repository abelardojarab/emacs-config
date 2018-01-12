;;; setup-ergoemacs.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017  Abelardo Jara-Berrocal

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

;; Ergoemacs
(use-package ergoemacs-mode-fixed
  :defer t
  :if (and (executable-find "gzip")
           (display-graphic-p))
  :load-path (lambda () (expand-file-name "ergoemacs-mode/" user-emacs-directory))
  :commands (ergoemacs-mode ergoemacs-mode-after-init-emacs)
  :config (progn
            (ergoemacs-mode-after-init-emacs)
            (setq ergoemacs-theme              "standard"
                  ergoemacs-keyboard-layout    "us"
                  ergoemacs-ignore-prev-global nil)
            (ergoemacs-mode t)))

(provide 'setup-ergoemacs)
;;; setup-ergoemacs.el ends here

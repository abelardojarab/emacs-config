;;; setup-ergoemacs.el ---                           -*- lexical-binding: t; -*-

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

;; Ergoemacs
(use-package ergoemacs-mode-fixed
  :disabled t
  :if (and (executable-find "gzip")
           (display-graphic-p))
  :commands (ergoemacs-mode
             ergoemacs-mode-after-init-emacs)
  :init (progn
          (setq ergoemacs-command-loop--modal-stack nil)
          (use-package ergoemacs-command-loop
            :demand t)
          (defun ergoemacs-command-loop--spinner-display (&optional string &rest args)
            t)
          (defun ergoemacs-command-loop--modal-p ()
            nil))
  :custom ((ergoemacs-mode               "standard")
           (ergoemacs-keyboard-layout    "us")
           (ergoemacs-ignore-prev-global nil))
  :config (progn
            (defadvice ergoemacs-map--lookup-map (around bar activate)
              (ignore-errors add-do-it))

            (ergoemacs-mode-after-init-emacs)
            (ergoemacs-mode t)))

(provide 'setup-ergoemacs)
;;; setup-ergoemacs.el ends here

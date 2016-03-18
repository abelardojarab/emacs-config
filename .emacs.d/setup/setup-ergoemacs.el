;;; setup-ergoemacs.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxcj9063.pdx.intel.com>
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
(use-package ergoemacs-mode
  ;; Ergoemacs does not work with Emacs 25
  :if (= emacs-major-version 24)
  :ensure nil
  :load-path (lambda () (expand-file-name "ergoemacs-mode/" user-emacs-directory))
  :init (progn
          (setq ergoemacs-theme nil)
          (setq ergoemacs-keyboard-layout "us")
          (setq ergoemacs-ignore-prev-global nil))
  :config (ergoemacs-mode))

(provide 'setup-ergoemacs)
;;; setup-ergoemacs.el ends here

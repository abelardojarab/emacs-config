;;; setup-cshandbash.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2015  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc26391.pdx.intel.com>
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

(defun my/tcsh-set-indent-functions ()
  (when (or (string-match ".*\\.alias" (buffer-file-name))
            (string-match ".*csh$" (file-name-extension (buffer-file-name))))
    (require 'csh-mode) ;; https://github.com/Tux/tcsh/blob/master/csh-mode.el
    (setq-local indent-line-function 'csh-indent-line)
    (setq-local indent-region-function 'csh-indent-region)))
(add-hook 'sh-mode-hook #'my/tcsh-set-indent-functions)
(add-hook 'sh-mode-hook (lambda () (electric-mode -1)))

(provide 'setup-cshandbash)
;;; setup-cshandbash.el ends here

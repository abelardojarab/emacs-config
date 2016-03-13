;;; setup-imenu.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara

;; Author: Abelardo Jara <abelardojara@Abelardos-MacBook-Pro.local>
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

;; iMenu
(set-default 'imenu-auto-rescan t)
(mapc (lambda (mode)
        (add-hook mode 'imenu-add-menubar-index))
      '(prog-mode-hook
        reftex-mode-hook
        reftex-load-hook
        org-mode-hook))

;; iMenus
(add-to-list 'load-path "~/.emacs.d/imenus")
(autoload 'imenus "imenus" nil t)
(autoload 'imenus-mode-buffers "imenus" nil t)

(provide 'setup-imenu)
;;; setup-imenu.el ends here

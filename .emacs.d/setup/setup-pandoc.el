;;; setup-pandoc.el ---                              -*- lexical-binding: t; -*-

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

;; Pandoc
(use-package pandoc-mode
  :load-path (lambda () (expand-file-name "pandoc-mode/" user-emacs-directory))
  :config (progn
            (add-hook 'markdown-mode-hook 'pandoc-load-default-settings)
            (add-hook 'org-mode-hook 'pandoc-load-default-settings)
            (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)))

(provide 'setup-pandoc)
;;; setup-pandoc.el ends here

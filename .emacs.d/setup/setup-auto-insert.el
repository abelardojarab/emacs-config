;;; setup-auto-insert.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@gmail.com>
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

;; Autoinsert skeletons and templates
(use-package autoinsert
  :defer t
  :commands (auto-insert-mode
             auto-insert)
  :hook (((prog-mode markdown-mode org-mode) . auto-insert-mode)
         (find-file                          . auto-insert))
  :custom (auto-insert-query nil))

;; Automated auto-insert of yasnippet templates on new files
(use-package yatemplate
  :defer t
  :commands yatemplate-fill-alist
  :config (yatemplate-fill-alist))

(provide 'setup-auto-insert)
;;; setup-auto-insert.el ends here

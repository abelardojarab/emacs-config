;;; setup-tabkey.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@ubuntu-MacBookPro>
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

;; Smart tab
(use-package smart-tab
  :diminish smart-tab-mode
  :load-path (lambda () (expand-file-name "smart-tab/" user-emacs-directory))
  :config (progn
            (cons 'yas-hippie-try-expand 'hippie-expand-try-functions-list)
            (setq smart-tab-using-hippie-expand t)
            (global-smart-tab-mode)))

(provide 'setup-tabkey)
;;; setup-tabkey.el ends here

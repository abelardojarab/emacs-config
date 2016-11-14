;;; setup-post.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc20122.pdx.intel.com>
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

;; Add all diminished modes here

;; Dashboard
(use-package dashboard
  :load-path (lambda () (expand-file-name "dashboard/" user-emacs-directory))
  :config (dashboard-setup-startup-hook))

(provide 'setup-post)
;;; setup-post.el ends here

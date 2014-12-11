;;; setup-csharp.el ---

;; Copyright (C) 2014

;; Author:  <ajaraber@AJARABER-MOBL5>
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

(add-to-list 'load-path "~/.emacs.d/csharp-mode")
(add-to-list 'load-path "~/.emacs.d/omnisharp-emacs")
(add-to-list 'load-path "~/.emacs.d/omnisharp-emacs/src")
(add-to-list 'load-path "~/.emacs.d/omnisharp-emacs/src/actions")

(when (require 'csharp-mode nil 'noerror)
  (require 'omnisharp)
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(provide 'setup-csharp)
;;; setup-csharp.el ends here

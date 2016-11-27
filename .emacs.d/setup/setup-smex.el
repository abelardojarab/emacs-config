;;; setup-smex.el ---

;; Copyright (C) 2016  Abelardo Jara-Berrocal

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

;; Better Alt-x

(use-package smex
  :commands (smex smex-major-mode-commands)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :load-path (lambda ()
               (expand-file-name
                (if (and (= emacs-major-version 24) (= emacs-minor-version 2))
                    "smex2/"
                  "smex/") user-emacs-directory))
  :config (smex-initialize))

(provide 'setup-smex)
;;; setup-modeline.el ends here

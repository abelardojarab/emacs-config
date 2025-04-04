;;; setup-dabbrev.el ---                             -*- lexical-binding: t; -*-

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

;; Abbrevs
(use-package abbrev
  :defer t
  :commands (abbrev-mode
             write-abbrev-file
             quietly-read-abbrev-file)
  :diminish (abbrev-mode . " Ⓐ")
  :hook (kill-emacs . write-abbrev-file)
  :init (progn
          (setq abbrev-file-name (concat (file-name-as-directory
                                          my/emacs-cache-dir)
                                         "abbrev_defs"))
          (if (file-exists-p abbrev-file-name)
              (quietly-read-abbrev-file))

          ;; Activate template autocompletion
          (dolist (mode my/abbrev-modes)
            (add-hook mode (lambda () (abbrev-mode 1)))))
  :custom (save-abbrevs 'silently))

(provide 'setup-dabbrev)
;;; setup-dabbrev.el ends here

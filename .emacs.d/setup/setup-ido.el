;;; setup-ido.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc25288.pdx.intel.com>
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

;; ido makes competing buffers and finding files easier

(require 'ido)
(ido-mode 'both)
(ido-everywhere 1)

(setq
 ido-max-dir-file-cache 0
 ido-show-dot-for-dired t
 ido-default-file-method 'samewindow
 ido-default-buffer-method 'samewindow
 ido-save-directory-list-file "~/.emacs.cache/ido.last"
 ido-ignore-buffers ;; ignore these guys
 '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
   "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
 ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~/workspace")
 ido-case-fold t
 ido-enable-last-directory-history t
 ido-auto-merge-work-directories-length -1
 ido-max-work-directory-list 15
 ido-max-work-file-list 10
 ido-use-filename-at-point nil
 ido-use-url-at-point nil
 ido-enable-flex-matching t
 ido-max-prospects 8
 ido-confirm-unique-completion t)

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

(provide 'setup-ido)
;;; setup-ido.el ends here

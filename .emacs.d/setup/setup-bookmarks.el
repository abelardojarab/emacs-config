;;; setup-bookmarks.el ---

;; Copyright (C) 2014  abelardo.jara-berrocal

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

;; Bookmark Plus
(add-to-list 'load-path "~/.emacs.d/bookmark-plus")
(require 'bookmark+)

;; Bookmarks
(add-to-list 'load-path "~/.emacs.d/bm")
(require 'bm)
(setq bm-highlight-style 'bm-highlight-line-and-fringe)
(setq bm-restore-repository-on-load t)
(setq bookmark-save-flag 1)

;; make bookmarks persistent as default
(setq-default bm-buffer-persistence t)

;; Loading the repository from file when on start up.
(add-hook 'after-init-hook 'bm-repository-load)

;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)

;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)

;; Saving the repository to file when on exit.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))

;; Better bookmark font
(set-face-attribute 'bm-face nil :underline t :foreground nil :background nil)

(provide 'setup-bookmarks)
;;; setup-bookmarks.el ends here

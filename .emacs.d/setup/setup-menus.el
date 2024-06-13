;;; setup-menus.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
;; Keywords: abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'setup-menu-macros)
(require 'setup-menu-styles)

;;; Reconfigure Text Mode Menu

(easy-menu-remove-item text-mode-menu nil "Center Line")
(easy-menu-remove-item text-mode-menu nil "Center Region")
(easy-menu-remove-item text-mode-menu nil "Center Paragraph")
(easy-menu-remove-item text-mode-menu nil "Paragraph Indent")
(easy-menu-remove-item text-mode-menu nil "---")

(easy-menu-add-item text-mode-menu nil my/transform-text-menu "Auto Fill")
(easy-menu-add-item text-mode-menu nil my/emphasize-menu "Auto Fill")

;;; Reconfigure Edit Menu

(easy-menu-add-item (lookup-key global-map [menu-bar edit]) nil
                    my/transpose-menu "Fill")

(easy-menu-add-item (lookup-key global-map [menu-bar edit]) nil
                    my/move-text-menu "Fill")

(easy-menu-add-item (lookup-key global-map [menu-bar edit]) nil
                    my/delete-space-menu "Fill")

(easy-menu-add-item global-map '(menu-bar edit)
                    ["Flush Lines…"
                     flush-lines
                     :help "Delete lines containing matches for REGEXP."
                     :visible (not buffer-read-only)]
                    "Fill")

(easy-menu-add-item global-map '(menu-bar edit)
                    ["Keep Lines…"
                     keep-lines
                     :help "Delete all lines except those containing matches \
for REGEXP."
                     :visible (not buffer-read-only)]
                    "Fill")

;;; Reconfigure Tools Menu

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Agenda - All TODOs"
                     (lambda () (interactive)(org-agenda nil "n"))
                     :help "Show Org agenda with all TODO tasks."]
                    "Shell Commands")

(keymap-set-after (lookup-key global-map [menu-bar tools])
				  "<separator-org>"
				  '(menu-item "--")
				  'Agenda\ -\ All\ TODOs)

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Find File…"
                     helm-find-files
                     :help "Fuzzy find file."]
                    "Shell Commands")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Find in Files (rgrep)…"
                     rgrep
                     :help "Recursively grep for REGEXP in FILES in directory \
tree rooted at DIR."]
                    "Shell Commands")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["IELM"
                     ielm
                     :help "Interactively evaluate Emacs Lisp expressions."]
                    "Language Server Support (Eglot)")

(keymap-set-after (lookup-key global-map [menu-bar tools])
				  "<separator-shell>"
				  '(menu-item "--")
				  'Search\ Org\ Notes…)

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Magit Status"
                     magit-status
                     :visible (vc-responsible-backend default-directory t)
                     :help "Show the status of the current Git repository \
in a buffer"]
                    "Version Control")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Count Words"
                     count-words
                     :help "Count words in buffer or region if active."]
                    "Calendar")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Eshell"
                     eshell
                     :help "Create an interactive Eshell buffer."]
                    "Calendar")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Python Shell"
                     run-python
                     :help "Run an inferior Python process."]
                    "Calendar")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["RE-Builder"
                     re-builder
                     :help "Construct a regexp interactively."]
                    "Calendar")

(keymap-set-after (lookup-key global-map [menu-bar tools])
				  "<separator-re>"
				  '(menu-item "--")
				  'RE-Builder)

(easy-menu-add-item global-map '(menu-bar tools)
                    ["World Clock"
                     world-clock
                     :help "Display a world clock buffer with times in \
various time zones."]
                    "Programmable Calculator")

(provide 'setup-menus)
;;; setup-menus.el ends here

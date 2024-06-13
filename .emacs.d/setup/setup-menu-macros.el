;;; setup-menu-macros.el ---                         -*- lexical-binding: t; -*-

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
(require 'ol)

(defmacro my/context-menu-item-separator (menu key)
  "Add single line separator to MENU with KEY."
  `(define-key-after ,menu [,key]
     '(menu-item "--single-line")))

(defmacro my/add-context-menu-item (menu command label help)
  "Add COMMAND to MENU annotated with LABEL and property HELP."
  `(define-key-after ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help)))

(defmacro my/add-context-menu-item-visible (menu command label help visible)
  "Add COMMAND to MENU annotated with LABEL and properties HELP, VISIBLE."
  `(define-key-after ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help
                 :visible ,visible)))

(defmacro my/add-context-menu-item-enable (menu command label help enable)
  "Add COMMAND to MENU annotated with LABEL and properties HELP, ENABLE."
  `(define-key-after ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help
                 :enable ,enable)))

(defmacro my/add-first-context-menu-item (menu command label help)
  "Add first COMMAND to MENU annotated with LABEL and HELP."
  `(define-key ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help)))

(defmacro my/add-context-menu-submenu (menu submenu label)
  "Add SUBMENU to MENU annotated with LABEL.
SUBMENU is a keymap."
  `(define-key-after ,menu [,submenu]
     (list 'menu-item ,label ,submenu)))

(defun my/context-menu-label (prefix)
  "Generate context menu label with region string prepended by PREFIX."
  (let* ((start (region-beginning))
         (end (region-end))
         (buf "")
         (max 25)
         (size (abs (- start end))))
    (if (> size max)
        (setq buf (concat prefix " “"(buffer-substring start (+ max start)) "…”"))
      (setq buf (concat prefix " “" (buffer-substring start end) "”")))
    buf))

(defun my/context-menu-last-word-in-region (prefix)
  "Generate context menu label with last word in region prepended by PREFIX."
  (let*  ((start (region-beginning))
          (end (region-end))
          (buf (buffer-substring start end))
          (last-word (car (last (split-string buf " ")))))
    (concat prefix " “" last-word "”")))

(defun my/org-stored-links-p ()
  "Predicate if `org-stored-links' is populated.
Return t if populated, nil otherwise."
  (if (> (length org-stored-links) 0)
      t
    nil))

(easy-menu-define my/transform-text-menu nil
  "Keymap for Transform Text submenu."
  '("Transform Text"
    :visible (region-active-p)
    ["Make Upper Case" upcase-region
     :enable (region-active-p)
     :help "Convert selected region to upper case"]
    ["Make Lower Case" downcase-region
     :enable (region-active-p)
     :help "Convert selected region to lower case"]
    ["Capitalize" capitalize-region
     :enable (region-active-p)
     :help "Convert the selected region to capitalized form"]))

(easy-menu-define my/transpose-menu nil
  "Keymap for Transpose submenu"
  '("Transpose"
    :visible (not buffer-read-only)
    ["Characters" transpose-chars
     :help "Interchange characters around point, moving forward one character."]

    ["Words" transpose-words
     :help "Interchange words around point, leaving point at end of them."]

    ["Lines" transpose-lines
     :help "Exchange current line and previous line, leaving point after both."]

    ["Sentences" transpose-sentences
     :help "Interchange the current sentence with the next one."]

    ["Paragraphs" transpose-paragraphs
     :help "Interchange the current paragraph with the next one."]

    ["Regions" transpose-regions
     :help "region STARTR1 to ENDR1 with STARTR2 to ENDR2."]

    ["Balanced Expressions (sexps)" transpose-sexps
     :help "Like C-t (‘transpose-chars’), but applies to balanced \
expressions (sexps)."]))

(easy-menu-define my/move-text-menu nil
  "Keymap for Move Text submenu"
  '("Move Text"
    :visible (not buffer-read-only)
    ["Word Forward" my/move-word-forward
     :help "Move word to the right of point forward one word."]

    ["Word Backward" my/move-word-backward
     :help "Move word to the right of point backward one word."]

    ["Sentence Forward" my/move-sentence-forward
     :help "Move sentence to the right of point forward one sentence."]

    ["Sentence Backward" my/move-sentence-backward
     :help "Move sentence to the right of point backward one sentence."]

    ["Balanced Expression (sexp) Forward" my/move-sexp-forward
     :help "Move balanced expression (sexp) to the right of point forward \
one sexp."]

    ["Balanced Expression (sexp) Backward" my/move-sexp-backward
     :help "Move balanced expression (sexp) to the right of point backward \
one sexp."]))

(easy-menu-define my/delete-space-menu nil
  "Keymap for Deleting Space submenu"
  '("Delete Space"
    :visible (not buffer-read-only)
    ["Join Line" join-line
     :help "Join this line to previous and fix up \
whitespace at join"]

    ["Just One Space" just-one-space
     :help "Delete all spaces and tabs around point, leaving \
one space."]

    ["Delete Horizontal Space" delete-horizontal-space
     :help "Delete all spaces and tabs around point."]

    ["Delete Blank Lines" delete-blank-lines
     :help "On blank line, delete all surrounding blank lines, \
leaving just one."]

    ["Whitespace Cleanup" whitespace-cleanup
     :help "Cleanup some blank problems in all buffer or at region."]

    ["Delete Trailing Whitespace" delete-trailing-whitespace
     :help "Delete trailing whitespace between START and END."]))

(provide 'setup-menu-macros)
;;; setup-menu-macros.el ends here

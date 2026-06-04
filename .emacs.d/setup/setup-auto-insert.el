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
  :custom (auto-insert-query nil)
  :config
  ;; Make skeleton prompts exitable under ido.
  ;;
  ;; Several built-in skeletons (e.g. the "Emacs Lisp header") gather a list in
  ;; a loop you leave by submitting EMPTY input -- the classic case is its
  ;; keyword prompt `(completing-read "Keyword, C-h: " ... nil t)'.  The default
  ;; `completing-read' returns "" on empty input *regardless of require-match*,
  ;; which ends the loop.  But `setup-ido' turns on `ido-everywhere' /
  ;; `ido-ubiquitous', routing every `completing-read' through ido, where RET
  ;; selects the highlighted candidate instead of submitting empty text -- so
  ;; the loop can never terminate (only C-j, or aborting with C-g, got you out).
  ;;
  ;; Fix it at the source for ALL auto-insert skeletons (not just elisp): run
  ;; their prompts through the default `completing-read', so an empty RET always
  ;; ends the loop.  ido stays active everywhere else.
  (advice-add 'auto-insert :around
              (lambda (orig &rest args)
                (let ((completing-read-function #'completing-read-default))
                  (apply orig args)))
              '((name . my/auto-insert-plain-completing-read))))

;; Automated auto-insert of yasnippet templates on new files
(use-package yatemplate
  :defer t
  :commands yatemplate-fill-alist
  :config (yatemplate-fill-alist))

(provide 'setup-auto-insert)
;;; setup-auto-insert.el ends here

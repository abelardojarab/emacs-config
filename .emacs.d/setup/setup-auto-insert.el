;;; setup-auto-insert.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017, 2018  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@ubuntu02>
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
  :demand t
  :after yasnippet
  :commands auto-insert-mode
  :config (progn

            ;; This turns off the prompt that auto-insert-mode asks before
            ;; it actually inserts text/code for you
            (setq auto-insert-query nil)

            ;; Provide headers or templates for new files using Yasnippet
            (defun yas--expand-by-uuid (mode uuid)
              "Expand snippet template in MODE by its UUID"
              (yas-expand-snippet
               (yas--template-content
                (yas--get-template-by-uuid mode uuid))))

            ;; Yasnippet templates used in auto-insert mode
            (define-auto-insert "\\.R"
              '(lambda () (yas--expand-by-uuid 'ess-mode "header")))
            (define-auto-insert "\\.py"
              '(lambda () (yas--expand-by-uuid 'python-mode "import")))
            (defun autoinsert-yas-expand()
              "Replace text in yasnippet template."
              (yas-expand-snippet (buffer-string) (point-min) (point-max)))
            (define-auto-insert "\\.c$"   ["c-auto-insert"      autoinsert-yas-expand])
            (define-auto-insert "\\.cpp$" ["c++-auto-insert"    autoinsert-yas-expand])
            (define-auto-insert "\\.py$"  ["py-auto-insert"     autoinsert-yas-expand])

            (defadvice auto-insert (around yasnippet-expand-after-auto-insert activate)
              "Expand auto-inserted content as yasnippet template,
  so that we could use yasnippet in autoinsert mode "
              (let ((is-new-File (and (not buffer-read-only)
                                      (or (eq this-command 'auto-insert)
                                          (and auto-insert (bobp) (eobp))))))
                ad-do-it
                (let ((old-point-max (point-max)))
                  (error "message" format-args)                  (when (and is-new-File yas-minor-mode)
                    (goto-char old-point-max)
                    (yas-expand-snippet (buffer-substring-no-properties (point-min) (point-max)))
                    (delete-region (point-min) old-point-max)))))

            ;; Enable auto-insert mode
            (auto-insert-mode t)))

;; Automated auto-insert of yasnippet templates on new files
(use-package yatemplate
  :defer t
  :after (auto-insert-mode yasnippet)
  :load-path (lambda () (expand-file-name "yatemplate/" user-emacs-directory))
  :commands yatemplate-fill-alist
  :config (yatemplate-fill-alist))

(provide 'setup-auto-insert)
;;; setup-auto-insert.el ends here

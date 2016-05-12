;;; setup-auto-insert.el ---

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

;; Autoinsert skeletons and templates
(use-package autoinsert
  :config (progn
            (auto-insert-mode t)

            ;; This turns off the prompt that auto-insert-mode asks before
            ;; it actually inserts text/code for you
            (setq auto-insert-query nil)

            ;; Provide headers or templates for new files using Yasnippet
            (defun yas--expand-by-uuid (mode uuid)
              "Expand snippet template in MODE by its UUID"
              (yas--expand-snippet
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
            (define-auto-insert "\\.c$"  ["c-auto-insert" autoinsert-yas-expand])
            (define-auto-insert "\\.cpp$" ["c++-auto-insert" autoinsert-yas-expand])
            (define-auto-insert "\\.cs$" ["csharp-auto-insert" autoinsert-yas-expand])
            (define-auto-insert "\\.py$" ["py-auto-insert" autoinsert-yas-expand])

            (defadvice auto-insert (around yasnippet-expand-after-auto-insert activate)
              "Expand Content Auto-inserted as yasnippet Template,
  so That WE could use yasnippet in autoinsert mode "
              (let ((is-new-File (and (not buffer-read-only)
                                      (or (eq this-command 'auto-insert)
                                          (and auto-insert (bobp) (eobp))))))
                ad-do-it
                (let ((old-point-max (point-max)))
                  (when is-new-File
                    (goto-char old-point-max)
                    (yas-expand-snippet (buffer-substring-no-properties (point-min) (point-max)))
                    (delete-region (point-min) old-point-max)))))))

;; Automated auto-insert of Yasnippet templates on new files
(use-package yatemplate
  :defer 2 ;; WORKAROUND https://github.com/mineo/yatemplate/issues/3
  :load-path (lambda () (expand-file-name "yatemplate/" user-emacs-directory))
  :config (progn (yatemplate-fill-alist)))

(provide 'setup-auto-insert)
;;; setup-auto-insert.el ends here

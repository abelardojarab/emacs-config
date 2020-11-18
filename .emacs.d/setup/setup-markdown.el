;;; setup-markdown.el ---                               -*- lexical-binding: t; -*-

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

;; Markdown
(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :after org
  :commands (markdown-mode
             my/markdown-mode-init)
  :hook (markdown-mode . my/markdown-mode-init)
  :custom ((markdown-asymmetric-header         t)
           (markdown-header-scaling            t)
           (markdown-enable-wiki-links         t)
           (markdown-list-indent-width         2)
           (markdown-enable-wiki-links         t)
           (markdown-footnote-location         'immediately)
           (markdown-wiki-link-fontify-missing t)
           (markdown-wiki-link-alias-first     nil)
           (markdown-indent-on-enter           'indent-and-new-item))
  :preface (defun my/set-markdown-header-font-sizes ()
             (dolist (face '((markdown-header-face-1 . 1.2)
                             (markdown-header-face-2 . 1.1)
                             (markdown-header-face-3 . 1.0)
                             (markdown-header-face-4 . 1.0)
                             (markdown-header-face-5 . 1.0)))
               (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))
  :config (progn
            ;; This function is buggy and deletes text
            (defun markdown-table-align ()
              nil)

            (use-package flycheck-mmark
              :disabled t
              :if (executable-find "mmark")
              :hook (flycheck-mode . flycheck-mmark-setup)
              :commands (flycheck-mmark-setup))

            ;; Markdown preferences
            (defun my/markdown-mode-init ()
              (my/set-markdown-header-font-sizes)

              ;; Do not wrap lines
              ;; we will use autofill
              (visual-line-mode          -1)
              (abbrev-mode               -1)
              (adaptive-wrap-prefix-mode t)

              ;; Org goodies
              ;; (orgtbl-mode               t)
              (orgstruct++-mode          t)

              ;; Extra modes
              (outline-minor-mode        t)
              (footnote-mode             t)
              (auto-fill-mode            t)

              ;; Style/syntax check
              (writegood-mode            t)
              (flyspell-mode             t)

              (toggle-truncate-lines     t)
              (setq truncate-lines       t)
              (if (or (executable-find "proselint")
                      (executable-find "mmark"))
                  (flycheck-mode t))

              (make-local-variable 'ispell-skip-region-alist)
              (add-to-list 'ispell-skip-region-alist '("~" "~"))
              (add-to-list 'ispell-skip-region-alist '("```" "```"))
              (setq orgstruct-heading-prefix-regexp "#\\+")
              (pandoc-mode 1))

            (defun my/cleanup-org-tables ()
              (save-excursion
                (goto-char (point-min))
                (while (search-forward "-+-" nil t) (replace-match "-|-")))
              (save-buffer)
              (set-buffer-modified-p nil))

            ;; no longer needed, markdown includes supports for tables now
            (add-hook 'markdown-mode-hook
                      (lambda () (add-hook 'after-save-hook 'my/cleanup-org-tables nil t)))

            ;; Github markdown style
            (setq markdown-css-paths `(,(expand-file-name "styles/github-pandoc.css" user-emacs-directory)))
            (setq markdown-command
                  (concat "pandoc -c "
                          (concat user-emacs-directory
                                  "/styles/panam-pandoc.css")
                          " --from gfm -t html5 --mathjax --highlight-style pygments --standalone"))))

(provide 'setup-markdown)
;;; setup-markdown.el ends here

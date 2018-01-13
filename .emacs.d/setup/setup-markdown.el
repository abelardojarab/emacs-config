;;; setup-markdown.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017, 2018  Abelardo Jara-Berrocal

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

;; Polymode; syntax highlighting inside markdown
(use-package polymode
  :defer t
  :after org
  :diminish polymode-minor-mode
  :commands polymode-minor-mode
  :load-path (lambda () (expand-file-name "polymode/" user-emacs-directory))
  :init (add-to-list 'load-path (expand-file-name "polymode/modes" user-emacs-directory))
  :config (progn
            (use-package poly-R)
            (use-package poly-markdown)
            (setq pm-weaver   "knitR-ESS" ;; Default weaver
                  pm-exporter "pandoc")))

;; Markdown
(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :after org
  :commands markdown-mode
  :load-path (lambda () (expand-file-name "markdown-mode-2.0/" user-emacs-directory))
  :config (progn

            ;; http://endlessparentheses.com/ispell-and-org-mode.html
            (defun my/markdown-ispell ()
              "Configure `ispell-skip-region-alist' for `markdown-mode'."
              (make-local-variable 'ispell-skip-region-alist)
              (add-to-list 'ispell-skip-region-alist '("~" "~"))
              (add-to-list 'ispell-skip-region-alist '("```" "```")))
            (add-hook 'markdown-mode-hook #'my/markdown-ispell)

            ;; Markdown preferences
            (add-hook 'markdown-mode-hook
                      (lambda ()
                        ;; Do not wrap lines
                        ;; we will use autofill
                        (visual-line-mode -1)
                        (toggle-truncate-lines t)
                        (setq truncate-lines t)

                        ;; Enable polymode minor mode
                        (polymode-minor-mode t)

                        ;; Org goodies; no longer needed
                        (orgtbl-mode t)
                        (orgstruct-mode t)
                        (orgstruct++-mode t)

                        ;; Extra modes
                        (outline-minor-mode t)
                        (footnote-mode t)
                        (auto-fill-mode t)

                        ;; Style/syntax check
                        (writegood-mode t)
                        (flyspell-mode t)
                        (if (executable-find "proselint")
                            (flycheck-mode t))))

            ;; http://stackoverflow.com/questions/14275122/editing-markdown-pipe-tables-in-emacs
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
                  (concat "pandoc --smart -c "
                          (concat user-emacs-directory
                                  "/styles/github-pandoc.css")
                          " --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone"))))

(provide 'setup-markdown)
;;; setup-markdown.el ends here

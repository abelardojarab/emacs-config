;;; setup-markdown.el ---

;; Copyright (C) 2016  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@Abelardos-MacBook-Pro.local>
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
  :mode ("\\.markdown\\'" "\\.mk?d\\'" "\\.te?xt\\'")
  :commands markdown-mode
  :load-path (lambda () (expand-file-name "markdown-mode/" user-emacs-directory))
  :config (progn
            (add-hook 'markdown-mode-hook
                      (lambda ()
                        (writegood-mode t)
                        (flyspell-mode t)))

            ;; Github markdown style
            (setq markdown-css-paths `(,(expand-file-name "styles/github-pandoc.css" user-emacs-directory)))
            (setq markdown-command
                  (concat "pandoc --smart -c "
                          (concat user-emacs-directory
                                  "/styles/github-pandoc.css")
                          " --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone"))))

(provide 'setup-markdown)
;;; setup-markdown.el ends here

;;; setup-org-plugins.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023  Abelardo Jara-Berrocal

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

;; Indentation, list bullets and checkboxes using monospace
(use-package org-variable-pitch
  :commands org-variable-pitch-minor-mode
  :hook (org-mode . org-variable-pitch-minor-mode))

;; Org tables
(use-package org-table
  :defer t
  :config (defconst org-table-border-regexp "^[ \t]*[^|]"
            "Regexp matching any line outside an Org table."))

;; Auto-align Org tables
(use-package org-table-auto-align
  :hook (org-mode .  org-table-auto-align-mode))

;; Prettier tables using unicode glyphs
(use-package org-pretty-table
  :defer t
  :commands org-pretty-table-mode)

;; Use footnotes as eldoc source
(use-package org-eldoc
  :disabled t
  :hook ((org-mode . org-eldoc-load)
         (org-mode . eldoc-mode))
  :commands org-eldoc-load
  :config (defun my/org-eldoc-get-footnote ()
            (save-excursion
              (let ((fn (org-between-regexps-p "\\[fn:" "\\]")))
                (when fn
                  (save-match-data
                    (nth 3 (org-footnote-get-definition (buffer-substring (+ 1 (car fn)) (- (cdr fn) 1)))))))))
  (advice-add 'org-eldoc-documentation-function
              :before-until #'my/org-eldoc-get-footnote))

;; Use UTF8 symbols for Org Todo priorities
(use-package org-fancy-priorities
  :disabled t ;; makes org hang
  :hook (org-mode . org-fancy-priorities-mode)
  :commands org-fancy-priorities-mode)

;; Org Table of Contents
(use-package toc-org
  :after org
  :hook (org-mode . toc-org-enable)
  :commands toc-org-enable)

;; Nice bulleted lists
(use-package org-bullets
  :if (display-graphic-p)
  :disabled t
  :after org
  :hook (org-mode . org-bullets-mode)
  :commands org-bullets-mode
  :config (setq org-bullets-bullet-list '("◉" "✚" "○" "✖" "•")))

;; replacement of org-bullets
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :delight org-superstar-mode
  :custom (org-superstar-special-todo-items t))

;; Seek headlines or content inside org buffers
(use-package org-seek
  :after org
  :commands (org-seek-string org-seek-regexp org-seek-headlines))

;; Automated bulleting
(use-package org-autolist
  :after org
  :hook (org-mode . org-autolist-mode)
  :commands org-autolist-mode)

;; Modern org styles
(use-package org-modern
  :after org
  :hook (org-agenda-finalize . org-modern-mode)
  :commands org-modern-mode)

(provide 'setup-org-plugins)
;;; setup-org-plugins.el ends here

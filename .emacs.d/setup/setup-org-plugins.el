;;; setup-org-plugins.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Abelardo Jara-Berrocal

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

;; Org Table of Contents
(use-package toc-org
  :defer t
  :after org
  :hook (org-mode . toc-org-enable)
  :commands toc-org-enable)

;; Nice bulleted lists
(use-package org-bullets
  :defer t
  :if (display-graphic-p)
  :after org
  :hook (org-mode . org-bullets-mode)
  :commands org-bullets-mode
  :config (setq org-bullets-bullet-list '("◉" "○" "•" "•")))

;; Seek headlines or content inside org buffers
(use-package org-seek
  :defer t
  :after org
  :commands (org-seek-string org-seek-regexp org-seek-headlines))

;; Automated bulleting
(use-package org-autolist
  :defer t
  :after org
  :hook (org-mode . org-autolist-mode)
  :commands org-autolist-mode)

(provide 'setup-org-plugins)
;;; setup-org-plugins.el ends here

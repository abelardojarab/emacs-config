;;; setup-org-plugins.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@ubuntu-MacBookPro>
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
  :load-path (lambda () (expand-file-name "toc-org/" user-emacs-directory))
  :config(add-hook 'org-mode-hook 'toc-org-enable))

;; Nice bulleted lists
(use-package org-bullets
  :if (display-graphic-p)
  :load-path (lambda () (expand-file-name "org-bullets/" user-emacs-directory))
  :config (progn
            (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

;; Automated bulleting
(use-package org-autolist
  :load-path (lambda () (expand-file-name "org-autolist/" user-emacs-directory))
  :config (add-hook 'org-mode-hook (lambda () (org-autolist-mode 1))))

;; ASCII doc
(use-package ox-asciidoc
  :load-path (lambda () (expand-file-name "org-asciidoc/" user-emacs-directory)))

;; Table ASCII plot
(use-package orgtbl-ascii-plot
  :load-path (lambda () (expand-file-name "orgtblasciiplot/" user-emacs-directory)))

;; Export org-mode to HTML5 slides
(use-package ox-html5presentation
  :load-path (lambda () (expand-file-name "org-html5presentation/" user-emacs-directory)))

;; Export org-mode to Google I/O HTML5 slides
(use-package ox-ioslide
  :load-path (lambda () (expand-file-name "org-ioslide/" user-emacs-directory))
  :config (use-package ox-ioslide-helper))

(provide 'setup-org-plugins)
;;; setup-org-plugins.el ends here

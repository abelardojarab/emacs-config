;;; setup-org-blog.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxcj9063.pdx.intel.com>
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

;; .authinfo parsing
(use-package netrc)

;; org2blog pre-requisite
(use-package metaweblog
  :defer t
  :load-path (lambda () (expand-file-name "metaweblog/" user-emacs-directory)))

(use-package org2blog
  :load-path (lambda () (expand-file-name "org2blog/" user-emacs-directory))
  :config (progn
            ;; blogging
            (setq personal-blog-name "personal-blog")
            (setq personal-blog-url "url/xmlrpc.php")
            (setq personal-blog (netrc-machine (netrc-parse "~/.authinfo") "machinename"  t)
                  org2blog/wp-blog-alist `((,personal-blog-name
                                            :url ,personal-blog-url
                                            ;; :username (netrc-get personal-blog "login")
                                            ;; :password (netrc-get personal-blog "password")
                                            )))))

(provide 'setup-org-blog)
;;; setup-org-blog.el ends here

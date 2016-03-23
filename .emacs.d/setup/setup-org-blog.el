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

;; global variables defcustoms
(defgroup emacs-custom-setup nil
  "Emacs rimero setup group."
  :group 'customize
  :prefix "ers-")

;; Credentials file for netrc authentication
;; Also symblink .authinfo to .netrc for offlineimap
(defcustom ers-secrets-file "~/.authinfo"
  "Secrets file."
  :type 'string
  :group 'emacs-custom-setup)

(use-package netrc)

(use-package metaweblog
  :defer t
  :load-path (lambda () (expand-file-name "metaweblog/" user-emacs-directory)))

(use-package org2blog
  :load-path (lambda () (expand-file-name "org2blog/" user-emacs-directory))
  :config (progn

            ;; blogging
            (setq personal-blog-name "wordpress-your-netrc-entry-in-dot-autinfo")
            (setq corporate-blog-name "wordpress-your-netrc-entry-in-dot-autinfo")
            (setq personal-blog-url "http://wordpress-site/xmlrpc.php")
            (setq corporate-blog-url "http://wordpress-site/xmlrpc.php")

            (setq corporate-blog (netrc-machine (netrc-parse ers-secrets-file) "corporate-blog" t)
                  personal-blog  (netrc-machine (netrc-parse ers-secrets-file) "personal-blog"  t)
                  org2blog/wp-blog-alist `((,corporate-blog-name
                                            :url ,corporate-blog-url
                                            :username (netrc-get corporate-blog "login")
                                            :password (netrc-get corporate-blog "password"))
                                           (,personal-blog-name
                                            :url ,personal-blog-url
                                            :username (netrc-get personal-blog "login")
                                            :password (netrc-get personal-blog "password"))))))

(provide 'setup-org-blog)
;;; setup-org-blog.el ends here

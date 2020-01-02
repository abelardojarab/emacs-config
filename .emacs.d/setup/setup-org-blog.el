;;; setup-org-blog.el ---                            -*- lexical-binding: t; -*-

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

;; org2blog pre-requisite
(use-package metaweblog
  :defer t)

;; org2blog
(use-package org2blog
  :defer t
  :after org
  :commands (org2blog/wp-login
             org2blog/wp-logout
             org2blog/wp-new-entry
             org2blog/post-buffer
             org2blog/post-buffer-as-page
             org2blog/post-buffer-and-publish)
  :config (progn
            (let (credentials)

              ;; Autoloads
              (use-package org2blog-autoloads)

              ;; Fetch credentials
              (setq credentials (auth-source-user-and-password "personal-blog"))

              ;; blogging
              (setq personal-blog-name "personal-blog")
              (setq personal-blog-url "https://jaraberrocal.readmyblog.org/xmlrpc.php")
              (setq org2blog/wp-confirm-post t
                    org2blog/wp-blog-alist `((,personal-blog-name
                                              :url ,personal-blog-url
                                              :username ,(car credentials)
                                              :password ,(cadr credentials)
                                              :default-categories ("org2blog" "Emacs")
                                              :tags-as-categories nil
                                              )))

              ;; htmlize` is required, else you get empty
              ;; code blocks https://github.com/punchagan/org2blog/blob/master/org2blog.el
              ;; with wp-use-sourcecode-shortcode set to 't, org2blog will use 1
              ;; shortcodes, and hence the SyntaxHighlighter Evolved plugin on your blog.
              ;; however, if you set this to nil, native Emacs highlighting will be used,
              ;; implemented as HTML styling. Your pick!
              (setq org2blog/wp-use-sourcecode-shortcode 't)

              ;; removed light="true"
              (setq org2blog/wp-sourcecode-default-params nil)

              ;; target language needs to be in here
              (setq org2blog/wp-sourcecode-langs
                    '("actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
                      "erlang" "fsharp" "diff" "groovy" "javascript" "java" "javafx" "matlab"
                      "objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql"
                      "vb" "xml" "sh" "emacs-lisp" "lisp" "lua")))))

(provide 'setup-org-blog)
;;; setup-org-blog.el ends here

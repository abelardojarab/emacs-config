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

;; htmlize, require to format source for wordpress (otherwise code blocks will appear empty)
;; Prefer the Org version
(use-package htmlize
  :load-path (lambda () (expand-file-name "htmlize" user-emacs-directory))
  ;; :commands (htmlize-faces-in-buffer
  ;;            htmlize-make-face-map
  ;;            htmlize-css-specs htmlize-region
  ;;            htmlize-region-for-paste
  ;;            htmlize-region-to-file
  ;;            htmlize-region-to-buffer)
  :config (progn
            (setq htmlize-html-major-mode 'web-mode)
            (setq htmlize-output-type 'inline-css)

            ;; It is required to disable `fci-mode' when `htmlize-buffer' is called;
            ;; otherwise the invisible fci characters show up as funky looking
            ;; visible characters in the source code blocks in the html file.
            ;; http://lists.gnu.org/archive/html/emacs-orgmode/2014-09/msg00777.html
            (with-eval-after-load 'fill-column-indicator
              (defvar my/htmlize-initial-fci-state nil
                "Variable to store the state of `fci-mode' when `htmlize-buffer' is called.")

              (defun my/htmlize-before-hook-fci-disable ()
                (setq my/htmlize-initial-fci-state fci-mode)
                (when fci-mode
                  (fci-mode -1)))

              (defun my/htmlize-after-hook-fci-enable-maybe ()
                (when my/htmlize-initial-fci-state
                  (fci-mode 1)))

              (add-hook 'htmlize-before-hook #'my/htmlize-before-hook-fci-disable)
              (add-hook 'htmlize-after-hook #'my/htmlize-after-hook-fci-enable-maybe))

            ;; `flyspell-mode' also has to be disabled because depending on the
            ;; theme, the squiggly underlines can either show up in the html file
            ;; or cause elisp errors like:
            ;; (wrong-type-argument number-or-marker-p (nil . 100))
            (with-eval-after-load 'flyspell
              (defvar my/htmlize-initial-flyspell-state nil
                "Variable to store the state of `flyspell-mode' when `htmlize-buffer' is called.")

              (defun my/htmlize-before-hook-flyspell-disable ()
                (setq my/htmlize-initial-flyspell-state flyspell-mode)
                (when flyspell-mode
                  (flyspell-mode -1)))

              (defun my/htmlize-after-hook-flyspell-enable-maybe ()
                (when my/htmlize-initial-flyspell-state
                  (flyspell-mode 1)))

              (add-hook 'htmlize-before-hook #'my/htmlize-before-hook-flyspell-disable)
              (add-hook 'htmlize-after-hook #'my/htmlize-after-hook-flyspell-enable-maybe))

            ;; Copying functions
            (defvar my/htmlize-output-directory
              (let ((dir (concat temporary-file-directory
                                 (getenv "USER") "/.htmlize/"))) ; must end with /
                (make-directory dir :parents)
                dir)
              "Output directory for files exported by `my/htmlize-region-to-file'.")

            (defvar my/htmlize-css-file (concat user-emacs-directory
                                                "/org-html-themes/styles/bigblow/css/htmlize.css")
              "CSS file to be embedded in the html file created using the
             `my/htmlize-region-to-file' function.")
            (setq my/htmlize-css-file (concat user-emacs-directory
                                              "/org-html-themes/styles/bigblow/css/htmlize.css"))

            (defun htmlize-region-to-file (option)
              "Export the selected region to an html file. If a region is not
selected, export the whole buffer.

The output file is saved to `my/htmlize-output-directory' and its fontification
is done using `my/htmlize-css-file'.

If OPTION is non-nil (for example, using `\\[universal-argument]' prefix), copy
the output file name to kill ring.
If OPTION is \\='(16) (using `\\[universal-argument] \\[universal-argument]' prefix),
do the above and also open the html file in the default browser."
              (interactive "P")
              (let ((org-html-htmlize-output-type 'css)
                    (org-html-htmlize-font-prefix "org-")
                    (fname (concat my/htmlize-output-directory
                                   (if (buffer-file-name)
                                       (file-name-nondirectory (buffer-file-name))
                                     "temp")
                                   ".html"))
                    start end html-string)
                (if (use-region-p)
                    (progn
                      (setq start (region-beginning))
                      (setq end (region-end)))
                  (progn
                    (setq start (point-min))
                    (setq end (point-max))))
                (setq html-string (htmlize-region-for-paste start end))
                ;; (setq html-string (org-html-htmlize-region-for-paste start end))
                (with-temp-buffer
                  ;; Insert the `my/htmlize-css-file' contents in the temp buffer
                  (insert-file-contents my/htmlize-css-file nil nil nil :replace)
                  ;; Go to the beginning of the buffer and insert comments and
                  ;; opening tags for `html', `head' and `style'. These are
                  ;; inserted *above* the earlier inserted css code.
                  (goto-char (point-min))
                  (insert (concat "<!-- This file is generated using the "
                                  "`htmlize-region-to-file' function -->\n"))
                  (insert "<html>\n<head>\n<style media=\"screen\" type=\"text/css\">\n")
                  ;; Go to the end of the buffer (end of the css code) and
                  ;; insert the closing tags for `style' and `head' and opening
                  ;; tag for `body'.
                  (goto-char (point-max))
                  (insert "</style>\n</head>\n<body>\n")
                  ;; Insert the HTML for fontified text in `html-string'.
                  (ignore-errors (insert html-string))
                  ;; Close the `body' and `html' tags.
                  (insert "</body>\n</html>\n")
                  (write-file fname)
                  (when option
                    (kill-new fname)
                    (when (= 16 (car option))
                      (browse-url-of-file fname))))))

              ;; From http://ruslanspivak.com/2007/08/18/htmlize-your-erlang-code-buffer/
              (defun htmlize-region-to-buffer (beg end)
                "Htmlize region and put into <pre> tag style that is left in <body> tag
plus add font-size: 8pt"
                (interactive "r")
                (let* ((buffer-faces (htmlize-faces-in-buffer))
                       (face-map (htmlize-make-face-map (adjoin 'default buffer-faces)))
                       (pre-tag (format
                                 "<pre style=\"%s font-size: 8pt\">"
                                 (mapconcat #'identity (htmlize-css-specs
                                                        (gethash 'default face-map)) " ")))
                       (htmlized-reg (htmlize-region-for-paste beg end)))
                  (switch-to-buffer-other-window "*htmlized output*")
                                        ; clear buffer
                  (kill-region (point-min) (point-max))
                                        ; set mode to have syntax highlighting
                  (web-mode)
                  (save-excursion
                    (insert htmlized-reg))
                  (while (re-search-forward "<pre>" nil t)
                    (replace-match pre-tag nil nil))
                  (goto-char (point-min))))))

;; .authinfo parsing (not longer valid for org2blog, use auth-source)
(use-package netrc)

;; .authinfo parsing
(use-package auth-source
  :config (progn
            (if (file-exists-p "~/.authinfo")
                (add-to-list 'auth-sources "~/.authinfo"))))

;; org2blog pre-requisite
(use-package metaweblog
  :defer t
  :load-path (lambda () (expand-file-name "metaweblog/" user-emacs-directory)))

;; org2blog
(use-package org2blog
  :after org
  :load-path (lambda () (expand-file-name "org2blog/" user-emacs-directory))
  :config (progn
            (let (credentials)

              ;; Autoloads
              (require 'org2blog-autoloads)

              ;; Fetch credentials
              (setq credentials (auth-source-user-and-password "personal-blog"))

              ;; blogging
              (setq personal-blog-name "personal-blog")
              (setq personal-blog-url "http://abelardojarab.dyndns.org/xmlrpc.php")
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

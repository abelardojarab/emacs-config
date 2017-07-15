;;; setup-html.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  Abelardo Jara-Berrocal

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

;; htmlize, require to format source for wordpress (otherwise code blocks will appear empty)
;; Prefer the Org version
(use-package htmlize
  :load-path (lambda () (expand-file-name "htmlize" user-emacs-directory))
  :commands (htmlize-faces-in-buffer
             htmlize-make-face-map
             htmlize-css-specs htmlize-region
             htmlize-region-for-paste
             htmlize-region-to-file
             htmlize-region-to-buffer)
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
                                                "/styles/github-pandoc.css")
              "CSS file to be embedded in the html file created using the
             `my/htmlize-region-to-file' function.")
            (setq my/htmlize-css-file (concat user-emacs-directory
                                              "/styles/github-pandoc.css"))

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

;; Formatted copy
(use-package ox-clip
  :after (htmlize org)
  :commands ox-clip-formatted-copy
  :if (or (executable-find "xclip")
          (executable-find "python"))
  :load-path (lambda () (expand-file-name "ox-clip/" user-emacs-directory)))

;; XML mode
(use-package nxml-mode
  :after (hideshow smartparens)
  :init (progn
          (use-package sgml-mode)

          (add-to-list 'hs-special-modes-alist
                       '(nxml-mode
                         "<!--\\|<[^/>]*[^/]>"
                         "-->\\|</[^/>]*[^/]>"

                         "<!--"
                         sgml-skip-tag-forward
                         nil)))
  :config (progn
            (setq nxml-slash-auto-complete-flag t)

            ;; don't try and complete tag end - breaks nxml completion etc
            (sp-local-pair 'nxml-mode "<" ">" :actions '(:rem insert))))

;; Required by web-mode
(use-package web-completion-data
  :load-path (lambda () (expand-file-name "web-completion-data/" user-emacs-directory)))

;; HTML mode
(use-package web-mode
  :mode ("\\.css?\\'" "\\.html?\\'")
  :after (nxml-mode web-completion-data)
  :commands (web-mode htmlize-region-to-file)
  :load-path (lambda () (expand-file-name "web-mode/" user-emacs-directory))
  :config (progn
            (setq web-mode-enable-css-colorization t
                  web-mode-enable-auto-quoting t
                  web-mode-enable-auto-closing t
                  web-mode-style-padding 2
                  web-mode-script-padding 2
                  web-mode-markup-indent-offset 2
                  web-mode-code-indent-offset 2
                  web-mode-enable-current-element-highlight t)))

;; Company backend
(use-package company-web
  :after (company web-mode web-completion-data)
  :load-path (lambda () (expand-file-name "company-web/" user-emacs-directory))
  :config (add-hook 'web-mode-hook
                    (lambda () (set (make-local-variable 'company-backends)
                               '((company-web-html
                                  company-capf
                                  company-files
                                  company-abbrev))))))

(provide 'setup-html)
;;; setup-org-html.el ends here

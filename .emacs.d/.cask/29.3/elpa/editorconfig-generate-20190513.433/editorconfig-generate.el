;;; editorconfig-generate.el --- Generate .editorconfig  -*- lexical-binding: t; -*-

;; Author: 10sr <8.slashes@gmail.com>
;; URL: https://github.com/10sr/editorconfig-generate-el
;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; editorconfig-generate.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; editorconfig-generate.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; editorconfig-generate.el . If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate .editorconfig content for buffer from current Emacs configuration.

;; `M-x editorconfig-generate` to open a buffer that has `.editorconfig` content
;; for current buffer.  Properties are extracted from current Emacs configurations.


;;; Code:

;; Variable Definition derived from editorconfig.el `editorconfig-indentation-alist'
;; http://github.com/editorconfig/editorconfig-emacs#readme
(defvar editorconfig-generate-mode-offset-alist
  '((apache-mode apache-indent-level)
    (awk-mode c-basic-offset)
    (c++-mode c-basic-offset)
    (c-mode c-basic-offset)
    (cmake-mode cmake-tab-width)
    (coffee-mode coffee-tab-width)
    (cperl-mode cperl-indent-level)
    (crystal-mode crystal-indent-level)
    (csharp-mode c-basic-offset)
    (css-mode css-indent-offset)
    (emacs-lisp-mode lisp-indent-offset)
    (erlang-mode erlang-indent-level)
    (ess-mode ess-indent-offset)
    (feature-mode feature-indent-offset
                  feature-indent-level)
    (fsharp-mode fsharp-continuation-offset
                 fsharp-indent-level
                 fsharp-indent-offset)
    (groovy-mode groovy-indent-offset)
    (haskell-mode haskell-indent-spaces
                  haskell-indent-offset
                  haskell-indentation-layout-offset
                  haskell-indentation-left-offset
                  haskell-indentation-starter-offset
                  haskell-indentation-where-post-offset
                  haskell-indentation-where-pre-offset
                  shm-indent-spaces)
    (idl-mode c-basic-offset)
    (jade-mode jade-tab-width)
    (java-mode c-basic-offset)
    (js-mode js-indent-level)
    (js-jsx-mode js-indent-level sgml-basic-offset)
    (js2-mode js2-basic-offset)
    (js2-jsx-mode js2-basic-offset sgml-basic-offset)
    (js3-mode js3-indent-level)
    (json-mode js-indent-level)
    (julia-mode julia-indent-offset)
    (latex-mode tex-indent-basic)
    (lisp-mode lisp-indent-offset)
    (livescript-mode livescript-tab-width)
    (lua-mode lua-indent-level)
    (matlab-mode matlab-indent-level)
    (mustache-mode mustache-basic-offset)
    (nginx-mode nginx-indent-level)
    (nxml-mode nxml-child-indent)
    (objc-mode c-basic-offset)
    (octave-mode octave-block-offset)
    (perl-mode perl-indent-level)
    ;; No need to change `php-mode-coding-style' value for php-mode
    ;; since we run editorconfig later than it resets `c-basic-offset'.
    ;; See https://github.com/editorconfig/editorconfig-emacs/issues/116
    ;; for details.
    (php-mode c-basic-offset)
    (pike-mode c-basic-offset)
    (ps-mode ps-mode-tab)
    (pug-mode pug-tab-width)
    (puppet-mode puppet-indent-level)
    (python-mode python-indent-offset
                 python-indent
                 py-indent-offset)
    (ruby-mode ruby-indent-level)
    (rust-mode rust-indent-offset)
    (scala-mode scala-indent:step)
    (scss-mode css-indent-offset)
    (sgml-mode sgml-basic-offset)
    (sh-mode sh-basic-offset sh-indentation)
    (slim-mode slim-indent-offset)
    (tcl-mode tcl-indent-level
              tcl-continued-indent-level)
    (typescript-mode typescript-indent-level)
    (verilog-mode verilog-indent-level
                  verilog-indent-level-behavioral
                  verilog-indent-level-declaration
                  verilog-indent-level-module
                  verilog-cexp-indent
                  verilog-case-indent)
    (web-mode web-mode-indent-style
              web-mode-attr-indent-offset
              web-mode-attr-value-indent-offset
              web-mode-code-indent-offset
              web-mode-css-indent-offset
              web-mode-markup-indent-offset
              web-mode-sql-indent-offset
              web-mode-block-padding
              web-mode-script-padding
              web-mode-style-padding)
    (yaml-mode yaml-indent-offset)
    )
  "Alist of modes and its candidate for indentation offset.

Each element should be like (MODE . VARIABLE-CANDIDATES) .
The indentation offset will be gotten from the first valid value
 (varible is defined it value is not nil)."
  )


(defvar editorconfig-generate-properties-alist
  '(
    ("indent_style" . (if indent-tabs-mode
                          "tab"
                        "space"))
    ("indent_size" . (let ((s (editorconfig-generate-get-indent-size major-mode)))
                       (if s
                           (int-to-string s)
                         nil)))
    ("tab_width" . (int-to-string tab-width))
    ("end_of_line" . (let ((type (car (last (split-string (symbol-name buffer-file-coding-system)
                                                          "-")))))
                       (cond ((string-equal type "unix")
                              "lf")
                             ((string-equal type "mac")
                              "cr")
                             ((string-equal type "dos")
                              "crlf")
                             )))
    ("charset" . (let ((coding (symbol-name buffer-file-coding-system)))
                   (cond ((or (string-match-p "^utf-8" coding)
                              (string-match-p "^prefer-utf-8" coding))
                          "utf-8")
                         ((string-match-p "^latin-1" coding)
                          "latin1")
                         ((string-match-p "^utf-16-be" coding)
                          "utf-16be")
                         ((string-match-p "^utf-16-le" coding)
                          "utf-16le")
                         )))
    ("trim_trailing_whitespace" . (if (or (memq 'delete-trailing-whitespace
                                                before-save-hook)
                                          (memq 'delete-trailing-whitespace
                                                write-file-functions)
                                          ;; There might be other hooks that
                                          ;; have this
                                          )
                                      "true"
                                    "false"))
    ("insert_final_newline" . (if require-final-newline
                                  ;; require-final-newline can take some sort of
                                  ;; values, but here only nil is translated
                                  ;; into false
                                  "true"
                                "false"))
    )
  "Alist of EditorConfig properties and how to get value.
Each element should be like (PROP . SEXP)")

(defun editorconfig-generate-get-indent-size (mode)
  "Get indentation offset for major mode MODE.

If MODE is a derived mode of other mode and no suitable offset value was found,
it will go up recursively and take the first valid value.
If MODE is nil this function allways returns nil."
  (when mode
    (let ((var-list (cdr (assq mode
                               editorconfig-generate-mode-offset-alist))))
      (or (editorconfig-generate-take-first-valid var-list)
          (editorconfig-generate-get-indent-size (get mode
                                                  'derived-mode-parent))))))

(defun editorconfig-generate-take-first-valid (l)
  "Accept list of variables L and return the first valid value."
  (when l
    (let ((v (car l)))
      (or (and (boundp v)
               (eval v))
          (editorconfig-generate-take-first-valid (cdr l))))))

;;;###autoload
(defun editorconfig-generate (&optional buf)
  "Generate EditorConfig content for buffer BUF.
if BUF is omitted or nil, works for current buffer."
  (interactive)
  (setq buf (or buf
                (current-buffer)))
  (let* ((filename (file-name-nondirectory buffer-file-name))
         (output-buf (generate-new-buffer (concat "*editorconfig<"
                                                  filename
                                                  ">*"))))
    (with-current-buffer output-buf
      (when (fboundp 'editorconfig-conf-mode)
        (editorconfig-conf-mode))
      (insert "["
              filename
              "]\n\n"))
    (dolist (prop editorconfig-generate-properties-alist)
      (let ((value (eval (cdr prop))))
        (message "val:%S" output-buf)
        (when value
          (with-current-buffer output-buf
            (insert (car prop)
                    " = "
                    value
                    "\n")))))
    (display-buffer output-buf)))

(provide 'editorconfig-generate)

;;; editorconfig-generate.el ends here

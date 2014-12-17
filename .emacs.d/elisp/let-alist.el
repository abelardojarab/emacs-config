;;; let-alist.el --- Easily let-bind values of an assoc-list by their names.

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Author: Artur Malabarba <address@hidden>
;; Maintainer: Artur Malabarba <address@hidden>
;; Version: 1.0
;; Keywords: extensions lisp
;; Prefix: let-alist
;; Separator: -

;;; Commentary:
;;
;; This package offers a single macro, `let-alist'.  This macro takes a
;; first argument (whose value must be an alist) and a body.
;;
;; The macro expands to a let form containing body, where each dotted
;; symbol inside body is let-bound to their cdrs in the alist.  Dotted
;; symbol is any symbol starting with a `.'.  Only those present in
;; the body are let-bound and this search is done at compile time.
;;
;; For instance, the following code
;;
;;   (let-alist alist
;;     (if (and .title .body)
;;         .body
;;       .site))
;;
;; expands to
;;
;;   (let ((.title (cdr (assq 'title alist)))
;;         (.body (cdr (assq 'body alist)))
;;         (.site (cdr (assq 'site alist))))
;;     (if (and .title .body)
;;         .body
;;       .site))

;;; License:
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; News:
;;; Code:


(defun let-alist--deep-dot-search (data)
  "Return alist of symbols inside DATA which start with a `.'.
Perform a deep search and return an alist where each car is the
symbol, and each cdr is the same symbol without the `.'."
  (cond
   ((symbolp data)
    (let ((name (symbol-name data)))
      (when (string-match "\\`\\." name)
        ;; Return the cons cell inside a list, so it can be appended
        ;; with other results in the clause below.
        (list (cons data (intern (replace-match "" nil nil name)))))))
   ((not (listp data)) nil)
   (t (apply #'append
             (remove nil (mapcar #'let-alist--deep-dot-search data))))))

;;;###autoload
(defmacro let-alist (alist &rest body)
  "Let-bind dotted symbols to their cdrs in ALIST and execute BODY.
Dotted symbol is any symbol starting with a `.'.  Only those
present in BODY are let-bound and this search is done at compile
time.

For instance, the following code

  (let-alist alist
    (if (and .title .body)
        .body
      .site))

expands to

  (let ((.title (cdr (assq 'title alist)))
        (.body (cdr (assq 'body alist)))
        (.site (cdr (assq 'site alist))))
    (if (and .title .body)
        .body
      .site))"
  (declare (indent 1) (debug t))
  `(let ,(mapcar (lambda (x) `(,(car x) (cdr (assq ',(cdr x) ,alist))))
                 (delete-dups (let-alist--deep-dot-search body)))
     ,@body))

(provide 'let-alist)
;;; let-alist.el ends here


;; Local Variables:
;; sentence-end-double-space: t
;; End:

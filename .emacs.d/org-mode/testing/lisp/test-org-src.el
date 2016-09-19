;;; test-org-src.el --- tests for org-src.el

;; Copyright (C) 2012-2015  Le Wang

;; Author: Le Wang <l26wang at gmail dot com>

;; This file is not part of GNU Emacs.

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

;;; Code:

(require 'org-test)



(ert-deftest test-org-src/basic ()
  "Editing regular block works, with point on source block."
  (org-test-with-temp-text
      "
<point>#+begin_src emacs-lisp
  (message hello)
#+end_src
"
    (let ((org-edit-src-content-indentation 2)
          (org-src-preserve-indentation nil))
      (org-edit-special)
      (insert "blah")
      (org-edit-src-exit)
      (should (equal (buffer-string) "
#+begin_src emacs-lisp
  blah(message hello)
#+end_src
"))
      (should (looking-at-p "(message hello)")))))

(ert-deftest test-org-src/point-outside-block ()
  "Editing with point before/after block signals expected error."
  (org-test-with-temp-text
      "
#+begin_src emacs-lisp
  (message hello)
#+end_src
"
    (goto-line 1)
    (should-error (org-edit-special))
    (goto-char (point-max))
    (should-error (org-edit-special))))

(ert-deftest test-org-src/empty-block ()
  "Editing empty block."
  (org-test-with-temp-text
      "
<point>#+begin_src emacs-lisp
#+end_src
"
    (let ((org-edit-src-content-indentation 0)
          (org-src-preserve-indentation nil))
      (org-edit-special)
      (insert "blah")
      (org-edit-src-exit)
      (should (equal (buffer-string) "
#+begin_src emacs-lisp
blah
#+end_src
"))
      (should
       (equal (buffer-substring (line-beginning-position) (point)) "blah")))))

(ert-deftest test-org-src/blank-line-block ()
  "Editing block with just a blank line."
  (org-test-with-temp-text-in-file
      "
#+begin_src emacs-lisp

#+end_src
"
    (let ((org-edit-src-content-indentation 2)
          (org-src-preserve-indentation nil))
      (goto-line 2)
      (org-edit-special)
      (insert "blah")
      (org-edit-src-exit)
      (should (equal (buffer-string) "
#+begin_src emacs-lisp
  blah
#+end_src
")))))

(ert-deftest test-org-src/preserve-tabs ()
  "Editing block preserve tab characters."
  ;; With `org-src-preserve-indentation' set to nil.
  (should
   (equal "
#+begin_src emacs-lisp
  This is a tab:\t.
#+end_src"
          (org-test-with-temp-text
              "
#+begin_src emacs-lisp
<point>This is a tab:\t.
#+end_src"
            (let ((org-edit-src-content-indentation 2)
                  (org-src-preserve-indentation nil))
              (org-edit-special)
              (org-edit-src-exit)
              (buffer-string)))))
  ;; With `org-src-preserve-indentation' set to t.
  (should
   (equal "
#+begin_src emacs-lisp
This is a tab:\t.
#+end_src"
          (org-test-with-temp-text
              "
#+begin_src emacs-lisp
<point>This is a tab:\t.
#+end_src"
            (let ((org-edit-src-content-indentation 2)
                  (org-src-preserve-indentation t))
              (org-edit-special)
              (org-edit-src-exit)
              (buffer-string))))))

(ert-deftest test-org-src/coderef-format ()
  "Test `org-src-coderef-format' specifications."
  ;; Regular tests in a src block, an example block and an edit
  ;; buffer.
  (should
   (equal "foo"
          (let ((org-coderef-label-format "foo"))
            (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n0\n#+END_SRC"
              (org-src-coderef-format)))))
  (should
   (equal "foo"
          (let ((org-coderef-label-format "foo"))
            (org-test-with-temp-text "#+BEGIN_EXAMPLE\n0\n#+END_EXAMPLE"
              (org-src-coderef-format)))))
  (should
   (equal "foo"
          (let ((org-coderef-label-format "foo") result)
            (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n0\n#+END_SRC"
              (org-edit-special)
              (setq result (org-src-coderef-format))
              (org-edit-src-exit)
              result))))
  ;; When a local variable in the source buffer is available, use it.
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo"))
            (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n0\n#+END_SRC"
              (setq-local org-coderef-label-format "bar")
              (org-src-coderef-format)))))
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo") result)
            (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n0\n#+END_SRC"
              (setq-local org-coderef-label-format "bar")
              (org-edit-special)
              (setq result (org-src-coderef-format))
              (org-edit-src-exit)
              result))))
  ;; Use provided local format even if in an edit buffer.
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo"))
            (org-test-with-temp-text
		"#+BEGIN_SRC emacs-lisp -l \"bar\"\n0\n#+END_SRC"
              (org-src-coderef-format)))))
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo") result)
            (org-test-with-temp-text
                "#+BEGIN_SRC emacs-lisp -l \"bar\"\n0\n#+END_SRC"
              (org-edit-special)
              (setq result (org-src-coderef-format))
              (org-edit-src-exit)
              result))))
  ;; Local format has precedence over local variables.
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo"))
            (org-test-with-temp-text
		"#+BEGIN_SRC emacs-lisp -l \"bar\"\n0\n#+END_SRC"
	      (setq-local org-coderef-label-format "foo")
              (org-src-coderef-format)))))
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo") result)
            (org-test-with-temp-text
                "#+BEGIN_SRC emacs-lisp -l \"bar\"\n0\n#+END_SRC"
	      (setq-local org-coderef-label-format "foo")
              (org-edit-special)
              (setq result (org-src-coderef-format))
              (org-edit-src-exit)
              result))))
  ;; When optional argument provides a coderef format string, use it.
  (should
   (equal "bar"
	  (let ((org-coderef-label-format "foo")
		(element (org-element-create 'src-block '(:label-fmt "bar"))))
	    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n0\n#+END_SRC"
	      (org-src-coderef-format element)))))
  (should
   (equal "baz"
          (let ((org-coderef-label-format "foo")
		(element (org-element-create 'src-block '(:label-fmt "baz"))))
            (org-test-with-temp-text
		"#+BEGIN_SRC emacs-lisp -l \"bar\"\n0\n#+END_SRC"
	      (setq-local org-coderef-label-format "foo")
              (org-src-coderef-format element)))))
  ;; If it doesn't provide any label format string, fall back to
  ;; regular checks.
  (should
   (equal "foo"
	  (let ((org-coderef-label-format "foo")
		(element (org-element-create 'src-block)))
	    (org-test-with-temp-text "#+BEGIN_SRC emacs-lisp\n0\n#+END_SRC"
	      (org-src-coderef-format element)))))
  (should
   (equal "bar"
          (let ((org-coderef-label-format "foo")
		(element (org-element-create 'src-block)))
            (org-test-with-temp-text
		"#+BEGIN_SRC emacs-lisp -l \"bar\"\n0\n#+END_SRC"
	      (setq-local org-coderef-label-format "foo")
              (org-src-coderef-format element))))))

(ert-deftest test-org-src/coderef-regexp ()
  "Test `org-src-coderef-regexp' specifications."
  ;; Regular test.
  (should
   (string-match-p (org-src-coderef-regexp "; ref:%s")
		   "#+BEGIN_SRC emacs-lisp\n0; ref:label\n#+END_SRC"))
  ;; Ignore white space around the coderef.
  (should
   (string-match-p (org-src-coderef-regexp "; ref:%s")
		   "#+BEGIN_SRC emacs-lisp\n0 ; ref:label\n#+END_SRC"))
  (should
   (string-match-p (org-src-coderef-regexp "; ref:%s")
		   "#+BEGIN_SRC emacs-lisp\n0 ; ref:label  \n#+END_SRC"))
  ;; Only match regexp at the end of the line.
  (should-not
   (string-match-p (org-src-coderef-regexp "; ref:%s")
		   "#+BEGIN_SRC emacs-lisp\n0; ref:label (+ 1 2)\n#+END_SRC"))
  ;; Do not match an empty label.
  (should-not
   (string-match-p (org-src-coderef-regexp "; ref:%s")
		   "#+BEGIN_SRC emacs-lisp\n0; ref:\n#+END_SRC"))
  ;; When optional argument LABEL is provided, match given label only.
  (should
   (string-match-p (org-src-coderef-regexp "; ref:%s" "label")
		   "#+BEGIN_SRC emacs-lisp\n0; ref:label\n#+END_SRC"))
  (should-not
   (string-match-p (org-src-coderef-regexp "; ref:%s" "label2")
		   "#+BEGIN_SRC emacs-lisp\n0; ref:label\n#+END_SRC")))

(provide 'test-org-src)
;;; test-org-src.el ends here

;;; test-basic-c-compile.el --- Testing of basic-c-compile with buttercup.
;;
;; Filename: buttercup-basic-c-compile.el
;; Description:
;; Author: Nick Spain
;; Maintainer:
;; Created: Fri Jul 29 22:11:40 2016 (+1000)
;; Version:
;; Last-Updated:
;;           By:
;;     Update #: 1
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 30-Jul-2016 Nick Spain
;;    Initial commit. Add tests for basic-c-compile--files-to-compile
;;    and basic-c-compile--c-file-extension-p.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'basic-c-compile)
(require 'buttercup)



(describe "Function: basic-c-compile--c-file-extension-p"
  (it "Returns true if the file ends in a '.c' extension"
    ;; True testing
    (dolist (file '("test.c" "test.1.c" "test_test_test.c"))
      (expect (basic-c-compile--c-file-extension-p file)
              :to-be-truthy)))
  (it "Returns false if the file does not end in a '.c' extension"
    ;; False testing
    (dolist (file '("test.pl" "test.py" "test.java" "test"))
      (expect (basic-c-compile--c-file-extension-p file)
              :not :to-be-truthy)))
  (it "Throws an exception for blank strings"
    (expect (basic-c-compile--c-file-extension-p "")
            :to-throw)))



(describe "Function: basic-c-compile--files-to-compile"
    ;; Set-up
    (before-all
      (mkdir "test/")
      (dolist (file '("test/test.c" "test/test1.c" "test/test2.c"
                      "test/test3.java" "test/test4.py"))
        (write-region nil nil file)))
    ;; Tear down
    (after-all
      (delete-directory "test" t))

    (it "Returns all '.c' files when first argument is 'all'."
      (expect (basic-c-compile--files-to-compile "all"
                                                 "test/test.c")
              :to-equal "test.c test1.c test2.c"))

    (it "Returns a selection of files if given one"
      (expect (basic-c-compile--files-to-compile "selection"
                                                 "test.c"
                                                 '("test.c"
                                                   "test1.c" "test2.c"))
              :to-equal
              '("test.c" "test1.c" "test2.c")))

    (it "Returns only to input file if selector is not 'all' or 'selection'"
      (expect (basic-c-compile--files-to-compile nil
                                                 "test.c")
              :to-equal "test.c")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test-basic-c-compile.el ends here

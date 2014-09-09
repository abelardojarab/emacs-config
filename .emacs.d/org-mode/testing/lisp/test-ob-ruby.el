;;; test-ob-ruby.el --- tests for ob-ruby.el

;; Copyright (c) 2013 Oleh Krehel
;; Authors: Oleh Krehel

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
(org-test-for-executable "ruby")
(unless (featurep 'ob-ruby)
  (signal 'missing-test-dependency "Support for Ruby code blocks"))

(ert-deftest test-ob-ruby/session-output ()
    (should (equal (org-test-with-temp-text "#+begin_src ruby :session :results output
s = \"1\"
s = \"2\"
s = \"3\"
puts s
#+end_src"
  (org-ctrl-c-ctrl-c)
  (substring-no-properties
   (buffer-string)))
		   "#+begin_src ruby :session :results output
s = \"1\"
s = \"2\"
s = \"3\"
puts s
#+end_src

#+RESULTS:
: 
: 3

")))

(provide 'test-ob-ruby)

;;; test-ob-ruby.el ends here

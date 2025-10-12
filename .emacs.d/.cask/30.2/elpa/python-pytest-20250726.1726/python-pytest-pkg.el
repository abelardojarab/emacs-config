;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "python-pytest" "20250726.1726"
  "Helpers to run pytest."
  '((emacs     "24.4")
    (dash      "2.18.0")
    (transient "0.3.7")
    (s         "1.12.0"))
  :url "https://github.com/wbolster/emacs-python-pytest"
  :commit "ed2ecee09d1cccb4245842860d91940cb2fda769"
  :revdesc "ed2ecee09d1c"
  :keywords '("pytest" "test" "python" "languages" "processes" "tools")
  :authors '(("wouter bolsterlee" . "wouter@bolsterl.ee"))
  :maintainers '(("wouter bolsterlee" . "wouter@bolsterl.ee")))

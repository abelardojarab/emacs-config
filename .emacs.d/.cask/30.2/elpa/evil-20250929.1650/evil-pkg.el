;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "evil" "20250929.1650"
  "Extensible vi layer."
  '((emacs    "24.1")
    (cl-lib   "0.5")
    (goto-chg "1.6")
    (nadvice  "0.3"))
  :url "https://github.com/emacs-evil/evil"
  :commit "b06f644bdb5b06c6ac46c11b0259f15ac9ffd5da"
  :revdesc "b06f644bdb5b"
  :keywords '("emulations")
  :maintainers '(("Tom Dalziel" . "tom.dalziel@gmail.com")))

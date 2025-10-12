;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "ergoemacs-mode" "20240809.2246"
  "Emacs mode based on common modern interface and ergonomics."
  '((emacs   "24.1")
    (cl-lib  "0.5")
    (nadvice "0.4"))
  :url "https://github.com/ergoemacs/ergoemacs-mode"
  :commit "3c9081fe83f70cf791abc98d6b9184f8ea7fb714"
  :revdesc "3c9081fe83f7"
  :keywords '("convenience")
  :authors '(("Xah Lee" . "xah@xahlee.org")
             ("David Capello" . "davidcapello@gmail.com")
             ("Matthew L. Fidler" . "matthew.fidler@gmail.com")
             ("Kim F. Storm -- CUA approach for C-x and C-c" . "storm@cua.dk"))
  :maintainers '(("Matthew L. Fidler" . "matthew.fidler@gmail.com")))

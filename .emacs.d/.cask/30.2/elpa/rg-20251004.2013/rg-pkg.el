;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "rg" "20251004.2013"
  "A search tool based on ripgrep."
  '((emacs     "26.1")
    (transient "0.9.2")
    (wgrep     "2.1.10"))
  :url "https://github.com/dajva/rg.el"
  :commit "dcd65fd34b5fcd7c28fd83275129a2ebc7fa8f17"
  :revdesc "dcd65fd34b5f"
  :keywords '("matching" "tools")
  :authors '(("David Landell" . "david.landell@sunnyhill.email")
             ("Roland McGrath" . "roland@gnu.org"))
  :maintainers '(("David Landell" . "david.landell@sunnyhill.email")
                 ("Roland McGrath" . "roland@gnu.org")))

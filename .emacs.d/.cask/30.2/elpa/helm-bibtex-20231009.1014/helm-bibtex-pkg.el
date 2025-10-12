;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "helm-bibtex" "20231009.1014"
  "A bibliography manager based on Helm."
  '((bibtex-completion "1.0.0")
    (helm              "1.5.5")
    (cl-lib            "0.5")
    (emacs             "24.1"))
  :url "https://github.com/tmalsburg/helm-bibtex"
  :commit "d8baeaa4c69fde4a179102c8271c1db804e5155e"
  :revdesc "d8baeaa4c69f"
  :authors '(("Titus von der Malsburg" . "malsburg@posteo.de"))
  :maintainers '(("Titus von der Malsburg" . "malsburg@posteo.de")))

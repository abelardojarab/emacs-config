;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "helm-bm" "20240812.1738"
  "Helm sources for bm.el."
  '((bm     "1.0")
    (cl-lib "0.5")
    (helm   "1.9.3"))
  :url "https://github.com/emacs-helm/helm-bm"
  :commit "4744b5784df5800f36c3c54de5269034191155f5"
  :revdesc "4744b5784df5"
  :keywords '("helm" "bookmark")
  :authors '(("Yasuyuki Oka" . "yasuyk@gmail.com")
             ("Thierry Volpiatto" . "thievol@posteo.net"))
  :maintainers '(("Thierry Volpiatto" . "thievol@posteo.net")))

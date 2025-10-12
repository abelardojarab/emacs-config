;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org2blog" "20250722.223"
  "Blog from Org mode to WordPress."
  '((emacs          "29.4")
    (htmlize        "1.58")
    (hydra          "0.15.0")
    (xml-rpc        "1.6.15")
    (writegood-mode "2.2.0")
    (metaweblog     "1.1.18"))
  :url "https://github.com/org2blog/org2blog"
  :commit "d0168606e60df2267b451dfe92975ad3f5c7919c"
  :revdesc "d0168606e60d"
  :keywords '("comm" "convenience" "outlines" "wp")
  :authors '(("Puneeth Chaganti" . "punchagan+org2blog@gmail.com"))
  :maintainers '(("Grant Rettke" . "grant@wisdomandwonder.com")))

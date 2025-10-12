;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "pass" "20250721.1935"
  "Major mode for password-store.el."
  '((emacs              "25.1")
    (password-store     "1.7.4")
    (password-store-otp "0.1.5")
    (f                  "0.17"))
  :url "https://github.com/NicolasPetton/pass"
  :commit "7651389c52919f5e0e41d9217b29c7166e3a45c2"
  :revdesc "7651389c5291"
  :keywords '("tools" "files")
  :authors '(("Nicolas Petton" . "petton.nicolas@gmail.com")
             ("Damien Cassou" . "damien@cassou.me"))
  :maintainers '(("Nicolas Petton" . "petton.nicolas@gmail.com")
                 ("Damien Cassou" . "damien@cassou.me")))

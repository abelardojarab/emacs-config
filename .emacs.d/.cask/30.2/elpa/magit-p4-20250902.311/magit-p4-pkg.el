;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit-p4" "20250902.311"
  "Git-p4 plug-in for Magit."
  '((emacs       "27.1")
    (magit       "4.0.0")
    (transient   "0.8.0")
    (p4          "12.0")
    (cl-lib      "1.0")
    (with-editor "3.4.1"))
  :url "https://github.com/qoocku/magit-p4"
  :commit "19c54db7423ef87a3688b0ac1e882c2341efee84"
  :revdesc "19c54db7423e"
  :keywords '("vc" "tools")
  :authors '(("Damian T. Dobroczyński" . "qoocku@gmail.com")
             ("Aleksey Fedotov" . "lexa@cfotr.com"))
  :maintainers '(("Maciej Katafiasz" . "mathrick@gmail.com")))

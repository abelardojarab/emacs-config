;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-jira" "20251004.1853"
  "Syncing between Jira and Org-mode."
  '((emacs   "24.5")
    (cl-lib  "0.5")
    (request "0.2.0")
    (dash    "2.14.1"))
  :url "https://github.com/ahungry/org-jira"
  :commit "3f4bc7f984301b458c193c47931e8098eca2189d"
  :revdesc "3f4bc7f98430"
  :keywords '("ahungry" "jira" "org" "bug" "tracker")
  :maintainers '(("Matthew Carter" . "m@ahungry.com")))

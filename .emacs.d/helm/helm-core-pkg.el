;;; helm-pkg.el --- define helm for package.el

(define-package "helm-core" "1.7.9"
  "Development files for Helm"
  '((emacs "24")
    (cl-lib "0.5")
    (async "1.5"))
  :url "https://emacs-helm.github.io/helm/")

;; Local Variables:
;; no-byte-compile: t
;; End:

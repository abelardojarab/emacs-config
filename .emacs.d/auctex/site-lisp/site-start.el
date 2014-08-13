;; Load files in `site-start.d' directory.
(dolist (file (directory-files
	       (concat (file-name-directory load-file-name) "site-start.d")
	       t "\\.el\\'"))
  (load file nil t t))

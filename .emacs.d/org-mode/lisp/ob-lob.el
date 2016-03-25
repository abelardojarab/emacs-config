;;; ob-lob.el --- functions supporting the Library of Babel

;; Copyright (C) 2009-2016 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	 Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(eval-when-compile
  (require 'cl))
(require 'ob-core)
(require 'ob-table)

(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))

(defvar org-babel-library-of-babel nil
  "Library of source-code blocks.
This is an association list.  Populate the library by adding
files to `org-babel-lob-files'.")

(defcustom org-babel-lob-files nil
  "Files used to populate the `org-babel-library-of-babel'.
To add files to this list use the `org-babel-lob-ingest' command."
  :group 'org-babel
  :version "24.1"
  :type '(repeat file))

(defvar org-babel-default-lob-header-args '((:exports . "results"))
  "Default header arguments to use when exporting #+lob/call lines.")

(defun org-babel-lob-ingest (&optional file)
  "Add all named source blocks defined in FILE to `org-babel-library-of-babel'."
  (interactive "fFile: ")
  (let ((lob-ingest-count 0))
    (org-babel-map-src-blocks file
      (let* ((info (org-babel-get-src-block-info 'light))
	     (source-name (nth 4 info)))
	(when source-name
	  (setq source-name (intern source-name)
		org-babel-library-of-babel
		(cons (cons source-name info)
		      (assq-delete-all source-name org-babel-library-of-babel))
		lob-ingest-count (1+ lob-ingest-count)))))
    (message "%d src block%s added to Library of Babel"
	     lob-ingest-count (if (> lob-ingest-count 1) "s" ""))
    lob-ingest-count))

;; Functions for executing lob one-liners.

;;;###autoload
(defun org-babel-lob-execute-maybe ()
  "Execute a Library of Babel source block, if appropriate.
Detect if this is context for a Library Of Babel source block and
if so then run the appropriate source block from the Library."
  (interactive)
  (let ((info (org-babel-lob-get-info)))
    (when info
      (org-babel-lob-execute info)
      t)))

;;;###autoload
(defun org-babel-lob-get-info (&optional datum)
  "Return a Library of Babel function call as a string.
Return nil when not on an appropriate location.  Build string
from `inline-babel-call' or `babel-call' DATUM, when provided."
  (let* ((context (or datum (org-element-context)))
	 (type (org-element-type context)))
    (when (memq type '(babel-call inline-babel-call))
      (list (format "%s%s(%s)"
		    (org-element-property :call context)
		    (let ((in (org-element-property :inside-header context)))
		      (if in (format "[%s]" in) ""))
		    (or (org-element-property :arguments context) ""))
	    (org-element-property :end-header context)
	    (org-element-property :name context)
	    (org-element-property
	     (if (eq type 'babel-call) :post-affiliated :begin)
	     datum)))))

(defvar org-babel-default-header-args:emacs-lisp) ; Defined in ob-emacs-lisp.el
(defun org-babel-lob-execute (info)
  "Execute the lob call specified by INFO."
  (let* ((mkinfo (lambda (p)
		   ;; Make plist P compatible with
		   ;; `org-babel-get-src-block-info'.
		   (list
		    "emacs-lisp" "results" p nil (nth 2 info) (nth 3 info))))
	 (pre-params
	  (apply #'org-babel-merge-params
		 org-babel-default-header-args
		 org-babel-default-header-args:emacs-lisp
		 (append
		  (org-babel-params-from-properties)
		  (list
		   (org-babel-parse-header-arguments
		    (org-no-properties
		     (concat ":var results="
			     (mapconcat #'identity (butlast info 2) " "))))))))
	 (pre-info (funcall mkinfo pre-params))
	 (cache-p (and (cdr (assoc :cache pre-params))
		       (string= "yes" (cdr (assoc :cache pre-params)))))
	 (new-hash (when cache-p
		     (org-babel-sha1-hash
		      ;; Do *not* pre-process params for call line
		      ;; hash evaluation, since for a call line :var
		      ;; extension *is* execution.
		      (let* ((params (nth 2 pre-info))
			     (sha1-nth2 (list
				    (cons
				     (cons :c-var (cdr (assoc :var params)))
				     (assq-delete-all :var (copy-tree params)))))
			     (sha1-info (copy-tree pre-info)))
			(prog1 sha1-info
			  (setcar (cddr sha1-info) sha1-nth2))))))
	 (old-hash (when cache-p (org-babel-current-result-hash pre-info)))
	 (org-babel-current-src-block-location (point-marker)))
    (if (and cache-p (equal new-hash old-hash))
	(save-excursion (goto-char (org-babel-where-is-src-block-result
				    nil pre-info))
			(forward-line 1)
			(message "%S" (org-babel-read-result)))
      (prog1 (let* ((proc-params (org-babel-process-params pre-params))
		     org-confirm-babel-evaluate)
	       (org-babel-execute-src-block nil (funcall mkinfo proc-params)))
	;; update the hash
	(when new-hash
	  (org-babel-set-current-result-hash new-hash pre-info))))))

(provide 'ob-lob)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ob-lob.el ends here

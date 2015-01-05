;;; helm-dash.el --- Offline documentation browser for +150 APIs using Dash docsets.
;; Copyright (C) 2013-2014  Raimon Grau
;; Copyright (C) 2013-2014  Toni Reina

;; Author: Raimon Grau <raimonster@gmail.com>
;;         Toni Reina  <areina0@gmail.com>
;;
;; URL: http://github.com/areina/helm-dash
;; Version: 1.1
;; Package-Requires: ((helm "0.0.0") (cl-lib "0.5"))
;; Keywords: docs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Clone the functionality of dash using helm foundation.  Browse
;; documentation via dash docsets.
;;
;; M-x helm-dash
;; M-x helm-dash-at-point
;;
;; More info in the project site https://github.com/areina/helm-dash
;;
;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-match-plugin)
(require 'json)
(require 'xml)

(defgroup helm-dash nil
  "Search Dash docsets using helm."
  :prefix "helm-dash-"
  :group 'applications)

(defcustom helm-dash-docsets-path
  (let ((original-dash-path (expand-file-name "~/Library/Application Support/Dash/DocSets")))
    (if (and (string-equal system-type 'darwin)
             (file-directory-p original-dash-path))
        original-dash-path
      (expand-file-name "~/.docsets")))
  "Default path for docsets.
If you're setting this option manually, set it to an absolute
path.  You can use `expand-file-name' function for that."
  :set (lambda (opt val) (set opt (expand-file-name val)))
  :group 'helm-dash)

(defcustom helm-dash-docsets-url "https://raw.github.com/Kapeli/feeds/master"
  "Feeds URL for dash docsets." :group 'helm-dash)

(defcustom helm-dash-min-length 3
  "Minimum length to start searching in docsets.
0 facilitates discoverability, but may be a bit heavy when lots
of docsets are active.  Between 0 and 3 is sane."
  :group 'helm-dash)

(defvar helm-dash-common-docsets
  '() "List of Docsets to search active by default.")


(defun helm-dash-docset-path (docset)
  "Return the full path of the directory for DOCSET."
  (let ((top-level (format "%s/%s.docset" (helm-dash-docsets-path) docset))
        (nested (format "%s/%s/%s.docset" (helm-dash-docsets-path) docset docset)))
    (if (file-directory-p top-level)
        top-level
      nested)))

(defun helm-dash-docset-db-path (docset)
  "Compose the path to sqlite DOCSET."
  (expand-file-name "Contents/Resources/docSet.dsidx" (helm-dash-docset-path docset)))

(defvar helm-dash-connections nil
  "List of conses like (\"Go\" . connection).")

(defcustom helm-dash-browser-func 'browse-url
  "Default function to browse Dash's docsets.
Suggested values are:
 * `browse-url'
 * `eww'"
  :group 'helm-dash)

(defun helm-dash-docsets-path ()
  "Return the path where Dash's docsets are stored."
  (expand-file-name helm-dash-docsets-path))

(defun helm-dash-sql (db-path sql)
  ""
  (mapcar (lambda (x) (split-string x "|" t))
          (split-string
           (with-output-to-string
             (call-process-shell-command
              (format "sqlite3 \"%s\" \"%s\"" db-path sql) nil standard-output)) "\n" t)))

(defun helm-dash-filter-connections ()
  "Filter connections using `helm-dash-connections-filters'."
  (let ((docsets (helm-dash-buffer-local-docsets))
        (connections nil))
    (setq docsets (append docsets helm-dash-common-docsets))
    (delq nil (mapcar (lambda (y)
                        (assoc y helm-dash-connections))
             docsets))))

(defun helm-dash-buffer-local-docsets ()
  "Get the docsets configured for the current buffer."
  (with-helm-current-buffer
    (or (and (boundp 'helm-dash-docsets) helm-dash-docsets)
        '())))

(defun helm-dash-create-common-connections ()
  "Create connections to sqlite docsets for common docsets."
  (when (not helm-dash-connections)
    (setq helm-dash-connections
          (mapcar (lambda (x)
                    (let ((db-path (helm-dash-docset-db-path x)))
                      (list x db-path (helm-dash-docset-type db-path))))
                  helm-dash-common-docsets))))

(defun helm-dash-create-buffer-connections ()
  "Create connections to sqlite docsets for buffer-local docsets."
  (mapc (lambda (x) (when (not (assoc x helm-dash-connections))
                 (let ((connection  (helm-dash-docset-db-path x)))
                   (setq helm-dash-connections
                         (cons (list x connection (helm-dash-docset-type connection))
                               helm-dash-connections)))))
        (helm-dash-buffer-local-docsets)))

(defun helm-dash-reset-connections ()
  "Wipe all connections to docsets."
  (interactive)
  (setq helm-dash-connections nil))

(defun helm-dash-docset-type (db-path)
  "Return the type of the docset based in db schema.
Possible values are \"DASH\" and \"ZDASH\.
The Argument DB-PATH should be a string with the sqlite db path."
  (let ((type_sql "SELECT name FROM sqlite_master WHERE type = 'table' LIMIT 1"))
    (if (member "searchIndex" (car (helm-dash-sql db-path type_sql)))
	"DASH"
      "ZDASH")))

(defun helm-dash-search-all-docsets ()
  "Fetch docsets from the original Kapeli's feed."
  (let ((url "https://api.github.com/repos/Kapeli/feeds/contents/"))
    (with-current-buffer
        (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defvar helm-dash-ignored-docsets
  '("Bootstrap" "Drupal" "Zend_Framework" "Ruby_Installed_Gems" "Man_Pages")
  "Return a list of ignored docsets.
These docsets are not available to install.
See here the reason: https://github.com/areina/helm-dash/issues/17.")

(defun helm-dash-available-docsets ()
  "Return a list of official docsets (http://kapeli.com/docset_links)."
  (delq nil (mapcar (lambda (docset)
                      (let ((name (assoc-default 'name (cdr docset))))
                        (if (and (equal (file-name-extension name) "xml")
                                 (not
                                  (member (file-name-sans-extension name) helm-dash-ignored-docsets)))
                            (file-name-sans-extension name))))
                    (helm-dash-search-all-docsets))))

(defun helm-dash-installed-docsets ()
  "Return a list of installed docsets."
  (let ((docset-path (helm-dash-docsets-path)))
    (cl-loop for dir in (directory-files docset-path)
             for full-path = (expand-file-name dir docset-path)
             when (or (string-match-p "\\.docset\\'" dir)
                      (file-directory-p (expand-file-name (format "%s.docset" dir) full-path)))
             collecting (replace-regexp-in-string "\\.docset\\'" "" dir))))

(defun helm-dash-read-docset (prompt choices)
  "PROMPT user to choose one of the docsets in CHOICES.
Report an error unless a valid docset is selected."
  (let ((completion-ignore-case t))
    (completing-read (format "%s (%s): " prompt (car choices))
                     choices nil t nil nil choices)))

(defun helm-dash-activate-docset (docset)
  "Activate DOCSET.  If called interactively prompts for the docset name."
  (interactive (list (helm-dash-read-docset
                      "Activate docset"
                      (helm-dash-installed-docsets))))
  (add-to-list 'helm-dash-common-docsets docset)
  (helm-dash-reset-connections))

;;;###autoload
(defun helm-dash-install-docset (docset-name)
  "Download docset with specified DOCSET-NAME and move its stuff to docsets-path."
  (interactive (list (helm-dash-read-docset
                      "Install docset"
                      (helm-dash-available-docsets))))
  (let ((feed-url (format "%s/%s.xml" helm-dash-docsets-url docset-name))
        (docset-tmp-path (format "%s%s-docset.tgz" temporary-file-directory docset-name))
        (feed-tmp-path (format "%s%s-feed.xml" temporary-file-directory docset-name))
        (docset-path (helm-dash-docsets-path))
        )
    (url-copy-file feed-url feed-tmp-path t)
    (url-copy-file (helm-dash-get-docset-url feed-tmp-path) docset-tmp-path t)

    (when (and (not (file-directory-p docset-path))
	       (y-or-n-p (format "Directory %s does not exist.  Want to create it? "
				 docset-path)))
      (mkdir docset-path t))
    (let ((docset-folder
	   (helm-dash-docset-folder-name
	    (shell-command-to-string (format "tar xvf %s -C %s" docset-tmp-path (helm-dash-docsets-path))))))
      (helm-dash-activate-docset docset-folder)
      (message (format
		"Docset installed. Add \"%s\" to helm-dash-common-docsets or helm-dash-docsets."
		docset-folder)))))

(defalias 'helm-dash-update-docset 'helm-dash-install-docset)

(defun helm-dash-docset-folder-name (tar-output)
  "Return the name of the folder where the docset has been extracted.
The argument TAR-OUTPUT should be an string with the output of a tar command."
  (let ((last-line
	 (car (last (split-string tar-output "\n" t)))))
    (replace-regexp-in-string "^x " "" (car (split-string last-line "\\." t)))))

(defun helm-dash-get-docset-url (feed-path)
  "Parse a xml feed with docset urls and return the first url.
The Argument FEED-PATH should be a string with the path of the xml file."
  (let* ((xml (xml-parse-file feed-path))
         (urls (car xml))
         (url (xml-get-children urls 'url)))
    (cl-caddr (cl-first url))))

(defvar helm-dash-sql-queries
  '((DASH . ((select . (lambda (pattern)
                         (let ((like (helm-dash-sql-compose-like "t.name" pattern))
                               (query "SELECT t.type, t.name, t.path FROM searchIndex t WHERE %s ORDER BY LOWER(t.name) LIMIT 100"))
                           (format query like))))))
    (ZDASH . ((select . (lambda (pattern)
                          (let ((like (helm-dash-sql-compose-like "t.ZTOKENNAME" pattern))
                                (query "SELECT ty.ZTYPENAME, t.ZTOKENNAME, f.ZPATH, m.ZANCHOR FROM ZTOKEN t, ZTOKENTYPE ty, ZFILEPATH f, ZTOKENMETAINFORMATION m WHERE ty.Z_PK = t.ZTOKENTYPE AND f.Z_PK = m.ZFILE AND m.ZTOKEN = t.Z_PK AND %s ORDER BY LOWER(t.ZTOKENNAME) LIMIT 100"))
                            (format query like))))))))

(defun helm-dash-sql-compose-like (column pattern)
  ""
  (let ((conditions (mapcar (lambda (word) (format "%s like '%%%s%%'" column word))
                            (split-string pattern " "))))
    (format "%s" (mapconcat 'identity conditions " AND "))))

(defun helm-dash-sql-execute (query-type docset-type pattern)
  ""
  (funcall (cdr (assoc query-type (assoc (intern docset-type) helm-dash-sql-queries))) pattern))

(defun helm-dash-maybe-narrow-to-one-docset (pattern)
  "Return a list of helm-dash-connections.
If PATTERN starts with the name of a docset followed by a space, narrow the
 used connections to just that one.  We're looping on all connections, but it
 shouldn't be a problem as there won't be many."
  (let ((conns (helm-dash-filter-connections)))
    (or (cl-loop for x in conns
                 if (string-prefix-p
                     (concat (downcase (car x)) " ")
                     (downcase pattern))
                 return (list x))
        conns)))

(defun helm-dash-sub-docset-name-in-pattern (pattern docset-name)
  "Remove from PATTERN the DOCSET-NAME if this includes it.
If the search starts with the name of the docset, ignore it.
Ex: This avoids searching for redis in redis unless you type 'redis redis'"
  (replace-regexp-in-string
   (format "^%s " (regexp-quote (downcase docset-name)))
   ""
   pattern))

(defun helm-dash-search ()
  "Iterates every `helm-dash-connections' looking for the `helm-pattern'."
  (let ((full-res (list))
        (connections (helm-dash-maybe-narrow-to-one-docset helm-pattern)))

    (dolist (docset connections)
      (let* ((docset-type (cl-caddr docset))
             (res
	      (helm-dash-sql
	       (cadr docset)
	       (helm-dash-sql-execute 'select
				      docset-type
				      (helm-dash-sub-docset-name-in-pattern helm-pattern (car docset))))))
        ;; how to do the appending properly?
        (setq full-res
              (append full-res
                      (mapcar (lambda (x)
                                (cons (format "%s %s"  (car docset) (cadr x)) (list (car docset) x)))
                              res)))))
    full-res))

(defun helm-dash-result-url (docset-name filename &optional anchor)
  "Return the full, absolute URL to documentation: either a file:// URL joining
DOCSET-NAME, FILENAME & ANCHOR with sanitization of spaces or a http(s):// URL
formed as-is if FILENAME is a full HTTP(S) URL."
  (let ((path (format "%s%s" filename (if anchor (format "#%s" anchor) ""))))
    (if (string-match-p "^https?://" path)
        path
      (replace-regexp-in-string
       " "
       "%20"
       (concat
        "file://"
        (expand-file-name "Contents/Resources/Documents/" (helm-dash-docset-path docset-name))
        path)))))

(defun helm-dash-browse-url (search-result)
  "Call to `browse-url' with the result returned by `helm-dash-result-url'.
Get required params to call `helm-dash-result-url' from SEARCH-RESULT."
  (let ((docset-name (car search-result))
	(filename (nth 2 (cadr search-result)))
	(anchor (nth 3 (cadr search-result))))
    (funcall helm-dash-browser-func (helm-dash-result-url docset-name filename anchor))))

(defun helm-dash-add-to-kill-ring (search-result)
  "Add to kill ring a formatted string to call `helm-dash-browse-url' with SEARCH-RESULT."
  (kill-new (format "(helm-dash-browse-url '%s)" search-result)))

(defun helm-dash-actions (actions doc-item)
  "Return an alist with the possible actions to execute with DOC-ITEM."
  `(("Go to doc" . helm-dash-browse-url)
    ("Copy to clipboard" . helm-dash-copy-to-clipboard)))

(defun helm-source-dash-search ()
  "Return an alist with configuration options for Helm."
  `((name . "Dash")
    (volatile)
    (delayed)
    (requires-pattern . ,helm-dash-min-length)
    (candidates-process . helm-dash-search)
    (action-transformer . helm-dash-actions)))

;;;###autoload
(defun helm-dash ()
  "Bring up a Dash search interface in helm."
  (interactive)
  (helm-dash-create-common-connections)
  (helm-dash-create-buffer-connections)
  (helm :sources (list (helm-source-dash-search))
	:buffer "*helm-dash*"))

;;;###autoload
(defun helm-dash-at-point ()
  "Bring up a Dash search interface in helm using the symbol at
point as prefilled search."
  (interactive)
  (helm-dash-create-common-connections)
  (helm-dash-create-buffer-connections)
  (helm :sources (list (helm-source-dash-search))
	:buffer "*helm-dash*"
	:input (thing-at-point 'symbol)))

(provide 'helm-dash)

;;; helm-dash.el ends here

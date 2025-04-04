;;; openalex.el --- Org-ref interface to OpenAlex 

;;; Commentary:
;; This is an elisp interface to OpenAlex (https://docs.openalex.org/) for org-ref.
;;
;; This provides functionality for the OpenAlex APIs.
;;
;; `oa-query' provides a general interface to all the endpoints with a filter.
;; It is not interactive though.
;;
;; `oa-author' provides an interactive way to search for an author and then see
;; a Google Scholar like org buffer with information about the author.
;;
;; `oa-fulltext-search' provides an interactive full text search of Works in
;; OpenAlex. You get an org-buffer of results, with links to subsequent pages of
;; results.
;;
;; `oa-coa' is an interactive command to generate the NSF COA form data for
;; coauthors.
;;
;; `oa-get-bibtex-entries' is an interactive command to download all bibtex
;; entries from headings in the current buffer with a DOI property.
;;
;; if you have an OpenAlex API you can set `oa-api-key' to use it. The
;; `user-mail-address' value will be added to the queries if it exists so you
;; will get the polite pool.
;;
;; This library extends the `org-ref-citation-hydra' and adds keys to get to
;; cited by, references and related documents in OpenAlex.

(require 'dash)
(require 's)
(require 'request)
(require 'doi-utils)
(require 'org-ref-citation-links)

(declare-function org-ref-possible-bibfiles "org-ref-core")
(declare-function org-ref-find-bibliography "org-ref-core")
(declare-function org-ref-get-bibtex-key-and-file "org-ref-core")
(declare-function bibtex-completion-show-entry "bibtex-completion")
(declare-function bibtex-completion-apa-format-reference "bibtex-completion")
(declare-function ivy-read "ivy")

(defcustom oa-api-key
  nil
  "Your API key if you have one."
  :group 'openalex
  :type 'string)

;;; Code:

(defun oa--response-parser ()
  "Parse the response from json to elisp."
  (let ((json-array-type 'list)
	(json-object-type 'plist)
	(json-key-type 'keyword)
	(json-false nil)
	(json-encoding-pretty-print nil))
    (json-read)))


(defun oa-get (data query &optional iterable)
  "Get fields from DATA with QUERY.
QUERY is a dot notation string.
key1.key2.key3 represents a nested item.
key1.key2[].key3 represents key3 on all items in key1.key2.

Tested with up to two [] in the query.

Assumes data is in plist form."
  (let* ((fields (and query (split-string query "\\.")))
	 (current-field (and fields (pop fields))))

    (cond
     ;; return condition
     ((null query)
      data)

     ;; query[] means get query then turn iteration on
     ((and (s-ends-with-p "[]" current-field) (null iterable))
      (setq current-field (substring current-field 0 -2))
      (oa-get (plist-get data (intern-soft (concat ":" current-field)))
	      (when fields
		(string-join fields "."))
	      t))

     ;; this means another level of iteration. You already have a collection. we
     ;; have to iterate over each one I think.
     ((and (s-ends-with-p "[]" current-field) iterable)
      (setq current-field (substring current-field 0 -2))
      (cl-loop for item in data collect
	       (oa-get (plist-get item (intern-soft (concat ":" current-field)))
		       (when fields
			 (string-join fields "."))
		       t)))

     ;; single keyword, iterate over collection
     (iterable
      (oa-get (cl-loop for item in data collect
		       (plist-get item (intern-soft (concat ":" current-field))))
	      (when fields
		(string-join fields "."))
	      t))

     ;; single keyword
     (t
      (oa-get (plist-get data (intern-soft (concat ":" current-field)))
	      (when fields
		(string-join fields ".")))))))


;; * General query
(defun oa--query-all-data (endpoint &rest filter)
  (let* ((page 1)
	 (url (concat "https://api.openalex.org/" endpoint))
	 (filter-string (string-join (cl-loop for key in (plist-get-keys filter) collect
					      (concat (substring (symbol-name key) 1)
						      ":"
						      (url-hexify-string
						       (plist-get filter key))))
				     ","))
	 (params `(("mailto" . ,user-mail-address)
		   ("api_key" . ,oa-api-key)
		   ("filter" . ,filter-string)
		   ("page" . ,page)))
	 
	 (req (request url :sync t :parser 'oa--response-parser :params params))
	 (data (request-response-data req))
	 (meta (plist-get data :meta))
	 (count (plist-get meta :count))
	 (per-page (plist-get meta :per_page))
	 (pages (ceiling (/ (float count) per-page)))
	 (results (plist-get data :results)))
    (cl-loop for i from 2 to pages do
	     (setf (cdr (assoc "page" params)) i)
	     (setq req (request purl :sync t :parser 'oa--response-parser :params params)
		   data (request-response-data req)
		   results (append results (plist-get data :results))))
    results))


(defun oa-query (endpoint &rest filter)
  "Run a query at ENDPOINT with FILTER.
ENDPOINT can be works, authors, sources, institutions, concepts,
publishers, or funders.

FILTER is a plist (:field value) where :field is a valid filter
field (see
https://docs.openalex.org/how-to-use-the-api/get-lists-of-entities/filter-entity-lists),
and value is what you want to filter on. The following logic is supported:

!value is negation
<value is less than
>value is greater than
value1+value2 is and within a field
value1|value2 is or within a field

Your email address will be added if `user-mail-address' is
non-nil, and `oa-api-key' if it is non-nil to the API url."
  (let* ((page (if (plist-get filter :page)
		   (prog1
		       (string-to-number (plist-get filter :page))
		     (setq filter (org-plist-delete filter :page)))
		 1))
	 (base-url "https://api.openalex.org")
	 (url (concat base-url "/" endpoint "?filter="))
	 (filter-string (string-join
			 (cl-loop for key in (plist-get-keys filter) collect
				  (concat (substring (symbol-name key) 1)
					  ":"
					  (url-hexify-string
					   (plist-get filter key))))
			 ","))
	 
	 (req (request url :sync t :parser 'oa--response-parser
		:params `(("mailto" . ,user-mail-address)
			  ("api_key" . ,oa-api-key)
			  ("filter" . ,filter-string)
			  ("page" . ,page))))
	 (data (request-response-data req))
	 (meta (plist-get data :meta))
	 (count (plist-get meta :count))
	 (per-page (plist-get meta :per_page))
	 (pages (ceiling (/ (float count) per-page)))
	 (results (plist-get data :results))
	 (next-page (format "[[elisp:(oa-query \"%s\" %s :page \"%s\")][Next page: %s]]"
			    endpoint 
			    (string-join (cl-loop for x in filter
						  collect
						  (if (keywordp x)
						      (format "%s" x)
						    (format "%S" x)))
					 " ")
			    (+ page 1)
			    (+ page 1)))
	 (buf (generate-new-buffer "*OpenAlex - Query*")))
    
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert (concat
	       (format "#+title: %s
** Results
:PROPERTIES:
:FILTER: %s
:COUNT: %s
:END:

%s
\n\n"
		       filter-string
		       filter-string
		       count
		       next-page)
	       (cond
		((string= endpoint "works")
		 (string-join
		  (cl-loop for wrk in results collect
			   (s-format "*** ${title}
:PROPERTIES:
:HOST: ${primary_location.source.display_name}
:YEAR: ${publication_year}
:CITED_BY_COUNT: ${cited_by_count}
:AUTHOR: ${authors}
:DOI: ${doi}
:OPENALEX: ${id}
:CREATED_DATE: ${created_date}
:END:


${get-bibtex}

- ${oa-refs}
- ${oa-related}
- ${oa-cited}

${abstract}

"
				     (lambda (key data)
				       (or (cdr (assoc key data)) ""))
				     `(("title" . ,(oa--title wrk))
				       ("primary_location.source.display_name" . ,(oa-get wrk "primary_location.source.display_name"))
				       ("publication_year" . ,(oa-get wrk "publication_year"))
				       ("cited_by_count" . ,(oa-get wrk "cited_by_count"))
				       ("authors" . ,(oa--authors wrk))
				       ("doi" . ,(oa-get wrk "doi"))
				       ("id" . ,(oa-get wrk "id"))
				       ("created_date" . ,(oa-get wrk "created_date"))
				       ("get-bibtex" . ,(oa--elisp-get-bibtex wrk))
				       ("oa-refs" . ,(oa--elisp-get-oa-refs wrk))
				       ("oa-related" . ,(oa--elisp-get-oa-related wrk))
				       ("oa-cited" . ,(oa--elisp-get-oa-cited-by wrk))
				       ("abstract" . ,(oa--abstract wrk)))))
		  "\n"))
		(t
		 (format "%s" results)
		 )))))
    (pop-to-buffer buf)
    (goto-char (point-min))))


;; * Work object

(defun oa--work (entity-id &optional filter)
  "Retrieve json data for a Work object for ENTITY-ID.
ENTITY-ID is an OpenAlex ID, DOI, Pubmed id,etc.

ENTITY-ID may also be a list of ids with a filter.

If FILTER is non-nil it should be a string like \"filter=openalex:\"

https://docs.openalex.org/api-entities/works"

  (let* ((url (concat "https://api.openalex.org/works"
		      ;; This is hackier than I prefer, but sometimes entity-id
		      ;; is nil, or starts with ? for a filter, and I couldn't
		      ;; see a cleaner way to solve this. this function is used
		      ;; in a lot of places.
		      (cond
		       ((string-prefix-p "?" entity-id)
			entity-id)
		       (t
			(format "/%s" entity-id)))))
	 (req (request url :sync t :parser 'oa--response-parser
		:params `(("mailto" . ,user-mail-address)
			  ("api_key" . ,oa-api-key)
			  ("filter" . ,filter))))
	 (data (request-response-data req)))
    ;; this is for convenience to inspect data in a browser, e.g. you can click
    ;; on the url in Emacs and it opens in a browser.
    (plist-put data :oa-url url)
    data))


;; * autocomplete works

(defun oa--works-candidates (query)
  "Retrieve autocomplete works from OpenAlex."
  (or
   (ivy-more-chars)
   (let* ((url "https://api.openalex.org/autocomplete/works")
	  (req (request url :sync t :parser 'oa--response-parser
		 :params `(("mailto" . ,user-mail-address)
			   ("api_key" . ,oa-api-key)
			   ("q" . ,query))))
	  (data (request-response-data req))
	  (results (plist-get data :results)))
     (cl-loop for work in results collect
	      (propertize
	       (format "%s, %s"
		       (plist-get work :display_name)
		       (plist-get work :hint))
	       'oaid (plist-get work :id))))))


(defun oa-works ()
  "Autocomplete works.
This doesn't seem as useful as it could be."
  (interactive)
  
  (ivy-read "Work: " #'oa--works-candidates
	    :dynamic-collection t
	    :action
	    '(1 
	      ("o" (lambda (candidate)
		     (browse-url (get-text-property 0 'oaid candidate)))
	       "Open in browser"))))

;; * Viewing works


;; ** help functions for complex data
;;
;; Some things like authors need to be constructed, and cannot just be looked
;; up. In other cases, I want logic, e.g. if data is there provide something,
;; and if not return an empty string. These functions do that work.
(defun oa--authors (wrk)
  "Return an author string for WRK.
The string is a comma-separated list of links to author pages in OpenAlex."
  (s-join ", " (cl-loop for author in (plist-get wrk :authorships)
			collect
			(format "[[elisp:(oa--author-org \"%s\")][%s]]"
				(plist-get
				 (plist-get author :author)
				 :id)
				(plist-get
				 (plist-get author :author)
				 :display_name)))))


(defun oa--title (wrk)
  "Return a title from WRK with linebreaks removed."
  (string-replace "\n" " " (plist-get wrk :title)))


;; I want some links if they can be made so the buffer is interactive. It might
;; be nice to integrate M-, navigation.
(defun oa--elisp-get-bibtex (wrk)
  "Return a elisp link to get a bibtex entry for WRK if there is a doi."
  (if-let ((doi (plist-get wrk :doi)))
      (format "[[elisp:(doi-add-bibtex-entry \"%s\")][Get bibtex entry]]" doi)
    ""))


(defun oa--elisp-get-oa-related (wrk)
  "Return a elisp link to get related works for WRK."
  (format "[[elisp:(progn (xref--push-markers (current-buffer) (point)) (oa--related-works \"%s\"))][Get related work (%s)]]"
	  (plist-get wrk :id)
	  (length (plist-get wrk :related_works))))


(defun oa--elisp-get-oa-refs (wrk)
  "Return a elisp link to get references for WRK."
  (format "[[elisp:(progn (xref--push-markers (current-buffer) (point)) (oa--referenced-works \"%s\"))][Get references (%s)]]"
	  (plist-get wrk :id)
	  (length  (plist-get wrk :referenced_works))))


(defun oa--elisp-get-oa-cited-by (wrk)
  "Return a elisp link to get works that cite WRK."
  (format "[[elisp:(progn (xref--push-markers (current-buffer) (point)) (oa--cited-by-works \"%s\"))][Get cited by (%s)]]"
	  (plist-get wrk :id)
	  (plist-get wrk :cited_by_count)))


(defun oa--works-entries (works)
  "Return a list of org-formatted entries in WORKS.
WORKS is a list of results from OpenAlex."
  (cl-loop for wrk in (plist-get works :results)
	   collect
	   (s-format "** ${title}
:PROPERTIES:
:HOST: ${primary_location.source.display_name}
:YEAR: ${publication_year}
:CITED_BY_COUNT: ${cited_by_count}
:AUTHOR: ${authors}
:DOI: ${doi}
:OPENALEX: ${id}
:END:


${get-bibtex}

- ${oa-refs}
- ${oa-related}
- ${oa-cited}

"
		     (lambda (key data)
		       (or (cdr (assoc key data)) ""))
		     `(("title" . ,(oa-get wrk "title"))
		       ("primary_location.source.display_name" . ,(oa-get wrk "primary_location.source.display_name"))
		       ("publication_year" . ,(oa-get wrk "publication_year"))
		       ("cited_by_count" . ,(oa-get wrk "cited_by_count"))
		       ("authors" . ,(oa--authors wrk))
		       ("doi" . ,(oa-get wrk "doi"))
		       ("id" . ,(oa-get wrk "id"))
		       ("get-bibtex" . ,(oa--elisp-get-bibtex wrk))
		       ("oa-refs" . ,(oa--elisp-get-oa-refs wrk))
		       ("oa-related" . ,(oa--elisp-get-oa-related wrk))
		       ("oa-cited" . ,(oa--elisp-get-oa-cited-by wrk))))))


(defun oa--works-buffer (bufname header entries)
  "Create an org-buffer with BUFNAME representing the results in WORKS.
HEADER is the first thing in the buffer
WORKS is usually a list of results from OpenAlex.
Argument ENTRIES A list of strings for each org entry."
  (let ((buf (get-buffer-create bufname)))
    
    (with-current-buffer buf
      (erase-buffer)
      (insert header)
      (insert "#+COLUMNS: %25ITEM %YEAR %CITED_BY_COUNT
elisp:org-columns    elisp:org-columns-quit


#+caption: Sort
| year     | [[elisp:(oa-buffer-sort-year t)][old first]] | [[elisp:(oa-buffer-sort-year)][new first]] |
| cited by | [[elisp:(oa-buffer-sort-cited-by-count t)][low first]] | [[elisp:(oa-buffer-sort-cited-by-count)][high first]] |

")
      (insert (s-join "\n" entries))
      (org-mode)
      (goto-char (point-min))
      (org-next-visible-heading 1))
    ;; (display-buffer-in-side-window buf '((side . right)))
    (pop-to-buffer buf)))


(defun oa--related-works (entity-id)
  "Show the Related works buffer for ENTITY-ID."
  (let* ((wrk (oa--work entity-id))
	 (related-work (plist-get wrk :related_works))
	 split
	 entries)
    (while related-work
      (setq split (-split-at 25 related-work)
	    related-work (nth 1 split))
      
      ;; split is what we process now
      (setq entries (append entries
			    (oa--works-entries
			     (oa--work (format "?filter=openalex:%s" (s-join "|" (nth 0 split))))))))
    
    (oa--works-buffer
     "*OpenAlex - Related works*"
     (format "* OpenAlex - Related works for %s ([[%s][json]])
%s\n\n"
	     entity-id
	     (plist-get wrk :oa-url)
	     (s-format ":PROPERTIES:
:TITLE: ${title}
:HOST: ${primary_location.source.display_name}
:AUTHOR: ${authors}
:DOI: ${doi}
:YEAR: ${publication_year}
:OPENALEX: ${id}
:END:

Found ${nentries} results.
"
		       (lambda (key data)
			 (or (cdr (assoc key data)) ""))
		       `(("title" . ,(oa-get wrk "title"))
			 ("primary_location.source.display_name" . ,(oa-get wrk "primary_location.source.display_name"))
			 ("authors" . ,(oa--authors wrk))
			 ("doi" . ,(oa-get wrk "doi"))
			 ("publication_year" . ,(oa-get wrk "publication_year"))
			 ("id" . ,(oa-get wrk "id"))
			 ("nentries" . ,(length entries)))))
     entries)))


(defun oa--referenced-works (entity-id)
  "Show the Referenced work for ENTITY-ID."
  (let* ((wrk (oa--work entity-id))
	 (referenced-work (plist-get wrk :referenced_works))
	 split
	 (entries '()))
    (while referenced-work
      (setq split (-split-at 25 referenced-work)
	    referenced-work (nth 1 split))
      ;; split is what we process now
      (setq entries (append entries
			    (oa--works-entries
			     (oa--work (format "?filter=openalex:%s"    
					       (s-join "|" (nth 0 split))))))))
    (oa--works-buffer
     "*OpenAlex - References*"
     (format "* OpenAlex - References from %s ([[%s][json]])
%s\n\n"
	     entity-id
	     (plist-get wrk :oa-url)
	     (s-format ":PROPERTIES:
:TITLE: ${title}
:HOST: ${primary_location.source.display_name}
:AUTHOR: ${authors}
:DOI: ${doi}
:YEAR: ${publication_year}
:OPENALEX: ${id}
:END:

Found ${nentries} results.
"
		       (lambda (key data)
			 (or (cdr (assoc key data)) ""))
		       `(("title" . ,(oa-get wrk "title"))
			 ("primary_location.source.display_name" . ,(oa-get wrk "primary_location.source.display_name"))
			 ("authors" . ,(oa--authors wrk))
			 ("doi" . ,(oa-get wrk "doi"))
			 ("publication_year" . ,(oa-get wrk "publication_year"))
			 ("id" . ,(oa-get wrk "id"))
			 ("nentries" . ,(length entries)))))
     entries)))


;; This function is different than the previous two. First we follow a URL
;; provided by the data, and second, here we do follow pages.
(defun oa--cited-by-works (entity-id)
  "Show the Cited by buffer for ENTITY-ID."
  (let* ((wrk (oa--work entity-id))
	 (url (plist-get wrk :cited_by_api_url))
	 (cited-by-works (request-response-data
			  (request url
			    :sync t
			    :parser 'oa--response-parser
			    :params `(("mailto" . ,user-mail-address)
				      ("api_key" . ,oa-api-key)))))
	 (count (plist-get (plist-get cited-by-works :meta) :count))
	 (per-page (plist-get (plist-get cited-by-works :meta) :per_page))
	 (entries '())
	 (page 2))
    ;; get first page
    (setq entries (oa--works-entries cited-by-works))
    (while (> count (* per-page (- page 1)))
      (setq cited-by-works (request-response-data
			    (request url
			      :sync t
			      :parser 'oa--response-parser
			      :params `(("mailto" . ,user-mail-address)
					("api_key" . ,oa-api-key)
					("page" . ,page)))))
      (setq entries (append entries (oa--works-entries cited-by-works)))
      (cl-incf page))
    
    (oa--works-buffer
     "*OpenAlex - Cited by*"
     (format "* OpenAlex - %s Cited by ([[%s][json]])
%s"
	     entity-id
	     url
	     (s-format ":PROPERTIES:
:TITLE: ${title}
:HOST: ${primary_location.source.display_name}
:AUTHOR: ${authors}
:DOI: ${doi}
:YEAR: ${publication_year}
:OPENALEX: ${id}
:END:

Found ${nentries} results.

"
		       (lambda (key data)
			 (or (cdr (assoc key data)) ""))
		       `(("title" . ,(oa-get wrk "title"))
			 ("primary_location.source.display_name" . ,(oa-get wrk "primary_location.source.display_name"))
			 ("authors" . ,(oa--authors wrk))
			 ("doi" . ,(oa-get wrk "doi"))
			 ("publication_year" . ,(oa-get wrk "publication_year"))
			 ("id" . ,(oa-get wrk "id"))
			 ("nentries" . ,(length entries)))))
     entries)))


;; ** buffer utilities for sorting entries

(defun oa-buffer-sort-year (&optional ascending)
  "Sort org headings by year in descending order (new to old).
With prefix arg ASCENDING, sort in ascending order (old to new)"
  (interactive "P")
  (if ascending
      (org-sort-entries nil ?f
			(lambda () (string-to-number (or (org-entry-get (point) "YEAR") "0")))
			(lambda (y1 y2)
			  (< y1 y2)))
    (org-sort-entries nil ?f
		      (lambda () (string-to-number (or (org-entry-get (point) "YEAR") "0")))
		      (lambda (y1 y2)
			(> y1 y2))))
  (org-fold-show-all))


(defun oa-buffer-sort-cited-by-count (&optional ascending)
  "Sort org headings by cited by count in descending order high to low.
With prefix arg ASCENDING sort from low to high."
  (interactive "P")
  (if ascending
      (org-sort-entries nil ?f
			(lambda ()
			  (string-to-number
			   (or (org-entry-get (point) "CITED_BY_COUNT")
			       "0")))
			#'<)
    (org-sort-entries nil ?f
		      (lambda ()
			(string-to-number
			 (or
			  (org-entry-get (point) "CITED_BY_COUNT")
			  "0")))
		      #'>))
  (org-fold-show-all))


;; * Interactive versions for org-ref citations

(defun oa-related-works ()
  "Open the side window for Related works on cite at point."
  (interactive)
  (oa--related-works (concat "https://doi.org/" (org-ref-get-doi-at-point))))


(defun oa-referenced-works ()
  "Open the side window for References from the cite at point."
  (interactive)
  (oa--referenced-works (concat "doi:" (org-ref-get-doi-at-point))))


(defun oa-cited-by-works ()
  "Open the side window for Citing works for the cite at point."
  (interactive)
  (oa--cited-by-works (concat "doi:" (org-ref-get-doi-at-point))))


(defun oa-open ()
  "Open the cite at point in OpenAlex."
  (interactive)
  (let* ((url (concat
	       "https://api.openalex.org/works/https://doi.org/"
	       (org-ref-get-doi-at-point)))
	 (req (request url :sync t :parser 'oa--response-parser))
	 (data (request-response-data req)))
    (browse-url (plist-get data :id))))


(defhydra+ org-ref-citation-hydra ()
  "Add open from action to `org-ref-citation-hydra'."
  ("xa" oa-open "Open in OpenAlex" :column "OpenAlex")
  ("xr" oa-related-works "Related documents" :column "OpenAlex")
  ("xc" oa-cited-by-works "Cited by documents" :column "OpenAlex")
  ("xf" oa-referenced-works "References from" :column "OpenAlex"))


;; * Author object

(defun oa--author (entity-id &optional filter)
  "Get an Author object for ENTITY-ID.
FILTER is an optional string to add to the URL."
  (let* ((url (concat  "https://api.openalex.org/authors"
		       entity-id))
	 (req (request url :sync t :parser 'oa--response-parser
		:params `(("mailto" . ,user-mail-address)
			  ("api_key" . ,oa-api-key)
			  ("filter" . ,filter))))
	 (data (request-response-data req))) 
    ;; this is for convenience to inspect data in a browser.
    (plist-put data :oa-url url)
    data))


(defun oa--author-entries (works-data url)
  "Get entries from WORKS-DATA."
  (let* ((meta (plist-get works-data :meta)) 
	 (per-page (plist-get meta :per_page))
	 (count (plist-get meta :count))
	 (pages (/ count per-page))
	 (entries '())
	 purl)
    ;; if there is a remainder we need to get the rest
    (when (> (mod count per-page) 0) (cl-incf pages))
    
    ;; Now we have to loop through the pages
    (cl-loop for i from 1 to pages
	     do
	     (setq works-data (request-response-data
			       (request url
				 :sync t
				 :parser 'oa--response-parser
				 :params `(("mailto" . ,user-mail-address)
					   ("api_key" . ,oa-api-key)
					   ("page" . ,i))))
		   entries (append entries
				   (cl-loop for result in (plist-get works-data :results)
					    collect
					    (s-format "*** ${title}
:PROPERTIES:
:ID: ${id}
:DOI: ${ids.doi}
:PDF: ${primary_location.pdf_url}
:LANDING_PAGE: ${primary_location.landing_page_url}
:YEAR: ${publication_year}
:HOST_VENUE: ${primary_location.source.display_name}
:AUTHORS: ${authors}
:CITED_BY_COUNT: ${cited_by_count}
:END:

${get-bibtex}

- ${oa-refs}
- ${oa-related}
- ${oa-cited}

${abstract}

    " (lambda (key data)
	(or (cdr (assoc key data)) ""))
    `(("title" . ,(oa--title result))
      ("id" . ,(oa-get result "id"))
      ("ids.doi" . ,(oa-get result "ids.doi"))
      ("publication_year" . ,(oa-get result "publication_year"))
      ("primary_location.pdf_url" . ,(oa-get result "primary_location.pdf_url"))
      ("primary_location.landing_page_url" . ,(oa-get result "primary_location.landing_page_url"))
      ("primary_location.source.display_name" . ,(oa-get result "primary_location.source.display_name"))
      ("authors" . ,(oa--authors result))
      ("cited_by_count" . ,(oa-get result "cited_by_count"))
      ("get-bibtex" . ,(oa--elisp-get-bibtex result))
      ("oa-refs" . ,(oa--elisp-get-oa-refs result))
      ("oa-related" . ,(oa--elisp-get-oa-related result))
      ("oa-cited" . ,(oa--elisp-get-oa-cited-by result))
      ("abstract" . ,(oa--abstract result))))))))
    entries))


(defun oa--abstract (wrk)
  "Construct an abstract from a WRK."
  (let* ((aii (plist-get wrk :abstract_inverted_index))
	 (word_index '())
	 sorted)

    (cl-loop for (k v) on aii by 'cddr
	     do
	     (cl-loop for index in v
		      do
		      (push (list k index) word_index)))

    (setq sorted (sort
		  word_index
		  (lambda (a b)
		    (<
		     (nth 1 a)
		     (nth 1 b)))))

    (string-join (mapcar
		  (lambda (x)
		    (substring
		     (symbol-name (car x))
		     1))
		  sorted)
		 " ")))


(defun oa--author-candidates (query)
  "Retrieve autocomplete authors from OpenAlex."
  (or
   (ivy-more-chars)
   (let* ((url "https://api.openalex.org/autocomplete/authors")
	  (req (request url :sync t :parser 'oa--response-parser
		 :params `(("mailto" . ,user-mail-address)
			   ("api_key" . ,oa-api-key)
			   ("q" . ,query))))
	  (data (request-response-data req))
	  (results (plist-get data :results)))
     (cl-loop for author in results collect
	      (propertize
	       (format "%s - %s"
		       (plist-get author :display_name)
		       (plist-get
			author :hint))
	       'oaid (plist-get author :id))))))


(defun oa--counts-by-year (data)
  "Get citation counts by year and make a graph.
DATA is an author from OpenAlex.
Requires gnuplot. Generates a temporary file."
  (if (executable-find "gnuplot")
      (let* ((pngfile (make-temp-file "oa-" nil ".png"))
	     (counts (sort
		      (cl-loop for i from 1 for item in (plist-get data :counts_by_year)
			       collect
			       (list
				(plist-get item :year)
				(plist-get item :cited_by_count)
				(plist-get item :works_count)))
		      (lambda (a b)
			(< (nth 0 a)
			   (nth 0 b)))))
	     (count-string (cl-loop for i from 1 for (year cites works) in counts
				    concat
				    (format "%s \"%s\" %s %s\n"
					    i year cites works)))
	     (gnuplot (format "set terminal \"png\" size 800,400
set output \"%s\"
$counts << EOD
%s
EOD
set boxwidth 0.5
set style fill solid noborder
set style line 1 lc rgb \"grey\"
set ylabel \"Citation count\"
set y2label \"Document count\"
set y2tics nomirror
set key left top
plot $counts using 1:3:xtic(2) with boxes lc rgb \"grey\" title \"Citations per year\", \"\" using 1:4 axes x1y2 with lines title \"Document count\"
"
			      pngfile
			      count-string))
	     (cmdfile (make-temp-file "gnuplot-cmds-" nil ".gpl"))
             (shellcmd (format "gnuplot --persist -c \"%s\"" cmdfile)))
	(with-temp-file cmdfile
	  (insert gnuplot))
	(shell-command shellcmd)
	(delete-file cmdfile)
	(format "[[%s]]" pngfile))
    "Gnuplot required to see citation graph. Please install it."))


(defun oa--author-org (entity-id)
  "Generate an org-buffer for an author with ENTITY-ID."
  (let* ((buf (get-buffer-create "*OpenAlex - Author*"))
	 (data (oa--author entity-id))
	 (citations-image (oa--counts-by-year data))
	 (works-count (plist-get data :works_count))
	 (works-url (plist-get data :works_api_url))
	 (works-data (request-response-data
		      (request works-url
			:sync t
			:parser 'oa--response-parser
			:params `(("mailto" . ,user-mail-address)
				  ("api_key" . ,oa-api-key))))))
    (with-current-buffer buf
      (erase-buffer)
      (insert (s-format "* ${display_name} ([[${oa-url}][json]])
:PROPERTIES:
:OPENALEX: ${id}
:ORCID: ${orcid}
:SCOPUS: ${ids.scopus}
:WORKS_COUNT: ${works_count}
:CITED_BY_COUNT: ${cited_by_count}
:INSTITUTION: ${last_known_institution.display_name}, ${last_known_institution.country_code}
:END:

#+COLUMNS: %25ITEM %YEAR %CITED_BY_COUNT
elisp:org-columns    elisp:org-columns-quit

${citations-image}

** Articles

#+caption: Sort
| year     | [[elisp:(oa-buffer-sort-year t)][old first]] | [[elisp:(oa-buffer-sort-year)][new first]] |
| cited by | [[elisp:(oa-buffer-sort-cited-by-count t)][low first]] | [[elisp:(oa-buffer-sort-cited-by-count)][high first]] |

"
			(lambda (key data)
			  (or (cdr (assoc key data)) ""))
			`(("display_name" . ,(oa-get data "display_name"))
			  ("oa-url" . ,(oa-get data "oa-url"))
			  ("id" . ,(oa-get data "id"))
			  ("orcid" . ,(oa-get data "orcid"))
			  ("ids.scopus" . ,(oa-get data "ids.scopus"))
			  ("works_count" . ,(oa-get data "works_count"))
			  ("last_known_institution.display_name" . ,(oa-get data "last_known_institution.display_name"))
			  ("last_known_institution.country_code" . ,(oa-get data "last_known_institution.country_code"))
			  ("citations-image" . ,citations-image))))
      (insert (s-join "\n" (oa--author-entries works-data works-url)))

      (org-mode)
      (goto-char (point-min))
      (org-next-visible-heading 1))
    (pop-to-buffer buf)))


(defun oa-author ()
  "Get data and act on it for an author."
  (interactive)
  
  (ivy-read "Author: " #'oa--author-candidates
	    :dynamic-collection t
	    :action
	    '(1
	      ("o" (lambda (candidate)
		     (oa--author-org (get-text-property 0 'oaid candidate)))
	       "Open org file")
	      ("l" (lambda (candidate)
		     (insert (format "[[%s][%s]]"
				     (get-text-property 0 'oaid candidate)
				     candidate))))
	      ("u" (lambda (candidate)
		     (browse-url (get-text-property 0 'oaid candidate)))
	       "Open in browser"))))


;; * Full text search


(defun oa-fulltext-search (query &optional page)
  "Perform a fulltext search on QUERY.
PAGE is optional, and loads that page of results. Defaults to 1."
  (interactive (list (read-string "Query: ")
		     nil))
  (when (null page) (setq page 1))
  (let* ((url "https://api.openalex.org/works")
	 (req (request url
		:sync t
		:parser #'oa--response-parser
		:params `(("mailto" . ,user-mail-address)
			  ("api_key" . ,oa-api-key)
			  ("page" . ,page)
			  ("filter" . ,(format "fulltext.search:%s" query)))))
	 (data (request-response-data req))
	 (metadata (plist-get data :meta))
	 (count (plist-get metadata :count))
	 (per-page (plist-get metadata :per_page))
	 (npages (+ (/ count per-page) (if (= 0 (mod count per-page)) 0 1)))
	 (results (plist-get data :results))
	 (next-page (format "[[elisp:(oa-fulltext-search \"%s\" %s)][Next page: %s of %s]]"
			    query
			    (+ page 1)
			    (+ page 1)
			    npages))
	 (buf (get-buffer-create "*OpenAlex Full-text search*")))
    
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert (s-format "#+title: Full-text search: ${query}

[[elisp:(oa-fulltext-search \"${query}\" ${page})]]"
			'aget
			`(("query" . ,query)
			  ("page" . ,page))))
      (insert (s-format
	       "
${meta.count} results: Page ${meta.page} of ${s1} ${s2}
\n\n"
	       (lambda (key data)
		 (or (cdr (assoc key data)) ""))
	       `(("meta.page" . ,(oa-get data "meta.page"))
		 ("s1" . ,(format "%s" npages))
		 ("s2" . ,(format "%s" next-page)))))
      
      (insert
       (cl-loop for result in results concat 
		(s-format "* ${title}
:PROPERTIES:
:JOURNAL: ${primary_location.source.display_name}
:AUTHOR: ${authors}
:YEAR: ${publication_year}
:OPENALEX: ${id}
:DOI: ${ids.doi}
:END:

${get-bibtex}

- ${oa-refs}
- ${oa-related}
- ${oa-cited}

" (lambda (key data)
    (or (cdr (assoc key data)) ""))

`(("title" . ,(oa--title result))
  ("primary_location.source.display_name" . ,(oa-get result "primary_location.source.display_name"))
  ("authors" . ,(oa--authors result))
  ("publication_year" . ,(oa-get result "publication_year"))
  ("id" . ,(oa-get result "id"))
  ("ids.doi" . ,(oa-get result "ids.doi"))
  ("get-bibtex" . ,(oa--elisp-get-bibtex result))
  ("oa-refs" . ,(oa--elisp-get-oa-refs result))
  ("oa-related" . ,(oa--elisp-get-oa-related result))
  ("oa-cited" . ,(oa--elisp-get-oa-cited-by result))))))

      (insert next-page)
      
      (goto-char (point-min)))
    (pop-to-buffer buf)))


;; * NSF Collaborators and Other Affiliations
(defun oa-coa (entity-id &optional COA-file)
  "Get a list of collaborators for the past 5 years in tab-delimited form.
This is for Table 4 in the COA_template at
https://www.nsf.gov/bfa/dias/policy/coa/coa_template.xlsx.

ENTITY-ID is an identifier that OpenAlex can use. Used
interactively you can query and select an author.

If COA-FILE is non-nil write results to that file, otherwise save
to the clipboard. You should be able to paste the results
directly into Excel.

Results are sorted in alphaphabetical order by last name.

Caveats: OpenAlex provides the name in Firstname Initial Lastname
form. I assume this can be split on spaces, and the last word is
the last name. That is not always correct, so some manual name
fixing may be required.

The Institutions are not always reliable. I use the most recent
institution if an author is listed multiple times. Sometimes this
is empty, and sometimes an author has multiple institutions
listed.

There may be duplicates for people who have different names in
OpenAlex, e.g. missing initials, differences in spelling,
abbreviations, including having a period or not.

Your name will be included, you will need to delete this manually
in the Excel sheet.

This only gets the coauthors in publications known to OpenAlex.
Recently published papers are probably missing.
"
  (interactive (list
		(get-text-property 0 'oaid
				   (ivy-read "Author: " #'oa--author-candidates
					     :dynamic-collection t))
		(when (y-or-n-p "Save to file?")
		  (read-file-name "File: "))))
  
  (let* ((data (oa--author entity-id))
	 (works-url (plist-get data :works_api_url)) 
	 (works-data (request-response-data
		      (request works-url
			:sync t
			:parser 'oa--response-parser
			:params `(("mailto" . ,user-mail-address)
				  ("api_key" . ,oa-api-key)))))
	 (meta (plist-get works-data :meta))
	 (count (plist-get meta :count))
	 (per-page (plist-get meta :per_page))
	 (pages 1)
	 (results (plist-get works-data :results))
	 (current-year (string-to-number (format-time-string "%Y" (current-time))))
	 (current-authors '())
	 purl)

    ;; Now we need to accumulate the rest of the results from other pages
    (when (> (mod count per-page) 0) (cl-incf pages))

    (cl-loop for i from 2 to pages
	     do
	     (setq works-data (request-response-data
			       (request works-url
				 :sync t
				 :parser 'oa--response-parser
				 :params `(("mailto" . ,user-mail-address)
					   ("api_key" . ,oa-api-key)
					   ("page" . ,i))))
		   results (append results (plist-get works-data :results))))
    
    ;; Now results is a list of your publications. We need to iterate over each
    ;; one, and accumulate author information
    (cl-loop for result in results do
	     (let ((year (plist-get result :publication_year)))
	       (when (> year (- current-year 5))
		 (cl-loop for authorship in (plist-get result :authorships) do
			  (let* ((author (plist-get authorship :author))
				 (name (plist-get author :display_name))
				 (name-parts (mapcar #'capitalize (split-string name)))
				 (name-string (concat (car (last name-parts)) ", "
						      (string-join (butlast name-parts) " ")))
				 
				 (institutions (plist-get authorship :institutions)) 
				 (institution (plist-get (car institutions) :display_name)))
			    ;; name, institution, contact info, last-active
			    ;; we won't have contact info from openalex.
			    (push (list name-string institution "" year
					(plist-get result :publication_date))
				  current-authors))))))
    
    (setq current-authors (sort current-authors
				(lambda (a b)
				  "Sort first on name, then on year in descending order"
				  (if (string= (nth 0 a) (nth 0 b))
				      (> (nth 3 a) (nth 3 b))
				    (string< (car a) (car b))))))
    
    ;; now filter for unique authors
    (setq current-authors (cl-loop for group in (seq-group-by (lambda (x)
								(car x))
							      current-authors)	   
   				   collect (cadr group)))

    ;; Finally lets fix the year so Excel reads it correctly. I use the publication date
    (setq current-authors (cl-loop for row in current-authors 
				   collect
				   (list "A:"
					 (nth 0 row)
					 (nth 1 row)
					 (nth 2 row)
					 (nth 4 row))))

    (if COA-file
	(with-temp-file COA-file
	  (cl-loop for row in current-authors do
		   (insert (string-join (mapcar (lambda (x)
						  (format "%s" (or x "")))
						row)
					"\t")
			   "\n")))
      

      (kill-new (mapconcat (lambda (row)
			     (concat (string-join (mapcar (lambda (x)
							    (format "%s" (or x "")))
							  row)
						  "\t")
				     "\n"))
			   current-authors))
      (message "COA data on the clipboard."))))



;; * utilities

(defun oa-kill-buffers ()
  "Kill OpenAlex buffers."
  (interactive)
  (cl-loop for buf in (buffer-list)
	   do
	   (when (s-starts-with? "*OpenAlex" (buffer-name buf))
	     (kill-buffer buf))))


(defun oa-get-bibtex-entries ()
  "Download all the bibtex entries in the buffer.
Operates on headings with a DOI property."
  (interactive)
  (let ((bibfile (completing-read "Bibfile: " (org-ref-possible-bibfiles))))
    (org-map-entries
     (lambda ()
       (kill-new (org-entry-get (point) "DOI"))
       (doi-utils-add-bibtex-entry-from-doi
	(doi-utils-maybe-doi-from-region-or-current-kill) 
	bibfile))
     "DOI<>\"\"")))


(provide 'openalex)

;;; openalex.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings-wide)
;; End:

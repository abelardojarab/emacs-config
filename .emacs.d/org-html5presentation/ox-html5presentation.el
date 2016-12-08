;;; ox-html5presentation.el --- HTML5 Presentation Back-End for Org Export Engine

;; Copyright (C) 2014, 2015 Takumi Kinjo.

;; Author: Takumi KINJO <takumi dot kinjo at gmail dot org>
;; URL: https://github.com/kinjo/org-html5presentation.el
;; Version: 0.1
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; This library implements a HTML5 Presentation back-end for Org
;; generic exporter based on ox-html.el.

;; About HTML5 presentation, see http://slides.html5rocks.com.

;; Original author: Carsten Dominik <carsten at orgmode dot org>
;;      Jambunathan K <kjambunathan at gmail dot com>

;;; Code:

;;; Dependencies

(require 'ox-html)

;;; Define Back-End

(org-export-define-derived-backend 'html5presentation 'html
  :translate-alist 
  '((headline . org-html5presentation-headline)
    (inner-template . org-html5presentation-inner-template)
    (section . org-html5presentation-section)
    (template . org-html5presentation-template))
  :menu-entry
  '(?p "Export to HTML5 Presentation"
       ((?P "As HTML buffer" org-html5presentation-export-as-html)
	(?p "As HTML file" org-html5presentation-export-to-html)
	(?o "As HTML file and open"
	    (lambda (a s v b)
	      (if a (org-html5presentation-export-to-html t s v b)
		(org-open-file (org-html5presentation-export-to-html nil s v b)))))))
  :options-alist
  '((:html-doctype "HTML_DOCTYPE" nil org-html5presentation-doctype)
    (:prettify-css "PRETTIFY_CSS" nil org-html5presentation-prettify-css)
    (:fonts-css "FONTS_CSS" nil org-html5presentation-fonts-css)
    (:presentation-css "PRESENTATION_CSS" nil org-html5presentation-presentation-css)
    (:common-css "COMMON_CSS" nil org-html5presentation-common-css)
    (:default-css "DEFAULT_CSS" nil org-html5presentation-default-css)
    (:moon-css "MOON_CSS" nil org-html5presentation-moon-css)
    (:sand-css "SAND_CSS" nil org-html5presentation-sand-css)
    (:sea-wave-css "SEA_WAVE_CSS" nil org-html5presentation-sea-wave-css)
    (:ie-lt-9-js "IE_LT_9_JS" nil org-html5presentation-ie-lt-9-js)
    (:prettify-js "PRETTIFY_JS" nil org-html5presentation-prettify-js)
    (:utils-js "UTILS_JS" nil org-html5presentation-utils-js)))

;;;; Template :: Generic

(defcustom org-html5presentation-doctype "html5"
  "Document type definition to use for exported HTML files.
Can be set with the in-buffer HTML_DOCTYPE property or for
publishing, with :html-doctype."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-html5presentation-divs
  '((container "div" "flex-container")
    (slides    "div" "slides"))
  "Alist of the section elements for HTML export."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(list :greedy t
	       (list :tag "Content"
		     (const :format "" content)
		     (string :tag "element") (string :tag "     id"))))

(defcustom org-html5presentation-prettify-css "resources/styles/prettify.css"
  "Default URL of prettify.css."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-html5presentation-fonts-css "resources/styles/fonts.css"
  "Default URL of fonts.css."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-html5presentation-presentation-css "resources/styles/presentation.css"
  "Default URL of presentation.css."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-html5presentation-common-css "resources/styles/common.css"
  "Default URL of common.css."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-html5presentation-default-css "resources/styles/default.css"
  "Default URL of default.css."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-html5presentation-moon-css "resources/styles/moon.css"
  "Default URL of moon.css."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-html5presentation-sand-css "resources/styles/sand.css"
  "Default URL of sand.css."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-html5presentation-sea-wave-css "resources/styles/sea_wave.css"
  "Default URL of sea_wave.css."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-html5presentation-ie-lt-9-js "http://ajax.googleapis.com/ajax/libs/chrome-frame/1/CFInstall.min.js"
  "Default URL of CFInstall.min.js."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-html5presentation-prettify-js "resources/js/prettify.js"
  "Default URL of prettify.js."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-html5presentation-utils-js "resources/js/utils.js"
  "Default URL of utils.js."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

;;;; template :: Navi
(defcustom org-html5presentation-navi "<nav id=\"helpers\">
<button title=\"Previous slide\" id=\"nav-prev\" class=\"nav-prev\">&#8701;</button>
<button title=\"Jump to a random slide\" id=\"slide-no\">5</button>
<button title=\"Next slide\" id=\"nav-next\" class=\"nav-next\">&#8702;</button>
<menu>
<!-- <button type=\"checkbox\" data-command=\"toc\" title=\"Table of Contents\" class=\"toc\">TOC</button> -->
<!-- <button type=\"checkbox\" data-command=\"resources\" title=\"View Related Resources\">&#9734;</button> -->
<!-- <button type=\"checkbox\" data-command=\"notes\" title=\"View Slide Notes\">&#9999;</button> -->
<!-- <button type=\"checkbox\" data-command=\"source\" title=\"View slide source\">&#8635;</button> -->
<button type=\"checkbox\" data-command=\"help\" title=\"View Help\">?</button>
</menu>
</nav>
"
  "Help section describes usage of the HTML5 Presentation slides."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)
;;;###autoload
(put 'org-html5presentation-navi 'safe-local-variable 'stringp)

;;;; template :: Loading
(defcustom org-html5presentation-loading "<div id=\"presentation-counter\">Loading...</div>
"
  "Loading message will be shown when contents are loading."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)
;;;###autoload
(put 'org-html5presentation-loading 'safe-local-variable 'stringp)

;;;; template :: Help
(defcustom org-html5presentation-help "<aside id=\"help\" class=\"sidebar invisible\" style=\"display: none;\">
<table>
<caption>Help</caption>
<tbody>
<tr>
<th>Move Around</th>
<td>&larr;&nbsp;&rarr;</td>
</tr>
<tr>
<th>Change Theme</th>
<td>t</td>
</tr>
<tr>
<th>Syntax Highlight</th>
<td>h</td>
</tr>
<tr>
<th>Toggle 3D</th>
<td>3</td>
</tr>
<tr>
<th>Help</th>
<td>0</td>
</tr>
</tbody>
</table>
</aside>
"
  "Help template."
  :group 'org-export-html5presentation
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)
;;;###autoload
(put 'org-html5presentation-help 'safe-local-variable 'stringp)


;;; Template

(defun org-html5presentation--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (let ((protect-string
	 (lambda (str)
	   (replace-regexp-in-string
	    "\"" "&quot;" (org-html-encode-plain-text str))))
	(title (org-export-data (plist-get info :title) info))
	(author (and (plist-get info :with-author)
		     (let ((auth (plist-get info :author)))
		       (and auth
			    ;; Return raw Org syntax, skipping non
			    ;; exportable objects.
			    (org-element-interpret-data
			     (org-element-map auth
				 (cons 'plain-text org-element-all-objects)
			       'identity info))))))
	(description (plist-get info :description))
	(keywords (plist-get info :keywords))
	(charset (or (and org-html-coding-system
			  (fboundp 'coding-system-get)
			  (coding-system-get org-html-coding-system
					     'mime-charset))
		     "iso-8859-1")))
    (concat
     (format "<title>%s</title>\n" title)
     (when (plist-get info :time-stamp-file)
       (format-time-string
	 (concat "<!-- " org-html-metadata-timestamp-format " -->\n")))
     (org-html-close-tag
      "meta" " http-equiv=\"X-UA-Compatible\" content=\"IE=Edge;chrome=1\"" info) "\n"
     (format
      (if (org-html-html5-p info)
	  (org-html-close-tag "meta" " charset=\"%s\"" info)
	(org-html-close-tag
	 "meta" " http-equiv=\"Content-Type\" content=\"text/html;charset=%s\""
	 info))
      charset) "\n"
     (org-html-close-tag "meta" " name=\"generator\" content=\"Org-mode\"" info)
     "\n"
     (and (org-string-nw-p author)
	  (concat
	   (org-html-close-tag "meta"
			       (format " name=\"author\" content=\"%s\""
				       (funcall protect-string author))
			       info)
	   "\n"))
     (and (org-string-nw-p description)
	  (concat
	   (org-html-close-tag "meta"
			       (format " name=\"description\" content=\"%s\"\n"
				       (funcall protect-string description))
			       info)
	   "\n"))
     (and (org-string-nw-p keywords)
	  (concat
	   (org-html-close-tag "meta"
			       (format " name=\"keywords\" content=\"%s\""
				       (funcall protect-string keywords))
			       info)
	   "\n")))))

(defun org-html5presentation-style-template (info)
  "Return string of styles. INFO is a plist holding export options."
  (format "<link id=\"prettify-link\" href=\"%s\" rel=\"stylesheet\" disabled />
<link href=\"%s\" rel=\"stylesheet\" type=\"text/css\" media=\"screen\" />
<link href=\"%s\" rel=\"stylesheet\" type=\"text/css\" media=\"screen\" />
<link href=\"%s\" rel=\"stylesheet\" type=\"text/css\" media=\"screen\" />
<link class=\"theme\" href=\"%s\" rel=\"stylesheet\" type=\"text/css\" media=\"screen\" />
<link class=\"theme\" href=\"%s\" rel=\"stylesheet\" type=\"text/css\" media=\"screen\" />
<link class=\"theme\" href=\"%s\" rel=\"stylesheet\" type=\"text/css\" media=\"screen\"/>
<link class=\"theme\" href=\"%s\" rel=\"stylesheet\" type=\"text/css\" media=\"screen\"/>
"
	  (plist-get info :prettify-css)
	  (plist-get info :fonts-css)
	  (plist-get info :presentation-css)
	  (plist-get info :common-css)
	  (plist-get info :default-css)
	  (plist-get info :moon-css)
	  (plist-get info :sand-css)
	  (plist-get info :sea-wave-css)))

(defun org-html5presentation-ie-lt-9-js (info)
  "Return JavaScript string for IE version less than 9."
  (format "<!--[if lt IE 9]>
<script src=\"%s\">
</script>
<script>CFInstall.check({ mode: \"overlay\" });</script>
<![endif]-->\n"
	  (plist-get info :ie-lt-9-js)))

(defun org-html5presentation-prettify-js (info)
  "Return string of prettify.js. INFO is a plist used as a communication channel."
  (format "<script src=\"%s\" onload=\"prettyPrint();\" defer></script>\n"
	  (plist-get info :prettify-js)))

(defun org-html5presentation-utils-js (info)
  "Return string of utils.js. INFO is a plist used as a communication channel."
  (format "<script src=\"%s\"></script>\n"
	  (plist-get info :utils-js)))

(defun org-html5presentation--build-head (info)
  "Return information for the <head>..</head> of the HTML output.
INFO is a plist used as a communication channel."
  (org-element-normalize-string
   (concat
    (when (plist-get info :html-head-include-default-style)
      (org-element-normalize-string (org-html5presentation-style-template info)))
    (org-element-normalize-string (plist-get info :html-head))
    (org-element-normalize-string (plist-get info :html-head-extra))
    (when (and (plist-get info :html-htmlized-css-url)
	       (eq org-html-htmlize-output-type 'css))
      (org-html-close-tag "link"
			  (format " rel=\"stylesheet\" href=\"%s\" type=\"text/css\""
				  (plist-get info :html-htmlized-css-url))
			  info))
    )))

(defun org-html5presentation--build-author (author)
  "Return string of author(s). 

AUTHOR would be separated by commas if multiple authors are contained.
This replaces the separator(,) AUTHOR contains with the line break(<br>)."
  (mapconcat 'identity (split-string author "[ \t]*,[ \t]*") "<br>"))

(defun org-html5presentation-title-slide-template (info)
  "Return document string of the title slide. INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
	(subtitle ""))
    (format "<div class=\"slide title-slide\" id=\"landing-slide\">
<section class=\"middle\">
<hgroup>
<h1>%s</h1>
<h2>%s</h2>
</hgroup>
<p>Press <span id=\"left-init-key\" class=\"key\">&rarr;</span> key to advance.</p>
%s
</section>
</div>
"
	    title
	    subtitle
	    (let ((spec (org-html-format-spec info)))
	      (let ((date (cdr (assq ?d spec)))
		    (author (cdr (assq ?a spec)))
		    (email (cdr (assq ?e spec)))
		    (creator (cdr (assq ?c spec)))
		    (timestamp (cdr (assq ?T spec)))
		    (validation-link (cdr (assq ?v spec))))
		(concat
		 (when (and (plist-get info :with-date)
			    (org-string-nw-p date))
		   (format "<p class=\"date\">%s: %s</p>\n"
			   (org-html--translate "Date" info)
			   date))
		 (when (and (plist-get info :with-author)
			    (org-string-nw-p author))
		   (format "<p class=\"author\">%s: %s</p>\n"
			   (org-html--translate "Author" info)
			   (org-html5presentation--build-author author)))
		 (when (and (plist-get info :with-email)
			    (org-string-nw-p email))
		   (format "<p class=\"email\">%s: %s</p>\n"
			   (org-html--translate "Email" info)
			   email))
		 (when (plist-get info :time-stamp-file)
		   (format
		    "<p class=\"date\">%s: %s</p>\n"
		    (org-html--translate "Created" info)
		    (format-time-string org-html-metadata-timestamp-format)))
		 (when (plist-get info :with-creator)
		   (format "<p class=\"creator\">%s</p>\n" creator))
		 (format "<p class=\"validation\">%s</p>\n"
			 validation-link)))))))

(defun org-html5presentation-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Title slide.
   (org-html5presentation-title-slide-template info)
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-html5presentation-toc depth info)))
   ;; Document contents.
   (save-match-data 
     ;; Remove first `</div>' tag contained in contents string.
     ;; To prevent nested sections, couple of div tags are being reversed.
     ;; Thus, `</div>' tag will be at the head of document string.
     (if (string-match "^<\/div>" contents)
	 (substring contents (match-end 0))))
   "</div>"))

(defun org-html5presentation-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let ((decl (or (and (stringp org-html-xml-declaration)
			      org-html-xml-declaration)
			 (cdr (assoc (plist-get info :html-extension)
				     org-html-xml-declaration))
			 (cdr (assoc "html" org-html-xml-declaration))

			 "")))
       (when (not (or (eq nil decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
		  (or (and org-html-coding-system
			   (fboundp 'coding-system-get)
			   (coding-system-get org-html-coding-system 'mime-charset))
		      "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	   (when (org-html-xhtml-p info)
	     (format
	      " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
	      (plist-get info :language) (plist-get info :language)))
	   ">\n")
   "<head>\n"
   (org-html5presentation--build-meta-info info)
   (org-html5presentation--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format org-html-home/up-format
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; Document contents.
   (format "<%s id=\"%s\">\n"
	   (nth 1 (assq 'container org-html5presentation-divs))
	   (nth 2 (assq 'container org-html5presentation-divs)))
   ;; Navi.
   org-html5presentation-navi
   (format "<%s class=\"%s\">\n"
	   (nth 1 (assq 'slides org-html5presentation-divs))
	   (nth 2 (assq 'slides org-html5presentation-divs)))
   ;; Loading message.
   org-html5presentation-loading
   ;; Contents.
   contents
   ;; Help.
   org-html5presentation-help
   (format "</%s>\n"
	   (nth 1 (assq 'slides org-html5presentation-divs)))
   (format "</%s>\n"
	   (nth 1 (assq 'container org-html5presentation-divs)))
   ;; JavaScript for IE less than version 9.
   (org-html5presentation-ie-lt-9-js info)
   ;; JavaScript for prettify.js.
   (org-html5presentation-prettify-js info)
   ;; JavaScript for util.js.
   (org-html5presentation-utils-js info)
   ;; Closing document.
   "</body>\n</html>"))



;;; Tables of Contents

(defun org-html5presentation-toc (depth info)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is a
plist used as a communication channel.  Return the table of
contents as a string, or nil if it is empty."
  (let ((toc-entries
	 (mapcar (lambda (headline)
		   (cons (org-html5presentation--format-toc-headline headline info)
			 (org-export-get-relative-level headline info)))
		 (org-export-collect-headlines info depth)))
	(outer-tag (if (and (org-html-html5-p info)
			    (plist-get info :html-html5-fancy))
		       "nav"
		     "div")))
    (when toc-entries
      (concat (format "<%s class=\"slide\" id=\"table-of-contents\">\n" outer-tag)
	      (format "<header>%s</header>\n"
		      (org-html--translate "Table of Contents" info))
	      "<section id=\"toc-list\">"
	      (org-html--toc-text toc-entries)
	      "</section>\n"
	      (format "</%s>\n" outer-tag)))))

(defun org-html5presentation--format-toc-headline (headline info)
  "Return an appropriate table of contents entry for HEADLINE.
INFO is a plist used as a communication channel."
  (let* ((headline-number (org-export-get-headline-number headline info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-data-with-backend
		(org-export-get-alt-title headline info)
		;; Create an anonymous back-end that will ignore any
		;; footnote-reference, link, radio-target and target
		;; in table of contents.
		(org-export-create-backend
		 :parent 'html
		 :transcoders '((footnote-reference . ignore)
				(link . (lambda (object c i) c))
				(radio-target . (lambda (object c i) c))
				(target . ignore)))
		info))
	 (tags (and (eq (plist-get info :with-tags) t)
		    (org-export-get-tags headline info))))
    (format "<a data-hash=\"%s\">%s</a>"
	    ;; Label.
	    (org-export-solidify-link-text
	     (or (org-element-property :CUSTOM_ID headline)
		 (org-export-get-headline-id headline info)))
	    ;; Body.
	    (concat
	     (and (not (org-export-low-level-p headline info))
		  (org-export-numbered-headline-p headline info)
		  (concat (mapconcat #'number-to-string headline-number ".")
			  ". "))
	     (apply (plist-get info :html-format-headline-function)
		    todo todo-type priority text tags :section-number nil)))))


;;; Transcode Functions

;;;; Headline

(defun org-html5presentation-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (section-number (and numbers
				(mapconcat #'number-to-string numbers "-")))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents ""))
           (ids (delq nil
                      (list (org-element-property :CUSTOM_ID headline)
                            (org-export-get-headline-id headline info)
                            (org-element-property :ID headline))))
           (preferred-id (car ids))
           (extra-ids (mapconcat
                       (lambda (id)
                         (org-html--anchor
                          (org-export-solidify-link-text
                           (if (org-uuidgen-p id) (concat "ID-" id) id))
                          nil nil info))
                       (cdr ids) "")))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((type (if numberedp 'ordered 'unordered))
                 (itemized-body
                  (org-html-format-list-item
                   contents type nil info nil
                   (concat (org-html--anchor preferred-id nil nil info)
                           extra-ids
                           full-text))))
            (concat (and (org-export-first-sibling-p headline info)
                         (org-html-begin-plain-list type))
                    itemized-body
                    (and (org-export-last-sibling-p headline info)
                         (org-html-end-plain-list type))))
        (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
              (first-content (car (org-element-contents headline))))
          ;; Standard headline.  Export it as a section.
	  (format "</%s>\n<%s id=\"%s\" class=\"%s\">%s%s"
		  (org-html--container headline info)
		  (org-html--container headline info)
		  preferred-id
		  "slide transitionSlide"
		  (format "\n<header>%s%s</header>\n"
			  (mapconcat
			   (lambda (x)
			     (let ((id (org-export-solidify-link-text
					(if (org-uuidgen-p x) (concat "ID-" x)
					  x))))
			       (org-html--anchor id)))
			   extra-ids "")
			  full-text)
		  ;; When there is no section, pretend there is an empty
		  ;; one to get the correct <div class="outline- ...>
		  ;; which is needed by `org-info.js'.
		  (if (not (eq (org-element-type first-content) 'section))
		      (concat (org-html5presentation-section first-content "" info)
			      contents)
		    contents)))))))

;;;; Section

(defun org-html5presentation-section (section contents info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-export-get-parent-headline section)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent) contents
      ;; Build return value.
      (format "<section>\n%s</section>"
	      contents))))


;;; End-user functions

;;;###autoload
(defun org-html5presentation-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'html5presentation "*Org HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-html5presentation-convert-region-to-html ()
  "Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in an HTML buffer and use this
command to convert it."
  (interactive)
  (org-export-replace-region-by 'html5presentation))

;;;###autoload
(defun org-html5presentation-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'html5presentation file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-html5presentation-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'html5presentation filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension "html"))
		      plist pub-dir))


(provide 'ox-html5presentation)
;;; ox-html5presentation.el ends here

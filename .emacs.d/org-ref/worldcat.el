
;; https://www.worldcat.org/wcpa/content/affiliate/default.jsp


;; https://www.worldcat.org/affiliate/tools?atype=text

;;Key Z65ynvQVhbmmxq07PX3IN1n71Z91pjUXYSSCCP9p2n8gD241P46jMyMHDuAvqRejIX1g8YbrwOAbobBM
;;Secret XAWkEvC+kOKPpGgtBVwmFQ==
;; Status ACTIVE
;; Environment Sandbox
;; Expiration Date 06/27/2016
;; Registry ID 128807
;; Override Institutions
;; Redirect URI
;; Services
;; WorldCat Search API (wcapi)
;; View API Documentation
;; Sandbox Info: Requests limited to 100/day

(defun worldcat-query-all (query)
  "Open browser to Worldcat QUERY."
  (browse-url
   (format
    "http://www.worldcat.org/search?qt=worldcat_org_all&q=%s"
    query)))

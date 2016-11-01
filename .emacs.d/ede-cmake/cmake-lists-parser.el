;; cmake-lists-parser.el --- Parser for CMakeLists.txt files

;; Author: Alastair Rankine <alastair@girtby.net>

;; TODO: rewrite with the semantic grammar framework

(require 'eieio)

(defclass cmake-lists-parser ()
  ((command-handers
    :type list
    :initarg :command-handlers
    :documentation "Alist of parsers, one for each command name")
   )
  "Parser for CMakeLists.txt files"
)

(defmethod parse-buffer ((this cmake-lists-parser) buf)
  "Parse BUF and call the command handlers for each command encountered"
  (while (re-search-forward ("^\s*\(\w+\)(\(.+"))
    (match-string 1)
    nil))


(defmethod parse ((this cmake-lists-parser) file)
  "Parse FILE and call the command handlers for each command encountered"
  (let ((oldbuf (current-buffer)))
    (save-current-buffer
      (set-buffer (get-buffer-create "*CMakeLists*"))
      (insert-file-contents file nil nil nil t)
      (parse-buffer this current-buffer))))

  
;;; setup-elnode.el ---

;; Copyright (C) 2014  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc25288.pdx.intel.com>
;; Keywords:

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

;;

;;; Code:
(add-to-list 'load-path "~/.emacs.d/kv")
(add-to-list 'load-path "~/.emacs.d/db")
(add-to-list 'load-path "~/.emacs.d/noflet")
(add-to-list 'load-path "~/.emacs.d/fakir")
(add-to-list 'load-path "~/.emacs.d/web")
(add-to-list 'load-path "~/.emacs.d/elnode")
(require 'elnode)

(defun elnode-org-update-handler (httpcon)
  "Elnode handler to do org-mode updates.

Specify `file-name' for the file to update, `node-match' for an
org-agenda match, `in-node-match' to specify what will be
replaced in the node matched org line, `replace-match' for the
replacement."
  (elnode-method httpcon
      (POST
       (let* ((file-name (elnode-http-param httpcon "file-name"))
              (node-match (elnode-http-param httpcon "node-match"))
              (in-node-match (elnode-http-param httpcon "in-node-match"))
              (replace-match (elnode-http-param httpcon "replace-match")))
         (elnode-org--update
          file-name
          node-match
          in-node-match
          replace-match)))))

(defun elnode-org--update (file-name node-match in-node-match replace-match)
  "Update org-mode specified FILE-NAME.

NODE-MATCH specifies a match expression in the manner of org-agenda views.

IN-NODE-MATCH specifies a string match expression used with the
bounds of the matched node line.

REPLACE-MATCH specifies the replacement for the IN-NODE-MATCH."
  (with-current-buffer (find-file-noselect file-name)
    (org-map-entries
     (lambda ()
       (replace-regexp
        in-node-match
        replace-match
        nil
        (point)
        (line-end-position)))
     node-match)))

(provide 'setup-elnode)
;;; setup-elnode.el ends here

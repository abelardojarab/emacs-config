;;; helm-tramp.el --- Tramp with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by masasam

;; Author: masasam
;; URL: https://github.com/masasam/emacs-helm-tramp
;; Version: 0.01
;; Package-Requires: ((helm "1.7.7") (emacs "24"))

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

;; tramp with helm interface

;;; Code:

(require 'helm)
(require 'tramp)

(defgroup helm-tramp nil
  "tramp with helm interface"
  :group 'helm)

(defun helm-tramp--candidates ()
  (let ((source (split-string
                 (with-temp-buffer
                   (insert-file-contents "~/.ssh/config")
                   (buffer-string))
                 "\n"))
        (hosts (list)))
    (dolist (host source)
      (when (string-match "[H\\|h]ost +\\(.+?\\)$" host)
	(setq host (match-string 1 host))
	(if (string-match "[ \t\n\r]+\\'" host)
	    (replace-match "" t t host))
	(if (string-match "\\`[ \t\n\r]+" host)
	    (replace-match "" t t host))
        (unless (string= host "*")
          (push
	   (concat "/" tramp-default-method ":" host ":/")
	   hosts)
	  (push
	   (concat "/" tramp-default-method ":" host "|sudo:" host ":/")
	   hosts))))
    (reverse hosts)))

(defun helm-tramp-open (path)
  "Tramp open with PATH."
  (find-file path))

(defvar helm-tramp--source
  (helm-build-sync-source "Tramp"
    :candidates #'helm-tramp--candidates
    :volatile t
    :action (helm-make-actions
             "Tramp" #'helm-tramp-open)))

;;;###autoload
(defun helm-tramp ()
  "Open your ~/.ssh/config with helm interface.
You can connect your server with tramp"
  (interactive)
  (unless (file-exists-p "~/.ssh/config")
    (error "There is no ~/.ssh/config"))
  (helm :sources '(helm-tramp--source) :buffer "*helm tramp*"))

(provide 'helm-tramp)

;;; helm-tramp.el ends here

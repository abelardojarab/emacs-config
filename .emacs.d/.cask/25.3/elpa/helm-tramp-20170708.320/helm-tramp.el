;;; helm-tramp.el --- Tramp helm interface for ssh, docker, vagrant -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Masashı Mıyaura

;; Author: Masashı Mıyaura
;; URL: https://github.com/masasam/emacs-helm-tramp
;; Package-Version: 20170708.320
;; Version: 0.5.4
;; Package-Requires: ((emacs "24.3") (helm "2.0"))

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

;; helm-tramp provides interfaces of Tramp
;; You can also use tramp with helm interface as root
;; If you use it with docker-tramp, you can also use docker with helm interface
;; If you use it with vagrant-tramp, you can also use vagrant with helm interface

;;; Code:

(require 'helm)
(require 'tramp)
(require 'cl-lib)

(defgroup helm-tramp nil
  "Tramp with helm interface for ssh, docker, vagrant"
  :group 'helm)

(defcustom helm-tramp-docker-user nil
  "If you want to use login user name when docker-tramp used, set variable."
  :group 'helm-tramp
  :type 'string)

(defun helm-tramp--candidates ()
  "Collect candidates for helm-tramp."
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
	   (concat "/ssh:" host "|sudo:" host ":/")
	   hosts))))
    (when (package-installed-p 'docker-tramp)
      (cl-loop for line in (cdr (ignore-errors (apply #'process-lines "docker" (list "ps"))))
	       for info = (split-string line "[[:space:]]+" t)
	       collect (progn (push
			       (concat "/docker:" (car info) ":/")
			       hosts)
			      (unless (null helm-tramp-docker-user)
				(push
				 (concat "/docker:" helm-tramp-docker-user "@" (car info) ":/")
				 hosts)))))
    (when (package-installed-p 'vagrant-tramp)
      (cl-loop for box-name in (map 'list 'cadr (vagrant-tramp--completions))
               do (progn
                    (push (concat "/vagrant:" box-name ":/") hosts)
                    (push (concat "/vagrant:" box-name "|sudo:" box-name ":/") hosts))))
    (push "/sudo:root@localhost:/" hosts)
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
  (when (package-installed-p 'docker-tramp)
    (unless (executable-find "docker")
      (error "'docker' is not installed")))
  (when (package-installed-p 'vagrant-tramp)
    (unless (executable-find "vagrant")
      (error "'vagrant' is not installed")))
  (helm :sources '(helm-tramp--source) :buffer "*helm tramp*"))

(provide 'helm-tramp)

;;; helm-tramp.el ends here

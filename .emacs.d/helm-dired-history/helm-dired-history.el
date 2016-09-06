;;; helm-dired-history.el --- Show dired history with helm.el support.

;; Author: Joseph(纪秀峰) <jixiuf@gmail.com>
;; Copyright (C) 2011,2012, Joseph(纪秀峰), all rights reserved.
;; Created: 2011-03-26
;; Version: 0.1.1
;; X-URL:https://github.com/jixiuf/helm-dired-history
;; Keywords: helm, dired history
;; Package-Requires: ((helm "1.9.8")(cl-lib "0.5"))
;;
;; Features that might be required by this library:
;;
;; `helm' `dired'
;;
;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Someone like to reuse the current dired buffer to visit
;; another directory, so that you just need open one dired
;; buffer. but the bad point is ,you can't  easily go
;; forward and back in different dired directory. this file
;; can remember dired directory you have visited and list them
;; using `helm.el'.

;;; Installation:

;; (require 'savehist)
;; (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
;; (savehist-mode 1)
;; (eval-after-load 'dired
;;   '(progn (require 'helm-dired-history)
;;           (define-key dired-mode-map "," 'helm-dired-history-view)))
;;
;;; Commands:
;;
;; Below are complete command list:
;;
;;  `helm-dired-history-view'
;;    call `helm' to show dired history.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'helm)
(require 'helm-types)
(require 'helm-files)
(require 'dired)
(require 'cl-lib)

(defgroup helm-dired-history nil
  "dired history for Helm."
  :group 'helm)


(defcustom helm-dired-history-fuzzy-match t
  "Enable fuzzy matching in `helm-dired-history-source' when non--nil."
  :type 'boolean
  :group 'helm-dired-history)

(defvar helm-dired-history-variable nil)

(defvar helm-dired-history-cleanup-p nil)

(defun helm-dired-history-update()
  "update variable `helm-dired-history-variable'."
  (unless helm-dired-history-cleanup-p
    (setq helm-dired-history-cleanup-p t)
    (let ((tmp-history ))
      (dolist (d helm-dired-history-variable)
        (when (or (file-remote-p d) (file-directory-p d))
          (add-to-list 'tmp-history d t)))
      (setq helm-dired-history-variable tmp-history)))
  (setq helm-dired-history-variable
        (delete-dups (delete (dired-current-directory) helm-dired-history-variable)))
  (setq helm-dired-history-variable
        (append (list (dired-current-directory)) helm-dired-history-variable)))

;;when you open dired buffer ,update `helm-dired-history-variable'.
(add-hook 'dired-after-readin-hook 'helm-dired-history-update)

(defun helm-dired-history-transform (candidates _source)
  (cl-loop for c in candidates
           if (file-remote-p (cdr c))
           collect (cons (propertize (car c) 'face 'font-lock-warning-face) (cdr c))
           else collect c))

(defclass helm-dired-history-source (helm-source-sync helm-type-file)
  ((candidates :initform (lambda () helm-dired-history-variable))
   (keymap :initform helm-generic-files-map)
   (filtered-candidate-transformer :initform 'helm-dired-history-transform)
   (help-message :initform helm-generic-file-help-message)))

(defvar helm-source-dired-history
  (helm-make-source "Dired History" 'helm-dired-history-source
    :fuzzy-match helm-dired-history-fuzzy-match))

;;;###autoload
(defun helm-dired-history-view()
  "call `helm' to show dired history."
  (interactive)
  (let ((helm-execute-action-at-once-if-one t)
        (helm-quit-if-no-candidate
         (lambda () (message "No history record."))))
    (helm '(helm-source-dired-history)
          ;; Initialize input with current symbol
          ""  nil nil)))

(provide 'helm-dired-history)
;;; helm-dired-history.el ends here.

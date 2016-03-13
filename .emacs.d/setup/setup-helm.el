;;; setup-helm.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2016  Abelardo Jara

;; Author: Abelardo Jara <abelardojara@Abelardos-MacBook-Pro.local>
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

;; Helm
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)

;; Indent semantic entries
(require 'helm-imenu)
(defun my-helm-imenu-transformer (cands)
  (with-helm-current-buffer
    (save-excursion
      (cl-loop for (func-name . mrkr) in cands
               collect
               (cons (format "Line %4d: %s"
                             (line-number-at-pos mrkr)
                             (progn (goto-char mrkr)
                                    (buffer-substring mrkr (line-end-position))))
                     (cons func-name mrkr))))))

(defvar my-helm-imenu-source  (helm-make-source "Imenu" 'helm-imenu-source
                                :candidate-transformer
                                'my-helm-imenu-transformer))
(defun my-helm-imenu ()
  (interactive)
  (let ((imenu-auto-rescan t)
        (str (thing-at-point 'symbol))
        (helm-execute-action-at-once-if-one
         helm-imenu-execute-action-at-once-if-one))
    (helm :sources 'my-helm-imenu-source
          :preselect str
          :buffer "*helm imenu*")))

(provide 'setup-helm)
;;; setup-helm.el ends here

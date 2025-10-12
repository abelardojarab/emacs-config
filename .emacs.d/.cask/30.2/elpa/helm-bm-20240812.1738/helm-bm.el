;;; helm-bm.el --- helm sources for bm.el -*- lexical-binding: t -*-

;; Copyright (C) 2013-2016 Yasuyuki Oka <yasuyk@gmail.com>
;; Copyright (C) 2024 Thierry Volpiatto <thievol@posteo.net>

;; Author: Yasuyuki Oka <yasuyk@gmail.com>
;;         Thierry Volpiatto <thievol@posteo.net>
;;
;; Maintainer: Thierry Volpiatto <thievol@posteo.net>

;; URL: https://github.com/emacs-helm/helm-bm
;; Package-Requires: ((bm "1.0") (cl-lib "0.5") (helm "1.9.3"))
;; Package-Version: 20240812.1738
;; Package-Revision: 4744b5784df5
;; Keywords: helm, bookmark

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

;; Helm UI for bm.el (https://github.com/joodland/bm).

;; Basically bm.el allow bookmarking positions in buffers, helm-bm
;; list these positions and allow jumping to them easily.  This is
;; particularly useful when working on a set of functions distributed
;; in various places of a large buffer or in several different buffers.

;; Installation:

;; Add this file in your `load-path' and the following to your Emacs init file:
;;
;; (autoload 'helm-bm "helm-bm" nil t) ;; Not necessary if using ELPA package.

;; Settings:

;; Bind helm-bm to a key of your choice, e.g.
;; (global-set-key (kbd "C-c b") 'helm-bm)
;;
;; Show bookmarks from all buffers or only from current buffer according
;; to `bm-cycle-all-buffers' value.

;;; Code:

(require 'bm)
(require 'cl-lib)
(require 'helm)

(declare-function helm-goto-char "ext:helm-utils")
(declare-function helm-highlight-current-line "ext:helm-utils")

(defgroup helm-bm nil
  "Bookmarks of bm.el related Applications and libraries for Helm."
  :prefix "helm-bm-" :group 'helm)

(defcustom helm-bm-sort-from-pos nil
  "Sort bookmarks according to current position when non nil."
  :type 'boolean)

(defun helm-bm-action-bookmark-edit-annotation (candidate)
  "Edit bookmark annotation of CANDIDATE."
  (let ((annotation (read-string "Edit annotation: "
                                 (overlay-get candidate 'annotation))))
    (bm-bookmark-annotate candidate annotation)))

(defun helm-bm-action-switch-to-buffer (candidate)
  "Switch to buffer of CANDIDATE."
  (require 'helm-utils)
  (let ((pos (overlay-get candidate 'position))
        (buf (overlay-buffer candidate)))
    (when (and pos buf)
      (switch-to-buffer buf)
      (helm-goto-char pos)
      (helm-highlight-current-line))))

(defun helm-bm-action-remove-marked-bookmarks (_candidate)
  "Remove marked bookmarks."
  (mapc 'bm-bookmark-remove (helm-marked-candidates)))

(defun helm-bm-bookmarks-in-all-buffers ()
  (cl-loop for buf in (buffer-list)
           nconc (helm-bm-bookmarks-in-buffer buf)))

(defun helm-bm-bookmarks-in-buffer (buf)
  "Gets a list of bookmarks in BUF, which can be a string or a buffer."
  (with-current-buffer buf
    (if helm-bm-sort-from-pos
        (helm-fast-remove-dups (helm-flatten-list (bm-lists)) :test 'eql)
      (delq nil (mapcar #'bm-bookmarkp (overlays-in (point-min) (point-max)))))))

(defun helm-bm-candidate-transformer-display
    (bufname lineno content annotation)
  "Return a string displayed in helm buffer.

BUFNAME, LINENO, CONTENT and ANNOTATION are concatenated to the string."
  (format "%s:%s:%s%s"
          (propertize bufname 'face 'font-lock-type-face)
          (propertize lineno 'face 'font-lock-keyword-face)
          content
          (if (or (null annotation) (string= annotation ""))
              ""
            (concat "\n  "
                    (propertize
                     annotation 'face 'font-lock-keyword-face)))))

(defun helm-bm-transform-to-candicate (bm)
  "Convert a BM to a CANDICATE."
  (let ((current-buf (overlay-buffer bm)))
    (with-current-buffer current-buf
      (let* ((start (overlay-start bm))
             (end (overlay-end bm))
             (bufname (buffer-name current-buf))
             (annotation (overlay-get bm 'annotation))
             (lineno (line-number-at-pos start)))
        (unless (< (- end start) 1)
          (helm-bm-candidate-transformer-display
           bufname (int-to-string lineno)
           (buffer-substring-no-properties start (1- end)) annotation))))))

(defun helm-bm-goto-next-buffer ()
  (interactive)
  (let* ((sel (helm-get-selection))
         (buf (overlay-buffer sel)))
    (while (eql buf (overlay-buffer (helm-get-selection)))
      (helm-next-line 1))))

(defun helm-bm-goto-precedent-buffer ()
  (interactive)
  (let* ((sel (helm-get-selection))
         (buf (overlay-buffer sel)))
    (while (eql buf (overlay-buffer (helm-get-selection)))
      (helm-previous-line 1))))

(defvar helm-bm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<up>")   'helm-bm-goto-precedent-buffer)
    (define-key map (kbd "M-<down>") 'helm-bm-goto-next-buffer)
    map))

(defvar helm-source-bm
  (helm-build-sync-source "Visible bookmarks"
    :multiline t
    :candidates (lambda () (if bm-cycle-all-buffers
                               (helm-bm-bookmarks-in-all-buffers)
                             (helm-bm-bookmarks-in-buffer helm-current-buffer)))
    :keymap 'helm-bm-map
    :candidate-transformer
    (lambda (candidates)
      (cl-loop for ov in candidates
               collect (cons (helm-bm-transform-to-candicate ov) ov)))
    :action '(("Jump to BM" . helm-bm-action-switch-to-buffer)
              ("Remove BM bookmark(s)" . helm-bm-action-remove-marked-bookmarks)
              ("Edit annotation"
               . helm-bm-action-bookmark-edit-annotation))))

(defun helm-bm-nearest-bm-from-pos ()
  "Return position of bm at point or nearest bm from point."
  (with-helm-current-buffer
    (let ((cpos (point)))
      (or (helm-aand (overlays-at cpos)
                     (bm-bookmarkp (car it))
                     (overlay-get it 'position))
          (helm-closest-number-in-list
           cpos
           (cl-loop for ov in (overlays-in (point-min) (point-max))
                    when (and ov (bm-bookmarkp ov))
                    collect (overlay-get ov 'position)))))))

;;;###autoload
(defun helm-bm ()
  "Show bookmarks of bm.el with `helm'.

Show bookmarks from all buffers or only `current-buffer' according to
`bm-cycle-all-buffers' value."
  (interactive)
  (helm :sources '(helm-source-bm)
        :quit-if-no-candidate
        (lambda ()
          (if bm-cycle-all-buffers
              (message "No BM candidates found in buffers")
            (message "No BM candidates in this buffer")))
        :preselect (lambda ()
                     (unless helm-bm-sort-from-pos
                       (let ((nearest (helm-bm-nearest-bm-from-pos)))
                         (when nearest
                           (goto-char (point-min))
                           (helm-skip-header-and-separator-line 'next)
                           (while (not (eql nearest
                                            (overlay-get
                                             (helm-get-selection) 'position)))
                             (helm-next-line 1))))))
        :buffer "*helm bm*"))

(provide 'helm-bm)


;;; helm-bm.el ends here

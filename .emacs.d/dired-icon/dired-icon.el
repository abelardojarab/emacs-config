;;; dired-icon.el --- A minor mode to display a list of associated icons in dired buffers. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Hong Xu <hong@topbug.net>

;; Author: Hong Xu <hong@topbug.net>
;; URL: https://gitlab.com/xuhdev/dired-icon
;; Version: 0.2
;; Keywords: dired, files
;; Package-Requires: ((emacs "24.3"))

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
;;
;; This package provides a minor mode `dired-icon-mode' to display an icon for
;; each file type in dired buffers.  Currently systems which run GTK 3, such as
;; GNU/Linux, GNU/kFreeBSD and FreeBSD, are fully supported (pre-requisition:
;; PyGObject for Python 3
;; <https://wiki.gnome.org/action/show/Projects/PyGObject> and optionally the
;; file command <http://darwinsys.com/file/>).  On other systems, currently only
;; directory icons are displayed.

;; To display the icons in a dired buffer, simply call M-x `dired-icon-mode'
;; inside a dired buffer.  To always display the file icons in dired buffers,
;; add the following to your ~/.emacs or ~/.emacs.d/init.el:
;;
;;     (add-hook 'dired-mode-hook 'dired-icon-mode)

;; To report bugs and make feature requests, please open a new ticket at the
;; issue tracker <https://gitlab.com/xuhdev/dired-icon/issues>.  To contribute,
;; please create a merge request at
;; <https://gitlab.com/xuhdev/dired-icon/merge_requests>.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'ezimage)
(require 'mailcap)

(defgroup dired-icon nil
  "Display icons for files in dired buffers."
  :group 'dired
  :prefix 'dired-icon)

(defcustom dired-icon-file-executable "file"
  "The path of the executable of the \"file\" executable."
  :type 'string)

(defcustom dired-icon-python-executable "python3"
  "The path of the executable of the \"python\" executable.
Python 3 is recommended."
  :type 'string)

(defcustom dired-icon-gtk-image-size 16
  "Image size on GTK systems, such as 16, 32, 64."
  :type 'integer)

(defvar dired-icon--script-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory)
  "The directory of this script.")

(defvar dired-icon--image-hash (make-hash-table :test 'equal)
  "A hash table that maps image path to the image object by \"create-image\".")

(defvar-local dired-icon--overlays nil
  "The overlays generated by dired-icon.")

(defun dired-icon--guess-mime-type (file-name)
  "Guess the mime type from a file name FILE-NAME."
   (cond
    ;; Use the file command to detect, for local readable files only.
    ((and (executable-find dired-icon-file-executable)
          (not (file-remote-p file-name))
          (file-readable-p file-name))
     (with-temp-buffer
       (when (call-process dired-icon-file-executable nil t nil
                           "-b" "--mime-type" (file-chase-links file-name))
         (substring (buffer-string) 0 -1))))
    ;; Use mailcap-extension-to-mime as a fallback
    (t (if (file-directory-p file-name)
           "inode/directory"
         (let ((ext (file-name-extension file-name)))
           (when ext (mailcap-extension-to-mime ext)))))))

(defun dired-icon--get-icons (file-names)
  "Create an alist, which maps the files FILE-NAMES to image objects."
  (cond
   ;; GTK 3
   ((and (executable-find dired-icon-python-executable)
         (= 0 (call-process dired-icon-python-executable nil nil nil
                            (expand-file-name
                             "get-icon-path-gtk3.py"
                             dired-icon--script-directory) "test")))
    (with-temp-buffer
      ;; insert the list of mimetypes into the temp buffer
      (dolist (fn file-names)
        (goto-char (point-min))  ;; reverse the file name insertion order
        (insert (concat (dired-icon--guess-mime-type fn) "\n")))
      ;; replace the current buffer with an icon file name in each line
      (call-process-region (point-min) (point-max)
                           dired-icon-python-executable
                           t t nil
                           (expand-file-name
                            "get-icon-path-gtk3.py"
                            dired-icon--script-directory)
                           (number-to-string dired-icon-gtk-image-size))
      ;; create an image object for each icon
      (let ((icon-images nil))
        (dolist (icon-fname (split-string (buffer-string) "\n" nil))
          (if (string= icon-fname "")
              (push nil icon-images)
            (let ((image (gethash icon-fname dired-icon--image-hash)))
              (unless image
                (setq image (create-image icon-fname))
                (puthash icon-fname image dired-icon--image-hash))
              (push image icon-images))))
        ;; The first element is an nil caused by the file end \n. Remove
        ;; it.
        (pop icon-images)
        (cl-pairlis file-names icon-images))))
   (t  ;; other unsupported systems
    (cl-pairlis file-names
                (make-list (length file-names) nil)))))

(defun dired-icon--get-files ()
  "List all files in the current dired buffer."
  (save-excursion
    (let ((files))
      (goto-char (point-min))
      (while (not (eobp))
        (unless (member (dired-get-filename 'verbatim t) '("." ".."))
          (let ((file (dired-get-filename nil t)))
            (when file (push file files))))
        (forward-line 1))
      files)))

(defun dired-icon--clear-icons ()
  "Clear the icons in the current dired buffer."
  (when (boundp 'dired-icon--overlays)
    (dolist (o dired-icon--overlays)
      (delete-overlay o)))
  (setq-local dired-icon--overlays nil))

(defun dired-icon--display ()
  "Display the icons of files in a dired buffer."
  ;; always clear the overlays from last readin
  (dired-icon--clear-icons)
  (let* ((files (dired-icon--get-files))
         (file-icons (dired-icon--get-icons files)))
    (save-excursion
      (cl-loop for (fn . icon) in file-icons
               count
               (when (dired-goto-file fn)
                 (let ((image
                        (or icon
                            ;; Even if the directory icon do not exist, we can
                            ;; still use the directory image from ezimage.
                            (when (file-directory-p fn)
                              ezimage-directory))))
                   (when image
                     (dired-move-to-filename)
                     (push (put-image image (point))
                           dired-icon--overlays))))))))

;;;###autoload
(define-minor-mode dired-icon-mode
  "Display icons according to the file types in dired buffers."
  :lighter "dired-icon"
  (if dired-icon-mode
      (progn
        (add-hook 'dired-after-readin-hook 'dired-icon--display)
        (when (eq major-mode 'dired-mode)
          (dired-icon--display)))
    (remove-hook 'dired-after-readin-hook 'dired-icon--display)
    (dired-icon--clear-icons)))

(provide 'dired-icon)

;;; dired-icon.el ends here

;; Local Variables:
;; coding: utf-8
;; fill-column: 80
;; indent-tabs-mode: nil
;; sentence-end-double-space: t
;; End:
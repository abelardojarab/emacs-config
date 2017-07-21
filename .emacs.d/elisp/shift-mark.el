;;; shift-mark.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  lab

;; Author: lab <lab@lab-vm>
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

(defun shift-mark (cmd)
  "Expands marked region to the point (position of cursor) after executing
command 'cmd'. If no region is marked, we mark one first."
  (interactive "_a")
  (if (not (region-active-p))
      (progn (set-mark-command nil)
             (command-execute cmd))
    (command-execute cmd)))

(defun shift-mark-forward-char ()
  (interactive)
  (shift-mark 'forward-char))

(defun shift-mark-backward-char ()
  (interactive)
  (shift-mark 'backward-char))

(defun shift-mark-forward-word ()
  (interactive)
  (shift-mark 'forward-word))

(defun shift-mark-backward-word ()
  (interactive)
  (shift-mark 'backward-word))

(defun shift-mark-forward-paragraph ()
  (interactive)
  (shift-mark 'forward-paragraph))

(defun shift-mark-backward-paragraph ()
  (interactive)
  (shift-mark 'backward-paragraph))

(defun shift-mark-forward-page ()
  (interactive)
  (if (region-active-p)
      (shift-mark 'forward-page)
    (scroll-down)))

(defun shift-mark-backward-page ()
  (interactive)
  (if (region-active-p)
      (shift-mark 'backward-page)
    (scroll-up)))

(defun shift-mark-previous-line ()
  (interactive)
  (shift-mark 'previous-line))

(defun shift-mark-next-line ()
  (interactive)
  (shift-mark 'next-line))

(defun backspace-delete-marked-region ()
  (interactive)
  (if (region-active-p)
      (kill-region (mark) (point))
    (delete-backward-char 1)))

(provide 'shift-mark)
;;; shift-mark.el ends here

;;; setup-functions-deferred.el ---             -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
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

;; Missing functions
(defun goto-eol ()
  (end-of-line))

(defun goto-bol ()
  (beginning-of-line))

(defun line-beginning-position (&optional n)
  (save-excursion
    (if (not (or (null n) (eql n 1)))
        (forward-line n))
    (goto-bol) (point)))

(defun line-end-position (&optional n)
  (save-excursion
    (if (not (or (null nil) (eql n 1)))
        (forward-line (1- n)))
    (goto-eol) (point)))

(defun regexp-match-p (regexps string)
  (and string
       (catch 'matched
         (let ((inhibit-changing-match-data t)) ;; small optimization
           (dolist (regexp regexps)
             (when (string-match regexp string)
               (throw 'matched t)))))))

(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq-default line-spacing 0.5)
    (setq-default line-spacing nil))
  (redraw-display))

(defun refresh-file ()
  (interactive)
  (revert-buffer t t t))

(defun put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun remove-trailing-spaces ()
  "Remove trailing spaces in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d Trailing spaces removed from buffer." remove-count))))))

(defun dos2unix ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match ""))
  (set-buffer-file-coding-system 'unix 't))

(defun unix2dos ()
  "Opposite of dos2unix"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

(defun beautify-region (beg end)
  (interactive "r")
  (setq end (save-excursion (goto-char end) (point-marker)))
  (indent-region beg end nil))

(defun beautify-buffer ()
  "Beautify buffer by applying indentation, whitespace fixup, alignment, and
case fixing to entire buffer. Calls `vhdl-beautify-region' for the entire
buffer."
  (interactive)
  (beautify-region (point-min) (point-max))
  (when noninteractive (save-buffer)))

(defun goto-match-paren-or-up (arg)
  "Go to the matching parenthesis if on parenthesis. Else go to
   the opening parenthesis one level up."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))))))))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise
   insert the character typed."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun indent-block ()
  (shift-region my/tab-width)
  (setq deactivate-mark nil))

(defun unindent-block()
  (shift-region (- my/tab-width))
  (setq deactivate-mark nil))

(defun shift-region(numcols)
  "Trick to expand the region to the beginning and end of the area selected
 much in the handy way I liked in the Dreamweaver editor."
  (if (< (point)(mark))
      (if (not(bolp))    (progn (beginning-of-line)(exchange-point-and-mark) (end-of-line)))
    (progn (end-of-line)(exchange-point-and-mark)(beginning-of-line)))
  (setq region-start (region-beginning))
  (setq region-finish (region-end))
  (save-excursion
    (if (< (point) (mark)) (exchange-point-and-mark))
    (let ((save-mark (mark)))
      (indent-rigidly region-start region-finish numcols))))

(defun unindent-block-or-line()
  "Unindent line, or block if it's a region selected.
When pressing Shift+tab, erase words backward (one at a time) up to the beginning of line.
Now it correctly stops at the beginning of the line when the pointer is at the first char of an indented line. Before the command would (unconveniently)  kill all the white spaces, as well as the last word of the previous line."
  (interactive)
  (if mark-active
      (unindent-block)
    (progn
      (unless(bolp)
        (if (looking-back "^[ \t]*")
            (progn
              ;;"a" holds how many spaces are there to the beginning of the line
              (let ((a (length(buffer-substring-no-properties (point-at-bol) (point)))))
                (progn
                  ;; delete backwards progressively in my/tab-width steps, but without going further of the beginning of line.
                  (if (> a my/tab-width)
                      (delete-backward-char my/tab-width)
                    (backward-delete-char a)))))
          ;; delete tab and spaces first, if at least 2 exist, before removing words
          (progn
            (if(looking-back "[ \t]\\{2,\\}")
                (delete-horizontal-space)
              (backward-kill-word 1))))))))

(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

(defun delete-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun number-rectangle (start end format-string from)
  "Delete (don't save) text in the region-rectangle, then number it."
  (interactive
   (list (region-beginning) (region-end)
         (read-string "Number rectangle: "
                      (if (looking-back "^ *") "%d. " "%d"))
         (read-number "From: " 1)))
  (save-excursion
    (goto-char start)
    (setq start (point-marker))
    (goto-char end)
    (setq end (point-marker))
    (delete-rectangle start end)
    (goto-char start)
    (cl-loop with column = (current-column)
          while (and (<= (point) end) (not (eobp)))
          for i from from   do
          (move-to-column column t)
          (insert (format format-string i))
          (forward-line 1)))
  (goto-char start))

(defun toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(defun my/next-user-buffer ()
  "Switch to the next user buffer.
 “user buffer” is a buffer whose name does not start with “*”.
If `my/switch-buffer-ignore-dired' is true, also skip directory buffer.
2015-01-05 URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (or
           (string-equal "*" (substring (buffer-name) 0 1))
           (if (string-equal major-mode "dired-mode")
               my/switch-buffer-ignore-dired
             nil
             ))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun my/previous-user-buffer ()
  "Switch to the previous user buffer.
 “user buffer” is a buffer whose name does not start with “*”.
If `my/switch-buffer-ignore-dired' is true, also skip directory buffer.
2015-01-05 URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (or
           (string-equal "*" (substring (buffer-name) 0 1))
           (if (string-equal major-mode "dired-mode")
               my/switch-buffer-ignore-dired
             nil
             ))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun update-progress-bar ()
  "Add one more step to the progress bar"
  ;; Use this for debugging, each step should take approximately the same time
  ;; (message "update-progress-bar: %s"
  ;;          (format "%.1fs" (float-time (time-subtract (current-time) my/start-time))))
  (when my/progress-bar
    (setq my/loading-string
          (concat my/loading-string
                  (make-string my/loading-step-size
                               my/loading-char)))
    (setq mode-line-format my/loading-string)
    (redisplay)))

(defun my/directory-tree (dir)
  "Returns the list of subdirs of 'dir' excluding any dot
dirs. Input is a string and output is a list of strings."
  (let* ((dir   (directory-file-name dir))
         (dirs  '())
         (files (directory-files dir nil nil t)))
    (dolist (f files)
      (unless (string-equal "." (substring f 0 1))
        (let ((f (concat dir "/" f)))
          (when (file-directory-p f)
            (setq dirs (append (cons f (my/directory-tree f))
                               dirs))))))
    dirs))

(defun my/read-file-lines (file)
  "Return a list of lines (strings) of the specified file"
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun my/read-file-as-string (file)
  "Return the content of the specified file as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun my/parent-directory (dir)
  "Return the path of the dir's parent directory"
  (file-name-directory (directory-file-name dir)))

(defun insert-datestamp ()
  "Produces and inserts a partial ISO 8601 format timestamp."
  (interactive)
  (insert (format-time-string "%F")))

(defun insert-datestamp-us ()
  "Produces and inserts a US datestamp."
  (interactive)
  (insert (format-time-string "%m/%d/%y")))

(defun insert-datestamp-us-full-year-and-dashes ()
  "Produces and inserts a US datestamp with full year and dashes."
  (interactive)
  (insert (format-time-string "%m-%d-%Y")))

(defun insert-datestamp-us-full-year ()
  "Produces and inserts a US datestamp with full year."
  (interactive)
  (insert (format-time-string "%m/%d/%Y")))

(defun insert-datestamp-us-words ()
  "Produces and inserts a US datestamp using words."
  (interactive)
  (insert (format-time-string "%A %B %d, %Y")))

(defun insert-timestamp-no-colons ()
  "Inserts a full ISO 8601 format timestamp with colons replaced by hyphens."
  (interactive)
  (insert (get-timestamp-no-colons)))

(defun insert-datestamp ()
  "Produces and inserts a partial ISO 8601 format timestamp."
  (interactive)
  (insert (format-time-string "%F")))

(defun get-timestamp-no-colons ()
  "Produces a full ISO 8601 format timestamp with colons replaced by hyphens."
  (interactive)
  (let* ((timestamp (get-timestamp))
         (timestamp-no-colons (replace-regexp-in-string ":" "-" timestamp)))
    timestamp-no-colons))

(defun get-timestamp ()
  "Produces a full ISO 8601 format timestamp."
  (interactive)
  (let* ((timestamp-without-timezone (format-time-string "%Y-%m-%dT%T"))
         (timezone-name-in-numeric-form (format-time-string "%z"))
         (timezone-utf-offset
          (concat (substring timezone-name-in-numeric-form 0 3)
                  ":"
                  (substring timezone-name-in-numeric-form 3 5)))
         (timestamp (concat timestamp-without-timezone
                            timezone-utf-offset)))
    timestamp))

(defun insert-timestamp ()
  "Inserts a full ISO 8601 format timestamp."
  (interactive)
  (insert (get-timestamp)))

(defun org-time-stamp-with-seconds-now ()
  (interactive)
  (let ((current-prefix-arg '(16)))
    (call-interactively 'org-time-stamp)))

(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
  Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".png"))
         (data (x-export-frames nil 'png)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

(provide 'setup-functions-deferred)
;;; setup-functions-deferred.el ends here

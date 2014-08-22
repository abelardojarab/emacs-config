;;; setup-utilities.el ---

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

;; Line spacing
(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq-default line-spacing 0.5)
    (setq-default line-spacing nil))
  (redraw-display))

;; Put file name on clip board
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

;; Remove the trailing spaces
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

;; Unfill
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the reverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the reverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

;; dos2unix
(defun dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))

;; Remove or add ending chars
(defun compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)
  (let (currentStateIsCompact (bigFillColumnVal 4333999) (deactivate-mark nil))
    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ) )

      (if (region-active-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))) )
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil)) ) )

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)) ) ) )

;; Beautify (poner bonito) tabulaciones en nuestro pograma en C/C++
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
(global-unset-key "\C-b")
(global-set-key "\C-b" 'beautify-buffer)

;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                         (save-excursion
                           (untabify (point-min) (point-max)))) nil))

(provide 'setup-utilities)
;;; setup-utilities.el ends here

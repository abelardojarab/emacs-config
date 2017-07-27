;;; indent-hint-mod.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Abelardo Jara-Berrocal

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

;; -*- encoding: utf-8-unix; -*-
(require 'cl)
(setq indent-hint-prefix "il-"
      indent-hint-key 'indent-hint-id
      indent-hint-bg 'indent-hint-bg
      indent-hint-gc-timer 5
      indent-hint-counter 0
      indent-hint-gc-counter 0
      indent-hint-list nil
      indent-hint-with-white-line nil
      indent-hint-lazy nil)

(make-variable-buffer-local
 (defvar indent-hint-counter 0
   "Count number of overlays in current buffer."))

(make-variable-buffer-local
 (defvar indent-hint-list nil
   "Set of hint lines in the current buffer."))

(make-variable-buffer-local
 (defvar indent-hint-regexp-list '(("^[ \t]*\\([^ \t]\\)"))
   "Set of hint lines in the current buffer."))

(defun indent-hint-genid ()
  (let ((i 'indent-hint-counter))
    (intern
     (concat "*" indent-hint-prefix
             (number-to-string
              (set i (1+ (eval i)))) "*"))))

(defun indent-hint-init(&optional l)
  (mapc
   (lambda(x) (or (local-variable-p x)
             (make-local-variable x)))
   '(indent-hint-counter
     indent-hint-lazy
     indent-hint-with-white-line
     indent-hint-gc-counter))
  (if l (setq indent-hint-with-white-line t))
  (indent-hint-bgo-init)
  (add-hook 'post-command-hook 'indent-hint-bgo-mv t t))

;; Creates the overlay
(defun indent-hint-make-overlay (b e)
  (let* ((ov (make-overlay b e)))
    (move-overlay ov b e)
    (setq indent-hint-list (cons ov indent-hint-list))
    ov))

(defun indent-hint-delete-overlay (o)
  (let ((ov o))
    (delete-overlay ov)))

;; Function to remove all overlays
(defun indent-hint-delete-all-overlays ()
  (dolist (o (overlays-in (window-start) (window-end)))
    (when (or (overlay-get o 'ov-one)
              (overlay-get o 'ov-two))
      (delete-overlay o))))

(defun indent-hint-gc ()
  (if (< indent-hint-gc-counter indent-hint-gc-timer)
      (setq indent-hint-gc-counter (1+ indent-hint-gc-counter))
    (progn
      (setq indent-hint-gc-counter 0)
      (dolist (x indent-hint-list)
        (if (null (eval x))
            (and (setq indent-hint-list
                       (delq x indent-hint-list))
                 (unintern x)))))))

;; *xpm
(defun make-indent-hint-xpm (width height color &optional lor)
  (let* ((w width)
         (h height)
         (s1 (concat "\"" (make-string w (string-to-char " ")) "\""))
         (s2 (cond
              ((eq lor 0)
               (concat "\"." (make-string (1- w) (string-to-char " ")) "\""))
              ((eq lor 1)
               (concat "\"" (make-string (1- w) (string-to-char " ")) ".\""))
              ((null lor)
               (concat "\"" (make-string (- (1- w)(/ (1- w) 2))(string-to-char " "))
                       "." (make-string (/ (1- w) 2)(string-to-char " ")) "\""))))
         (sa (concat s1 ",\n" s2 ",\n")))
    (eval `(concat "/* XPM */
static char * dot_vline_xpm[] = {
\"" (number-to-string w) " " (number-to-string h) " 2 1\",
\"  c None\",
\". c " color "\",\n"
,@(mapcar (lambda(x) sa)
          (make-list (1- (/ h 2)) 0))
s1 ",\n" s2 "};"
))))

(defvar ih-line-height (or (car (window-line-height)) 20))
(defvar ih-img (ih-make-xpm 9 ih-line-height "#4D4D4D"))
(defvar ih-img-lgc (ih-make-xpm 9 ih-line-height "#5d478b"))
(defvar ih-img-mtd (ih-make-xpm 9 ih-line-height "khaki"))
(defvar ih-img-dat (ih-make-xpm 9 ih-line-height "#008b45"))

(defun kill-indent-hint (m &optional n)
  (let ((n (or n (1+ m))))
    (mapc
     (lambda(x)
       (let ((i (overlay-get x 'indent-hint-id)))
         (if (overlayp i)
             (progn
               (mapc
                (lambda(y) (indent-hint-delete-overlay y))
                (eval i))
               (setq indent-hint-list
                     (delq i indent-hint-list))

               (delete-overlay i)
               (unintern i)))))
     (overlays-in m n))))

(defun erase-indent-hint (overlay after? beg end &optional length)
  (let ((inhibit-modification-hooks t)
        p1 p2)
    (if after?
        (save-excursion
          (forward-line)
          (setq p1 (line-beginning-position)
                p2 (+ p1 (current-indentation)))
          (kill-indent-hint p1 p2)
          (font-lock-fontify-block))
      (setq p1 (line-beginning-position) ;; (point)
            p2 (+ p1 (current-indentation)))
      (kill-indent-hint p1 p2))))

(defun what-overlays (&optional p)
  (interactive)
  (print
   (let ((pt (or p (point))))
     (cons (cons pt (current-column))
           (mapcar
            (lambda(x) (remove-if
                   nil
                   `(,x
                     ,(overlay-get x 'indent-hint-id)
                     ,(if (overlay-get x indent-hint-bg) 'bg)
                     ,(if (eq (overlay-get x 'face) 'hl-line) 'hl-line))))
            (overlays-at pt))))))

(defun draw-indent-hint-func (ov img color)
  (if (display-graphic-p)
      (overlay-put ov 'display
                   `(display (image
                              :type xpm
                              :data ,img
                              :pointer text
                              :ascent center
                              :mask (heuristic t))
                             rear-nonsticky (display)
                             fontified t))
    (overlay-put o 'display
                 "|")))

(defun draw-indent-hint (beg end id &optional img color)
  (let ((img (or img ih-img))
        (color (or color "#4D4D4D"))
        (ov (indent-hint-make-overlay beg end)))
    (overlay-put ov 'indent-hint-id t)
    (draw-indent-hint-func ov img color)
    ov))

(defun indent-hint-overlay-exist (p k)
  (let (r (l (overlays-at p)))
    (while (and l
                (null
                 (if (overlay-get (car l) k)
                     (setq r t)
                   nil)))
      (setq l (cdr l)))
    r))

;; Inserts the character over which font-lock will be applied
(defun indent-hint-white-line (&optional n)
  (save-excursion
    (let* ((i (current-indentation))
           (y (eq i (progn
                      (goto-char (line-end-position))
                      (current-column)))))
      (if (and y (> n i))
          (insert (make-string (- n i) 32)))
      y)))

;; Draws the line in the font-lock
(defun draw-indent-hint-line (&optional column img color)
  (interactive "P")
  (save-excursion
    (let* ((line (indent-hint-genid))
           (i (or column (current-indentation))))
      (make-local-variable line)
      (while (< i (if (<= (point-max) (line-end-position))
                      (forward-line)
                    (if indent-hint-with-white-line
                        (indent-hint-white-line (1+ i)))
                    (current-indentation)))
        (move-to-column i)
        (let* ((p1 (point)) (p2 (1+ p1)))
          (kill-indent-hint p1)

          ;; This is where the line is drawn
          (set line (cons (draw-indent-hint p1 p2 line img color) (eval line)))
          )))))

(defun indent-hint-bgo-init (&optional r)
  (let* ((b (line-beginning-position))
         (e (+ b (current-indentation)))
         o)
    (setq r (or r indent-hint-background-overlay))
    (when (not (or (equal r t) (equal r nil)))
      (make-local-variable r)
      (setq o (make-overlay b e))

      ;; Give it a property we can check later
      (overlay-put o 'indent-hint-id t)

      (setq indent-hint-list (cons o indent-hint-list))
      (overlay-put o 'modification-hooks '(erase-indent-hint))
      (overlay-put o 'insert-in-front-hooks '(erase-indent-hint))
      (overlay-put o 'insert-behind-hooks '(erase-indent-hint))
      (set r o))))

(defun indent-hint-bgo-mv (&optional o)
  (let* ((b (line-beginning-position))
         (e (+ b (current-indentation))))
    (when (not (or (equal o t) (equal o nil)))
      (move-overlay o b e))))

(defun indent-hint-add-fontlock (&optional regexp column img color mode)
  (interactive)
  (let ((x (or regexp "^")))
    (font-lock-add-keywords mode
                            `((,x
                               (0 (draw-indent-hint-line ,column ,img ,color)))))))

(defun indent-hint-remove-fontlock (&optional regexp column img color mode)
  "Remove keywords from major MODE, or from current buffer if nil"
  (interactive)
  (let ((x (or regexp "^")))
    (font-lock-remove-keywords mode
                               `((,x
                                  (0 (draw-indent-hint-line ,column ,img ,color)))))))

(defun indent-hint-current-column ()
  (save-excursion
    (goto-char (match-beginning 1))
    (current-column)))

(defun indent-hint-function (&optional lst l)
  (interactive)
  (let* ((c '(indent-hint-current-column))
         (lst (or lst '(("^[ \t]*\\([^ \t]\\)"))))
         (lst (if l lst (reverse lst))))
    (indent-hint-init l)
    (setq indent-hint-regexp-list l)
    (dolist (x lst)
      (indent-hint-add-fontlock (car x)
                                c
                                (cadr x)))))

(defun indent-hint (&optional regexp column img color)
  (interactive)
  (let ((x (or regexp "^")))
    (font-lock-add-keywords
     nil `((,x
            (0 (draw-indent-hint-line ,column ,img ,color)))))))

;; Define custom indent hint mode
(define-minor-mode indent-hint-mode
  "A minor mode to show indent hints."
  :init-value nil
  :lighter "ih"
  :group 'indent-hint-font-lock
  (if indent-hint-mode
      (let* ((c '(indent-hint-current-column)))
      (progn
        (setq-local indent-hint-list nil)
        (indent-hint-init indent-hint-list)
        (dolist (x indent-hint-regexp-list)
          (indent-hint (car x) c (cadr x)))))
    (progn
      (remove-hook 'post-command-hook 'indenst-hint-bgo-mv)
      (dolist (x indent-hint-regexp-list)
        (indent-hint-remove-fontlock (car x) (cadr x)))
      (dolist (ov indent-hint-list)
        (delete-overlay ov))
      (dolist (x indent-hint-list)
        (if (null (eval x))
            (and (unintern x)
                 (setq indent-hint-list
                       (delq x indent-hint-list)))))))

  ;; As of Emacs 24.4, 'font-lock-fontify-buffer' is not legal to
  ;; call, instead 'font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

;; example
(defun indent-hint-lisp ()
  (interactive)
  (indent-hint-function
   '(("^[ \t]*\\((\\)")
     ("\\((lambda\\|(defun\\|(defmacro\\)" ih-img-mtd)
     ("\\((let\\*?\\|(if\\|(cond\\|(case\\|(when\\|(progn\\|(for.*\\(map.*\\|(save-excursion\\)" ih-img-lgc)
     ("\\((setq\\|(defvar\\)" ih-img-dat)
     ("[,`#']+\\((\\)" ih-img-dat))))

(defun indent-hint-fixed (&optional img)
  (interactive)
  (indent-hint-function
   `(( "^[ \t]*\\([^ \t]\\)"
       ,img))))

(defun indent-hint-js ()
  (interactive)
  (indent-hint-function
   '(("^[ \t]*\\([^ \t}(]\\)")
     ("\\(function\\|var\\)" ih-img-mtd)
     ("\\(if\\|for\\|else\\|switch\\)" ih-img-lgc)
     ("^[ \t]*\\((\\)" ih-img-dat))))

(provide 'indent-hint-mod)
;;; indent-hint-mod.el ends here

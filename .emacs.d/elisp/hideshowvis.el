;;; hideshowvis.el --- Add markers to the fringe for regions foldable by hideshow.el
;;
;; Copyright 2008-2012, 2014, 2015 Jan Rehders
;;
;; Author: Jan Rehders <cmdkeen@gmx.de>
;; Contributions by Bryan Waite and Michael Heerdegen
;;
;; Version: 0.4
;; Additional contributions: 2010, 2012, Jason Milkins.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;
;;; Commentary:
;;
;; This minor mode will add little arrow bitmaps to the fringe for,
;; foldable regions in the buffer and to folded regions. It is
;; indented to be used in conjunction with hideshow.el which is a part
;; of GNU Emacs since version 20.
;;
;; Currently it works for me but is not tested heavily. Please report
;; any bugs to the above email address
;;
;;; Installation:
;; Add the following to your .emacs:
;;
;; (autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
;;
;; (autoload 'hideshowvis-minor-mode
;;   "hideshowvis"
;;   "Will indicate regions foldable with hideshow in the fringe."
;;   'interactive)
;;
;;
;; (dolist (hook (list 'emacs-lisp-mode-hook
;;                     'c++-mode-hook))
;;   (add-hook hook 'hideshowvis-enable))
;;
;; If enabling hideshowvis-minor-mode is slow on your machine use M-x,
;; customize-option, hideshowvis-ignore-same-line and set it to
;; nil. This will then display - icons for foldable regions of one
;; line, too but is faster
;;
;; At the end of this file you will find code to add to your .emacs to
;; enable displaying a + symbol in the fringe for folded regions. It
;; is not enabled by default because it might interfere with custom
;; hs-set-up-overlay functions
;;
;;; Changelog
;;
;; v0.4, 2010-07-03
;; - 8x8 arrow bitmaps ▶▼ now replace the existing + / - bitmaps.
;;
;; v0.3, 2010-08-26
;; - added autoload cookies
;; - fixed bug causing major mode menu to disappear, among other things
;;
;; v0.2, 2009-08-09
;; - '-' symbol in fringe is clickable
;; - don't show '-' in fringe if the foldable region ends on the same line
;;

;; arrow pointing down
(define-fringe-bitmap 'hideshowvis-hideable-marker [0 0 254 124 56 16 0 0])

(defconst hideshowvis-version "v0.4" "Version of hideshowvis minor mode")

(defface hideshowvis-hidable-face
  '((t (:foreground "#555" :box t)))
  "Face to highlight foldable regions"
  :group 'hideshow)

(defcustom hideshowvis-ignore-same-line t
  "Do not display foldable regions in the fringe if the matching
  closing parenthesis is on the same line. Set this to nil if
  enabling the minor mode is slow on your machine"
  :group 'hideshow)

(defun hideshowvis-highlight-hs-regions-in-fringe (&optional start end old-text-length)
  t)

;;;###autoload
(defun hideshowvis-click-fringe (event)
  (interactive "e")
  (mouse-set-point event)
  (end-of-line)
  (if (save-excursion
        (end-of-line 1)
        (or (hs-already-hidden-p)
            (progn
              (forward-char 1)
              (hs-already-hidden-p))))
      (hs-show-block)
    (hs-hide-block)
    (beginning-of-line)))

(defvar hideshowvis-mode-map
  (let ((hideshowvis-mode-map (make-sparse-keymap)))
    (define-key hideshowvis-mode-map [left-fringe mouse-1]
      'hideshowvis-click-fringe)
    hideshowvis-mode-map)
  "Keymap for hideshowvis mode")

;;;###autoload
(define-minor-mode hideshowvis-minor-mode ()
  "Will indicate regions foldable with hideshow in the fringe."
  :init-value nil
  :require 'hideshow
  :group 'hideshow
  :keymap hideshowvis-mode-map
  (condition-case nil
      (if hideshowvis-minor-mode
          (progn
            (hs-minor-mode 1)
            (hideshowvis-highlight-hs-regions-in-fringe (point-min) (point-max) 0)
            (add-to-list 'after-change-functions
                         'hideshowvis-highlight-hs-regions-in-fringe))
        (remove-overlays (point-min) (point-max) 'hideshowvis-hs t)
        (setq after-change-functions
              (remove 'hideshowvis-highlight-hs-regions-in-fringe
                      after-change-functions)))
    (error
     (message "Failed to toggle hideshowvis-minor-mode")
     )))

;;;###autoload
(defun hideshowvis-enable ()
  "Will enable hideshowvis minor mode"
  (interactive)
  (hideshowvis-minor-mode 1))


;; Uncomment the following to enable a clickable arrow pointing right,
;; and a small indicator box, which displays the number of lines
;; hidden.

;; (define-fringe-bitmap 'hs-marker [0 32 48 56 60 56 48 32]) ;; arrow pointing right.
;;
;; (defcustom hs-fringe-face 'hs-fringe-face
;;   "*Specify face used to highlight the fringe on hidden regions."
;;   :type 'face
;;   :group 'hideshow)
;;
;; (defface hs-fringe-face
;;   '((t (:foreground "#888" :box (:line-width 2 :color "grey75" :style released-button))))
;;   "Face used to highlight the fringe on folded regions"
;;   :group 'hideshow)
;;
;; (defcustom hs-face 'hs-face
;;   "*Specify the face to to use for the hidden region indicator"
;;   :type 'face
;;   :group 'hideshow)
;;
;; (defface hs-face
;;   '((t (:background "#210" :box t :height 70)))
;;   "Face to hightlight the ... area of hidden regions"
;;   :group 'hideshow)
;;
;; (defun display-code-line-counts (ov)
;;   (when (eq 'code (overlay-get ov 'hs))
;;     (let* ((marker-string "*fringe-dummy*")
;;            (marker-length (length marker-string))
;;            (display-string (format "(%d)..." (count-lines (overlay-start ov) (overlay-end ov))))
;;            )
;;       (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
;;       (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
;;       (overlay-put ov 'before-string marker-string)
;;       (put-text-property 0 (length display-string) 'face 'hs-face display-string)
;;       (overlay-put ov 'display display-string)
;;       )))
;;
;; (setq hs-set-up-overlay 'display-code-line-counts)

(provide 'hideshowvis)

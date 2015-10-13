;;; setup-hideshow.el ---

;; Copyright (C) 2014, 2015  abelardo.jara-berrocal

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

(require 'folding)
(defun my-folding-check-folded ()
  "Function to determine if this file is in folded form."
  (let ((folding-re1 "^.?.?.?{{{")
        (folding-re2 "[\r\n].*}}}"))
    (save-excursion
      (goto-char (point-min))
      ;;  If we found both, we assume file is folded
      (and (assq major-mode folding-mode-marks-alist)
         (< (point-max) 10000)
         (re-search-forward folding-re1 nil t)
         ;; if file is folded, there are \r's
         (re-search-forward "[\r\n]" nil t)
         (re-search-forward folding-re2 nil t)))))
(setq folding-check-folded-file-function 'my-folding-check-folded)
(folding-mode-add-find-file-hook)
(global-set-key (kbd "M-s i") folding-mode-prefix-map)
(define-key folding-mode-prefix-map (kbd "i") 'folding-shift-in)
(define-key folding-mode-prefix-map (kbd "o") 'folding-shift-out)
(define-key folding-mode-prefix-map (kbd "<SPC>") 'folding-context-next-action)
(define-key folding-mode-prefix-map (kbd "j") 'folding-next-visible-heading)
(define-key folding-mode-prefix-map (kbd "n") 'folding-next-visible-heading)
(define-key folding-mode-prefix-map (kbd "k") 'folding-previous-visible-heading)
(define-key folding-mode-prefix-map (kbd "p") 'folding-previous-visible-heading)

;; why open-fold is defined but not called in this function?
(defun folding-shift-in (&optional noerror)
  (interactive)
  (labels
      ((open-fold nil
                  (let ((data (folding-show-current-entry noerror t)))
                    (and data
                       (progn
                         (when folding-narrow-by-default
                           (setq folding-stack
                                 (if folding-stack
                                     (cons (cons (point-min-marker)
                                                 (point-max-marker))
                                           folding-stack)
                                   '(folded)))
                           (folding-set-mode-line))
                         (folding-narrow-to-region (car data) (nth 1 data)))))))
    (let ((goal (point)))
      (while (folding-skip-ellipsis-backward)
        (beginning-of-line)
        (open-fold)
        (goto-char goal))
      (if folding-narrow-by-default
          (open-fold)
        (widen)))))

;; add keywords to current buffer directly, overwrite the original function in folding.el
(defun folding-font-lock-support ()
  "Add font lock support."
  (ignore-errors
    (font-lock-add-keywords nil (folding-font-lock-keywords major-mode))))

(require 'fold-dwim)
(defun folding-marker-p (&optional pos)
  (eq (get-char-property (or pos (point)) 'face) 'fringe))

(defadvice fold-dwim-toggle (around toggle-folding-on-folding-marker activate)
  (if (folding-marker-p)
      (folding-toggle-show-hide)
    ad-do-it))

(defadvice forward-comment (around stop-at-folding-header (count) activate)
  (if (= 0 (ad-get-arg 0))
      (progn ad-do-it)
    (if (folding-marker-p)
        (setq ad-return-value nil)
      (let ((loop-times (abs count))
            (direction (/ count (abs count))))
        (ad-set-arg 0 direction)
        (setq ad-return-value t)
        (while (and (> loop-times 0) ad-return-value)
          ad-do-it
          (when ad-return-value
            (if (> direction 0)
                (if (folding-marker-p)
                    (setq ad-return-value nil)
                  (when (folding-marker-p (- (point) 2))
                    (setq ad-return-value nil)
                    (forward-char -2)
                    (beginning-of-line)))
              (when (folding-marker-p)
                (end-of-line)
                (setq ad-return-value nil)))
            (setq loop-times (1- loop-times))))))))

(defadvice fold-dwim-hide-all (around folding-open-first activate)
  (if (and (boundp 'folding-mode) folding-mode)
      (progn
        (folding-uninstall)
        (let ((hs-hide-comments-when-hiding-all nil))
          ad-do-it)
        (folding-mode))
    ad-do-it))

;; Hideshow Visualization
(when window-system
  (require 'hideshowvis)
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (dolist (x '(prog emacs-lisp lisp java perl sh python js2 nxml))
    (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'hs-minor-mode))
  (dolist (x '(prog emacs-lisp lisp java perl sh python js2 nxml))
    (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'hideshowvis-enable)))

;; Origami
(add-to-list 'load-path "~/.emacs.d/origami")
(require 'origami)

(provide 'setup-hideshow)
;;; setup-hideshow.el ends here

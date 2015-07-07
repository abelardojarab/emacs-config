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
(defun iy-folding-check-folded ()
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
(setq folding-check-folded-file-function 'iy-folding-check-folded)
(folding-mode-add-find-file-hook)
(global-set-key (kbd "M-s i") folding-mode-prefix-map)
(folding-add-to-marks-list 'ruby-mode "# {{{" "# }}}" nil t)
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

;; Keys
(global-set-key (kbd "M-s <SPC>") 'fold-dwim-toggle)
(global-set-key (kbd "M-s M-<SPC>") 'fold-dwim-toggle)
(global-set-key (kbd "M-s C-<SPC>") 'fold-dwim-hide-all)
(global-set-key (kbd "M-s M-<SPC>") 'fold-dwim-show-all)

;; Code folding
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)
(dolist (hook (list 'lisp-mode-hook
                    'js2-mode-hook
                    'emacs-lisp-mode-hook
                    'c++-mode-hook))
  (add-hook hook 'hideshowvis-enable))
(global-set-key (kbd "<f7>") 'hs-hide-block)
(global-set-key (kbd "S-<f7>") 'hs-show-block)
(global-set-key (kbd "C-c @ SPC") 'hs-show-block) ; second binding

;; enable `hs-minor-mode' at startup
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(dolist (x '(emacs-lisp lisp java perl sh python js2))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'hs-minor-mode))

(defun hs-minor-mode-settings ()
  "settings of `hs-minor-mode'."
  (defvar hs-headline-max-len 30 "*Maximum length of `hs-headline' to display.")
  (setq hs-isearch-open t)

  (defun hs-display-headline ()
    (let* ((len (length hs-headline))
           (headline hs-headline)
           (postfix ""))
      (when (>= len hs-headline-max-len)
        (setq postfix "...")
        (setq headline (substring hs-headline 0 hs-headline-max-len)))
      (if hs-headline (concat headline postfix " ") "")))

  (setq-default mode-line-format
                (append '((:eval (hs-display-headline))) mode-line-format))

  ;; Add the following to your .emacs and uncomment it in order to get a + symbol
  (define-fringe-bitmap 'hs-marker [0 32 48 56 60 56 48 32])

  (defcustom hs-fringe-face 'hs-fringe-face
    "*Specify face used to highlight the fringe on hidden regions."
    :type 'face
    :group 'hideshow)

  (defface hs-fringe-face
    '((t (:foreground "#999" :box (:line-width 2 :color "grey75" :style released-button))))
    "Face used to highlight the fringe on folded regions"
    :group 'hideshow)

  (defcustom hs-face 'hs-face
    "*Specify the face to to use for the hidden region indicator"
    :type 'face
    :group 'hideshow)

  (defface hs-face
    '((t (:background "#558" :box t)))
    "Face to hightlight the ... area of hidden regions"
    :group 'hideshow)

  (defun display-code-line-counts (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((marker-string "*fringe-dummy*")
             (marker-length (length marker-string))
             (display-string (format "(%d)..." (count-lines
                                                (overlay-start ov) (overlay-end ov)))))
        (overlay-put ov 'help-echo "Hiddent text. M-s <SPC> to show")
        (put-text-property 0 marker-length 'display (list 'left-fringe
                                                          'hs-marker 'hs-fringe-face) marker-string)
        (overlay-put ov 'before-string marker-string)
        (put-text-property 1 (length display-string) 'face 'hs-face
                           display-string)
        (overlay-put ov 'display display-string))))
  (setq hs-set-up-overlay 'display-code-line-counts)

  (defadvice folding-subst-regions (around toggle-fringe (list find replace) activate)
    ad-do-it
    (save-excursion
      (while list
        (let* ((begin (car list))
               (end (cadr list))
               bol eol
               (marker-string "*fringe-dummy*")
               (marker-length (length marker-string)))
          (dolist (ov (overlays-in begin end))
            (when (overlay-get ov 'fringe-folding-p)
              (delete-overlay ov)))
          (when (and (eq find ?\n) (eq replace ?\r))
            ;; \\n -> \\r add fringe
            (goto-char begin)
            (search-forward "\r")
            (forward-char -1)
            (let* ((ov (make-overlay (point) end))
                   (display-string (format " (%d)..." (count-lines begin end))))
              (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'fringe-face) marker-string)
              (overlay-put ov 'before-string marker-string)
              (put-text-property 1 (length display-string) 'face 'collapsed-face display-string)
              (overlay-put ov 'display display-string)
              (overlay-put ov 'priority 9999)
              (overlay-put ov 'fringe-folding-p t))))
        (setq list (cdr (cdr list))))))

  ;; Support to toggle/untoggle all
  (defvar hs-hide-all nil "Current state of hideshow for toggling all.")
  (make-local-variable 'hs-hide-all)

  (defun hs-toggle-hiding-all ()
    "Toggle hideshow all."
    (interactive)
    (setq hs-hide-all (not hs-hide-all))
    (if hs-hide-all
        (hs-hide-all)
      (hs-show-all)))

  (defvar fold-all-fun nil "Function to fold all.")
  (make-variable-buffer-local 'fold-all-fun)
  (defvar fold-fun nil "Function to fold.")
  (make-variable-buffer-local 'fold-fun)

  (defun toggle-fold-all ()
    "Toggle fold all."
    (interactive)
    (if fold-all-fun
        (call-interactively fold-all-fun)
      (hs-toggle-hiding-all)))

  (defun toggle-fold ()
    "Toggle fold."
    (interactive)
    (if fold-fun
        (call-interactively fold-fun)
      (hs-toggle-hiding))))

(eval-after-load "hideshow"
  '(hs-minor-mode-settings))

;; Yafolding
(add-to-list 'load-path "~/.emacs.d/yafolding")
(when (require 'yafolding nil 'noerror)
  (add-hook 'prog-mode-hook
            (lambda () (yafolding-mode t)))
  (define-key yafolding-mode-map (kbd "<C-S-return>") nil)
  (define-key yafolding-mode-map (kbd "<C-return>") nil)
  (define-key yafolding-mode-map (kbd "C-c <C-S-return>") 'yafolding-toggle-all)
  (define-key yafolding-mode-map (kbd "C-c <C-return>") 'yafolding-toggle-element))

;; Indent Hint
;; (require 'indent-hint)

(provide 'setup-hideshow)
;;; setup-hideshow.el ends here

;;; setup-folding.el ---

;; Copyright (C) 2014, 2015, 2016, 2017  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@Abelardos-MacBook-Pro.local>
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

(use-package folding
  :config (progn
            (defun my/folding-check-folded ()
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
            (setq folding-check-folded-file-function 'my/folding-check-folded)
            (folding-mode-add-find-file-hook)
            (define-key folding-mode-prefix-map (kbd "<SPC>") 'folding-context-next-action)

            ;; add keywords to current buffer directly, overwrite the original function in folding.el
            (defun folding-font-lock-support ()
              "Add font lock support."
              (ignore-errors
                (font-lock-add-keywords nil (folding-font-lock-keywords major-mode))))))

;; Enable fold dwim (do what i mean)
(use-package fold-dwim
  :config (progn
            (defun folding-marker-p (&optional pos)
              (eq (get-char-property (or pos (point)) 'face) 'fringe))

            (defadvice fold-dwim-toggle (around toggle-folding-on-folding-marker activate)
              (if (folding-marker-p)
                  (folding-toggle-show-hide)
                ad-do-it))

            (defadvice fold-dwim-hide-all (around folding-open-first activate)
              (if (and (boundp 'folding-mode) folding-mode)
                  (progn
                    (folding-uninstall)
                    (let ((hs-hide-comments-when-hiding-all nil))
                      ad-do-it)
                    (folding-mode))
                ad-do-it))))

;; hideshow (hs-minor-mode)
(use-package hideshow
  :defer t
  :commands hs-toggle-hiding)

;; Visual hideshow mode
(use-package hideshowvis
  :if (display-graphic-p)
  :after hideshow
  :commands (hideshowvis-enable toggle-fold toggle-fold-all hs-toggle-hiding-all)
  :diminish hs-minor-mode
  :init (progn
          ;; enable `hs-minor-mode' and 'hideshowvis-minor-mode
          (dolist (hook my/hideshow-modes)
            (add-hook hook (lambda ()
                             (progn
                               (hs-minor-mode 1)
                               (hideshowvis-enable))))))
  :config (progn

            (defvar hs-special-modes-alist
              (mapcar 'purecopy
                      '((c-mode "{" "}" "/[*/]" nil nil)
                        (c++-mode "{" "}" "/[*/]" nil nil)
                        (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
                        (java-mode "{" "}" "/[*/]" nil nil)
                        (js-mode "{" "}" "/[*/]" nil)
                        (javascript-mode  "{" "}" "/[*/]" nil))")"))

            (defun hs-minor-mode-settings ()
              "settings of `hs-minor-mode'."
              (defvar hs-headline-max-len 20 "*Maximum length of `hs-headline' to display.")
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

              ;; Redefine display functions
              (defun display-code-line-counts (ov)
                (when (eq 'code (overlay-get ov 'hs))
                  (let* ((marker-string "*fringe-dummy*")
                         (marker-length (length marker-string))
                         (display-string (format " ... %s <%d> ... "
                                                 (replace-regexp-in-string
                                                  "\n" ""
                                                  (replace-regexp-in-string
                                                   "^[ \t]*" ""
                                                   (replace-regexp-in-string
                                                    "[ \t]*$" ""
                                                    (buffer-substring (overlay-start ov)
                                                                      (+ (overlay-start ov) 40)))))
                                                 (count-lines (overlay-start ov)
                                                              (overlay-end ov)))))
                    (overlay-put ov 'help-echo "Hidden text... ")
                    (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'fringe-face) marker-string)
                    (overlay-put ov 'before-string marker-string)
                    (put-text-property 1 (length display-string) 'face 'outline-4 display-string)
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
                               (display-string (format " ... %s <%d> ... "
                                                       (replace-regexp-in-string
                                                        "\n" ""
                                                        (replace-regexp-in-string
                                                         "^[ \t]*" ""
                                                         (replace-regexp-in-string
                                                          "[ \t]*$" ""
                                                          (buffer-substring (overlay-start ov)
                                                                            (+ (overlay-start ov) 40)))))
                                                       (count-lines (overlay-start ov)
                                                                    (overlay-end ov)))))
                          (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'fringe-face) marker-string)
                          (overlay-put ov 'before-string marker-string)
                          (put-text-property 1 (length display-string) 'face 'outline-4 display-string)
                          (overlay-put ov 'display display-string)
                          (overlay-put ov 'priority 9999)
                          (overlay-put ov 'fringe-folding-p t))))
                    (setq list (cdr (cdr list))))))

              ;; Add the following to your .emacs and uncomment it in order to get a right arrow symbol
              (define-fringe-bitmap 'hs-marker [0 32 48 56 60 56 48 32])

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

            (hs-minor-mode-settings)))

;; Origami mode
(use-package origami
  :commands (origami-toggle-node
             origami-toggle-all-nodes
             origami-show-only-node
             origami-recursively-toggle-node
             origami-mode)
  :bind (("C-c SPC" . origami-recursively-toggle-node)
         ("C-c TAB" . origami-toggle-all-nodes)
         ("C-c *" . origami-toggle-all-nodes)
         ("C-c +" . origami-open-all-nodes))
  :load-path (lambda () (expand-file-name "origami/" user-emacs-directory))
  :config (progn
            (global-origami-mode)))

;; Vi-like fold
(use-package vimish-fold
  :commands (vimish-fold-mode vimish-fold-global-mode)
  :bind (:map
         vimish-fold-folded-keymap ("<tab>" . vimish-fold-unfold)
         :map
         vimish-fold-unfolded-keymap ("<tab>" . vimish-fold-refold))
  :load-path (lambda () (expand-file-name "vimish-fold/" user-emacs-directory))
  :config (progn
            (setq-default
             vimish-fold-dir "~/.emacs.cache/.vimish-fold/"
             vimish-fold-header-width 79)))

(provide 'setup-folding)
;;; setup-hideshow.el ends here

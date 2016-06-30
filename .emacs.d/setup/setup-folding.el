;;; setup-folding.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

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

(use-package folding
  :config (progn
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
            (define-key folding-mode-prefix-map (kbd "<SPC>") 'folding-context-next-action)

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

;; Enable hideshowvis
(use-package hideshowvis
  :if (display-graphic-p)
  :commands (hideshowvis-enable toggle-fold toggle-fold-all hs-toggle-hiding hs-toggle-hiding-all)
  :diminish hs-minor-mode
  :config (progn

            ;; enable `hs-minor-mode' at startup
            (dolist (hook (list 'prog-mode-hook))
              (add-hook hook (lambda () (hs-minor-mode 1))))

            (dolist (hook (list 'prog-mode-hook))
              (add-hook hook 'hideshowvis-enable))

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
                    (overlay-put ov 'help-echo "Hiddent text... ")
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
  :commands (origami-toggle-node origami-toggle-all-nodes origami-show-only-node origami-mode)
  :bind (:map ctl-x-map
              ("." . origami-toggle-all-nodes)
              ("-" . origami-toggle-node)
              ("+" . origami-show-only-node))
  :load-path (lambda () (expand-file-name "origami/" user-emacs-directory))
  :config (progn
            (global-origami-mode)))

(provide 'setup-folding)
;;; setup-hideshow.el ends here

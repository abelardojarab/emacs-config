;;; setup-indent.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

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

;; Set indent to 4 instead of 2
(defvar my/tab-width 4)
(setq standard-indent my/tab-width)

;; My personal configurations, has to use setq-default
(setq-default indent-tabs-mode   nil
              default-tab-width  my/tab-width
              tab-width          my/tab-width
              tabify-regexp      "^\t* [ \t]+")

(defun unindent-dwim (&optional count-arg)
  "Keeps relative spacing in the region.  Unindents to the next multiple of the current tab-width"
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning)) (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position)))
        (min-indentation)
        (count (or count-arg 1)))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (add-to-list 'min-indentation (current-indentation))
        (forward-line)))
    (if (< 0 count)
        (if (not (< 0 (apply 'min min-indentation)))
            (error "Can't indent any more.  Try `indent-rigidly` with a negative arg.")))
    (if (> 0 count)
        (indent-rigidly beg end (* (- 0 tab-width) count))
      (let (
            (indent-amount
             (apply 'min (mapcar (lambda (x) (- 0 (mod x tab-width))) min-indentation))))
        (indent-rigidly beg end (or
                                 (and (< indent-amount 0) indent-amount)
                                 (* (or count 1) (- 0 tab-width))))))))

(defun my/tab-forward-char (n)
  (let ((space (- (line-end-position) (point))))
    (if (> space my/tab-width)
        (forward-char n)
      (move-end-of-line 1)
      (insert (make-string (- n space) ? )))))

(defun my/tab-jump ()
  (interactive)
  (let ((shift (mod (current-column) my/tab-width)))
    (my/tab-forward-char (- my/tab-width shift))))

;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                    (save-excursion
                      (untabify (point-min) (point-max)))) nil))

;; auto-indent pasted code
(defadvice yank (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode lisp-mode c-mode c++-mode
                                objc-mode latex-mode plain-tex-mode python-mode java-mode js2-mode))
      (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode lisp-mode c-mode c++-mode
                                objc-mode latex-mode plain-tex-mode python-mode java-mode js2-mode))
      (indent-region (region-beginning) (region-end) nil)))

;; Disable electric indent
(if (featurep 'electric-indent-mode)
    (add-hook 'prog-mode-hook (lambda () (electric-indent-local-mode -1))))

;; Auto-indent mode
(use-package auto-indent-mode
  :pin manual
  :defer t
  :commands auto-indent-mode
  :custom ((auto-indent-indent-style              'conservative)
           (auto-indent-on-visit-file             nil)
           (auto-indent-blank-lines-on-move       nil)
           (auto-indent-next-pair-timer-geo-mean  (quote ((default 0.0005 0))))
           (auto-indent-disabled-modes-list       (list (quote vhdl-mode)))))

;; Permanent indentation guide
(use-package indent-hint
  :defer t
  :commands indent-hint-mode
  :custom ((indent-hint-background-overlay t)
           (indent-hint-bg                 nil)))

;; Transient indentation guide
(use-package highlight-indent-guides
  :defer t
  :diminish t
  :commands highlight-indent-guides-mode
  :custom ((highlight-indent-guides-method                   'character)
           (highlight-indent-guides-responsive               'top)
           (highlight-indent-guides-delay                    0)
           (highlight-indent-guides-auto-character-face-perc 7))
  :config (progn
            (when (display-graphic-p)
              (setq highlight-indent-guides-auto-enabled nil)
              (set-face-background 'highlight-indent-guides-odd-face "lightgray")
              (set-face-background 'highlight-indent-guides-even-face "lightgray")
              (set-face-foreground 'highlight-indent-guides-character-face "dimgray"))))

;; Highlight indentation levels
(use-package highlight-indentation
  :defer t
  :commands highlight-indentation-mode)

(provide 'setup-indent)

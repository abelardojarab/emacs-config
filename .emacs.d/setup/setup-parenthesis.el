;;; setup-parenthesis.el ---                               -*- lexical-binding: t; -*-

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

;; Show-paren-mode: subtle blinking of matching paren (defaults are ugly)
(use-package paren
  :defer t
  :hook (after-init . show-paren-mode)
  :commands show-paren-mode
  :custom ((show-paren-style                   'paren)
           (show-paren-delay                   0.03)
           (show-paren-highlight-openparen     t)
           (show-paren-when-point-inside-paren nil)
           (show-paren-when-point-in-periphery t))
  :init (defun display-line-overlay+ (pos str &optional face)
          "Display line at POS as STR with FACE.
FACE defaults to inheriting from default and highlight."
          (let ((ol (save-excursion
                      (goto-char pos)
                      (make-overlay (line-beginning-position)
                                    (line-end-position)))))
            (overlay-put ol 'display str)
            (overlay-put ol 'face
                         (or face '(:inherit default :inherit highlight)))
            ol))
  :config (progn
            ;; Show paren-mode when off-screen
            (defadvice show-paren-function (after show-matching-paren-offscreen activate)
              "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
              (interactive)
              (ignore-errors
                (let* ((cb (char-before (point)))
                       (matching-text (and cb
                                           (char-equal (char-syntax cb) ?\) )
                                           (blink-matching-open))))
                  (when matching-text (message matching-text)))))

            ;; we will call `blink-matching-open` ourselves...
            (remove-hook 'post-self-insert-hook
                         #'blink-paren-post-self-insert-function)

            ;; this still needs to be set for `blink-matching-open` to work
            (setq blink-matching-paren 'show)
            (let ((ov nil)) ; keep track of the overlay
              (advice-add
               #'show-paren-function
               :after
               (defun show-paren--off-screen+ (&rest _args)
                 "Display matching line for off-screen paren."
                 (when (overlayp ov)
                   (delete-overlay ov))
                 ;; check if it's appropriate to show match info,
                 ;; see `blink-paren-post-self-insert-function'
                 (when (and (overlay-buffer show-paren--overlay)
                            (not (or cursor-in-echo-area
                                     executing-kbd-macro
                                     noninteractive
                                     (minibufferp)
                                     this-command))
                            (and (not (bobp))
                                 (memq (char-syntax (char-before)) '(?\) ?\$)))
                            (= 1 (logand 1 (- (point)
                                              (save-excursion
                                                (forward-char -1)
                                                (skip-syntax-backward "/\\")
                                                (point))))))
                   ;; rebind `minibuffer-message' called by
                   ;; `blink-matching-open' to handle the overlay display
                   (cl-letf (((symbol-function #'minibuffer-message)
                              (lambda (msg &rest args)
                                (let ((msg (apply #'format-message msg args)))
                                  (setq ov (display-line-overlay+
                                            (window-start) msg))))))
                     (blink-matching-open))))))

            ;; Enable mode
            (show-paren-mode 1)))

;; Smartparens
(use-package smartparens
  :defer t
  :diminish smartparens-mode
  :commands (smartparens-global-mode
             smartparens-mode
             show-smartparens-global-mode
             show-smartparens-mode)
  :hook (after-init . (smartparens-global-mode show-smartparens-global-mode))
  :init (use-package smartparens-config)
  :custom ((sp-max-pair-length            2)
           (sp-escape-quotes-after-insert nil))
  :config (progn
            ;; disable pairing of ' in minibuffer
            (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

            ;; use smartparens to automatically indent correctly when opening new block
            (dolist (mode '(c-mode c++-mode java-mode))
              (sp-local-pair mode "{" nil :post-handlers '((apm-c-mode-common-open-block "RET"))))

            (dolist (mode '(xml-mode php-mode))
              (sp-local-pair mode "<!--" "-->"
                             :post-handlers '(("| " "SPC"))))))

;; Autopair
(use-package autopair
  :defer t
  :commands (autopair-mode
             autopair-global-mode)
  :diminish autopair-mode
  :config (progn
            (autopair-global-mode) ;; enable autopair in all buffers
            (setq autopair-autowrap t)
            (put 'autopair-insert-opening         'delete-selection t)
            (put 'autopair-skip-close-maybe       'delete-selection t)
            (put 'autopair-insert-or-skip-quote   'delete-selection t)
            (put 'autopair-extra-insert-opening   'delete-selection t)
            (put 'autopair-extra-skip-close-maybe 'delete-selection t)
            (put 'autopair-backspace              'delete-selection 'supersede)
            (put 'autopair-newline                'delete-selection t)

            ;; only needed if you use autopair
            (add-hook 'text-mode-hook
                      (lambda () (setq autopair-dont-activate t)))
            (add-hook 'org-mode-hook
                      (lambda () (setq autopair-dont-activate t)))))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :defer t
  :commands rainbow-delimiters-mode
  :hook ((emacs-lisp-mode lisp-mode prog-mode) . rainbow-delimiters-mode))

;; Legacy minor mode
(use-package paredit
  :defer t
  :commands paredit-mode)

(provide 'setup-parenthesis)
;;; setup-autopair.el ends here

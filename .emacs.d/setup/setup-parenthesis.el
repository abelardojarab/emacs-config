;;; setup-parenthesis.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Abelardo Jara-Berrocal

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
  :init (show-paren-mode t)
  :commands show-paren-mode
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
  :init (progn
          (use-package smartparens-config)

          ;; subtle blinking of matching paren (defaults are ugly)
          (show-smartparens-global-mode t)

          (smartparens-global-mode t))
  :config (progn

            ;; Faster performance
            (setq-default sp-max-pair-length 2)

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

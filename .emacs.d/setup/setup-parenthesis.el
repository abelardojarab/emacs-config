;;; setup-parenthesis.el ---

;; Copyright (C) 2014, 2016  abelardo.jara-berrocal

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

;; Show-paren-mode: subtle blinking of matching paren (defaults are ugly)
(use-package paren
  :config (progn
            ;; Show paren-mode when off-screen
            (defadvice show-paren-function
                (after show-matching-paren-offscreen activate)
              "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
              (interactive)
              (let* ((cb (char-before (point)))
                     (matching-text (and cb
                                         (char-equal (char-syntax cb) ?\) )
                                         (blink-matching-open))))
                (when matching-text (message matching-text))))

            ;; Opening bracket to be highlighted when the point is on the closing bracket
            (defadvice show-paren-function
                (around show-paren-closing-before
                        activate compile)
              (if (eq (syntax-class (syntax-after (point))) 5)
                  (save-excursion
                    (forward-char)
                    ad-do-it)
                ad-do-it))

            (show-paren-mode 1)))

;; Smartparens
(use-package smartparens
  :diminish smartparens-mode
  :load-path "~/.emacs.d/smartparens"
  :init (progn
          (require 'smartparens-config))
  :config (progn
            (show-smartparens-global-mode 1)))

;; Autopair
(use-package autopair
  :load-path "~/.emacs.d/autopair"
  :diminish autopair-mode
  :config (progn
            (autopair-global-mode) ;; enable autopair in all buffers
            (setq autopair-autowrap t)
            (put 'autopair-insert-opening 'delete-selection t)
            (put 'autopair-skip-close-maybe 'delete-selection t)
            (put 'autopair-insert-or-skip-quote 'delete-selection t)
            (put 'autopair-extra-insert-opening 'delete-selection t)
            (put 'autopair-extra-skip-close-maybe 'delete-selection t)
            (put 'autopair-backspace 'delete-selection 'supersede)
            (put 'autopair-newline 'delete-selection t)

            ;; only needed if you use autopair
            (add-hook 'text-mode-hook
                      #'(lambda () (setq autopair-dont-activate t)))
            (add-hook 'org-mode-hook
                      #'(lambda () (setq autopair-dont-activate t)))))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :load-path "~/.emacs.d/rainbow-delimiters"
  :config (progn
            (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
            (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(provide 'setup-parenthesis)
;;; setup-autopair.el ends here

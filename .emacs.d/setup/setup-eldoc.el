;;; setup-eldoc.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

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

(use-package eldoc
  :diminish (eldoc-mode my/contextual-help-mode)
  :commands turn-on-eldoc-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'c-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'c++-mode-hook 'turn-on-eldoc-mode)

          ;; Enable eldoc in eval-expression
          (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

          ;; Show eldoc help in *Help* buffer
          ;; http://emacs.stackexchange.com/questions/22132/help-buffer-on-hover-possible
          (define-minor-mode my/contextual-help-mode
              "Displays help for the current symbol whenever the *Help* buffer is visible.
Advises `eldoc-print-current-symbol-info'."
              :lighter " C-h"
              :global t
              (require 'help-mode) ;; for `help-xref-interned'
              (message "Contextual help is %s" (if my/contextual-help-mode "on" "off"))
              (and my/contextual-help-mode
                   (eldoc-mode 1)
                   (eldoc-current-symbol)
                   (my/contextual-help :force)))

          (defadvice eldoc-print-current-symbol-info (before my/contextual-help activate)
            "Triggers contextual elisp *Help*. Enabled by `my/contextual-help-mode'."
            (and my/contextual-help-mode
                 (my/contextual-help)))

          (defun my/contextual-help (&optional force)
            "Display function or variable at point in *Help* buffer, if visible."
            (when (or force (get-buffer-window (help-buffer)))
              (let ((sym (eldoc-current-symbol)))
                ;; If something else changes the help buffer contents, ensure we
                ;; don't immediately revert back to the current symbol's help.
                (and sym
                     (not (keywordp sym))
                     (not (eq sym (get 'my/contextual-help 'last-sym)))
                     (put 'my/contextual-help 'last-sym sym)
                     (save-selected-window
                       (help-xref-interned sym))))))

          (my/contextual-help-mode 1)

          ;; https://www.topbug.net/blog/2016/11/03/emacs-display-function-or-variable-information-near-point-cursor/
          (defun my/eldoc-display-message (format-string &rest args)
            "Display eldoc message near point."
            (when format-string
              (pos-tip-show (apply 'format format-string args))))
          ;; (if (display-graphic-p)
          ;;     (setq eldoc-message-function #'my/eldoc-display-message))
          ))

(provide 'setup-eldoc)
;;; setup-eldoc.el ends here

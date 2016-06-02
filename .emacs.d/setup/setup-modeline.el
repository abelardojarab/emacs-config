;;; setup-modeline.el ---

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

;; Nyan cat
(use-package nyan-mode
  :if (display-graphic-p)
  :load-path (lambda () (expand-file-name "nyan-mode/" user-emacs-directory))
  :config (nyan-mode t))

;; Powerline
(use-package powerline
  :commands powerline-default-theme
  :load-path (lambda () (expand-file-name "powerline/" user-emacs-directory))
  :init (progn

          ;; Cleaning the mode line
          ;; https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
          (defvar mode-line-cleaner-alist
            `((auto-complete-mode . " α")
              (yas/minor-mode . "")
              (eldoc-mode . "")
              (abbrev-mode . "")
              (git-gutter+-mode . "")
              (smartparens-mode . " π")
              (paredit-mode . " π")
              (undo-tree-mode . " ϔ")

              ;; Major modes
              (hi-lock-mode . ""))

            "Alist for `clean-mode-line'.
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

          (defun clean-mode-line ()
            (interactive)
            (loop for cleaner in mode-line-cleaner-alist
                  do (let* ((mode (car cleaner))
                            (mode-str (cdr cleaner))
                            (old-mode-str (cdr (assq mode minor-mode-alist))))
                       (when old-mode-str
                         (setcar old-mode-str mode-str))
                       ;; major mode
                       (when (eq mode major-mode)
                         (setq mode-name mode-str)))))
          (if (display-graphic-p)
              (add-hook 'after-change-major-mode-hook 'clean-mode-line))

          (setq powerline-default-separator 'arrow-fade))
  :config (progn
            ;; (powerline-default-theme)
            ))

(use-package spaceline
  :init (setq powerline-default-separator 'arrow-fade)
  :load-path (lambda () (expand-file-name "spaceline/" user-emacs-directory))
  :config (progn (require 'spaceline-config)
                 (spaceline-spacemacs-theme)
                 (spaceline-helm-mode)))

(provide 'setup-modeline)
;;; setup-modeline.el ends here

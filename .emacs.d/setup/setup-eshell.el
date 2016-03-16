;;; setup-eshell.el ---

;; Copyright (C) 2014, 2015, 2016

;; Author:  <ajaraber@AJARABER-MOBL5>
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

(setq-default eshell-directory-name "~/.emacs.cache/eshell")

;; ansi-term doesn’t obey usage of utf-8-unix
(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

;; Configuration choices
(add-hook 'sh-mode-hook
          (lambda ()
            (show-paren-mode -1)
            (flycheck-mode -1)
            (setq blink-matching-paren nil)))

;; When compiling from shell, display error result as in compilation
;; buffer, with links to errors.
(add-hook 'sh-mode-hook 'compilation-shell-minor-mode)
(add-hook 'sh-mode-hook 'ansi-color-for-comint-mode-on)

;; CShell mode
(use-package csh-mode
            :config (progn
                      (dolist (elt interpreter-mode-alist)
                        (when (member (car elt) (list "csh" "tcsh"))
                          (setcdr elt 'csh-mode)))

                      ;; Fix issues in csh indentation
                      (add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
                      (add-to-list 'auto-mode-alist '("\\.csh\\'" . sh-mode))
                      (add-to-list 'auto-mode-alist '("\\.cshrc\\'" . sh-mode))
                      (defun my/tcsh-set-indent-functions ()
                        (when (or (string-match ".*\\.alias" (buffer-file-name))
                                  (string-match "\\(.*\\)\\.csh$" (file-name-nondirectory (buffer-file-name))))
                          (setq-local indent-line-function   #'csh-indent-line)
                          (setq-local indent-region-function #'csh-indent-region)))
                      (add-hook 'sh-mode-hook #'my/tcsh-set-indent-functions)
                      (add-hook 'sh-set-shell-hook #'my/tcsh-set-indent-functions)
                      (add-hook 'sh-mode-hook (lambda () (electric-indent-mode -1)))))

(provide 'setup-eshell)
;;; setup-eshell.el ends here

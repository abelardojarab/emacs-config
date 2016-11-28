;;; setup-general.el ---

;; Copyright (C) 2016  Abelardo Jara-Berrocal

;; Author: Abelardo Jara <abelardojara@Abelardos-MacBook-Pro.local>
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

;; With-editor (emacsclient support)
(use-package with-editor
  :load-path (lambda () (expand-file-name "with-editor/" user-emacs-directory))
  :init (progn
          (add-hook 'shell-mode-hook  'with-editor-export-editor)
          (add-hook 'eshell-mode-hook 'with-editor-export-editor)))

;; Pos-tip library
(use-package pos-tip
  :load-path (lambda () (expand-file-name "pos-tip/" user-emacs-directory))
  :config (progn
            (defadvice popup-menu-show-quick-help
                (around pos-tip-popup-menu-show-quick-help () activate)
              "Show quick help using `pos-tip-show'."
              (if (display-graphic-p)
                  (let ((doc (popup-menu-document
                              menu (or item
                                       (popup-selected-item menu)))))
                    (when (stringp doc)
                      (pos-tip-show doc nil
                                    (if (popup-hidden-p menu)
                                        (or (plist-get args :point)
                                            (point))
                                      (overlay-end (popup-line-overlay
                                                    menu (+ (popup-offset menu)
                                                            (popup-selected-line menu)))))
                                    nil 0) nil))
                ad-do-it))))

;; Turn on subword-mode for non-lispy languages
(use-package subword
  :config (progn (mapc (lambda (mode)
                         (add-hook mode 'subword-mode))
                       '(c-common-mode-hook
                         python-mode-hook
                         js2-mode-hook
                         java-mode-hook))))

;; Uniquify-buffers
(use-package uniquify
  :config (progn
            (setq
             uniquify-buffer-name-style 'post-forward
             uniquify-separator " • "
             ;; rename after killing uniquified
             uniquify-after-kill-buffer-p t
             ;; don't muck with special buffers
             uniquify-ignore-buffers-re "^\\*")))

;; Unfill and fill
(use-package unfill
  :commands (unfill-region unfill-paragraph toggle-fill-unfill))

;; Browse kill ring
(use-package browse-kill-ring
  :load-path (lambda () (expand-file-name "browse-kill-ring/" user-emacs-directory)))

;; ediff
(use-package ediff
  :init (progn
          (defun my/setup-ediff ()
            (interactive)
            (ediff-setup-keymap))
          (add-hook 'ediff-mode-hook 'my/setup-ediff))
  :config (progn
            (setq ediff-window-setup-function 'ediff-setup-windows-plain
                  ;; Always split nicely for wide screens
                  ediff-split-window-function 'split-window-horizontally
                  ;; Ignore whitespace
                  ediff-diff-options "-w")))

;; async
(use-package async
  :load-path (lambda () (expand-file-name "async/" user-emacs-directory))
  :config (progn
            (require 'dired-async)
            (dired-async-mode 1)))

;; Unicode viewer (charmap)
(use-package charmap
  :commands charmap
  :defer t
  :load-path (lambda () (expand-file-name "charmap/" user-emacs-directory))
  :config (setq charmap-text-scale-adjust 2))

;; Calendar viewer
(use-package calfw
  :commands cfw:open-org-calendar
  :after org
  :load-path (lambda () (expand-file-name "calfw/" user-emacs-directory))
  :defer 0.5
  :config (use-package calfw-org))

;; Persistent scratch buffer
(use-package persistent-scratch
  :load-path (lambda () (expand-file-name "persistent-scratch/" user-emacs-directory))
  :config (progn
            (setq persistent-scratch-save-file "~/.emacs.cache/persistent-scratch-buffer")
            (persistent-scratch-setup-default)))

;; Page break lines
(use-package page-break-lines
  :load-path (lambda () (expand-file-name "page-break-lines/" user-emacs-directory)))

(provide 'setup-general)
;;; setup-general.el ends here

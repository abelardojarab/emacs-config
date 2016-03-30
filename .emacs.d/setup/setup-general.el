;;; setup-general.el ---

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

;; Exec path from shell in Mac OSX
(use-package exec-path-from-shell
  :if (equal system-type 'darwin)
  :load-path (lambda () (expand-file-name "exec-path-from-shell/" user-emacs-directory))
  :config (progn
            (setq exec-path-from-shell-check-startup-files nil)
        ;; (exec-path-from-shell-initialize)
    ))

;; Popup, used by auto-complete and other tools
(use-package popup
  ;; We don't ensure this package, because we definitely don't want to have this
  ;; mess, but unfortunately it's a dependency of Ensime :(
  :load-path (lambda () (expand-file-name "popup/" user-emacs-directory))
  :config (progn
            (setq popup-use-optimized-column-computation t)))

;; Fix indent guide issue with column-number
(defvar sanityinc/column-number-mode-suppressed nil)
(defadvice popup-create (before column-number-mode activate)
  "Suspend column-number-mode while popups are visible"
  (let ((column-number-enabled (and (boundp 'column-number-mode) column-number-mode)))
    (set (make-local-variable 'sanityinc/column-number-mode-suppressed) column-number-mode)
    (when column-number-enabled
      (column-number-mode nil))))
(defadvice popup-delete (after column-number-mode activate)
  "Restore column-number-mode when all popups have closed"
  (let ((column-number-enabled (and (boundp 'column-number-mode) column-number-mode)))
    (when (and (not popup-instances) sanityinc/column-number-mode-suppressed)
      (setq sanityinc/column-number-mode-suppressed nil)
      (column-number-mode 1))))

;; Pos-tip library
(use-package pos-tip
  :defer t
  :load-path (lambda () (expand-file-name "pos-tip/" user-emacs-directory)))

;; Drop down list support, related to popup
(use-package dropdown-list)

;; Manage popup windows
(use-package popwin
  :load-path (lambda () (expand-file-name "popwin/" user-emacs-directory))
  ;; popwin conflicts with ecb
  :config (popwin-mode -1))

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
             uniquify-separator ":"
             ;; rename after killing uniquified
             uniquify-after-kill-buffer-p t
             ;; don't muck with special buffers
             uniquify-ignore-buffers-re "^\\*")))

;; Browse kill ring
(use-package browse-kill-ring
  :load-path (lambda () (expand-file-name "browse-kill-ring/" user-emacs-directory)))

;; log4e
(use-package log4e
  :load-path (lambda () (expand-file-name "log4e/" user-emacs-directory)))

;; yaxception
(use-package yaxception
  :load-path (lambda () (expand-file-name "yaxception/" user-emacs-directory)))

(provide 'setup-general)
;;; setup-general.el ends here

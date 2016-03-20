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

;; Fix indent guide issue
;; (defvar sanityinc/indent-guide-mode-suppressed nil)
;; (defadvice popup-create (before indent-guide-mode activate)
;;   "Suspend indent-guide-mode while popups are visible"
;;   (let ((indent-guide-enabled (and (boundp 'indent-guide-mode) indent-guide-mode)))
;;     (set (make-local-variable 'sanityinc/indent-guide-mode-suppressed) indent-guide-mode)
;;     (when indent-guide-enabled
;;       (indent-guide-mode nil))))
;; (defadvice popup-delete (after indent-guide-mode activate)
;;   "Restore indent-guide-mode when all popups have closed"
;;   (let ((indent-guide-enabled (and (boundp 'indent-guide-mode) indent-guide-mode)))
;;     (when (and (not popup-instances) sanityinc/indent-guide-mode-suppressed)
;;       (setq sanityinc/indent-guide-mode-suppressed nil)
;;       (indent-guide-mode 1))))

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

;; Multiple cursors
(use-package multiple-cursors
  :load-path (lambda () (expand-file-name "multiple-cursors/" user-emacs-directory)))

;; log4e
(use-package log4e
  :load-path (lambda () (expand-file-name "log4e/" user-emacs-directory)))

;; yaxception
(use-package yaxception
  :load-path (lambda () (expand-file-name "yaxception/" user-emacs-directory)))

(provide 'setup-general)
;;; setup-general.el ends here

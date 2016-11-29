;;; setup-email.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@ubuntu-MacBookPro>
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

;; Flim, wanderlust requirement
(use-package std11
  :load-path (lambda () (expand-file-name "flim/" user-emacs-directory)))

;; Wanderlust
(use-package wl
  :defer t
  :init (progn
          (add-to-list 'load-path (expand-file-name "wanderlust/elmo" user-emacs-directory))
          (add-to-list 'load-path (expand-file-name "semi" user-emacs-directory))

          ;; message preferences
          (add-hook 'message-mode-hook #'flyspell-mode)
          (add-hook 'message-mode-hook #'turn-on-orgstruct)
          (add-hook 'message-mode-hook #'turn-on-orgstruct++)
          (add-hook 'message-mode-hook #'turn-on-orgtbl)
          (add-hook 'message-mode-hook #'typo-mode)
          (add-hook 'message-mode-hook #'flyspell-mode)
          (add-hook 'message-mode-hook #'turn-on-auto-fill))
  :commands wl
  :load-path (lambda () (expand-file-name "wanderlust/wl/" user-emacs-directory))
  :config (let ((wl-root-dir "~/.emacs.cache/wl/"))
            (setq wl-init-file (concat wl-root-dir "wl.el")
                  wl-folders-file (concat wl-root-dir "folders"))))

(provide 'setup-email)
;;; setup-email.el ends here

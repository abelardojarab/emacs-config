;;; setup-keys-extensions.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara

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

;; Showkey as typed
(use-package showkey
  :if (display-graphic-p)
  :defer t
  :load-path (lambda () (expand-file-name "dadams/" user-emacs-directory))
  :commands (showkey-tooltip-mode showkey-log-mode))

;; visual feedback when pressing keys
(use-package which-key
  :load-path (lambda () (expand-file-name "which-key/" user-emacs-directory))
  :diminish which-key-mode
  :config (progn
            (unless (or (equal system-type 'windows-nt)
                        (not (display-graphic-p)))
              (add-to-list 'which-key-key-replacement-alist '("TAB" . "↹"))
              (add-to-list 'which-key-key-replacement-alist '("RET" . "⏎"))
              (add-to-list 'which-key-key-replacement-alist '("DEL" . "⇤"))
              (add-to-list 'which-key-key-replacement-alist '("SPC" . "␣")))

            ;; Side window setup
            ;; (setq which-key-popup-type 'side-window)
            (setq which-key-popup-type 'minibuffer)
            (setq which-key-side-window-location 'right)
            (setq which-key-side-window-max-width 0.33)
            (which-key-mode)))

;; Mac OS X extensions
(use-package mac-key-mode
  :commands (mac-key-mode mac-key-speak-region mac-key-speak-buffer mac-key-quick-look)
  :if (equal system-type 'darwin))

(provide 'setup-keys-extensions)
;;; setup-keys-extensions.el ends here

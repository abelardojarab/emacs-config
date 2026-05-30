;;; setup-ergoemacs.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026  Abelardo Jara-Berrocal

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

;; Ergoemacs MENUS ONLY.
;;
;; We deliberately do NOT enable `ergoemacs-mode' as a minor mode: doing so
;; replaces `global-map' and pushes its keymap onto `emulation-mode-map-alists',
;; which clobbers the personal keybindings configured earlier in `setup-keys'
;; and `setup-keys-extensions'.  Instead we just load the package and install
;; its menu bar (File / Edit / Search / View / Major-Modes / Help), leaving all
;; keybindings untouched.  The Help menu's "Keyboard Layout" entry (a.k.a.
;; `M-x describe-ergoemacs-layout') shows the ergoemacs keyboard-legend diagram.
;;
;; This is intentionally plain top-level code rather than a `use-package' form:
;; the work is a one-shot menu install, and routing it through use-package's
;; `:init'/deferral machinery made the install timing unreliable.
(defun my/ergoemacs-install-menus ()
  "Install the ergoemacs menu bar without enabling ergoemacs keybindings.
Merely loading the package enables no minor mode, adds no command
hooks/advice, and does not modify `emulation-mode-map-alists', so
personal keybindings stay intact.  The menu builders only touch
`[menu-bar ...]' entries."
  (when (and (require 'ergoemacs-mode nil t)
             (require 'ergoemacs-themes nil t)
             (fboundp 'ergoemacs-set-menu-bar-file))
    ;; Layout used by the keyboard-legend helper (Help > Keyboard Layout, a.k.a.
    ;; `M-x describe-ergoemacs-layout', which renders the keyboard diagram).
    (setq ergoemacs-keyboard-layout "us")
    ;; Same order `ergoemacs-install-standard-theme' uses, so menus end up
    ;; positioned correctly (each builder uses `define-key-after').
    (ergoemacs-set-menu-bar-help)
    (ergoemacs-set-menu-bar-view)
    (ergoemacs-set-menu-bar-search)
    (ergoemacs-set-menu-bar-edit)
    (ergoemacs-set-menu-bar-major-modes)
    (ergoemacs-set-menu-bar-file)))

(when (and (executable-find "gzip")
           (display-graphic-p))
  ;; Install after startup so the standard menu-bar already exists, otherwise
  ;; run immediately (e.g. when this file is loaded after init has finished).
  (if after-init-time
      (my/ergoemacs-install-menus)
    (add-hook 'after-init-hook #'my/ergoemacs-install-menus)))

(provide 'setup-ergoemacs)
;;; setup-ergoemacs.el ends here

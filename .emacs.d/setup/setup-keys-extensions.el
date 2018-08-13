;;; setup-keys-extensions.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Abelardo Jara-Berrocal

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

;; Showkey as typed
(use-package showkey
  :defer t
  :if (display-graphic-p)
  :load-path (lambda () (expand-file-name "dadams/" user-emacs-directory))
  :commands (showkey-tooltip-mode showkey-log-mode))

;; visual feedback when pressing keys
(use-package which-key
  :defer t
  :commands which-key-mode
  :load-path (lambda () (expand-file-name "which-key/" user-emacs-directory))
  :diminish which-key-mode
  :init (add-hook 'after-init-hook #'which-key-mode)
  :config (progn
            (when (if (not (equal system-type 'windows-nt))
                      (display-graphic-p))
              (setq which-key-sort-order            #'which-key-prefix-then-key-order
                    which-key-sort-uppercase-first  nil
                    which-key-add-column-padding    1
                    which-key-max-display-columns   nil
                    which-key-min-display-lines     5
                    which-key-idle-delay            2.0
                    which-key-popup-type            'minibuffer
                    which-key-side-window-location  'right
                    which-key-side-window-max-width 0.33
                    which-key-key-replacement-alist
                    '(("<\\([[:alnum:]-]+\\)>" . "\\1")
                      ("TAB"                   . "↹")
                      ("RET"                   . "⏎")
                      ("SPC"                   . "␣")
                      ("up"                    . "↑")
                      ("right"                 . "→")
                      ("down"                  . "↓")
                      ("left"                  . "←")
                      ("DEL"                   . "⇤")
                      ("deletechar"            . "⌫")
                      ("RET"                   . "⏎"))
                    which-key-description-replacement-alist
                    '(("Prefix Command" . "prefix")
                      ;; Lambdas
                      ("\\`\\?\\?\\'"   . "λ")
                      ;; Prettify hydra entry points
                      ("/body\\'"       . "|=")
                      ;; Drop/shorten package prefixes
                      ("\\`lunaryorn-"  . "")
                      ("projectile-"    . "proj-")
                      ("magit-"         . "ma-")))

              (add-to-list 'which-key-key-replacement-alist '("TAB" . "↹"))
              (add-to-list 'which-key-key-replacement-alist '("RET" . "⏎"))
              (add-to-list 'which-key-key-replacement-alist '("DEL" . "⇤"))
              (add-to-list 'which-key-key-replacement-alist '("SPC" . "␣")))

            ;; which-key will truncate special keys by default, eg. SPC turns into
            ;; an orange D. Turn this off to avoid confusion.
            (setq-default which-key-special-keys nil)

            (which-key-mode t)))

;; Get an instant cheat sheet for your current major mode
;; with C-h C-m.
(use-package discover-my-major
  :load-path (lambda () (expand-file-name "discover-my-major/" user-emacs-directory))
  :commands (discover-my-major discover-my-mode)
  :bind ("C-h C-m" . discover-my-major))

;; Mac OS X extensions
(use-package mac-key-mode
  :commands (mac-key-mode mac-key-speak-region mac-key-speak-buffer mac-key-quick-look)
  :if (equal system-type 'darwin))

(provide 'setup-keys-extensions)
;;; setup-keys-extensions.el ends here

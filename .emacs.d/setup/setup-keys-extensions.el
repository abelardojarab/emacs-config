;;; setup-keys-extensions.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

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
  :commands (showkey-tooltip-mode
             showkey-log-mode))

;; visual feedback when pressing keys
(use-package which-key
  :defer t
  :commands which-key-mode
  :diminish which-key-mode
  :hook ((after-init . which-key-mode)
         (lsp-mode   . lsp-enable-which-key-integration))
  :if (and (not (equal system-type 'windows-nt))
           (display-graphic-p))
  :custom ((which-key-sort-order            #'which-key-prefix-then-key-order)
           (which-key-sort-uppercase-first  nil)
           (which-key-add-column-padding    1)
           (which-key-max-display-columns   nil)
           (which-key-min-display-lines     5)
           (which-key-idle-delay            0.8)
           (which-key-popup-type            'minibuffer)
           (which-key-side-window-location  'right)
           (which-key-side-window-max-width 0.33)
           (which-key-special-keys          nil))
  :config (progn
            (defadvice which-key--update (around bar activate)
              (ignore-errors add-do-it))
            (setq which-key-replacement-alist
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

            (add-to-list 'which-key-replacement-alist '("TAB" . "↹"))
            (add-to-list 'which-key-replacement-alist '("RET" . "⏎"))
            (add-to-list 'which-key-replacement-alist '("DEL" . "⇤"))
            (add-to-list 'which-key-replacement-alist '("SPC" . "␣"))))

;; which key posframe
(use-package which-key-posframe
  :defer t
  :if (and (window-system) (version<= "26.1" emacs-version))
  :after (which-key posframe)
  :custom (which-key-posframe-border-width 2)
  :hook (which-key-mode . which-key-posframe-mode))

;; Get an instant cheat sheet for your current major mode
(use-package discover-my-major
  :defer t
  :commands (discover-my-major
             discover-my-mode)
  :bind ("C-h C-m" . discover-my-major))

;; Mac OS X extensions
(use-package mac-key-mode
  :commands (mac-key-mode
             mac-key-speak-region
             mac-key-speak-buffer
             mac-key-quick-look)
  :if (equal system-type 'darwin))

(provide 'setup-keys-extensions)
;;; setup-keys-extensions.el ends here

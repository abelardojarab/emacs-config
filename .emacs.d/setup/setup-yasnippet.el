;;; setup-yasnippet.el ---                               -*- lexical-binding: t; -*-

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

;; Yasnippet (should be invoked before auto-complete)
(use-package yasnippet
  :defer t
  :diminish (yas-minor-mode . " Ⓨ")
  :bind (:map yas-minor-mode-map
              ;; Remove Yasnippet's default tab key binding (avoid collision with auto-complete)
              ("C-c r"       . yas-prev-field)
              ("C-c t"       . yas-next-field-or-maybe-expand)
              ;; Simplify navigation of yasnippet fields
              ("M-<right>"   . yas-next-field-or-maybe-expand)
              ("M-<left>"    . yas-prev-field)
              :map yas-keymap
              ([tab]         . yas-next-field-or-maybe-expand)
              ([?\t]         . yas-next-field-or-maybe-expand)
              ("C-c r"       . yas-prev-field)
              ("C-c t"       . yas-next-field-or-maybe-expand))
  :defines (yas-initialize)
  :commands (yas-global-mode
             yas-minor-mode
             yas-snippet-dirs
             yas-reload-all)
  :hook (on-first-input . yas-global-mode)
  :init (progn
          ;; `~/.emacs.d' is a symlink to `user-emacs-directory'
          ;; (~/workspace/emacs-config/.emacs.d), so the old code added the
          ;; snippets folder under BOTH paths -- yasnippet then loaded every
          ;; snippet twice ("Multiple snippets with same identity ..." and every
          ;; directive warning printed [2 times]).  Canonicalize with
          ;; `file-truename' and de-duplicate so each physical dir appears once.
          (setq yas-snippet-dirs
                (delete-dups
                 (mapcar #'file-truename
                         (list (expand-file-name "snippets" user-emacs-directory)))))
          (yas-initialize))
  :config (progn
            (add-to-list 'yas-key-syntaxes 'yas-shortest-key-until-whitespace)

            ;; Do not activate for read only and non-existent snippets
            (set-default 'yas--dont-activate
                         #'(lambda ()
                             (or buffer-read-only
                                 (and yas-snippet-dirs
                                      (null (yas--get-snippet-tables))))))

            ;; Select a snippet with popup library
            (setq yas-prompt-functions '(yas-dropdown-prompt
                                         yas-ido-prompt
                                         yas-completing-prompt
                                         yas-no-prompt))

            ;; use yas/completing-prompt ONLY when `M-x yas-insert-snippet'
            ;; thanks to capitaomorte for providing the trick.
            (defun my/use-completing-prompt--advice (orig &rest args)
              "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
              (let ((yas-prompt-functions '(yas-ido-prompt)))
                (apply orig args)))
            (advice-add 'yas-insert-snippet :around #'my/use-completing-prompt--advice)

            ;; Tweaking Yasnippet for Org mode
            (defun yas--org-very-safe-expand ()
              (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))
            (add-hook 'org-mode-hook
                      (lambda ()
                        (make-variable-buffer-local 'yas-trigger-key)
                        (setq yas-trigger-key [tab])
                        (add-to-list 'org-tab-first-hook 'yas--org-very-safe-expand)))))

;; Competitive snippets
(use-package competitive-programming-snippets)

(provide 'setup-yasnippet)
;;; setup-yasnippet.el ends here

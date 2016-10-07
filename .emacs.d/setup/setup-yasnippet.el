;;; setup-yasnippet.el ---

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

;; Yasnippet (should be invoked before auto-complete)
(use-package yasnippet
  :diminish yas-minor-mode
  :load-path (lambda () (expand-file-name "yasnippet/" user-emacs-directory))
  :config (progn
            (if (file-exists-p "~/.emacs.d/snippets")
                (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
              (setq yas-snippet-dirs '()))
            (setq yas-snippet-dirs (cons (expand-file-name "snippets" user-emacs-directory)
                                         (yas-snippet-dirs)))
            (setq yas-snippet-dirs (cons (expand-file-name "snippets-extra" user-emacs-directory)
                                         (yas-snippet-dirs)))

            (yas-initialize)
            (yas-global-mode 1)

            ;; Do not activate for read only and non-existent snippets
            (set-default 'yas--dont-activate
                         #'(lambda ()
                             (or buffer-read-only
                                 (and yas-snippet-dirs
                                      (null (yas--get-snippet-tables))))))

            ;; Remove Yasnippet's default tab key binding (avoid collision with auto-complete)
            (define-key yas-minor-mode-map (kbd "<tab>") nil)
            (define-key yas-minor-mode-map (kbd "TAB") nil)
            (define-key yas-minor-mode-map (kbd "M-<right>") 'yas-next-field-or-maybe-expand)
            (define-key yas-minor-mode-map (kbd "M-<left>") 'yas-next-field-or-maybe-expand)
            (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-insert-snippet)
            (define-key yas-minor-mode-map (kbd "<S-tab>") 'yas-insert-snippet)

            ;; Select a snippet with popup library
            (setq yas-prompt-functions '(yas-dropdown-prompt
                                         yas-ido-prompt
                                         yas-completing-prompt
                                         yas-no-prompt))

            ;; use yas/completing-prompt when ONLY when `M-x yas-insert-snippet'
            ;; thanks to capitaomorte for providing the trick.
            (defadvice yas-insert-snippet (around use-completing-prompt activate)
              "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
              (let ((yas-prompt-functions '(yas-ido-prompt)))
                ad-do-it))

            ;; Tweaking Yasnippet for Org mode
            (defun yas--org-very-safe-expand ()
              (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))
            (add-hook 'org-mode-hook
                      (lambda ()
                        (make-variable-buffer-local 'yas-trigger-key)
                        (setq yas-trigger-key [tab])
                        (add-to-list 'org-tab-first-hook 'yas--org-very-safe-expand)
                        (define-key yas-keymap [tab] 'yas-next-field)))

            ;; Auto-complete enhancement
            (when (featurep 'auto-complete)
              (defun yas/set-ac-modes ()
                "Add modes in `yas-snippet-dirs' to `ac-modes'. Call (yas/set-ac-modes) BEFORE (global-auto-complete-mode 1) or (ac-config-default)."
                (eval-after-load "auto-complete"
                  '(setq ac-modes
                         (append
                          (apply 'append (mapcar (lambda (dir) (mapcar 'intern (directory-files dir nil "-mode$")))
                                                 (yas-snippet-dirs)))
                          ac-modes))))
              (yas/set-ac-modes))

            ;; Visual indication that available snippet exists
            (setq default-cursor-color "gray")
            (setq yasnippet-can-fire-cursor-color "purple")

            ;; It will test whether it can expand, if yes, cursor color -> green.
            (defun yasnippet-can-fire-p (&optional field)
              (interactive)
              (setq yas--condition-cache-timestamp (current-time))
              (let (templates-and-pos)
                (unless (and yas-expand-only-for-last-commands
                             (not (member last-command yas-expand-only-for-last-commands)))
                  (setq templates-and-pos (if field
                                              (save-restriction
                                                (narrow-to-region (yas--field-start field)
                                                                  (yas--field-end field))
                                                (yas--templates-for-key-at-point))
                                            (yas--templates-for-key-at-point))))
                (and templates-and-pos (first templates-and-pos))))

            (defun my/change-cursor-color-when-can-expand (&optional field)
              (interactive)
              (when (eq last-command 'self-insert-command)
                (set-cursor-color (if (my/can-expand)
                                      yasnippet-can-fire-cursor-color
                                    default-cursor-color))))

            (defun my/can-expand ()
              "Return true if right after an expandable thing."
              (or (abbrev--before-point) (yasnippet-can-fire-p)))

            ;; As pointed out by Dmitri, this will make sure it will update color when needed.
            (remove-hook 'post-command-hook 'my/change-cursor-color-when-can-expand)))

(provide 'setup-yasnippet)
;;; setup-yasnippet.el ends here

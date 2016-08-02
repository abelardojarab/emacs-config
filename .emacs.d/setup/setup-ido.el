;;; setup-ido.el ---

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

;; ido makes competing buffers and finding files easier
(use-package ido
  :config (progn
            (ido-mode 'both)
            (ido-everywhere 1)
            (setq ido-max-dir-file-cache 0
                  ido-show-dot-for-dired t
                  ido-default-file-method 'samewindow
                  ido-default-buffer-method 'selected-window
                  ido-save-directory-list-file "~/.emacs.cache/ido.last"
                  ido-ignore-buffers ;; ignore these guys
                  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
                    "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
                  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~/workspace")
                  ido-case-fold t
                  ido-enable-last-directory-history t
                  ido-auto-merge-work-directories-length -1
                  ido-max-work-directory-list 15
                  ido-max-work-file-list 10
                  ido-use-filename-at-point nil
                  ido-use-url-at-point nil
                  ido-enable-flex-matching t
                  ido-enable-prefix t
                  ido-max-prospects 8
                  ido-confirm-unique-completion t
                  ido-auto-merge-delay-time 0.7
                  ido-auto-merge-work-directories-length -1)

            ;; This tab override shouldn't be necessary given ido's default
            ;; configuration, but minibuffer-complete otherwise dominates the
            ;; tab binding because of my custom tab-completion-everywhere
            ;; configuration.
            (add-hook 'ido-setup-hook
                      (lambda () (define-key ido-completion-map [tab] 'ido-complete)
                        (define-key ido-completion-map [up] 'previous-history-element)))

            ;; Paste file name with ctrl-v
            (defun ido-yank ()
              (interactive)
              (let ((path (current-kill 0)))
                (if (file-exists-p path)
                    (progn
                      (let ((dir (file-name-directory path)))
                        (if dir (ido-set-current-directory dir)))
                      (setq ido-exit 'refresh)
                      (setq ido-text-init (if (file-directory-p path) nil (file-name-nondirectory path)))
                      (setq ido-rotate-temp t)
                      (exit-minibuffer))
                  (yank))))

            (define-key ido-file-dir-completion-map (kbd "C-v") 'ido-yank)

            ;; when using ido, the confirmation is rather annoying...
            (setq confirm-nonexistent-file-or-buffer nil)))

;; Ido everywhere
(use-package ido-ubiquitous
  :load-path (lambda () (expand-file-name "ido-ubiquitous/" user-emacs-directory))
  :config (ido-ubiquitous-mode +1))

(provide 'setup-ido)
;;; setup-ido.el ends here

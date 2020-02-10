;;; setup-eshell.el ---                       -*- lexical-binding: t; -*-

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

(use-package ielm
  :defer t
  :bind (("C-'"      . ielm-repl)
         ("C-c C-z"  . ielm-repl)
         :map ctl-x-map
         (("C-z"     . ielm-repl)
          ("z"       . ielm-repl))
         :map ielm-map
         (("C-t"     . quit-window)))
  :config (defun ielm-repl ()
            (interactive)
            (pop-to-buffer (get-buffer-create "*ielm*"))
            (ielm)))

(use-package eshell
  :defer t
  :commands (eshell
             eshell-vertical
             eshell-horizontal)
  :bind (("C-c C-t"                 . eshell)
         :map ctl-x-map
         ("C-t"                     . eshell)
         :map eshell-mode-map
         (("C-t"                    . quit-window)
          ("C-c C-t"                . quit-window)
          ([remap eshell-pcomplete] . helm-esh-pcomplete)
          ("M-p"                    . helm-eshell-history)))
  :after helm
  :custom ((eshell-glob-case-insensitive     t)
           (eshell-scroll-to-bottom-on-input 'this)
           (eshell-buffer-shorthand          t)
           (eshell-history-size              1024)
           (eshell-buffer-shorthand          t)
           (eshell-cmpl-ignore-case          t)
           (eshell-cmpl-cycle-completions    nil)
           (eshell-last-dir-ring-size        512))
  :init (progn
          ;; Fix lack of eshell-mode-map
          (if (not (boundp 'eshell-mode-map))
              (defvar eshell-mode-map (make-sparse-keymap)))

          ;; First, Emacs doesn't handle less well, so use cat instead for the shell pager:
          (setenv "PAGER" "cat")
          (setq-default eshell-directory-name (concat (file-name-as-directory
                                                       my/emacs-cache-dir)
                                                      "eshell"))
          (setq eshell-aliases-file  (concat user-emacs-directory ".eshell-aliases"))

          ;; Vertical split eshell
          (defun eshell-vertical ()
            "opens up a new shell in the directory associated with the current buffer's file."
            (interactive)
            (let* ((parent (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             default-directory))
                   (name (car (last (split-string parent "/" t)))))
              (split-window-right)
              (other-window 1)
              (eshell "new")
              (rename-buffer (concat "*eshell: " name "*"))
              (eshell-send-input)))

          ;; Horizontal split eshell
          (defun eshell-horizontal ()
            "opens up a new shell in the directory associated with the current buffer's file."
            (interactive)
            (let* ((parent (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             default-directory))
                   (name (car (last (split-string parent "/" t)))))
              (split-window-below)
              (other-window 1)
              (eshell "new")
              (rename-buffer (concat "*eshell: " name "*"))
              (eshell-send-input))))
  :config (progn
            ;; Show fringe status for eshell
            (use-package eshell-fringe-status
              :defer t
              :if (display-graphic-p)
              :commands eshell-fringe-status-mode
              :hook (eshell-mode . eshell-fringe-status-mode))

            ;; Show git branches on the prompt
            (use-package eshell-prompt-extras
              :demand t
              :custom (eshell-prompt-function 'epe-theme-lambda)))

  ;; Get shell on current directory
  (use-package shell-pop
    :defer t
    :after eshell
    :bind (("C-t" . shell-pop))
    :config (progn
              (setq shell-pop-window-size 45)
              (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
              (setq shell-pop-term-shell "/bin/bash")
              ;; need to do this manually or not picked up by `shell-pop'
              (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)))

  ;; Shell mode
  (use-package sh-script
    :defer t
    :commands sh-mode
    :mode (("\\.sh$"         . sh-mode)
           ("\\.zsh\\'"      . sh-mode)
           ("\\.csh\\'"      . sh-mode)
           ("\\.cshrc\\'"    . sh-mode))
    :config (progn
              ;; create keymap
              (setq sh-mode-map (make-sparse-keymap))

              ;; Configuration choices
              (add-hook 'sh-mode-hook
                        (lambda ()
                          (setq sh-indentation  2
                                sh-basic-offset 2)
                          (compilation-shell-minor-mode t)
                          (ansi-color-for-comint-mode-on)
                          (electric-indent-mode -1)))))

  ;; CShell mode - fix issues in csh indentation
  (use-package csh-mode
    :demand t
    :config (progn
              (dolist (elt interpreter-mode-alist)
                (when (member (car elt) (list "csh" "tcsh"))
                  (setcdr elt 'csh-mode)))

              (defun my/tcsh-set-indent-functions ()
                (when (or (string-match ".*\\.alias" (buffer-file-name))
                          (string-match "\\(.*\\)\\.csh$" (file-name-nondirectory (buffer-file-name))))
                  (setq-local indent-line-function   #'csh-indent-line)
                  (setq-local indent-region-function #'csh-indent-region)))
              (add-hook 'sh-mode-hook      #'my/tcsh-set-indent-functions)
              (add-hook 'sh-set-shell-hook #'my/tcsh-set-indent-functions))))

;; Send expressions to a REPL line-by-line by hitting C-RET
(use-package eval-in-repl
  :defer t
  :commands (eir-eval-in-ielm
             eir-eval-in-shell
             eir-eval-in-python
             eir-eval-in-javascript)
  :custom ((eir-jump-after-eval                 nil)
           (eir-always-split-script-window      t)
           (eir-delete-other-windows            t)
           (eir-repl-placement                  'right))
  :config (progn
            ;; ielm support (for emacs lisp)
            (use-package eval-in-repl-ielm
              :commands eir-eval-in-ielm)

            ;; Shell support
            (use-package eval-in-repl-shell
              :commands eir-eval-in-shell)

            ;; Python support
            (use-package eval-in-repl-python
              :commands eir-eval-in-python)

            ;; Javascript support
            (use-package eval-in-repl-javascript
              :commands eir-eval-in-javascript)))

;; Emacs interaction with tmux
(use-package emamux
  :defer t
  :commands (emamux:send-command
             emamux:run-command
             emamux:run-last-command
             emamux:zoom-runner
             emamux:inspect-runner
             emamux:close-runner-pane
             emamux:close-panes
             emamux:clear-runner-history
             emamux:interrupt-runner
             emamux:copy-kill-ring
             emamux:yank-from-list-buffers))

;; Create buffer-associated terminals
(use-package multi-term
  :defer t
  :commands (multi-term
             my/multi-term-vertical
             my/multi-term-horizontal)
  :init (progn
          ;; Vertical split multi-term
          (defun my/multi-term-vertical ()
            "opens up a new terminal in the directory associated with the current buffer's file."
            (interactive)
            (split-window-right)
            (other-window 1)
            (multi-term))

          ;; Horizontal split multi-term
          (defun my/multi-term-horizontal ()
            "opens up a new terminal in the directory associated with the current buffer's file."
            (interactive)
            (split-window-below)
            (other-window 1)
            (multi-term))))

;; Configuration files
(use-package systemd
  :mode (("\\.service$" . systemd-mode)))

(provide 'setup-eshell)
;;; setup-eshell.el ends here

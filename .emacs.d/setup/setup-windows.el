;;; setup-windows.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Abelardo Jara-Berrocal

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

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; Put a nice title to the window, including filename
(add-hook 'window-configuration-change-hook
          (lambda ()
            (setq frame-title-format
                  (concat
                   invocation-name "@" system-name ": "
                   (replace-regexp-in-string
                    (concat "/home/" user-login-name) "~"
                    (or buffer-file-name "%b"))))))

;; default to maximised windows
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Resize by pixels
(setq frame-resize-pixelwise               t
      ;; Size new windows proportionally wrt other windows
      window-combination-resize            t
      ;; prompt when trying to switch out of a dedicated window
      switch-to-buffer-in-dedicated-window 'prompt
      ;; Treat all windows as same
      same-window-regexps                  '(".")
      ;; Resize frames implicitely
      frame-inhibit-implied-resize         t
      ;; Minibuffer resizing
      resize-mini-windows                  'grow-only
      ;; do not highlight region on non-selected windows
      highlight-nonselected-windows        nil
      ;; maximum small window height
      max-mini-window-height               30)

;; Manage popup windows
(use-package popwin
  :defer t
  :commands popwin-mode
  :load-path (lambda () (expand-file-name "popwin/" user-emacs-directory))
  :config (add-to-list popwin:special-display-config '(help-mode 0.5 :position below)))

;; Window purpose
(use-package window-purpose
  :defer t
  :after helm
  :commands purpose-mode
  :load-path (lambda () (expand-file-name "window-purpose/" user-emacs-directory))
  :init (progn
          ;; Prefer helm
          (setq purpose-preferred-prompt 'helm)

          ;; overriding `purpose-mode-map' with empty keymap, so it doesn't conflict
          ;; with original `C-x C-f', `C-x b', etc. and `semantic' key bindings. must
          ;; be done before `window-purpose' is loaded
          (setq purpose-mode-map (make-sparse-keymap)))
  :config (progn
            (setq purpose-user-name-purposes
                  '(("*ag*"                        . search)))

            (setq purpose-user-regexp-purposes
                  '(("^\\*elfeed"                  . admin)
                    ("^\\*Python Completions"      . minibuf)
                    ))

            (setq purpose-user-mode-purposes
                  '((eshell-mode                   . terminal)

                    (python-mode                   . py)
                    (inferior-python-mode          . py-repl)

                    (circe-chat-mode               . comm)
                    (circe-query-mode              . comm)
                    (circe-lagmon-mode             . comm)
                    (circe-server-mode             . comm)

                    (ess-mode                      . edit)
                    (inferior-ess-mode             . interactive)

                    (gitconfig-mode                . edit)

                    (mu4e-main-mode                . admin)
                    (mu4e-view-mode                . admin)
                    (mu4e-about-mode               . admin)
                    (mu4e-headers-mode             . admin)
                    (mu4e-compose-mode             . edit)

                    (pdf-view-mode                 . view)
                    (doc-view-mode                 . view)))

            ;; Prefer helm
            (setq purpose-preferred-prompt 'helm)

            ;; Load user preferences
            (purpose-compile-user-configuration)
            (purpose-mode -1)

            ;; Extensions for purpose
            (use-package window-purpose-x
              :after window-purpose
              :config (progn

                        ;; Single window magit
                        (purpose-x-magit-single-on)

                        ;; when killing a purpose-dedicated buffer that is displayed in a window,
                        ;; ensure that the buffer is replaced by a buffer with the same purpose
                        ;; (or the window deleted, if no such buffer)
                        (purpose-x-kill-setup)))))

;; Helm interface to purpose
(use-package helm-purpose
  :defer t
  :after (helm window-purpose)
  :commands (helm-purpose-switch-buffer-with-purpose helm-purpose-switch-buffer-with-some-purpose)
  :load-path (lambda () (expand-file-name "helm-purpose/" user-emacs-directory)))

;; Resize windows
(use-package resize-window
  :defer t
  :commands resize-window
  :load-path (lambda () (expand-file-name "resize-window/" user-emacs-directory)))

;; Perspective
(use-package perspective
  :defer t
  :commands persp-mode
  :load-path (lambda () (expand-file-name "perspective/" user-emacs-directory))
  :config (persp-mode))

;; Switch window
(use-package switch-window
  :defer t
  :if (display-graphic-p)
  :commands switch-window
  :load-path (lambda () (expand-file-name "switch-window/" user-emacs-directory)))

;; ace-window for switching windows, but we only call it as a subroutine from a hydra
(use-package ace-window
  :defer t
  :bind (:map ctl-x-map
              ("o" . ace-window))
  :load-path (lambda () (expand-file-name "ace-window/" user-emacs-directory))
  :config (progn
            ;; Customize font on ace-window leading char
            (if (display-graphic-p)
                (custom-set-faces
                 '(aw-leading-char-face
                   ((t (:inherit ace-jump-face-foreground :height 3.0))))))

            (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
                  aw-dispatch-always nil
                  aw-dispatch-alist
                  '((?x aw-delete-window     "Ace - Delete Window")
                    (?c aw-swap-window       "Ace - Swap Window")
                    (?n aw-flip-window)
                    (?v aw-split-window-vert "Ace - Split Vert Window")
                    (?h aw-split-window-horz "Ace - Split Horz Window")
                    (?g delete-other-windows "Ace - Maximize Window")
                    (?b balance-windows)
                    (?u winner-undo)
                    (?r winner-redo)))))

;; Change window sizes based on currently open buffers and focus
(use-package golden-ratio
  :defer t
  :commands golden-ratio-adjust
  :load-path (lambda () (expand-file-name "golden-ratio/" user-emacs-directory)))

;; Auto-dim unselected windows
(use-package auto-dim-other-buffers
  :defer t
  :commands auto-dim-other-buffers-mode
  :load-path (lambda () (expand-file-name "auto-dim-other-buffers/" user-emacs-directory))
  :config (auto-dim-other-buffers-mode 1))

;; Shackle windows (controls popup windows)
(use-package shackle
  :defer t
  :load-path (lambda () (expand-file-name "shackle/" user-emacs-directory))
  :commands shackle-mode
  :init (shackle-mode 1)
  :config (progn

            (defvar my/popup-window-parameters
              '(:noesc :modeline :autokill :autoclose :autofit :static)
              "A list of window parameters that are set (and cleared) when `doom-popup-mode
is enabled/disabled.'")

            (defvar my/popup-blacklist
              '("*Python-Help*")
              "TODO")

            ;; NOTE This is a temporary fix while I rewrite core-popups
            (defun my/display-buffer-condition (buffer _action)
              (and (cl-loop for re in my/popup-blacklist
                            when (string-match-p re buffer)
                            return nil
                            finally return t)
                   (shackle-match buffer)))

            (defun my/display-buffer-action (buffer alist)
              (shackle-display-buffer buffer alist (shackle-match buffer)))

            (add-hook 'after-init-hook
                      (lambda ()
                        (setq display-buffer-alist
                              (cons 'my/display-buffer-condition doom-display-buffer-action)
                                    display-buffer-alist)))

            ;; Prefer horizontal split (new window to the right)
            (setq split-height-threshold nil)
            (setq split-width-threshold 1)

            (setq helm-display-function         'pop-to-buffer
                  shackle-lighter               ""
                  shackle-select-reused-windows nil
                  shackle-default-alignment     'below
                  shackle-default-rule          '(:select t :autofit t :size 20)
                  shackle-default-size          0.5)  ;; default 0.5

            (setq shackle-rules
                  ;; CONDITION(:regexp)        :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
                  '((compilation-mode             :regexp nil :select nil :align t)
                    (help-mode                    :regexp nil :select nil :align t)
                    (apropos-mode                 :size 0.5   :autokill t :autoclose t)
                    (comint-mode                  :noesc  t)
                    (grep-mode                    :size 20    :noselect t :autokill t)
                    (Buffer-menu-mode             :size 20    :autokill t)
                    (tabulated-list-mode          :noesc t)
                    ("*Warnings*"                 :size 0.3   :noselect t :autofit t)
                    ("\\*Org Src.*"               :regexp t   :select nil :align t)
                    (" *Org todo*"                :regexp nil :select nil :align t)
                    ("*Flycheck errors*"          :size 8     :regexp nil :select t   :autofit t)
                    ("*undo-tree*"                :regexp nil :select t   :align t)
                    ("*eshell*"                   :regexp nil :select t   :align t)
                    ("*info*"                     :size 0.3   :select t   :autokill t)
                    ("\\*Async Shell.*\\*"        :regexp t   :select nil :align t)
                    ("*Help*"                     :size 0.3   :select nil :autokill t)
                    ("^\\*.*Shell Command.*\\*$"  :regexp t   :size 20    :noselect t :autokill t)
                    ("*Python-Help*"              :regexp nil :select nil :autofit t  :inhibit-window-quit t)
                    ("*Completions*"              :regexp nil :select nil :align t)
                    ("*Messages*"                 :regexp nil :select nil :align t)
                    ("\\`\\*helm.*?\\*\\'"        :regexp t   :select t   :align t)
                    ("*Calendar*"                 :regexp nil :select nil :align t)
                    ("^ ?\\*"                     :regexp t   :size 15    :noselect t :autokill t :autoclose t)))))

(provide 'setup-windows)
;;; setup-windows.el ends here

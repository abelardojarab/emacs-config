;;; setup-windows.el ---                             -*- lexical-binding: t; -*-

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
      ;; resize windows to accommodate new ones
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

;; Restore old window configurations
(use-package winner
  :defer t
  :hook (after-init . winner-mode)
  :commands (winner-mode
             winner-undo)
  :custom (winner-boring-buffers '("*Completions*"
                                   "*Compile-Log*"
                                   "*inferior-lisp*"
                                   "*Fuzzy Completions*"
                                   "*Apropos*"
                                   "*Help*"
                                   "*cvs*"
                                   "*Buffer List*"
                                   "*Ibuffer*"
                                   "*esh command on file*")))

;; Manage popup windows
(use-package popwin
  :defer t
  :commands popwin-mode
  :config (add-to-list popwin:special-display-config '(help-mode 0.5 :position below)))

;; Perspective
(use-package perspective
  :defer t
  :commands persp-mode
  :config (progn

            ;; Assure .emacs.cache/perspective-configs directory exists
            (if (not (file-exists-p (concat (file-name-as-directory
                                             my/emacs-cache-dir)
                                            "perspective-configs")))
                (make-directory (concat (file-name-as-directory
                                         my/emacs-cache-dir)
                                        "perspective-configs") t))

            (setq persp-auto-save-opt             0
                  persp-autokill-buffer-on-remove 'kill-weak
                  persp-auto-resume-time          0.1
                  persp-auto-save-num-of-backups  1
                  persp-autokill-buffer-on-remove 'kill-weak
                  persp-save-dir (file-name-as-directory
                                  (concat (file-name-as-directory my/emacs-cache-dir)
                                          "perspective-configs")))

            (setq persp-keymap-prefix (kbd "C-c p"))
            (persp-mode t)))

;; Window purpose
(use-package window-purpose
  :defer t
  :after helm
  :commands purpose-mode
  :init (purpose-mode)
  :config (progn

            ;; Prefer helm
            (setq purposxe-preferred-prompt 'helm)

            ;; overriding `purpose-mode-map' with empty keymap, so it doesn't conflict
            ;; with original `C-x C-f', `C-x b', etc. and `semantic' key bindings. must
            ;; be done before `window-purpose' is loaded
            (setq purpose-mode-map (make-sparse-keymap)))
  :config (progn
            ;; Assure .emacs.cache/layouts directory exists
            (if (not (file-exists-p (concat (file-name-as-directory
                                             my/emacs-cache-dir)
                                            "purpose-layouts")))
                (make-directory (concat (file-name-as-directory
                                         my/emacs-cache-dir)
                                        "purpose-layouts") t))

            (setq purpose-layout-dirs (concat (file-name-as-directory my/emacs-cache-dir)
                                              "purpose-layouts"))

            (setq purpose-user-name-purposes
                  '(("*ag*"                        . search)
                    ("todo.org"                    . agenda)
                    ("agenda.org"                  . agenda)
                    ("notas.org"                   . agenda)))

            (setq purpose-user-regexp-purposes
                  '(("^\\*elfeed"                  . internet)
                    ("^\\*Python Completions"      . minibuf)
                    ))

            (setq purpose-user-mode-purposes
                  '((eshell-mode                   . terminal)

                    (c-mode                        . edit)
                    (c++-mode                      . edit)
                    (js2-mode                      . edit)
                    (org-mode                      . org)

                    (python-mode                   . edit)
                    (inferior-python-mode          . interactive)

                    (ess-mode                      . edit)
                    (inferior-ess-mode             . interactive)

                    (gitconfig-mode                . edit)

                    (mu4e-main-mode                . internet)
                    (mu4e-view-mode                . internet)
                    (mu4e-about-mode               . internet)
                    (mu4e-headers-mode             . internet)
                    (mu4e-compose-mode             . edit)

                    (pdf-view-mode                 . view)
                    (doc-view-mode                 . view)))

            ;; Prefer helm
            (setq purpose-preferred-prompt 'helm)

            ;; Load user preferences
            (purpose-compile-user-configuration)
            (purpose-mode t)
            (define-purpose-prefix-overload purpose-switch-buffer-overload
              '(ivy-switch-buffer
                switch-buffer-without-purpose
                purpose-switch-buffer-with-purpose))

            (defadvice purpose-find-file-overload (after find-file-sudo activate)
              "Find file as root if necessary."
              (unless (and buffer-file-name
                           (file-writable-p buffer-file-name))

                (let* ((buffer-file (buffer-file-name))
                       (coincidence (string-match-p "@" buffer-file))
                       (hostname)
                       (buffer-name))
                  (if coincidence
                      (progn
                        (setq hostname (substring buffer-file (+ coincidence 1)
                                                  (string-match-p ":" buffer-file      (+ coincidence 1))))
                        (setq buffer-name
                              (concat
                               (substring buffer-file 0 coincidence) "@"
                               (replace-regexp-in-string ":" (concat "|sudo:" hostname ":")
                                                         buffer-file nil nil nil (+ coincidence 1))))
                        (find-alternate-file buffer-name))
                    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file))))))

            ;; Extensions for purpose
            (use-package window-purpose-x
              :after window-purpose
              :config (progn
                        (purpose-x-golden-ratio-setup)

                        ;; Integration with perspective
                        ;; (purpose-x-persp-setup)

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
  :commands (helm-purpose-switch-buffer-with-purpose
             helm-purpose-switch-buffer-with-some-purpose))

;; Ivy interface to purpose
(use-package ivy-purpose
  :defer t
  :commands (ivy-purpose-switch-buffer-with-purpose
             ivy-purpose-switch-buffer-with-some-purpose
             ivy-purpose-switch-buffer-without-purpose)
  :after (ivy window-purpose)
  :config (ivy-purpose-setup))

;; Resize windows
(use-package resize-window
  :defer t
  :commands resize-window)

;; Switch window
(use-package switch-window
  :defer t
  :if (display-graphic-p)
  :commands switch-window)

;; ace-window for switching windows, but we only call it as a subroutine from a hydra
(use-package ace-window
  :defer t
  :bind (:map ctl-x-map
              ("o" . ace-window))
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
  :commands golden-ratio-adjust)

;; Auto-dim unselected windows
(use-package auto-dim-other-buffers
  :defer t
  :commands auto-dim-other-buffers-mode
  :config (auto-dim-other-buffers-mode 1))

;; Shackle windows (controls popup windows)
(use-package shackle
  :defer t
  :commands shackle-mode
  :init (shackle-mode 1)
  :config (progn

            (with-eval-after-load "window"
              (defcustom split-window-below nil
                "If non-nil, vertical splits produce new windows below."
                :group 'windows
                :type 'boolean)

              (defcustom split-window-right nil
                "If non-nil, horizontal splits produce new windows to the right."
                :group 'windows
                :type 'boolean)

              (fmakunbound #'split-window-sensibly)
              (defun split-window-sensibly
                  (&optional window)
                "Split WINDOW in a way suitable for `display-buffer'."
                (setq window (or window (selected-window)))
                (or (and (window-splittable-p window t)
                         ;; Split window horizontally.
                         (split-window window nil (if split-window-right 'left  'right)))
                    (and (window-splittable-p window)
                         ;; Split window vertically.
                         (split-window window nil (if split-window-below 'above 'below)))
                    (and (eq window (frame-root-window (window-frame window)))
                         (not (window-minibuffer-p window))
                         ;; If WINDOW is the only window on its frame and is not the
                         ;; minibuffer window, try to split it horizontally disregarding the
                         ;; value of `split-width-threshold'.
                         (let ((split-width-threshold 0))
                           (when (window-splittable-p window t)
                             (split-window window nil (if split-window-right
                                                          'left
                                                        'right))))))))

            (setq split-window-right nil)
            (setq-default split-height-threshold  80 ;; the reasonable limit for vertical splits
                          split-width-threshold   0)

            (setq helm-display-function         'pop-to-buffer
                  shackle-lighter               ""
                  shackle-select-reused-windows nil
                  shackle-default-alignment     'right
                  shackle-default-rule          '(:select t :autofit t :align right :size 0.4 :same t :inhibit-window-quit nil)
                  shackle-default-size           0.4
                  shackle-default-ratio          0.4)  ;; default 0.5

            (setq shackle-rules
                  ;; CONDITION(:regexp)        :select|:noselect     :inhibit-window-quit   :size+:align|:other     :same|:popup
                  '((compilation-mode             :regexp nil)
                    (help-mode                    :regexp nil)
                    (apropos-mode                 :autokill t)
                    (comint-mode                  :noesc t)
                    (grep-mode                    :noselect t :autokill t)
                    (Buffer-menu-mode             :autokill t)
                    (tabulated-list-mode          :noesc t)
                    (magit-status-mode            :inhibit-window-quit t)
                    (magit-log-mode               :inhibit-window-quit t)
                    ("*Warnings*"                 :noselect t)
                    ("*Python-Help*"              :regexp nil :noselect t)
                    ("*Completions*"              :regexp nil :noselect t)
                    ("*Calendar*"                 :regexp nil :noselect t)
                    ("*Messages*"                 :regexp nil :noselect t)
                    ("*undo-tree*"                :regexp nil)
                    ("*eshell*"                   :regexp nil)
                    ("*Flycheck errors*"          :regexp nil)
                    ("*info*"                     :autokill t)
                    ("*Help*"                     :noselect t :autokill t)
                    ("\\magit:.*"                 :regexp t)
                    ("\\*Org Src.*"               :regexp t)
                    ("\\*Org todo.*"              :regexp t)
                    ("\\*Async Shell.*\\*"        :regexp t)
                    ("^\\*.*Shell Command.*\\*$"  :regexp t   :noselect t :autokill t)
                    ("\\`\\*helm.*?\\*\\'"        :regexp t)
                    ("^ ?\\*"                     :regexp t   :noselect t :autokill t :autoclose t)))))

(provide 'setup-windows)
;;; setup-windows.el ends here

;;; setup-windows.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@Abelardos-MacBook-Pro.local>
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

;; Removes *Help* from buffer after the mode has been set.
;; (defun remove-help-buffer ()
;;   (if (get-buffer "*Help*")
;;       (kill-buffer "*Help*")))
;; (add-hook 'after-change-major-mode-hook 'remove-help-buffer)

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
      same-window-regexps                  '("."))

;; Configure `display-buffer' behaviour for some special buffers.
(setq display-buffer-alist
      `(
        ;; Put REPLs and error lists into the bottom side window
        (,(rx bos
              (or "*Help*"                        ;; Help buffers
                  "*Warnings*"                    ;; Emacs warnings
                  "*Compile-Log*"                 ;; Emacs byte compiler log
                  "*compilation"                  ;; Compilation buffers
                  "*Flycheck errors*"             ;; Flycheck error list
                  "*shell"                        ;; Shell window
                  (and (1+ nonl) " output*")      ;; AUCTeX command output
                  ))
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side            . right)
         (reusable-frames . visible)
         (window-height   . 0.33))
        ;; Let `display-buffer' reuse visible frames for all buffers.  This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; later entry with more specific actions.
        ("." nil (reusable-frames . visible))))

;; Prefer horizontal window splitting (new window on the right)
;; http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 1000)

;; http://stackoverflow.com/questions/23659909/reverse-evaluation-order-of-split-height-threshold-and-split-width-threshold-in
(defun my/split-window-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding
             ;; the value of `split-width-threshold'.
             (let ((split-width-threshold 100))
               (when (window-splittable-p window t)
                 (with-selected-window window
                   (split-window-right))))))))

(setq split-window-preferred-function 'my/split-window-sensibly)
(defadvice org-agenda (around split-vertically activate)
  (let ((split-width-threshold 100)) ;; or whatever width makes sense for you
    ad-do-it))

;; Switch between vertical and horizontal splitting
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd
              (not (and (<= (car this-win-edges)
                            (car next-win-edges))
                        (<= (cadr this-win-edges)
                            (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; C-x 4 t 'toggle-window-split
(define-key ctl-x-4-map "t" 'toggle-window-split)

;; Manage popup windows
(use-package popwin
  :defer t
  :disabled t
  :commands popwin-mode
  :load-path (lambda () (expand-file-name "popwin/" user-emacs-directory)))

;; Window purpose
(use-package window-purpose
  :defer t
  :disabled t
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
                  '(("*ag*"               . search)))

            (setq purpose-user-regexp-purposes
                  '(("^\\*elfeed"                  . admin)
                    ("^\\*Python Completions"      . minibuf)
                    ))

            (setq purpose-user-mode-purposes
                  '((eshell-mode          . terminal)

                    (python-mode          . py)
                    (inferior-python-mode . py-repl)

                    (circe-chat-mode      . comm)
                    (circe-query-mode     . comm)
                    (circe-lagmon-mode    . comm)
                    (circe-server-mode    . comm)

                    (ess-mode             . edit)
                    (gitconfig-mode       . edit)
                    (inferior-ess-mode    . interactive)

                    (mu4e-main-mode       . admin)
                    (mu4e-view-mode       . admin)
                    (mu4e-about-mode      . admin)
                    (mu4e-headers-mode    . admin)
                    (mu4e-compose-mode    . edit)

                    (pdf-view-mode        . view)
                    (doc-view-mode        . view)))

            ;; Prefer helm
            (setq purpose-preferred-prompt 'helm)

            ;; Load user preferences
            (purpose-compile-user-configuration)
            (purpose-mode)

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
  :disabled t
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
            ;; http://oremacs.com/2015/02/27/ace-window-leading-char/
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
            (setq shackle-lighter               ""
                  shackle-select-reused-windows nil
                  shackle-default-alignment     'right
                  shackle-default-size          0.4)  ;; default 0.5

            (setq shackle-rules
                  ;; CONDITION(:regexp)        :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
                  '((compilation-mode          :select     nil)
                    ("*undo-tree*"             :size       0.25     :align               right)
                    ("*eshell*"                :select     t        :other               t)
                    ("*Shell Command Output*"  :select     nil)
                    ("\\*Async Shell.*\\*"     :regexp     t        :ignore              t)
                    (occur-mode                :select     nil      :align               t)
                    ("*Help*"                  :select     t        :inhibit-window-quit t       :other               t)
                    ("*Completions*"           :size       0.3      :align               t)
                    ("*Messages*"              :select     nil      :inhibit-window-quit t       :other               t)
                    ("\\*[Wo]*Man.*\\*"        :regexp     t        :select              t       :inhibit-window-quit t :other t)
                    ("\\`\\*helm.*?\\*\\'"     :regexp     t        :size                0.3     :align               t)
                    ("*Calendar*"              :select     t        :size                0.3     :align               below)))))

(provide 'setup-windows)
;;; setup-windows.el ends here

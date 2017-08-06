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

;; Put a nice title to the window, including filename
(add-hook 'window-configuration-change-hook
          (lambda ()
            (setq frame-title-format
                  (concat
                   invocation-name "@" system-name ": "
                   (replace-regexp-in-string
                    (concat "/home/" user-login-name) "~"
                    (or buffer-file-name "%b"))))))

;; Resize by pixels
(setq frame-resize-pixelwise t
      ;; Size new windows proportionally wrt other windows
      window-combination-resize t)

;; default to maximised windows
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; prompt when trying to switch out of a dedicated window
(setq switch-to-buffer-in-dedicated-window 'prompt)

;; Configure `display-buffer' behaviour for some special buffers.
(setq display-buffer-alist
      `(
        ;; Put REPLs and error lists into the bottom side window
        (,(rx bos
              (or "*Help"                         ;; Help buffers
                  "*Warnings*"                    ;; Emacs warnings
                  "*Compile-Log*"                 ;; Emacs byte compiler log
                  "*compilation"                  ;; Compilation buffers
                  "*Flycheck errors*"             ;; Flycheck error list
                  "*shell"                        ;; Shell window
                  (and (1+ nonl) " output*")      ;; AUCTeX command output
                  ))
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side            . bottom)
         (reusable-frames . visible)
         (window-height   . 0.33))
        ;; Let `display-buffer' reuse visible frames for all buffers.  This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; later entry with more specific actions.
        ("." nil (reusable-frames . visible))))

;; creating new frames
(defun clone-frame-1 (direction)
  (let* ((frame (selected-frame))
         (left (frame-parameter frame 'left))
         (top (frame-parameter frame 'top))
         (width (frame-width frame))
         (height (frame-height frame))
         (pixel-width (frame-pixel-width frame))
         (display-width (x-display-pixel-width))
         (x-offset 10) (y-offset 10))
    (make-frame
     `((left . ,(+ x-offset
                   (min (- display-width pixel-width)
                        (max 0 (+ left (* direction pixel-width))))))
       (top . ,(+ y-offset top))
       (width . ,width)
       (height . ,height)))))
(defun clone-frame-to-left ()
  "Create a new frame in the same size as the current frame and
place the new frame at the left side of the current frame."
  (interactive)
  (if (display-graphic-p)
      (clone-frame-1 -1)
    (select-frame (make-frame))))
(defun clone-frame-to-right ()
  "Create a new frame in the same size as the current frame and
place the new frame at the right side of the current frame."
  (interactive)
  (if (display-graphic-p)
      (clone-frame-1 1)
    (select-frame (make-frame))))

;; adjusting frame position
(defcustom desktop-offset-left 0
  "Left offset of the desktop in pixels"
  :type 'number
  :group 'frames)
(defcustom desktop-offset-top 0
  "Top offset of the desktop in pixels"
  :type 'number
  :group 'frames)
(defcustom desktop-offset-right 0
  "Right offset of the desktop in pixels"
  :type 'number
  :group 'frames)
(defcustom desktop-offset-bottom 0
  "Bottom offset of the desktop in pixels"
  :type 'number
  :group 'frames)
(defun screen-size ()
  (let ((screen-width 0) (screen-height 0))
    (dolist (attrs (display-monitor-attributes-list))
      (let* ((geometry (cdr (assq 'geometry attrs)))
             (right (+ (nth 0 geometry) (nth 2 geometry)))
             (bottom (+ (nth 1 geometry) (nth 3 geometry))))
        (when (> right screen-width) (setq screen-width right))
        (when (> bottom screen-height) (setq screen-height bottom))))
    (list screen-width screen-height)))
(defun fit-largest-display (position)
  (let ((frame (selected-frame))
        (largest-area 0) (screen (screen-size)) dimensions)
    (dolist (attrs (display-monitor-attributes-list))
      (let* ((geometry (cdr (assq 'geometry attrs)))
             (left (nth 0 geometry))
             (top (nth 1 geometry))
             (width (nth 2 geometry))
             (height (nth 3 geometry))
             (area (* width height))
             (right (+ left width))
             (bottom (+ top height)))
        (when (> area largest-area)
          (setq dimensions (list left top right bottom width height)
                largest-area area))))
    (when dimensions
      (let* ((frame-width (frame-pixel-width frame))
             (left (if (eq position 'left)
                       (max desktop-offset-left
                            (nth 0 dimensions))
                     (min (- (nth 0 screen) desktop-offset-right frame-width)
                          (- (nth 2 dimensions) frame-width))))
             (top (max desktop-offset-top (nth 1 dimensions)))
             (height (/ (- (nth 5 dimensions)
                           desktop-offset-top desktop-offset-bottom)
                        (frame-char-height frame))))
        (set-frame-position frame left top)
        (set-frame-height frame height)))))
(defun fit-largest-display-left ()
  "Fit the current frame to the left end of the largest display."
  (interactive)
  (fit-largest-display 'left))
(defun fit-largest-display-right ()
  "Fit the current frame to the left end of the largest display."
  (interactive)
  (fit-largest-display 'right))

;; Treat all windows as same
(setq same-window-regexps '("."))

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
  :commands popwin-mode
  :load-path (lambda () (expand-file-name "popwin/" user-emacs-directory))
  :config (progn
            (defvar popwin:special-display-config-backup popwin:special-display-config)
            (setq display-buffer-function 'popwin:display-buffer)

            ;; basic
            (push '("*Help*" :stick t :noselect t) popwin:special-display-config)
            (push '("*Cedet Global*" :stick t :noselect t) popwin:special-display-config)

            ;; magit
            (push '("*magit-process*" :stick t) popwin:special-display-config)

            ;; dictionaly
            (push '("*dict*" :stick t) popwin:special-display-config)
            (push '("*sdic*" :stick t) popwin:special-display-config)

            ;; Elisp
            (push '("*ielm*" :stick t) popwin:special-display-config)
            (push '("*eshell pop*" :stick t) popwin:special-display-config)

            ;; python
            (push '("*Python*"   :stick t) popwin:special-display-config)
            (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
            (push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

            ;; ecb
            (push '("*ECB History*" :position left :width 0.3 :stick t)
                  popwin:special-display-config)
            (push '("*ECB Methods*" :position left :width 0.3 :stick t)
                  popwin:special-display-config)
            (push '("*ECB Directories*" :position right :width 0.3 :stick t)
                  popwin:special-display-config)

            ;; sgit
            (push '("*sgit*" :position right :width 0.5 :stick t)
                  popwin:special-display-config)

            ;; git-gutter
            (push '("*git-gutter:diff*" :width 0.5 :stick t)
                  popwin:special-display-config)

            ;; es-results-mode
            (push '(es-result-mode :stick t :width 0.5)
                  popwin:special-display-config)

            ;; direx
            (push '(direx:direx-mode :position left :width 40 :dedicated t)
                  popwin:special-display-config)

            (push '("*Occur*" :stick t) popwin:special-display-config)

            ;; compilation
            (push '("*compilation*" :stick t :height 30)
                  popwin:special-display-config)

            ;; org-mode
            (push '("*Org tags*" :stick t :height 30)
                  popwin:special-display-config)

            ;; Completions
            (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)

            ;; ggtags
            (push '("*ggtags-global*" :stick t :noselect t :height 30) popwin:special-display-config)

            ;; async shell commands
            (push '("*Async Shell Command*" :stick t) popwin:special-display-config)

            ;; helm
            (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
            (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)

            ;; popwin conflicts with ecb
            (popwin-mode -1)))

;; Window purpose
(use-package window-purpose
  :after helm
  :defer t
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
            (purpose-mode)))

;; Extensions for purpose
(use-package window-purpose-x
  :after window-purpose
  :config (progn

            ;; Single window magit
            (purpose-x-magit-single-on)

            ;; when killing a purpose-dedicated buffer that is displayed in a window,
            ;; ensure that the buffer is replaced by a buffer with the same purpose
            ;; (or the window deleted, if no such buffer)
            (purpose-x-kill-setup)))

;; Helm interface to purpose
(use-package helm-purpose
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
  :config   (progn
              (setq shackle-lighter               ""
                    shackle-select-reused-windows nil
                    shackle-default-alignment     'below
                    shackle-default-size          0.4)  ;; default 0.5

              (setq shackle-rules
                    ;; CONDITION(:regexp)        :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
                    '((compilation-mode          :select     nil)
                      ("\\`\\*helm.*?\\*\\'"     :regexp     t        :align               t       :ratio               0.4)
                      ("*undo-tree*"             :size       0.25     :align               right)
                      ("*eshell*"                :select     t        :other               t)
                      ("*Shell Command Output*"  :select     nil)
                      ("\\*Async Shell.*\\*"     :regexp     t        :ignore t)
                      (occur-mode                :select     nil      :align               t)
                      ("*Help*"                  :select     t        :inhibit-window-quit t       :other               t)
                      ("*Completions*"           :size       0.3      :align               t)
                      ("*Messages*"              :select     nil      :inhibit-window-quit t       :other               t)
                      ("\\*[Wo]*Man.*\\*"        :regexp     t        :select              t       :inhibit-window-quit t :other t)
                      ("\\`\\*helm.*?\\*\\'"     :regexp     t        :size                0.3     :align               t)
                      ("*Calendar*"              :select     t        :size                0.3     :align               below)))))

(provide 'setup-windows)
;;; setup-windows.el ends here

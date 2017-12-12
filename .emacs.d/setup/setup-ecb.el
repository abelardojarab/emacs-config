;;; setup-ecb.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@ubuntu02>
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

;; Speedbar
(use-package sr-speedbar
  :defer t
  :if (display-graphic-p)
  :defines (sr-speedbar-exist-p)
  :commands (sr-speedbar-open
             sr-speedbar-toggle)
  :bind (:map speedbar-mode-map
              ([S-up]  . speedbar-up-directory)
              ([right] . speedbar-flush-expand-line)
              ([left]  . speedbar-contract-line))
  :config (progn
            (setq speedbar-hide-button-brackets-flag    t
                  speedbar-show-unknown-files           t
                  speedbar-smart-directory-expand-flag  t
                  speedbar-directory-button-trim-method 'trim
                  speedbar-use-images                   t
                  speedbar-indentation-width            2
                  speedbar-use-imenu-flag               t
                  speedbar-file-unshown-regexp          "flycheck-.*"
                  speedbar-update-flag                  t
                  sr-speedbar-width                     40
                  sr-speedbar-width-x                   40
                  sr-speedbar-auto-refresh              t
                  sr-speedbar-skip-other-window-p       t
                  sr-speedbar-right-side                nil)

            ;; Highlight the current line
            (add-hook 'speedbar-mode-hook '(lambda () (hl-line-mode 1)))

            ;; Add Javascript
            (speedbar-add-supported-extension ".js")
            (add-to-list 'speedbar-fetch-etags-parse-list
                         '("\\.js" . speedbar-parse-c-or-c++tag))
            (speedbar-add-supported-extension ".il")
            (speedbar-add-supported-extension ".ils")))

;; projectile and speedbar integration
(use-package projectile-speedbar
  :defer t
  :if (display-graphic-p)
  :after (sr-speedbar projectile)
  :load-path (lambda () (expand-file-name "projectile-speedbar/" user-emacs-directory))
  :commands projectile-speedbar-open-current-buffer-in-tree
  :init (progn
          (defadvice helm-projectile-find-file (after locate-file activate)
            (if (sr-speedbar-exist-p)
                (projectile-speedbar-open-current-buffer-in-tree)))
          (defadvice speedbar-item-load (after speedbar-highlight-file activate)
            (projectile-speedbar-open-current-buffer-in-tree))))

;; Code Browser
(use-package ecb
  :load-path (lambda () (expand-file-name "ecb/" user-emacs-directory))
  ;; :commands (ecb-activate) ;; enabling this line places ecb in autoload state
  :init (setq stack-trace-on-error t)
  :config (progn

            ;; Fix error with symboldef sync
            (defmacro ecb-with-readonly-buffer (buffer &rest body)
              "Make buffer BUFFER current but do not display it. Evaluate BODY in buffer
BUFFER \(not read-only an evaluation-time of BODY) and make afterwards BUFFER
read-only. Note: All this is done with `save-excursion' so after BODY that
buffer is current which was it before calling this macro."
              `(ignore-errors (if (buffer-live-p ,buffer)
                                  (with-current-buffer ,buffer
                                    (unwind-protect
                                        (progn
                                          (setq buffer-read-only nil)
                                          ,@body)
                                      (setq buffer-read-only t)))
                                (ecb-error "Try to set a not existing buffer."))))

            ;; ECB setup
            (if (ecb--semantic-active-p)
                (ecb-update-methods-buffer--internal nil nil t)
              (ecb-rebuild-methods-buffer-for-non-semantic))

            (defconst initial-frame-width (frame-width)
              "The width of frame will be changed ,remember the init value.")

            (setq ecb-tip-of-the-day nil
                  ecb-show-sources-in-directories-buffer 'always
                  ecb-compile-window-height nil
                  ecb-compile-window-width 'edit-window
                  ecb-compile-window-temporally-enlarge nil
                  ecb-eshell-fit-window-to-command-output nil
                  ecb-create-layout-file "~/.emacs.cache/ecb-user-layouts.el"
                  ecb-windows-width 35
                  ecb-fix-window-size 'width
                  ecb-layout-name my/ecb-layout-theme
                  ecb-history-make-buckets 'mode
                  ecb-highlight-tag-with-point-delay 180
                  ecb-kill-buffer-clears-history 'auto
                  ecb-tip-of-the-day nil
                  ecb-tip-of-the-day-file "~/.emacs.cache/ecb-tip-of-day.el"
                  ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2
                  semantic-decoration-styles (list
                                              '("semantic-decoration-on-protected-members" . t)
                                              '("semantic-decoration-on-private-members"   . t)
                                              '("semantic-decoration-on-includes"          . t)
                                              '("semantic-tag-boundary"                    . t)))

            ;; Keep line truncation
            (setq ecb-truncate-lines t)
            (add-hook 'ecb-show-ecb-windows-before-hook
                      'ecb-enlarge-frame-width-before-show)
            (add-hook 'ecb-hide-ecb-windows-before-hook
                      'ecb-shrink-frame-width-before-hide)
            (add-hook 'ecb-deactivate-hook
                      'ecb-shrink-frame-width-before-hide)
            (add-hook 'ecb-activate-before-layout-draw-hook
                      'ecb-enlarge-frame-width-before-activate)

            (defun frame-horizontal-maximized-p ()
              "Test current frame wheather be maxmized by test the frame width and height equal to the screen resolution"
              (interactive)
              (equal (frame-pixel-width) (display-pixel-width)))

            (defun ecb-enlarge-frame-width-before-show ()
              "Enlarge frame width before ecb shows layout."
              (if (and (ecb-windows-all-hidden)
                       (<= (+ (frame-pixel-width) (* (frame-char-width)
                                                     (+ ecb-windows-width 2)))
                           (display-pixel-width)))
                  (set-frame-width (selected-frame) (+ (frame-width) (+ ecb-windows-width 2)))))
            (defun ecb-shrink-frame-width-before-hide ()
              "Shrink frame width before ecb hide layout."
              (if (and (not (ecb-windows-all-hidden))

                       (not (eq (frame-pixel-width)
                                (display-pixel-width))))
                  (if (< (- (frame-width) (+ ecb-windows-width 2)) initial-frame-width)
                      (set-frame-width (selected-frame) initial-frame-width)
                    (set-frame-width (selected-frame) (- (frame-width) (+ ecb-windows-width 2))))))
            (defun ecb-enlarge-frame-width-before-activate ()
              "Enlarge frame width when ecb active and need it to."
              (let ((use-last-win-conf (and ecb-last-window-config-before-deactivation
                                            (equal ecb-split-edit-window-after-start
                                                   'before-deactivation)
                                            (not (ecb-window-configuration-invalidp
                                                  ecb-last-window-config-before-deactivation)))))
                (unless (or (and use-last-win-conf
                                 (eq (nth 5 ecb-last-window-config-before-deactivation)
                                     ecb-windows-hidden-all-value))
                            (> (+ (frame-pixel-width) (* (frame-char-width)
                                                         (+ ecb-windows-width 2)))
                               (display-pixel-width)))
                  (set-frame-width (selected-frame) (+ (frame-width) (+ ecb-windows-width 2))))))

            ;; reference path-to-ecb/ecb-layout-defs.el
            (ecb-layout-define "leftright-sa-m" left-right
              "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |  Sources     |                               |  Methods    |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |--------------|             Edit              |-------------|
   |              |                               |             |
   |              |                               |             |
   |  Analyse     |                               |             |
   |              |                               | Symbol Defs |
   |              |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
              (ecb-set-history-buffer)
              (ecb-split-ver 0.5)
              (ecb-set-analyse-buffer)
              (select-window (next-window (next-window)))
              (ecb-set-methods-buffer)
              (select-window (previous-window (previous-window (selected-window) 0) 0)))

            (ecb-layout-define "left-speedbar" left
              "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |   Speedbar   |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. "
              (ecb-set-speedbar-buffer)
              (select-window (next-window)))

            (ecb-layout-define "left-speedbar-right" left-right
              "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |  History    |
   |              |                               |             |
   |              |                               |             |
   |  Methods     |                               |             |
   |              |             Edit              |-------------|
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               | Speedbar    |
   |              |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. "
              (ecb-set-methods-buffer)
              (select-window (next-window (next-window)))
              (ecb-set-history-buffer)
              (ecb-split-ver 0.4)
              (ecb-set-analyse-buffer)
              (select-window (previous-window (selected-window) 0)))

            (ecb-layout-define "bodil" left
              "This function creates the following layout:
   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  History     |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |              |                                      |
   |  Sources     |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------
If you have not set a compilation-window in `ecb-compile-window-height' then the
layout contains no persistent compilation window and the other windows get a little
more place."
              (ecb-set-history-buffer)
              (ecb-split-ver 0.5)
              (ecb-set-methods-buffer)
              (select-window (next-window)))

            ;; disable global semantic idle scheduler.
            ;; it doesn't really seem to work all that well in automatically
            ;; reparsing buffers and it's actually intrusive when i'm typing:
            (add-hook 'ecb-activate-hook
                      '(lambda ()
                         (semantic-mode t)
                         (setq global-semantic-idle-scheduler-mode nil)))

            ;; Reparse after a file save
            (add-hook 'after-save-hook
                      '(lambda ()
                         (when (bound-and-true-p ecb-minor-mode)
                           (ignore-errors
                             ;; this is to get the methods buffer to refresh correctly.
                             ;; semantic idle mode refresh doesn't seem to work all that well.
                             (semantic-force-refresh)
                             (ecb-rebuild-methods-buffer)
                             (ecb-window-sync)))))

            ;; Finally activate ecb on HD-monitors or above
            (add-hook 'after-init-hook
                          (lambda ()
                            (if (or (and (> (car (screen-size)) 1900)
                                         (> (cadr (screen-size)) 1000))
                                    (not (display-graphic-p)))
                                (ecb-activate))))))

(provide 'setup-ecb)
;;; setup-ecb.el ends here

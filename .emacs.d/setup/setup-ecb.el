;;; setup-ecb.el --- 

;; Copyright (C) 2014  abelardo.jara-berrocal

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

;; Code Browser
(setq stack-trace-on-error t)
(setq after-find-file-from-revert-buffer t)
(add-to-list 'load-path "~/.emacs.d/ecb")

;; Enable ecb
(require 'ecb)
(setq ecb-show-sources-in-directories-buffer 'always)
(set-face-foreground 'ecb-default-general-face "#ffffff")
(setq ecb-tip-of-the-day nil)
(if (ecb--semantic-active-p)
    (ecb-update-methods-buffer--internal nil nil t)
  (ecb-rebuild-methods-buffer-for-non-semantic))

(defconst initial-frame-width (frame-width)
  "The width of frame will be changed ,remember the init value.")
(setq ecb-compile-window-height nil
      ecb-compile-window-width 'edit-window
      ecb-compile-window-temporally-enlarge 'both
      ecb-create-layout-file "~/.emacs.cache/auto-save-list/.ecb-user-layouts.el"
      ecb-windows-width 30
      ecb-fix-window-size 'width 
      ecb-layout-name "leftright-sa-m"
      ecb-history-make-buckets 'mode
      ecb-kill-buffer-clears-history 'auto
      ecb-tip-of-the-day nil
      ecb-tip-of-the-day-file "~/.emacs.cache/auto-save-list/.ecb-tip-of-day.el"
      ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2 
      semantic-decoration-styles (list '("semantic-decoration-on-includes" . t)
                                       '("semantic-tag-boundary" . t)))

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
   |  Sources     |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |--------------|             Edit              |  Methods    |
   |              |                               |             |
   |              |                               |             |
   |  Analyse     |                               |             |
   |              |                               |             |
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
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.4)
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

(provide 'setup-ecb)
;;; setup-ecb.el ends here

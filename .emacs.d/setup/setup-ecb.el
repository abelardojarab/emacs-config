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
  ;; (ecb-set-speedbar-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-analyse-buffer)
  (select-window (next-window (next-window)))
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-symboldef-buffer)
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

;; Speedbar
(require 'sr-speedbar)
(setq speedbar-hide-button-brackets-flag t
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-directory-button-trim-method 'trim
      speedbar-use-images nil
      speedbar-indentation-width 2
      speedbar-use-imenu-flag t
      speedbar-file-unshown-regexp "flycheck-.*"
      sr-speedbar-width 40
      sr-speedbar-width-x 40
      sr-speedbar-auto-refresh t
      sr-speedbar-skip-other-window-p t
      sr-speedbar-right-side nil)

;; Refresh the speedbar when relevant hooks are run.
(defvar graphene-speedbar-refresh-hooks-added nil
  "Whether hooks have been added to refresh speedbar.")
(defvar graphene-speedbar-refresh-hooks nil
  "Dummy variable.")

(add-hook 'speedbar-mode-hook
          (when (not graphene-speedbar-refresh-hooks-added)
            (lambda ()
              (mapc (lambda (hook)
                      (add-hook hook 'speedbar-refresh))
                    graphene-speedbar-refresh-hooks)
              (setq graphene-speedbar-refresh-hooks t))))

;; More familiar keymap settings.
(add-hook 'speedbar-reconfigure-keymaps-hook
          '(lambda ()
             (define-key speedbar-mode-map [S-up] 'speedbar-up-directory)
             (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
             (define-key speedbar-mode-map [left] 'speedbar-contract-line)))

;; Highlight the current line
(add-hook 'speedbar-mode-hook '(lambda () (hl-line-mode 1)))

;; Pin and unpin the speedbar
(defadvice speedbar-update-directory-contents
  (around graphene-speedbar-pin-directory activate disable)
  "Pin the speedbar to the directory set in graphene-speedbar-pinned-directory."
  (let ((default-directory graphene-speedbar-pinned-directory))
    ad-do-it))

(defadvice speedbar-dir-follow
  (around graphene-speedbar-prevent-follow activate disable)
  "Prevent speedbar changing directory on button clicks."
  (speedbar-toggle-line-expansion))

(defadvice speedbar-directory-buttons-follow
  (around graphene-speedbar-prevent-root-follow activate disable)
  "Prevent speedbar changing root directory on button clicks.")

(defvar graphene-speedbar-pin-advice
  '((speedbar-update-directory-contents around graphene-speedbar-pin-directory)
    (speedbar-dir-follow around graphene-speedbar-prevent-follow)
    (speedbar-directory-buttons-follow around graphene-speedbar-prevent-root-follow))
  "Advice to be enabled and disabled on graphene-[un]-pin-speedbar.")

(defun graphene-speedbar-pin-advice-activate ()
  "Activate the advice applied to speedbar functions in order to pin it to a directory."
  (mapc 'ad-activate (mapcar 'car graphene-speedbar-pin-advice)))

(defun graphene-pin-speedbar (directory)
  "Prevent the speedbar from changing the displayed root directory."
  (setq graphene-speedbar-pinned-directory directory)
  (mapc (lambda (ls) (apply 'ad-enable-advice ls)) graphene-speedbar-pin-advice)
  (graphene-speedbar-pin-advice-activate))

(defun graphene-unpin-speedbar ()
  "Allow the speedbar to change the displayed root directory."
  (mapc (lambda (ls) (apply 'ad-disable-advice ls)) graphene-speedbar-pin-advice)
  (graphene-speedbar-pin-advice-activate))

;; Always use the last selected window for loading files from speedbar.
(defvar last-selected-window
  (if (not (eq (selected-window) sr-speedbar-window))
      (selected-window)
    (other-window)))

(defadvice select-window (after remember-selected-window activate)
  "Remember the last selected window."
  (unless (or (eq (selected-window) sr-speedbar-window) (not (window-live-p (selected-window))))
    (setq last-selected-window (selected-window))))

(defun sr-speedbar-before-visiting-file-hook ()
  "Function that hooks `speedbar-before-visiting-file-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-before-visiting-tag-hook ()
  "Function that hooks `speedbar-before-visiting-tag-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-visiting-file-hook ()
  "Function that hooks `speedbar-visiting-file-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-visiting-tag-hook ()
  "Function that hooks `speedbar-visiting-tag-hook'."
  (select-window last-selected-window))

;; Add Javascript
(speedbar-add-supported-extension ".js")
(add-to-list 'speedbar-fetch-etags-parse-list
             '("\\.js" . speedbar-parse-c-or-c++tag))
(speedbar-add-supported-extension ".il")
(speedbar-add-supported-extension ".ils")

;; Nicer fonts for speedbar when in GUI
(when (and (window-system)
         (find-font (font-spec :name "Consolas")))
  ;; keep monospace buttons, but smaller height
  (set-face-attribute 'speedbar-button-face nil :height 100)

  ;; change to system default UI font for entries
  (dolist (face (list 'speedbar-file-face 'speedbar-directory-face
                      'speedbar-tag-face  'speedbar-selected-face
                      'speedbar-highlight-face))
    (if (eq system-type 'windows-nt)
        (set-face-attribute face nil :family "Consolas" :height 110))
    (set-face-attribute face nil :family "Consolas" :height 135)))

;; Projectile integration
(require'projectile-speedbar)

(provide 'setup-ecb)
;;; setup-ecb.el ends here

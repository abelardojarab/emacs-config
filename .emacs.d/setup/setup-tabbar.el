;;; setup-tabbar.el ---                         -*- lexical-binding: t; -*-

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

;; Tabbar ruler pre-requisites
(use-package mode-icons
  :if (display-graphic-p)
  :demand t)

;; Tabbar
(use-package tabbar
  :hook (after-init . tabbar-mode)
  :config (progn
            ;; Sort tabbar buffers by name
            (defun tabbar-add-tab (tabset object &optional append_ignored)
              "Add to TABSET a tab with value OBJECT if there isn't one there yet.
 If the tab is added, it is added at the beginning of the tab list,
 unless the optional argument APPEND is non-nil, in which case it is
 added at the end."
              (let ((tabs (tabbar-tabs tabset)))
                (if (tabbar-get-tab object tabset)
                    tabs
                  (let ((tab (tabbar-make-tab object tabset)))
                    (tabbar-set-template tabset nil)
                    (set tabset (sort (cons tab tabs)
                                      (lambda (a b) (string< (buffer-name (car a)) (buffer-name (car b))))))))))))

;; Tabbar ruler
(use-package tabbar-ruler
  :demand t
  :bind (("C-<prior>"   . tabbar-backward)
         ("C-<next>"    . tabbar-forward)
         ("C-c <right>" . tabbar-forward)
         ("C-c <left>"  . tabbar-backward))
  :custom ((tabbar-cycle-scope 'tabs)
           (tabbar-ruler-global-tabbar t)))

;; Newer built-in tabbar for Emacs
(use-package tab-line
  :disabled t
  :hook (after-init . global-tab-line-mode)
  :init (tabbar-mode -1)
  :custom ((tab-line-close-button-show t)
           (tab-line-new-button-show   nil)
           (tab-line-separator         ""))
  :config (progn
            (defun tab-line-close-tab (&optional e)
              "Close the selected tab.
If tab is presented in another window, close the tab by using
`bury-buffer` function.  If tab is unique to all existing
windows, kill the buffer with `kill-buffer` function.  Lastly, if
no tabs left in the window, it is deleted with `delete-window`
function."
              (interactive "e")
              (let* ((posnp (event-start e))
                     (window (posn-window posnp))
                     (buffer (get-pos-property 1 'tab (car (posn-string posnp)))))
                (with-selected-window window
                  (let ((tab-list (tab-line-tabs-window-buffers))
                        (buffer-list (flatten-list
                                      (seq-reduce (lambda (list window)
                                                    (select-window window t)
                                                    (cons (tab-line-tabs-window-buffers) list))
                                                  (window-list) nil))))
                    (select-window window)
                    (if (> (seq-count (lambda (b) (eq b buffer)) buffer-list) 1)
                        (progn
                          (if (eq buffer (current-buffer))
                              (bury-buffer)
                            (set-window-prev-buffers window (assq-delete-all buffer (window-prev-buffers)))
                            (set-window-next-buffers window (delq buffer (window-next-buffers))))
                          (unless (cdr tab-list)
                            (ignore-errors (delete-window window))))
                      (and (kill-buffer buffer)
                           (unless (cdr tab-list)
                             (ignore-errors (delete-window window)))))))))

            (defcustom tab-line-tab-min-width 10
              "Minimum width of a tab in characters."
              :type 'integer
              :group 'tab-line)

            (defcustom tab-line-tab-max-width 30
              "Maximum width of a tab in characters."
              :type 'integer
              :group 'tab-line)

            (defun my/tab-line-name-buffer (buffer &rest _buffers)
              "Create name for tab with padding and truncation.
If buffer name is shorter than `tab-line-tab-max-width' it gets
centered with spaces, otherwise it is truncated, to preserve
equal width for all tabs.  This function also tries to fit as
many tabs in window as possible, so if there are no room for tabs
with maximum width, it calculates new width for each tab and
truncates text if needed.  Minimal width can be set with
`tab-line-tab-min-width' variable."
              (with-current-buffer buffer
                (let* ((window-width (window-width (get-buffer-window)))
                       (close-button-size (if tab-line-close-button-show
                                              (length (substring-no-properties tab-line-close-button))
                                            0))
                       (tab-amount (length (tab-line-tabs-window-buffers)))
                       (window-max-tab-width (/ window-width tab-amount))
                       (tab-width (- (cond ((>= window-max-tab-width tab-line-tab-max-width)
                                            tab-line-tab-max-width)
                                           ((< window-max-tab-width tab-line-tab-min-width)
                                            tab-line-tab-min-width)
                                           (t window-max-tab-width))
                                     close-button-size))
                       (buffer-name (string-trim (buffer-name)))
                       (name-width (length buffer-name)))
                  (if (>= name-width (- tab-width 3))
                      (concat  " " (truncate-string-to-width buffer-name (- tab-width 3)) "… ")
                    (let* ((padding (make-string (/ (- tab-width name-width) 2) ?\s))
                           (buffer-name (concat padding buffer-name))
                           (name-width (length buffer-name)))
                      (concat buffer-name (make-string (- tab-width name-width) ?\s)))))))

            (setq tab-line-tab-name-function #'my/tab-line-name-buffer
                  tab-line-right-button (propertize (if (char-displayable-p ?▶) " ▶ " " > ")
                                                    'keymap tab-line-right-map
                                                    'mouse-face 'tab-line-highlight
                                                    'help-echo "Click to scroll right")
                  tab-line-left-button (propertize (if (char-displayable-p ?◀) " ◀ " " < ")
                                                   'keymap tab-line-left-map
                                                   'mouse-face 'tab-line-highlight
                                                   'help-echo "Click to scroll left")
                  tab-line-close-button (propertize (if (char-displayable-p ?×) "× " "x ")
                                                    'keymap tab-line-tab-close-map
                                                    'mouse-face 'tab-line-close-highlight
                                                    'help-echo "Click to close tab"))

            (let ((bg (face-attribute 'default :background))
                  (fg (face-attribute 'default :foreground))
                  (base (face-attribute 'mode-line :background))
                  (box-width (/ (line-pixel-height) 2)))
              (when (and (color-defined-p bg)
                         (color-defined-p fg)
                         (color-defined-p base)
                         (numberp box-width))
                (set-face-attribute 'tab-line-tab nil
                                    :foreground fg
                                    :background bg
                                    :weight 'normal
                                    :inherit nil
                                    :box (list :line-width box-width :color bg))
                (set-face-attribute 'tab-line-tab-inactive nil
                                    :foreground fg
                                    :background base
                                    :weight 'normal
                                    :inherit nil
                                    :box (list :line-width box-width :color base))
                (set-face-attribute 'tab-line-tab-current nil
                                    :foreground fg
                                    :background bg
                                    :weight 'normal
                                    :inherit nil
                                    :box (list :line-width box-width :color bg))))

            (dolist (mode '(ediff-mode
                            process-menu-mode
                            term-mode
                            vterm-mode))
              (add-to-list 'tab-line-exclude-modes mode))

            (defun my/tab-line-drop-caches ()
              "Drops `tab-line' cache in every window."
              (dolist (window (window-list))
                (set-window-parameter window 'tab-line-cache nil)))

            (add-hook 'window-configuration-change-hook #'my/tab-line-drop-caches)))

(provide 'setup-tabbar)
;;; setup-tabbar.el ends here

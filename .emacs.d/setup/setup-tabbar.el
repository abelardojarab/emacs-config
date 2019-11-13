;;; setup-tabbar.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019  Abelardo Jara-Berrocal

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

;; Tabbar mode
(use-package tabbar
  :demand t
  :commands (tabbar-mode
             my/modification-state-change
             my/on-buffer-modification)
  :bind (("C-c <right>" . tabbar-forward)
         ("C-c <left>"  . tabbar-backward)
         ("C-c <up>"    . tabbar-backward-group)
         ("C-c <down>"  . tabbar-forward-group))
  :hook ((after-save    . my/modification-state-change)
         (first-change  . my/on-buffer-modification))
  :custom ((tabbar-auto-scroll-flag  t)
           (tabbar-use-images        t)
           (tabbar-cycle-scope       (quote tabs))
           (table-time-before-update 0.1))
  :config (progn
            ;; Colors
            (set-face-attribute 'tabbar-default nil
                                :background "gray20" :foreground
                                "gray60" :distant-foreground "gray50"
                                :box nil)
            (set-face-attribute 'tabbar-unselected nil
                                :background "gray80" :foreground "black" :box nil)
            (set-face-attribute 'tabbar-modified nil
                                :foreground "red4" :box nil
                                :inherit 'tabbar-unselected)
            (set-face-attribute 'tabbar-selected nil
                                :background "#405040" :foreground "white" :box nil)
            (set-face-attribute 'tabbar-selected-modified nil
                                :inherit 'tabbar-selected :foreground "GoldenRod2" :box nil)
            (set-face-attribute 'tabbar-button nil
                                :box nil)

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
                                      (lambda (a b) (string< (buffer-name (car a)) (buffer-name (car b))))))))))

            ;; Use Powerline to make tabs look nicer
            ;; (this needs to run *after* the colors are set)
            (use-package powerline)
            (defvar my/tabbar-height 20)
            (defvar my/tabbar-left (powerline-roundstub 'tabbar-default nil my/tabbar-height))
            (defvar my/tabbar-right (powerline-roundstub nil 'tabbar-default my/tabbar-height))
            (defun my/tabbar-tab-label-function (tab)
              (powerline-render (list my/tabbar-left
                                      (format " %s  " (car tab))
                                      my/tabbar-right)))
            (setq tabbar-tab-label-function #'my/tabbar-tab-label-function)

            ;; Tweaking the tabbar
            (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
              (setq ad-return-value
                    (if (and (buffer-modified-p (tabbar-tab-value tab))
                             (buffer-file-name (tabbar-tab-value tab)))
                        (concat "+" (concat ad-return-value ""))
                      (concat "" (concat ad-return-value "")))))

            ;; called each time the modification state of the buffer changed
            (defun my/modification-state-change ()
              (tabbar-set-template tabbar-current-tabset nil)
              (tabbar-display-update))

            ;; first-change-hook is called BEFORE the change is made
            (defun my/on-buffer-modification ()
              (set-buffer-modified-p t)
              (my/modification-state-change))

            ;; Assure switching tabs uses switch-to-buffer
            (defun switch-tabbar (num)
              (let* ((tabs (tabbar-tabs
                            (tabbar-current-tabset)))
                     (tab (nth
                           (if (> num 0) (- num 1) (+ (length tabs) num))
                           tabs)))
                (if tab (switch-to-buffer (car tab)))))

            ;; Enable tabbar
            (tabbar-mode t)))

;; Tabbar ruler pre-requisites
(use-package mode-icons
  :if (display-graphic-p)
  :demand t)

;; more tweaking to tabbar
(use-package tabbar-ruler
  :if (display-graphic-p)
  :after (powerline tabbar mode-icons projectile)
  :config (progn
            (setq tabbar-ruler-global-tabbar 't)
            (tabbar-install-faces)
            (setq tabbar-ruler-movement-timer-delay 1000000)

            ;; Fix for tabbar under Emacs 24.4
            ;; store tabbar-cache into a real hash,
            ;; rather than in frame parameters
            (defvar tabbar-caches (make-hash-table :test 'equal))

            (defun tabbar-create-or-get-tabbar-cache ()
              "Return a frame-local hash table that acts as a memoization
       cache for tabbar. Create one if the frame doesn't have one yet."
              (or (gethash (selected-frame) tabbar-caches)
                  (let ((frame-cache (make-hash-table :test 'equal)))
                    (puthash (selected-frame) frame-cache tabbar-caches)
                    frame-cache)))

            ;; explicitely delete some files from tabbar list
            (defun my/hated-buffers ()
              "List of buffers I never want to see, converted from names to buffers."
              (remove-if
               (lambda (buffer)
                 (not (char-or-string-p (buffer-name buffer))))
               (delete nil
                       (append
                        (mapcar 'get-buffer my/ignored-buffers)
                        (mapcar (lambda (this-buffer)
                                  (if (string-match "^ " (buffer-name this-buffer))
                                      this-buffer))
                                (buffer-list))))))

            ;; Add some regular expression from some buffers we do not want to see
            (setq tabbar-buffer-list-function
                  (lambda ()
                    (remove-if
                     (lambda (buffer)
                       (or
                        ;; Do not show these buffers in tabbar
                        (and (not (eq (current-buffer) buffer))
                             (or (cl-search "diff-hl"    (buffer-name buffer))
                                 (cl-search "tmp-status" (buffer-name buffer))
                                 (cl-search "magit"      (buffer-name buffer))))

                        ;; Always include the current buffer.
                        (and (not (eq (current-buffer) buffer))
                             ;; remove buffer name in this list.
                             (loop for name in (mapcar (lambda (x) (buffer-name x))
                                                       (my/hated-buffers))
                                   thereis (cl-search name (buffer-name buffer))))))
                     (buffer-list))))

            ;; Enable tabbar
            (tabbar-mode t)))

(provide 'setup-tabbar)
;;; setup-tabbar.el ends here

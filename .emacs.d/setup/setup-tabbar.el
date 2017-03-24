;;; setup-tabbar.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  Abelardo Jara-Berrocal

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

;; Tabbar mode
(use-package tabbar
  :defer 1
  :load-path (lambda () (expand-file-name "tabbar/" user-emacs-directory))
  :bind (("C-c <right>" . tabbar-forward)
     ("C-c <left>" . tabbar-backward)
     ("C-c <up>" . tabbar-backward-group)
     ("C-c <down>" . tabbar-forward-group))
  :config (progn
            (setq tabbar-use-images t)
            (setq tabbar-cycle-scope (quote tabs))
            (setq table-time-before-update 0.1)

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
            (add-hook 'after-save-hook 'my/modification-state-change)
            (add-hook 'first-change-hook 'my/on-buffer-modification)

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
  :defer t
  :load-path (lambda () (expand-file-name "mode-icons/" user-emacs-directory)))

;; more tweaking to tabbar
(use-package tabbar-ruler
  :if (display-graphic-p)
  :after (powerline tabbar mode-icons projectile)
  :load-path (lambda () (expand-file-name "tabbar-ruler/" user-emacs-directory))
  :init (setq tabbar-ruler-global-tabbar 't)
  :config (progn

            ;; https://github.com/mattfidler/tabbar-ruler.el/issues/10
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

            ;; necessary support function for buffer burial
            (defun my/delete-these (delete-these from-this-list)
              "Delete DELETE-THESE FROM-THIS-LIST."
              (cond
               ((car delete-these)
                (if (member (car delete-these) from-this-list)
                    (my/delete-these (cdr delete-these) (delete (car delete-these)
                                                                from-this-list))
                  (my/delete-these (cdr delete-these) from-this-list)))
               (t from-this-list)))

            ;; might as well use this for both
            (defun my/hated-buffers ()
              "List of buffers I never want to see, converted from names to buffers."
              (delete nil
                      (append
                       (mapcar 'get-buffer my/ignored-buffers)
                       (mapcar (lambda (this-buffer)
                                 (if (string-match "^ " (buffer-name this-buffer))
                                     this-buffer))
                               (buffer-list)))))

            ;; bury buffer function
            (defun my/bury-buffer (&optional n)
              (interactive)
              (unless n
                (setq n 1))
              (let ((my/buffer-list (my/delete-these (my/hated-buffers)
                                                     (buffer-list (selected-frame)))))
                (switch-to-buffer
                 (if (< n 0)
                     (nth (+ (length my/buffer-list) n)
                          my/buffer-list)
                   (bury-buffer)
                   (nth n my/buffer-list)))))

            ;; Enable tabbar
            (tabbar-mode t)))

(provide 'setup-tabbar)
;;; setup-tabbar.el ends here

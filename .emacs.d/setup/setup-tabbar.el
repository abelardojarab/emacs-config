;;; setup-tabbar.el ---

;; Copyright (C) 2016  Abelardo Jara

;; Author: Abelardo Jara <abelardojara@Abelardos-MacBook-Pro.local>
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
  :load-path (lambda () (expand-file-name "tabbar/" user-emacs-directory))
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
            (defun ztl-modification-state-change ()
              (tabbar-set-template tabbar-current-tabset nil)
              (tabbar-display-update))

            ;; first-change-hook is called BEFORE the change is made
            (defun ztl-on-buffer-modification ()
              (set-buffer-modified-p t)
              (ztl-modification-state-change))
            (add-hook 'after-save-hook 'ztl-modification-state-change)
            (add-hook 'first-change-hook 'ztl-on-buffer-modification)

            ;; Assure switch tabbar uses switch-to-buffer
            (defun switch-tabbar (num)
              (let* ((tabs (tabbar-tabs
                            (tabbar-current-tabset)))
                     (tab (nth
                           (if (> num 0) (- num 1) (+ (length tabs) num))
                           tabs)))
                (if tab (switch-to-buffer (car tab)))))

            ;; Default tabbar theme-ing
            (require 'color)
            (defun my/set-face-tabbar()
              "Set the tabbar background to the same color as the regular background."
              (interactive)
              (setq tabbar-separator '(0.0))
              (setq my/tabbar-foreground-color
                    (face-foreground 'default))
              (setq my/tabbar-background-color
                    (face-background 'default))
              (setq my/tabbar-back-color
                    (color-lighten-name (face-background 'default) 12))
              (custom-set-faces
               ;; tabbar background
               `(tabbar-default ((t (:inherit fixed-pitch :background ,my/tabbar-back-color :foreground ,my/tabbar-foreground-color))))
               `(tabbar-button ((t (:inherit tabbar-default :foreground ,my/tabbar-background-color))))
               `(tabbar-button-highlight ((t (:inherit tabbar-default))))
               `(tabbar-highlight ((t (:underline t))))
               ;; selected tab
               `(tabbar-selected ((t (:inherit tabbar-default :background ,my/tabbar-background-color))))
               `(tabbar-separator ((t (:inherit tabbar-default :background ,my/tabbar-back-color))))
               `(tabbar-unselected ((t (:inherit tabbar-default))))))

            (add-hook 'after-init-hook #'my/set-face-tabbar)
            (add-hook 'window-configuration-change-hook #'my/set-face-tabbar)

            ;; Refresh the tabbar colorset after loading a theme
            (defadvice load-theme (after enable-theme-second activate)
              (my/set-face-tabbar))
            (ad-activate 'load-theme)))

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
  :init (setq tabbar-ruler-global-tabbar 't) ;; If you want tabbar
  :config (progn
            (global-set-key (kbd "C-c C-t") 'tabbar-ruler-move)

            ;; https://github.com/mattfidler/tabbar-ruler.el/issues/10
            (setq tabbar-ruler-movement-timer-delay 1000000)

            ;; Group user buffers
            (tabbar-ruler-group-by-projectile-project)

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
            (defun crs-delete-these (delete-these from-this-list)
              "Delete DELETE-THESE FROM-THIS-LIST."
              (cond
               ((car delete-these)
                (if (member (car delete-these) from-this-list)
                    (crs-delete-these (cdr delete-these) (delete (car delete-these)
                                                                 from-this-list))
                  (crs-delete-these (cdr delete-these) from-this-list)))
               (t from-this-list)))

            ;; this is the list of avoided buffers
            (defvar crs-hated-buffers
              '("*Compile-Log*"))
            (add-to-list 'crs-hated-buffers "^ ")
            (add-to-list 'crs-hated-buffers "*Messages*")
            (add-to-list 'crs-hated-buffers "*Completions*")
            (add-to-list 'crs-hated-buffers "*scratch*")
            (add-to-list 'crs-hated-buffers "*Python*")
            (add-to-list 'crs-hated-buffers "*GNU Emacs*")
            (add-to-list 'crs-hated-buffers "*compilation*")
            (add-to-list 'crs-hated-buffers "*cmake*")
            (add-to-list 'crs-hated-buffers "*etags tmp*")
            (add-to-list 'crs-hated-buffers "TAGS")
            (add-to-list 'crs-hated-buffers "*ECB")
            (add-to-list 'crs-hated-buffers "*Buffer")

            ;; might as well use this for both
            (defun crs-hated-buffers ()
              "List of buffers I never want to see, converted from names to buffers."
              (delete nil
                      (append
                       (mapcar 'get-buffer crs-hated-buffers)
                       (mapcar (lambda (this-buffer)
                                 (if (string-match "^ " (buffer-name this-buffer))
                                     this-buffer))
                               (buffer-list)))))

            ;; bury buffer function
            (defun crs-bury-buffer (&optional n)
              (interactive)
              (unless n
                (setq n 1))
              (let ((my-buffer-list (crs-delete-these (crs-hated-buffers)
                                                      (buffer-list (selected-frame)))))
                (switch-to-buffer
                 (if (< n 0)
                     (nth (+ (length my-buffer-list) n)
                          my-buffer-list)
                   (bury-buffer)
                   (nth n my-buffer-list)))))

            ;; Enable tab-bar
            (tabbar-mode t)))

(provide 'setup-tabbar)
;;; setup-tabbar.el ends here

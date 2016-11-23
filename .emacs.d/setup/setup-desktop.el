;;; setup-desktop.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

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

;; Assure .emacs.cache/backup directory exists
(if (not (file-exists-p "~/.emacs.cache/backups"))
    (make-directory "~/.emacs.cache/backups") t)

;; Backups
(setq make-backup-files nil
      backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.cache/backups"))
      version-control t
      kept-new-versions 2
      kept-old-versions 1
      delete-old-versions t)

;; Preserve the owner and group of the file you're editing
(setq backup-by-copying-when-mismatch t)

;; Autosave
(setq auto-save-default nil)
(setq auto-save-interval 500)
(defvar my-auto-save-folder "~/.emacs.cache/auto-save/") ;; folder for auto-saves
(setq auto-save-list-file-prefix "~/.emacs.cache/auto-save/.saves-") ;; set prefix for auto-saves
(setq auto-save-file-name-transforms `((".*" ,my-auto-save-folder t))) ;; location for all auto-save files
(make-directory my-auto-save-folder t)

;; Savehist: save some history
(use-package savehist
  :init (setq savehist-additional-variables '(search ring regexp-search-ring)
              savehist-autosave-interval 120
              savehist-file "~/.emacs.cache/savehist")
  :config (savehist-mode t))

;; filecache: http://www.emacswiki.org/cgi-bin/wiki/FileNameCache
(use-package file-cache
  :disabled t
  :config (progn
            (message "Loading file cache...")
            (file-cache-add-directory "~/")
            (file-cache-add-directory-list '("~/Desktop" "~/Documents" "~/workspace"))))

;; Remember the position where we closed a file
(use-package saveplace
  :init (progn
          (setq save-place-file "~/.emacs.cache/emacs.saveplace")
          (setq-default save-place t)))

;; Automatically save and restore sessions
(use-package desktop
  :init (progn
          ;; Save desktops a minute after Emacs was idle.
          (setq-default desktop-missing-file-warning nil)
          (setq desktop-dirname             "~/.emacs.cache/"
                desktop-base-file-name      "emacs.desktop"
                desktop-base-lock-name      "lock"
                desktop-path                (list desktop-dirname)
                desktop-save                t
                desktop-load-locked-desktop t
                desktop-save 'ask-if-new
                desktop-file-name-format 'absolute
                desktop-restore-frames nil
                desktop-restore-in-current-display t
                desktop-restore-forces-onscreen nil
                desktop-restore-eager 0
                desktop-auto-save-timeout 60
                desktop-globals-to-save
                '((extended-command-history . 30)
                  (file-name-history        . 100)
                  (grep-history             . 30)
                  (compile-history          . 30)
                  (minibuffer-history       . 50)
                  (query-replace-history    . 60)
                  (read-expression-history  . 60)
                  (regexp-history           . 60)
                  (regexp-search-ring       . 20)
                  (search-ring              . 20)
                  (shell-command-history    . 50)
                  tags-file-name
                  register-alist)
                desktop-locals-to-save
                (nconc '(word-wrap line-move-visual) desktop-locals-to-save)))
  :config (progn
            ;; Don't save Magit and Git related buffers
            (dolist (mode '(magit-mode magit-log-mode dired-mode Info-mode fundamental-mode DocView-mode))
              (add-to-list 'desktop-modes-not-to-save mode))
            ;; (add-to-list 'desktop-files-not-to-save (rx bos "COMMIT_EDITMSG"))

            ;; buffer-display-time is changed when desktop is loaded
            (add-to-list 'desktop-locals-to-save 'buffer-display-time-1)
            (make-variable-buffer-local 'buffer-display-time-1)
            (defun save-buffer-display-time ()
              (mapc (lambda (buf)
                      (with-current-buffer buf
                        (setq buffer-display-time-1
                              (or buffer-display-time (current-time)))))
                    (buffer-list)))
            (add-hook 'desktop-save-hook 'save-buffer-display-time)

            (defun set-buffer-display-time ()
              (mapc (lambda (buf)
                      (with-current-buffer buf
                        (setq buffer-display-time buffer-display-time-1)))
                    (buffer-list)))
            (add-hook 'desktop-after-read-hook 'set-buffer-display-time)
            (desktop-save-mode)))

(provide 'setup-desktop)
;;; setup-desktop.el ends here

;;; setup-dired.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Abelardo Jara-Berrocal

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

(use-package dired-x
  :defer t
  :commands (dired dired-jump)
  :bind (("C-x C-j"  . dired-jump)
         :map dired-mode-map
         (("u"       . dired-up-directory)
          ("RET"     . dired-find-alternate-file)
          ("C-c C-r" . my/dired-rsync)
          ("C-c C-d" . dired-filter-by-directory)
          ("C-c C-f" . dired-filter-by-file)))
  :config (progn

            ;; extra hooks
            (defun my/dired-mode-hook ()
              (setq-local truncate-lines t)
              (dired-omit-mode 1)
              (dired-hide-details-mode t)
              (hl-line-mode 1)
              (projectile-mode 1))
            (add-hook 'dired-mode-hook #'my/dired-mode-hook)

            ;; https://fuco1.github.io/2017-05-01-Support-for-imenu-in-dired.html
            (defun my/dired-imenu-prev-index-position (&optional arg)
              "Go to the header line of previous directory."
              (interactive "p")
              (unless (= (line-number-at-pos) 1)
                (call-interactively 'dired-prev-subdir)
                t))

            (defun my/dired-extract-index-name ()
              "Extract name of the current item for imenu."
              (save-excursion
                (back-to-indentation)
                (buffer-substring-no-properties
                 (point)
                 (1- (re-search-forward ":$")))))

            (defun my/dired-imenu-create-index ()
              "Create `imenu' index for dired."
              (let* ((alist (imenu-default-create-index-function))
                     (uniquified (f-uniquify-alist (-map 'car alist))))
                (--remove
                 (= 0 (length (car it)))
                 (--map (cons (cdr (assoc (car it) uniquified)) (cdr it))
                        alist))))

            (defun my/dired-imenu-init ()
              "Initialize `imenu' variables in current buffer."
              (setq-local imenu-prev-index-position-function
                          'my/dired-imenu-prev-index-position)
              (setq-local imenu-extract-index-name-function
                          'my/dired-extract-index-name)
              (setq-local imenu-create-index-function
                          'my/dired-imenu-create-index))
            (add-hook 'dired-mode-hook #'my/dired-imenu-init)

            ;; Emacs 24.4 defaults to an ls -1 view, not ls -l, but I want
            ;; details. This needs to be specified before requiring dired+
            (setq diredp-hide-details-initially-flag nil)

            ;; "In Dired, visit this file or directory instead of the Dired buffer."
            ;; Prevents buffers littering up things when moving around in Dired
            (put 'dired-find-alternate-file 'disabled nil)

            ;; toggle `dired-omit-mode' with C-x M-o
            (setq dired-omit-verbose nil)
            (setq-default dired-omit-mode t)
            (add-hook 'dired-mode-hook #'dired-omit-mode)
            (add-to-list 'dired-omit-extensions ".DS_Store")
            (setq dired-omit-files
                  (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$"))

            (setq ls-lisp-dirs-first                  t
                  dired-listing-switches              "-alhF --group-directories-first"
                  dired-recursive-copies              'top
                  dired-recursive-deletes             'top
                  dired-dwim-target                   t
                  ;; -F marks links with @
                  dired-ls-F-marks-symlinks           t
                  ;; Auto refresh dired
                  dired-auto-revert-buffer            t
                  ;; Auto refresh dired, but be quiet about it
                  global-auto-revert-non-file-buffers t)

            ;; rsync helper
            (defun my/dired-rsync (dest)
              (interactive
               (list
                (expand-file-name
                 (read-file-name
                  "Rsync to:"
                  (dired-dwim-target-directory)))))
              ;; store all selected files into "files" list
              (let ((files (dired-get-marked-files
                            nil current-prefix-arg))
                    ;; the rsync command
                    (my/rsync-command
                     "rsync -arvz --progress "))
                ;; add all selected file names as arguments
                ;; to the rsync command
                (dolist (file files)
                  (setq my/rsync-command
                        (concat my/rsync-command
                                (shell-quote-argument file)
                                " ")))
                ;; append the destination
                (setq my/rsync-command
                      (concat my/rsync-command
                              (shell-quote-argument dest)))
                ;; run the async shell command
                (async-shell-command my/rsync-command "*rsync*")
                ;; finally, switch to that window
                (other-window 1)))

            ;; dired-ranger pre-requisite
            (use-package dired-hacks-utils
              :demand t
              :after dired
              :load-path (lambda () (expand-file-name "dired-hacks-utils/" user-emacs-directory)))

            ;; Enable copying and pasting files
            (use-package dired-ranger
              :after (dired dired-hacks-utils)
              :load-path (lambda () (expand-file-name "dired-ranger/" user-emacs-directory))
              :bind (:map dired-mode-map
                          ("W" . dired-ranger-copy)
                          ("X" . dired-ranger-move)
                          ("Y" . dired-ranger-paste)))

            ;; Highlight dired buffer with K-shell coloring
            (use-package dired-k
              :after dired
              :load-path (lambda () (expand-file-name "dired-k/" user-emacs-directory))
              :bind (:map dired-mode-map
                          ("K" . dired-k))
              :commands (dired-k dired-k-no-revert)
              :init (progn
                      (add-hook 'dired-initial-position-hook #'dired-k)
                      (add-hook 'dired-after-readin-hook     #'dired-k-no-revert)))

            ;; Display file icons in dired
            (use-package dired-icon
              :after dired
              :load-path (lambda () (expand-file-name "dired-icon/" user-emacs-directory))
              :if (display-graphic-p)
              :commands dired-icon-mode
              :init (add-hook 'dired-mode-hook #'dired-icon-mode))

            ;; Facility to see images inside dired
            (use-package image-dired
              :disabled t ;; caused issue with emacs server
              :after dired
              :config (progn
                        (setq image-dired-cmd-create-thumbnail-options
                              (replace-regexp-in-string "-strip" "-auto-orient -strip" image-dired-cmd-create-thumbnail-options)
                              image-dired-cmd-create-temp-image-options
                              (replace-regexp-in-string "-strip" "-auto-orient -strip" image-dired-cmd-create-temp-image-options))))

            ;; Preview files in dired
            (use-package peep-dired
              :after dired
              :load-path (lambda () (expand-file-name "peep-dired/" user-emacs-directory))
              :bind (:map dired-mode-map
                          ("P" . peep-dired)))

            ;; All the icons on dired
            (use-package all-the-icons-dired
              :if (display-graphic-p)
              :after (dired all-the-icons)
              :diminish all-the-icons-dired-mode
              :load-path (lambda () (expand-file-name "all-the-icons-dired/" user-emacs-directory))
              :commands all-the-icons-dired-mode
              :init (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

            ;; async support for dired
            (use-package dired-async
              :commands dired-async-mode
              :load-path (lambda () (expand-file-name "async/" user-emacs-directory))
              :init (add-hook 'dired-mode-hook (lambda () (dired-async-mode 1))))

            ;; Simple directory explorer. It also works as a generic tree explore library
            (use-package direx
              :demand t
              :after dired
              :load-path (lambda () (expand-file-name "direx/" user-emacs-directory))
              :bind (:map dired-mode-map
                          ("b"       . direx:jump-to-directory)
                          :map direx:direx-mode-map
                          ("b"       . dired-jump)
                          ([mouse-1] . direx:mouse-2)
                          ([mouse-3] . direx:mouse-1))
              :config (setq direx:closed-icon "+ "
                            direx:leaf-icon   "| "
                            direx:open-icon   "> "))

            ;; Integration with projectile
            (use-package direx-project
              :demand t
              :after (direx projectile)
              :load-path (lambda () (expand-file-name "direx/" user-emacs-directory)))

            ;; Choose starting dired directory
            (use-package helm-dired-history
              :after (dired savehist helm)
              :bind (:map dired-mode-map
                          ("," . helm-dired-history-view))
              :load-path (lambda () (expand-file-name "helm-dired-history/" user-emacs-directory))
              :config (progn
                        (savehist-mode 1)
                        (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)))))

(provide 'setup-dired)
;;; setup-dired.el ends here

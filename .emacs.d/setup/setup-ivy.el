;;; setup-ivy.el ---                              -*- lexical-binding: t; -*-

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

(use-package swiper
  :defer t
  :commands (swiper
             swiper-all
             ivy-mode
             ivy-read
             ivy-completing-read
             ivy-resume
             ivy-switch-buffer
             ivy-switch-buffer-other-window)
  :load-path (lambda () (expand-file-name "swiper/" user-emacs-directory))
  :bind (("C-c C-r" . ivy-resume)
         :map ctl-x-map
         ("s"       . swiper))
  :config (progn

            (setq ivy-do-completion-in-region      nil
                  ivy-wrap                         t
                  ivy-fixed-height-minibuffer      t
                  ivy-height                       20
                  ivy-virtual-abbreviate           'full
                  ivy-initial-inputs-alist         nil
                  ivy-count-format                 "[%d/%d]"
                  ivy-display-style                'fancy
                  ivy-format-function              'ivy-format-function-arrow
                  ivy-use-virtual-buffers          t
                  ;; disable magic slash on non-match
                  ivy-magic-slash-non-match-action nil)

            (push #'+ivy-yas-prompt yas-prompt-functions)
            (setq completion-in-region-function #'ivy-completion-in-region)

            (ivy-set-actions
             'ivy-switch-buffer
             '(("k"
                (lambda (x)
                  (kill-buffer x)
                  (ivy--reset-state ivy-last))
                "kill")
               ("j"
                ivy--switch-buffer-other-window-action
                "other window")))

            ;; advise swiper to recenter on exit
            (defun my/swiper-recenter (&rest args)
              "recenter display after swiper"
              (recenter))
            (advice-add 'swiper :after #'my/swiper-recenter)))

(use-package counsel
  :defer t
  :load-path (lambda () (expand-file-name "swiper/" user-emacs-directory))
  :bind (("M-x"                     . counsel-M-x)
         ("M-y"                     . counsel-yank-pop)
         ("C-o"                     . counsel-find-file)
         ("C-c C-v"                 . counsel-yank-pop)
         ("C-c C-a"                 . counsel-git-grep)
         ("C-c C-g"                 . counsel-git-checkout)
         ([remap bookmark-jump]     . counsel-bookmark) ;; Jump to book or set it if it doesn't exist, C-x r b
         ([remap bookmark-set]      . counsel-bookmark)  ;; C-x r m
         ([remap find-file]         . counsel-find-file)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-function] . counsel-describe-function)
         :map ctl-x-map
         ("x"                       . counsel-M-x)
         :map ivy-minibuffer-map
         ("M-y"                     . ivy-next-line)
         :map read-expression-map
         ("C-r"                     . counsel-expression-history))
  :config (progn

            (defadvice counsel-find-file (after find-file-sudo activate)
              "Find file as root if necessary."
              (unless (and buffer-file-name
                           (file-writable-p buffer-file-name))

                (let* ((buffer-file (buffer-file-name))
                       (coincidence (string-match-p "@" buffer-file))
                       (hostname)
                       (buffer-name))
                  (if coincidence
                      (progn
                        (setq hostname (substring buffer-file (+ coincidence 1)
                                                  (string-match-p ":" buffer-file      (+ coincidence 1))))
                        (setq buffer-name
                              (concat
                               (substring buffer-file 0 coincidence) "@"
                               (replace-regexp-in-string ":" (concat "|sudo:" hostname ":")
                                                         buffer-file nil nil nil (+ coincidence 1))))
                        (find-alternate-file buffer-name))
                    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file))))))

            (defadvice find-file (after find-file-sudo activate)
              "Find file as root if necessary."
              (unless (and buffer-file-name
                           (file-writable-p buffer-file-name))
                (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))))

(use-package swiper-helm
  :defer t
  :after (swiper helm)
  :commands swiper-helm
  :load-path (lambda () (expand-file-name "swiper-helm/" user-emacs-directory)))

(use-package ivy-rich
  :demand t
  :after swiper
  :commands ivy-switch-buffer
  :load-path (lambda () (expand-file-name "ivy-rich/" user-emacs-directory))
  :config (progn
            (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)

            ;; Do not align the virtual buffers, breaks ivy-rich
            (setq ivy-rich-switch-buffer-align-virtual-buffer nil)))

(use-package all-the-icons-ivy
  :demand t
  :if (display-grayscale-p)
  :after swiper
  :load-path (lambda () (expand-file-name "all-the-icons-ivy/" user-emacs-directory))
  :config (all-the-icons-ivy-setup))

(provide 'setup-ivy)
;;; setup-swiper.el ends here

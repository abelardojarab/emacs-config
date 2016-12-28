;;; setup-swiper.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

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
         ("s" . swiper))
  :config (progn
            (set-variable 'ivy-on-del-error-function '(lambda()))
            (setq ivy-display-style 'fancy)
            (setq ivy-use-virtual-buffers t)
            (setq ivy-height 20)

            ;; advise swiper to recenter on exit
            (defun my/swiper-recenter (&rest args)
              "recenter display after swiper"
              (recenter))
            (advice-add 'swiper :after #'my/swiper-recenter)))

(use-package swiper-helm
  :defer t
  :after (swiper helm)
  :commands swiper-helm
  :load-path (lambda () (expand-file-name "swiper-helm/" user-emacs-directory)))

(use-package ivy-rich
  :after swiper
  :commands ivy-switch-buffer
  :bind (("C-b" . ivy-switch-buffer)
         :map ctl-x-map
         ("b" . ivy-switch-buffer))
  :load-path (lambda () (expand-file-name "ivy-rich/" user-emacs-directory))
  :config (progn
            (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)

            ;; Do not align the virtual buffers, breaks ivy-rich
            (setq ivy-rich-switch-buffer-align-virtual-buffer nil)))

(provide 'setup-swiper)
;;; setup-swiper.el ends here

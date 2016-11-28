;;; setup-org-image.el ---                           -*- lexical-binding: t; -*-

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

;; Manipulate images using ImageMagick
(use-package image+
  :if (and (executable-find "convert")
           (display-graphic-p))
  :load-path (lambda () (expand-file-name "image+/" user-emacs-directory))
  :init (require 'image)
  :config (progn
            (imagex-global-sticky-mode 1)
            (imagex-auto-adjust-mode 1)
            (setq imagex-quiet-error t)
            ;; Insert images from files like this:
            ;; #+BEGIN: image :file "~/Documents/personal/foo.png"
            ;; #+END
            (defun org-dblock-write:image (params)
              (let ((file (plist-get params :file)))
                (clear-image-cache file)
                (insert-image (create-image file))))))

;; Integrating graphics with text inside Emacs
(use-package iimage
  :if (display-graphic-p)
  :commands (iimage-mode org-turn-on-iimage org-insert-screenshot org-toggle-iimage org-reload-image-at-point org-resize-image-at-point)
  :config (progn

            ;; https://www.reddit.com/r/emacs/comments/55zk2d/adjust_the_size_of_pictures_to_be_shown_inside/
            (setq org-image-actual-width (/ (display-pixel-width) 3))

            (add-to-list 'iimage-mode-image-regex-alist '("@startuml\s+\\(.+\\)" . 1))
            (add-to-list 'iimage-mode-image-regex-alist (cons (concat "\[\[file:\(~?" iimage-mode-image-filename-regex "\)\]") 1))

            ;; Function to setup images for display on load
            (defun org-turn-on-iimage ()
              "display images in your org file"
              (interactive)
              (turn-on-iimage-mode)
              (set-face-underline-p 'org-link t)) ;; start with hidden images
            ;; (add-hook 'org-mode-hook '(lambda () (org-turn-on-iimage)))

            ;; Function to toggle images in a org buffer
            (defun org-toggle-iimage ()
              "display images in your org file"
              (interactive)
              (if (face-underline-p 'org-link)
                  (set-face-underline-p 'org-link nil)
                (set-face-underline-p 'org-link t))
              (call-interactively 'iimage-mode))

            ;; Image reloading
            (defun org-reload-image-at-point ()
              (interactive)
              (message "reloading image at point in the current buffer...")
              (image-refresh (get-text-property (point) 'display)))

            ;; Image resizing and reloading
            (defun org-resize-image-at-point ()
              (interactive)
              (message "resizing image at point in the current buffer...")
              (let* ((image-spec (get-text-property (point) 'display))
                     (file (cadr (member :file image-spec))))
                (message (concat "resizing image..." file))
                (shell-command (format "convert -resize %d %s %s "
                                       (* (window-width (selected-window)) (frame-char-width))
                                       file file))
                (org-reload-image-at-point)))))

(provide 'setup-org-image)
;;; setup-org-image.el ends here

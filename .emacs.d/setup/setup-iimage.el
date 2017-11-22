;;; setup-iimage.el ---                           -*- lexical-binding: t; -*-

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

;; Manipulate images using ImageMagick
(use-package image+
  :defer t
  :if (and (executable-find "convert")
           (display-graphic-p))
  :load-path (lambda () (expand-file-name "image+/" user-emacs-directory))
  :init (use-package image)
  :commands (imagex-global-sticky-mode
             imagex-auto-adjust-mode)
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
  :defer t
  :if (and (executable-find "convert")
           (display-graphic-p))
  :after (org markdown-mode)
  :commands (iimage-mode
             org-turn-on-iimage
             org-insert-screenshot
             org-toggle-iimage
             my/reload-image-at-point
             my/resize-image-at-point)
  :config (progn

            ;; Enable Imagemagick types
            (setq imagemagick-enabled-types t)

            ;; https://www.reddit.com/r/emacs/comments/55zk2d/adjust_the_size_of_pictures_to_be_shown_inside/
            (setq org-image-actual-width (/ (display-pixel-width) 3))

            (add-to-list 'iimage-mode-image-regex-alist '("@startuml\s+\\(.+\\)" . 1))
            (add-to-list 'iimage-mode-image-regex-alist (cons (concat "\[\[file:\(~?" iimage-mode-image-filename-regex "\)\]") 1))

            ;; Function to setup images for display on load
            (defun org-turn-on-iimage ()
              "display images in your org file"
              (interactive)
              (turn-on-iimage-mode)
              (set-face-underline-p 'org-link t))

            ;; Function to toggle images in a org buffer
            (defun org-toggle-iimage ()
              "display images in your org file"
              (interactive)
              (if (face-underline-p 'org-link)
                  (set-face-underline-p 'org-link nil)
                (set-face-underline-p 'org-link t))
              (call-interactively 'iimage-mode))

            ;; Insert screenshots into Org mode, very useful
            (defun org-insert-screenshot ()
              "Take a screenshot into a time stamped unique-named file in the same
directory as the org-buffer and insert
a link to this file."
              (interactive)
              (let ((case-fold-search nil))
                (setq tilde-buffer-filename
                      (replace-regexp-in-string "/" "\\" (buffer-file-name) t t))
                (setq tilde-buffer-filename
                      (replace-regexp-in-string ".org" "" tilde-buffer-filename t t))
                (setq filename
                      (concat
                       (make-temp-name
                        (concat tilde-buffer-filename
                                "_"
                                (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
                (setq filename (file-relative-name filename (file-name-directory (buffer-file-name))))
                (setq filename (replace-regexp-in-string "\\\\" "/" filename))
                (if (equal system-type 'windows-nt)
                    ;; Windows: Irfanview
                    (call-process "C:\\Program Files (x86)\\IrfanView\\i_view32.exe" nil nil nil (concat
                                                                                                  "/clippaste /convert=" filename))

                  (if (equal system-type 'darwin)
                      ;; Mac OSX pngpaste utility: https://github.com/jcsalterego/pngpaste
                      (call-process "pngpaste" nil nil nil filename)

                    ;; Linux: ImageMagick: (call-process "import" nil nil nil filename)
                    (call-process "import" nil nil nil filename))
                  ) ;; if
                (insert (concat "[[file:" filename "]]"))
                (org-display-inline-images)))

            ;; Image reloading
            (defun my/reload-image-at-point ()
              (interactive)
              (message "reloading image at point in the current buffer...")
              (image-refresh (get-text-property (point) 'display)))

            ;; Image resizing and reloading
            (defun my/resize-image-at-point ()
              (interactive)
              (message "Resizing image at point in the current buffer...")
              (let* ((image-spec (get-text-property (point) 'display))
                     (file (cadr (member :file image-spec))))
                (message (concat "Resizing image..." file))
                (shell-command (format "convert -resize %d %s %s "
                                       (* (/ (window-width (selected-window)) 3) (frame-char-width))
                                       file file))
                (my/reload-image-at-point)))))

(provide 'setup-iimage)
;;; setup-org-image.el ends here

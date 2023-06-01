;;; setup-iimage.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023  Abelardo Jara-Berrocal

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

;; Display tags everywere as fancy svg icons.
(use-package svg-tag-mode
  :if (display-graphic-p)
  :init (define-globalized-minor-mode global-svg-tag-mode svg-tag-mode
          (lambda () (svg-tag-mode 1)))
  :commands svg-tag-mode
  :hook (org-mode . svg-tag-mode)
  :config (progn
            (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
            (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
            (defconst day-re "[A-Za-z]\\{3\\}")
            (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))
            (defun svg-progress-percent (value)
              (svg-image (svg-lib-concat
                          (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                                nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                          (svg-lib-tag (concat value "%")
                                       nil :stroke 0 :margin 0)) :ascent 'center))

            (defun svg-progress-count (value)
              (let* ((seq (mapcar #'string-to-number (split-string value "/")))
                     (count (float (car seq)))
                     (total (float (cadr seq))))
                (svg-image (svg-lib-concat
                            (svg-lib-progress-bar (/ count total) nil
                                                  :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                            (svg-lib-tag value nil
                                         :stroke 0 :margin 0)) :ascent 'center)))

            (setq svg-tag-tags
                  `(
                    ;; Org tags
                    (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
                    (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))
                    ;; Task priority
                    ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                          (svg-tag-make tag :face 'org-priority
                                                        :beg 2 :end -1 :margin 0))))

                    ;; Progress
                    ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                                        (svg-progress-percent (substring tag 1 -2)))))
                    ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                                      (svg-progress-count (substring tag 1 -1)))))

                    ;; TODO / DONE
                    ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
                    ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


                    ;; Citation of the form [cite:@Knuth:1984]
                    ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                                      (svg-tag-make tag
                                                                    :inverse t
                                                                    :beg 7 :end -1
                                                                    :crop-right t))))
                    ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                               (svg-tag-make tag
                                                                             :end -1
                                                                             :crop-left t))))


                    ;; Active date (with or without day name, with or without time)
                    (,(format "\\(<%s>\\)" date-re) .
                     ((lambda (tag)
                        (svg-tag-make tag :beg 1 :end -1 :margin 0))))
                    (,(format "\\(<%s \\)%s>" date-re day-time-re) .
                     ((lambda (tag)
                        (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
                    (,(format "<%s \\(%s>\\)" date-re day-time-re) .
                     ((lambda (tag)
                        (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

                    ;; Inactive date  (with or without day name, with or without time)
                    (,(format "\\(\\[%s\\]\\)" date-re) .
                     ((lambda (tag)
                        (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
                    (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
                     ((lambda (tag)
                        (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
                    (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
                     ((lambda (tag)
                        (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))))

;; Manipulate images using ImageMagick
(use-package image+
  :defer t
  :if (and (executable-find "convert")
           (display-graphic-p))
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
  :custom ((imagemagick-enabled-types t))
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

;;; setup-ediff.el ---                               -*- lexical-binding: t; -*-

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

;; ediff
(use-package ediff
  :defer t
  :commands (ediff-current-file
             my/ediff-dwim
             my/update-modified-flag)
  :config (progn

            (defun my/update-modified-flag ()
              "Update the buffer modified flag."
              (interactive)
              (let* ((buffer (current-buffer))
                     (basefile
                      (or (buffer-file-name buffer)
                          (error "Buffer %s has no associated file" buffer)))
                     (tempfile (make-temp-file "buffer-content-")))
                (with-current-buffer buffer
                  (save-restriction
                    (widen)
                    (write-region (point-min) (point-max) tempfile nil 'silent)))
                (if (= (call-process "diff" nil nil nil basefile tempfile) 0)
                    (progn
                      (set-buffer-modified-p nil)
                      (message "Buffer matches file"))
                  (message "Buffer doesn't match file"))
                (delete-file tempfile)))

            (defun my/setup-ediff ()
              (interactive)
              (ediff-setup-keymap)
              (define-key ediff-mode-map (kbd "<down>") #'ediff-next-difference)
              (define-key ediff-mode-map (kbd "<up>")   #'ediff-previous-difference))
            (add-hook 'ediff-mode-hook #'my/setup-ediff))
  :config (progn
            (setq ediff-window-setup-function 'ediff-setup-windows-plain

                  ;; Always split nicely for wide screens
                  ediff-split-window-function 'split-window-horizontally

                  ;; Ignore whitespace
                  ediff-diff-options          "-w")

            ;; taken from http://kaushalmodi.github.io/2015/03/09/do-ediff-as-i-mean/
            (defun my/ediff-dwim ()
              "Do ediff as I mean.
If a region is active when command is called, call `ediff-regions-wordwise'.
Else if the current frame has 2 windows,
- Do `ediff-files' if the buffers are associated to files and the buffers
  have not been modified.
- Do `ediff-buffers' otherwise.
Otherwise call `ediff-buffers' interactively."
              (interactive)
              (if (region-active-p)
                  (call-interactively 'ediff-regions-wordwise)
                (if (= 2 (safe-length (window-list)))
                    (let (bufa bufb filea fileb)
                      (setq bufa  (get-buffer (buffer-name)))
                      (setq filea (buffer-file-name bufa))
                      (save-excursion
                        (other-window 1)
                        (setq bufb (get-buffer (buffer-name))))
                      (setq fileb (buffer-file-name bufb))
                      (if (or
                           ;; if either of the buffers is not associated to a file
                           (null filea) (null fileb)
                           ;; if either of the buffers is modified
                           (buffer-modified-p bufa) (buffer-modified-p bufb))
                          (progn
                            (message "Running (ediff-buffers \"%s\" \"%s\") .." bufa bufb)
                            (ediff-buffers bufa bufb))
                        (progn
                          (message "Running (ediff-files \"%s\" \"%s\") .." filea fileb)
                          (ediff-files filea fileb))))
                  (call-interactively 'ediff-buffers))))))

(provide 'setup-ediff)
;;; setup-ediff.el ends here

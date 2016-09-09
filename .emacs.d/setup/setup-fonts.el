;;; setup-fonts.el ---                               -*- lexical-binding: t; -*-

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

;; Adjust font when using graphical interface
(when (display-graphic-p)

  ;; Use 12-pt Consolas as default font
  (when (find-font (font-spec :name "Consolas"))
    (setq main-programming-font "Consolas-12")
    (set-face-attribute 'default nil :font main-programming-font)
    (set-face-attribute 'fixed-pitch nil :font main-programming-font)
    (add-to-list 'default-frame-alist '(font . "Consolas-12"))) ;; default font, used by speedbar

  (when (find-font (font-spec :name "Calibri"))
    (setq main-writing-font "Calibri-13")
    (set-face-attribute 'variable-pitch nil :font main-writing-font :weight 'normal)
    (add-hook 'text-mode-hook 'variable-pitch-mode))

  ;; Dynamic font adjusting based on monitor resolution, using Android fonts
  (when (find-font (font-spec :name "Roboto Mono"))

    (defun fontify-frame (&optional frame)
      (interactive)
      (let (main-writing-font main-programming-font)
        (setq main-programming-font "Roboto Mono")
        (if (find-font (font-spec :name "Menlo"))
            (setq main-writing-font "Menlo")
          (setq main-writing-font main-programming-font))

        ;; Adjust text size based on resolution
        (case system-type
          ('windows-nt
           (if (> (x-display-pixel-width) 2000)
               (progn ;; HD monitor in Windows
                 (setq main-programming-font (concat main-programming-font "-11"))
                 (setq main-writing-font (concat main-writing-font "-13")))
             (progn
               (setq main-programming-font (concat main-programming-font "-11"))
               (setq main-writing-font (concat main-writing-font "-13")))))
          ('darwin
           (if (> (x-display-pixel-width) 1800)
               (if (> (x-display-pixel-width) 2000)
                   (progn ;; Ultra-HD monitor in OSX
                     (setq main-programming-font (concat main-programming-font "-19"))
                     (setq main-writing-font (concat main-writing-font "-20")))
                 (progn ;; HD monitor in OSX
                   (setq main-programming-font (concat main-programming-font "-16"))
                   (setq main-writing-font (concat main-writing-font "-17"))))
             (progn
               (setq main-programming-font (concat main-programming-font "-16"))
               (setq main-writing-font (concat main-writing-font "-16")))))
          (t ;; Linux
           (if (> (x-display-pixel-width) 2000)
               (progn ;; Ultra-HD monitor in Linux
                 (setq main-programming-font (concat main-programming-font "-13"))
                 (setq main-writing-font (concat main-writing-font "-15")))
             (if (> (x-display-pixel-width) 1800)
                 (progn ;; HD monitor in Linux
                   (setq main-programming-font (concat main-programming-font "-13"))
                   (setq main-writing-font (concat main-writing-font "-14")))
               (progn
                 (setq main-programming-font (concat main-programming-font "-12"))
                 (setq main-writing-font (concat main-writing-font "-13")))))))

        ;; Apply fonts
        (set-default-font main-programming-font frame)
        (add-to-list 'default-frame-alist (cons 'font main-programming-font))
        (set-default-font main-programming-font frame)
        (set-frame-font main-programming-font t)
        (set-face-attribute 'default nil :font main-programming-font)
        (set-face-attribute 'fixed-pitch nil :font main-programming-font)
        (set-face-attribute 'variable-pitch nil :font main-writing-font :weight 'normal)))

    ;; Fontify current frame
    (fontify-frame nil)
    (let (frame (selected-frame))
      (fontify-frame frame))

    ;; Fontify any future frames for emacsclient
    (add-hook 'after-make-frame-functions #'fontify-frame)

    ;; hook for setting up UI when not running in daemon mode
    (add-hook 'emacs-startup-hook '(lambda () (fontify-frame (selected-frame))))

    ;; Use Symbola font on Unicode mathematical symbols
    (if (find-font (font-spec :name "Symbola"))
      (set-fontset-font t nil "Symbola"))))

;; Fixed pitch for HTML
(defun fixed-pitch-mode ()
  (buffer-face-mode -1))
(add-hook 'html-mode-hook 'fixed-pitch-mode)
(add-hook 'nxml-mode-hook 'fixed-pitch-mode)

;; Pretty lambdas
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'lisp-mode-hook 'pretty-lambdas)

;; Pretty mode
(use-package pretty-mode
  :commands pretty-mode
  :load-path (lambda () (expand-file-name "pretty-mode/" user-emacs-directory)))

(provide 'setup-fonts)
;;; setup-fonts.el ends here

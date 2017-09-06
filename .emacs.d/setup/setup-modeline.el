;;; setup-modeline.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017 Abelardo Jara-Berrocal

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

;; Nyan cat
(use-package nyan-mode
  :if (display-graphic-p)
  :commands nyan-mode
  :load-path (lambda () (expand-file-name "nyan-mode/" user-emacs-directory))
  :config (progn
            (nyan-mode t)
            (nyan-start-animation)))

;; Powerline
(use-package powerline
  :after projectile
  :load-path (lambda () (expand-file-name "powerline/" user-emacs-directory))
  :config (unless (display-graphic-p)
            (powerline-default-theme)))

;; Spaceline
(use-package spaceline
  :if (display-graphic-p)
  :after powerline
  :load-path (lambda () (expand-file-name "spaceline/" user-emacs-directory))
  :config (progn
            ;; Configure the mode-line
            (setq-default powerline-default-separator 'utf-8
                          powerline-height (truncate (* 1.0 (frame-char-height)))
                          spaceline-display-default-perspective t
                          spaceline-highlight-face-func 'spaceline-highlight-face-modified
                          spaceline-flycheck-bullet "• %s"
                          spaceline-separator-dir-left '(left . left)
                          spaceline-separator-dir-right '(right . right))))

;; Spaceline configuration
(use-package spaceline-config
  :if (display-graphic-p)
  :after spaceline
  :config (spaceline-helm-mode))

(use-package spaceline-all-the-icons
  :if (display-graphic-p)
  :after spaceline-config
  :load-path (lambda () (expand-file-name "spaceline-all-the-icons/" user-emacs-directory))
  :config (progn

            ;; Build a segment for the version control branch
            (spaceline-define-segment my/version-control
              (when vc-mode
                (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name)))))))

            ;; Build a segment for helm-follow-mode
            (spaceline-define-segment my/helm-follow
              (when (and (bound-and-true-p helm-alive-p)
                         spaceline--helm-current-source
                         (eq 1 (cdr (assq 'follow spaceline--helm-current-source))))
                (propertize "➟" 'face 'success)))

            ;; Build a segment for the active region
            (spaceline-define-segment my/selection-info
              (when mark-active
                (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
                       (chars (- (1+ (region-end)) (region-beginning)))
                       (cols (1+ (abs (- (spaceline--column-number-at-pos (region-end))
                                         (spaceline--column-number-at-pos (region-beginning))))))
                       (rect (bound-and-true-p rectangle-mark-mode))
                       (multi-line (> lines 1)))
                  (cond
                   (rect (format "%d × %d" (1- cols) lines))
                   (multi-line (format "%d lines" (if (eq (current-column) 0) (1- lines) lines)))
                   (t (format "%d chars" (1- chars)))))))

            ;; Build the mode-lines
            (spaceline-compile
             "custom"
             `( ;; All the icons segments
               (all-the-icons-anzu
                :face mode-line
                :skip-alternate t)

               (((major-mode :face default-face)
                 all-the-icons-modified
                 all-the-icons-bookmark
                 all-the-icons-dedicated
                 all-the-icons-window-number
                 all-the-icons-buffer-size) :face default-face :skip-alternate t)

               (all-the-icons-projectile :face default-face)
               ((all-the-icons-vc-status) " " :separator "")

               ((remote-host
                 all-the-icons-buffer-path
                 all-the-icons-buffer-id)
                :separator "")
               (all-the-icons-which-function :face powerline-active2))

             `((my/selection-info)
               ;; All the icons segments
               ((all-the-icons-process
                 all-the-icons-position
                 all-the-icons-region-info
                 all-the-icons-fullscreen
                 all-the-icons-text-scale)
                :face highlight-face
                :separator (spaceline-all-the-icons--separator "|" " "))

               (("" all-the-icons-flycheck-status)
                :face default-face)
               (all-the-icons-time)

               (global :face highlight-face)))

            (spaceline-install
             'helm
             '((helm-buffer-id :face spaceline-read-only)
               (helm-number)
               (my/helm-follow :fallback "➟")
               helm-prefix-argument)
             '((helm-help)
               (global :face spaceline-read-only)))

            ;; Enable modeline
            (setq-default mode-line-format '("%e" (:eval (spaceline-ml-custom))))))

;; Customize Emacs lighters
(use-package delight
  :load-path (lambda () (expand-file-name "delight/" user-emacs-directory))
  :config (progn
            (defadvice powerline-major-mode (around delight-powerline-major-mode activate)
              (let ((inhibit-mode-name-delight nil)) ad-do-it))
            (defadvice powerline-minor-modes (around delight-powerline-minor-modes activate)
              (let ((inhibit-mode-name-delight nil)) ad-do-it))))

(provide 'setup-modeline)
;;; setup-modeline.el ends here

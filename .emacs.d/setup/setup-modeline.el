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
  :load-path (lambda () (expand-file-name "powerline/" user-emacs-directory)))

;; Spaceline
(use-package spaceline
  :after powerline
  :load-path (lambda () (expand-file-name "spaceline/" user-emacs-directory))
  :config (progn
            ;; Configure the mode-line
            (setq-default
             mode-line-format '("%e" (:eval (spaceline-ml-main)))
             powerline-default-separator 'slant
             spaceline-display-default-perspective t
             powerline-height 20
             spaceline-highlight-face-func 'spaceline-highlight-face-modified
             spaceline-flycheck-bullet "• %s"
             spaceline-separator-dir-left '(left . left)
             spaceline-separator-dir-right '(right . right))

            (use-package spaceline-config)
            (spaceline-helm-mode)))

;; Spaceline configuration
(use-package spaceline-config
  :after spaceline
  :disabled t
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
                (propertize "" 'face 'success)))

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
            (spaceline-install
             `((major-mode :face highlight-face)
               ((remote-host buffer-id line) :separator ":")
               (anzu))
             `((my/selection-info)
               ((flycheck-error flycheck-warning flycheck-info) :when active)
               ((projectile-root my/version-control) :separator "  ")
               (workspace-number)
               (global :face highlight-face)))
            (spaceline-install
             'helm
             '((helm-buffer-id :face spaceline-read-only)
               (helm-number)
               (my/helm-follow :fallback "")
               helm-prefix-argument)
             '((helm-help)
               (global :face spaceline-read-only)))))

(use-package spaceline-all-the-icons
  :after spaceline
  :load-path (lambda () (expand-file-name "spaceline-all-the-icons/" user-emacs-directory))
  :init (setq spaceline-all-the-icons-separators-type 'slant)
  :config (spaceline-all-the-icons-theme))

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

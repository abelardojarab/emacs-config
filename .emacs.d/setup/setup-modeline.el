;;; setup-modeline.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc25288.pdx.intel.com>
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
  :load-path (lambda () (expand-file-name "nyan-mode/" user-emacs-directory))
  :config (nyan-mode t))

;; Powerline
(use-package powerline
  :commands powerline-default-theme
  :load-path (lambda () (expand-file-name "powerline/" user-emacs-directory))
  :init (setq powerline-default-separator 'wave)
  :config (progn
            ;; Cleaning the mode line
            ;; https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
            (defvar mode-line-cleaner-alist
              `((auto-complete-mode . " α")
                (yas/minor-mode . " υ")
                (paredit-mode . " π")
                (eldoc-mode . "")
                (abbrev-mode . "")
                (git-gutter+-mode . " ⇥")
                (smartparens-mode . " π")

                ;; Major modes
                (hi-lock-mode . ""))

              "Alist for `clean-mode-line'.
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

            (defun clean-mode-line ()
              (interactive)
              (loop for cleaner in mode-line-cleaner-alist
                    do (let* ((mode (car cleaner))
                              (mode-str (cdr cleaner))
                              (old-mode-str (cdr (assq mode minor-mode-alist))))
                         (when old-mode-str
                           (setcar old-mode-str mode-str))
                         ;; major mode
                         (when (eq mode major-mode)
                           (setq mode-name mode-str)))))
            (if (display-graphic-p)
                (add-hook 'after-change-major-mode-hook 'clean-mode-line))

            ;; Powerline setup
            (add-hook 'desktop-after-read-hook 'powerline-reset)
            (defun powerline-simpler-vc-mode (s)
              (if s
                  (replace-regexp-in-string "Git[:-]" "" s)
                s))

            ;; Some point, we could change the text of the minor modes, but we
            ;; need to get the text properties and sub them /back in/. To be
            ;; figured out later... Like:
            ;;   (let* ((props (text-properties-at 1 s))
            ;;          (apple (set-text-properties 0 1 props "⌘"))
            ;;          (fly-c (set-text-properties 0 1 props "✓"))
            ;;          (news2 (replace-regexp-in-string "FlyC" fly-c news1)))

            (defun powerline-simpler-minor-display (s)
              (replace-regexp-in-string
               (concat " " (mapconcat 'identity '("Projectile" "Fill" "BufFace") "\\|")) "" s))

            (defun powerline-ha-theme ()
              "A powerline theme that removes many minor-modes that don't serve much purpose on the mode-line."
              (interactive)
              (setq-default mode-line-format
                            '("%e"
                              (:eval
                               (let*
                                   ((active
                                     (powerline-selected-window-active))
                                    (mode-line
                                     (if active 'mode-line 'mode-line-inactive))
                                    (face1
                                     (if active 'powerline-active1 'powerline-inactive1))
                                    (face2
                                     (if active 'powerline-active2 'powerline-inactive2))
                                    (separator-left
                                     (intern
                                      (format "powerline-%s-%s" powerline-default-separator
                                              (car powerline-default-separator-dir))))
                                    (separator-right
                                     (intern
                                      (format "powerline-%s-%s" powerline-default-separator
                                              (cdr powerline-default-separator-dir))))
                                    (lhs
                                     (list
                                      (powerline-raw "%*" nil 'l)
                                      (powerline-buffer-id nil 'l)
                                      (powerline-raw " ")
                                      (funcall separator-left mode-line face1)
                                      (powerline-raw '(:eval (format " ⑆[%s]"
                                                                     (projectile-project-name))) face1)
                                      (if (executable-find "git")
                                          (powerline-raw '(:eval (format " ⑃[%s]"
                                                                       (s-trim
                                                                        (shell-command-to-string
                                                                         "git rev-parse --abbrev-ref HEAD"))) face1)))
                                      (powerline-raw " ƒ" face1)
                                      (powerline-raw mode-line-misc-info face1 'r)))
                                    (rhs
                                     (list
                                      (when (bound-and-true-p nyan-mode)
                                        (powerline-raw (list (nyan-create)) face1 'l))
                                      (powerline-raw "%4l" face1 'r)
                                      (powerline-raw ":" face1)
                                      (powerline-raw '(:eval
                                                       (propertize "%3c" 'face
                                                                   (if (>= (current-column) 90)
                                                                       'font-lock-warning-face 'nil))) face1 'l)
                                      (funcall separator-right face1 mode-line)
                                      (powerline-raw " ")
                                      (powerline-raw "%6p" nil 'r)
                                      (powerline-hud face2 face1)))
                                    (center
                                     (list
                                      (powerline-raw " " face1)
                                      (funcall separator-left face1 face2)
                                      (powerline-major-mode face2 'l)
                                      (powerline-process face2)
                                      (powerline-raw ":" face2)
                                      (powerline-simpler-minor-display
                                       (powerline-minor-modes face2 'l))

                                      (powerline-raw " " face2)
                                      (funcall separator-right face2 face1))))
                                 (concat
                                  (powerline-render lhs)
                                  (powerline-fill-center face1
                                                         (/
                                                          (powerline-width center)
                                                          2.0))
                                  (powerline-render center)
                                  (powerline-fill face1
                                                  (powerline-width rhs))
                                  (powerline-render rhs)))))))

            (powerline-ha-theme)))

(provide 'setup-modeline)
;;; setup-modeline.el ends here

;;; setup-helm.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2016  Abelardo Jara

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

;; Helm
(use-package helm-config
  :load-path (lambda () (expand-file-name "helm/" user-emacs-directory))
  :bind (("C-;" . helm-mini)
         ("C-0" . helm-buffers-list)
         ("C-`" . helm-semantic-or-imenu)
         ("C-S-v" . helm-show-kill-ring)
         ("M-y" . helm-show-kill-ring)
         :map isearch-mode-map
         ("C-h" . helm-occur-from-isearch)
         :map ctl-x-map
         ("p" . helm-show-kill-ring)
         ("x" . helm-M-x)
         ("n" . helm-mini)
         ("h" . helm-apropos)
         ("i" . helm-semantic-or-imenu)
         ("." . helm-semantic)
         ("u" . helm-resume)
         ("l" . helm-buffers-list)
         ("f" . helm-find-files)
         ("r" . helm-recentf))
  :config (progn
            (setq helm-delete-minibuffer-contents-from-point t)
            (setq helm-buffer-max-length 35)
            (defadvice helm-buffers-sort-transformer (around ignore activate)
              (setq ad-return-value (ad-get-arg 0)))

            ;; use silver searcher when available
            (when (executable-find "ag-grep")
              (setq helm-grep-default-command "ag-grep -Hn --no-group --no-color %e %p %f"
                    helm-grep-default-recurse-command "ag-grep -H --no-group --no-color %e %p %f"))

            ;; Make sure helm always pops up in bottom
            (setq helm-split-window-in-side-p t)

            (add-to-list 'display-buffer-alist
                         '("\\`\\*helm.*\\*\\'"
                           (display-buffer-in-side-window)
                           (inhibit-same-window . t)
                           (window-height . 0.2)))

            ;; provide input in the header line and hide the mode lines above
            (setq helm-echo-input-in-header-line t)

            (defvar bottom-buffers nil
              "List of bottom buffers before helm session.
      Its element is a pair of `buffer-name' and `mode-line-format'.")

            (defun bottom-buffers-init ()
              (setq-local mode-line-format (default-value 'mode-line-format))
              (setq bottom-buffers
                    (cl-loop for w in (window-list)
                             when (window-at-side-p w 'bottom)
                             collect (with-current-buffer (window-buffer w)
                                       (cons (buffer-name) mode-line-format)))))

            (defun bottom-buffers-hide-mode-line ()
              (setq-default cursor-in-non-selected-windows nil)
              (mapc (lambda (elt)
                      (with-current-buffer (car elt)
                        (setq-local mode-line-format nil)))
                    bottom-buffers))

            (defun bottom-buffers-show-mode-line ()
              (setq-default cursor-in-non-selected-windows t)
              (when bottom-buffers
                (mapc (lambda (elt)
                        (with-current-buffer (car elt)
                          (setq-local mode-line-format (cdr elt))))
                      bottom-buffers)
                (setq bottom-buffers nil)))

            (defun helm-keyboard-quit-advice (orig-func &rest args)
              (bottom-buffers-show-mode-line)
              (apply orig-func args))

            (add-hook 'helm-before-initialize-hook #'bottom-buffers-init)
            (add-hook 'helm-after-initialize-hook #'bottom-buffers-hide-mode-line)
            (add-hook 'helm-exit-minibuffer-hook #'bottom-buffers-show-mode-line)
            (add-hook 'helm-cleanup-hook #'bottom-buffers-show-mode-line)
            (advice-add 'helm-keyboard-quit :around #'helm-keyboard-quit-advice)

            ;; remove header lines if only a single source
            (setq helm-display-header-line nil)

            (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
            (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
            (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))

            (defun helm-toggle-header-line ()
              (if (> (length helm-sources) 1)
                  (set-face-attribute 'helm-source-header
                                      nil
                                      :foreground helm-source-header-default-foreground
                                      :background helm-source-header-default-background
                                      :box helm-source-header-default-box
                                      :height 1.0)
                (set-face-attribute 'helm-source-header
                                    nil
                                    :foreground (face-attribute 'helm-selection :background)
                                    :background (face-attribute 'helm-selection :background)
                                    :box nil
                                    :height 0.1)))

            (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)))

;; Indent semantic entries
(use-package helm-imenu
  :config (progn
            (defun my-helm-imenu-transformer (cands)
              (with-helm-current-buffer
                (save-excursion
                  (cl-loop for (func-name . mrkr) in cands
                           collect
                           (cons (format "Line %4d: %s"
                                         (line-number-at-pos mrkr)
                                         (progn (goto-char mrkr)
                                                (buffer-substring mrkr (line-end-position))))
                                 (cons func-name mrkr))))))

            (defvar my-helm-imenu-source  (helm-make-source "Imenu" 'helm-imenu-source
                                            :candidate-transformer
                                            'my-helm-imenu-transformer))
            (defun my-helm-imenu ()
              (interactive)
              (let ((imenu-auto-rescan t)
                    (str (thing-at-point 'symbol))
                    (helm-execute-action-at-once-if-one
                     helm-imenu-execute-action-at-once-if-one))
                (helm :sources 'my-helm-imenu-source
                      :preselect str
                      :buffer "*helm imenu*")))))

(provide 'setup-helm)
;;; setup-helm.el ends here

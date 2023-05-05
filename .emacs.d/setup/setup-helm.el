;;; setup-helm.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

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

;; Helm
(use-package helm-config
  :demand t
  :diminish (helm-mode . " Ⓗ")
  :bind (("C-`"   . helm-semantic-or-imenu)
         :map isearch-mode-map
         ("C-."   . helm-occur-from-isearch)
         :map minibuffer-local-isearch-map
         ("C-."   . helm-occur-from-isearch)
         :map ctl-x-map
         ("."     . helm-semantic-or-imenu)
         ("C-b"   . helm-buffers-list)
         ("b"     . helm-buffers-list)
         ("p"     . helm-show-kill-ring)
         ("n"     . helm-mini)
         ("h"     . helm-apropos)
         ("u"     . helm-resume)
         ("f"     . helm-for-files)
         ("<tab>" . helm-find-files)
         :map helm-command-map
         ;; rebind tab to run persistent action
         ("<tab>" . helm-execute-persistent-action)
         ;; make TAB works in terminal
         ("C-i"   . helm-execute-persistent-action)
         ;; list actions using C-z
         ("C-z"   . helm-select-action))
  :custom ((helm-mode-handle-completion-in-region     nil)
           (helm-bookmark-show-location               t)
           (helm-turn-on-show-completion              nil) ;; do not split window
           (helm-buffer-max-length                    48)
           (helm-always-two-windows                   t)
           (helm-quick-update                         t)
           (helm-org-headings-fontify                 t)
           (helm-window-prefer-horizontal-split       t)
           (helm-split-window-default-side            'right)
           (helm-split-window-in-side-p               t)
           (helm-echo-input-in-header-line            t)
           (helm-idle-delay                           0.0)
           (helm-input-idle-delay                     0.01)
           (helm-yas-display-key-on-candidate         t)
           (helm-M-x-requires-pattern                 nil)
           (helm-candidate-number-limit               50)
           (helm-move-to-line-cycle-in-source         nil)
           (helm-scroll-amount                        8)
           (helm-truncate-lines                       nil)
           (helm-grep-truncate-lines                  t)
           (helm-ff-auto-update-initial-value         nil)
           (helm-ff-search-library-in-sexp            t)
           (helm-ff-skip-boring-files                 t)
           (helm-ff-file-name-history-use-recentf     t)
           (helm-use-frame-when-more-than-two-windows nil)
           (helm-display-function                     'helm-default-display-buffer)
           (helm-display-buffer-reuse-frame           t)
           (helm-display-buffer-width                 180)
           (helm-display-buffer-height                60)
           (helm-use-undecorated-frame-option         t)
           (helm-dabbrev-related-buffer               nil))
  :config (progn
            (if (display-graphic-p)
                (setq helm-buffers-end-truncated-string "…"))

            (defadvice helm-buffers-sort-transformer (around ignore activate)
              (setq ad-return-value (ad-get-arg 0)))

            (defadvice helm-execute-persistent-action (around bar activate)
              (ignore-errors add-do-it))

            ;; use silver searcher when available
            (when (executable-find "ag-grep")
              (setq helm-grep-default-command              "ag-grep -Hn --no-group --no-color %e %p %f"
                    helm-grep-default-recurse-command      "ag-grep -H --no-group --no-color %e %p %f"))

            ;; use curl when available
            (when (executable-find "curl")
              (setq helm-net-prefer-curl           t
                    helm-google-suggest-use-curl-p t))

            ;; replace locate with spotlight on Mac
            (when (and (executable-find "mdfind")
                       (equal system-type 'darwin))
              (setq helm-locate-command "mdfind -name %s %s"))

            ;; Via: https://www.reddit.com/r/emacs/comments/3asbyn/new_and_very_useful_helm_feature_enter_search/
            (defun helm-hide-minibuffer-maybe ()
              (when (with-helm-buffer helm-echo-input-in-header-line)
                (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
                  (overlay-put ov 'window (selected-window))
                  (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                          `(:background ,bg-color :foreground ,bg-color)))
                  (setq-local cursor-type nil))))
            (add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

            ;; Helm fonts listing
            (setq font-helm-source
                  `((name . "Fonts")
                    (candidates . ,(font-family-list))
                    (action . (lambda (candidate) (progn
                                                    (setq my/main-programming-font candidate)
                                                    (set-face-attribute 'default nil :family candidate)
                                                    (fontify-frame (selected-frame)))))))

            (defun helm-fonts ()
              (interactive)
              (helm :sources '(font-helm-source)))))

;; Use helm for the kill ring too
(use-package helm-ring
  :bind (("M-y" . helm-show-kill-ring)))

;; Integrate helm with projectile
(use-package helm-projectile
  :defer t
  :after (projectile helm-config)
  :commands (helm-projectile-on
             helm-projectile-off
             helm-projectile-toggle
             my/helm-projectile-init)
  :hook ((projectile-find-file projectile-mode) . my/helm-projectile-init)
  :config (progn
            (defun helm-projectile-on ()
              "Turn on helm-projectile key bindings."
              (interactive)
              (helm-projectile-toggle 1))

            (defun helm-projectile-off ()
              "Turn off helm-projectile key bindings."
              (interactive)
              (helm-projectile-toggle -1))

            (defun my/helm-projectile-init ()
              (helm-projectile-on)
              (setq projectile-switch-project-action 'helm-projectile)
              (setq projectile-completion-system 'helm))))

;; Indent semantic entries
(use-package helm-imenu
  :config (progn
            (defun my/helm-imenu-transformer (cands)
              (with-helm-current-buffer
                (save-excursion
                  (cl-loop for (func-name . mrkr) in cands
                           collect
                           (cons (format "Line %4d: %s"
                                         (line-number-at-pos mrkr)
                                         (progn (goto-char mrkr)
                                                (buffer-substring mrkr (line-end-position))))
                                 (cons func-name mrkr))))))

            (defvar my/helm-imenu-source  (helm-make-source "Imenu" 'helm-imenu-source
                                            :candidate-transformer
                                            'my/helm-imenu-transformer))
            (defun my/helm-imenu ()
              (interactive)
              (let ((imenu-auto-rescan t)
                    (str (thing-at-point 'symbol))
                    (helm-execute-action-at-once-if-one
                     helm-imenu-execute-action-at-once-if-one))
                (helm :sources 'my/helm-imenu-source
                      :preselect str
                      :buffer "*helm imenu*")))))

;; Integration with helm
(use-package helm-lsp
  :defer t
  :commands helm-lsp-workspace-symbol)

;; Helm integration with posframe
(use-package helm-posframe
  :disabled t
  :defer t
  :if (and (window-system) (version<= "26.1" emacs-version))
  :commands (helm-posframe-cleanup
             helm-posframe-enable)
  :hook ((helm-org-rifle-after-command . helm-posframe-cleanup)
         (on-first-input               . helm-posframe-enable))
  :custom ((helm-posframe-poshandler 'posframe-poshandler-frame-center)
           (helm-posframe-height     10))
  :config (setq helm-posframe-width (round (* (frame-width) 0.49))
                helm-posframe-parameters '((internal-border-width . 10))))

(provide 'setup-helm)
;;; setup-helm.el ends here

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
  :bind (("C-'" . helm-semantic-or-imenu)
         :map isearch-mode-map
         ("C-f" . helm-occur-from-isearch)
         :map minibuffer-local-isearch-map
         ("C-f" . helm-occur-from-isearch)
         :map ctl-x-map
         ("C-r" . helm-recentf)
         ("p" . helm-show-kill-ring)
         ("x" . helm-M-x)
         ("n" . helm-mini)
         ("h" . helm-apropos)
         ("u" . helm-resume)
         ("b" . helm-buffers-list)
         ("f" . helm-for-files)
         ("<tab>" . helm-find-files))
  :config (progn
            (defadvice helm-buffers-sort-transformer (around ignore activate)
              (setq ad-return-value (ad-get-arg 0)))

            (setq helm-candidate-number-limit 100)

            ;; From https://gist.github.com/antifuchs/9238468
            (setq helm-idle-delay 0.0 ;; update fast sources immediately (doesn't).
                  helm-input-idle-delay 0.01  ;; this actually updates things quicker
                  helm-yas-display-key-on-candidate t
                  helm-quick-update t
                  helm-M-x-requires-pattern nil
                  helm-candidate-number-limit 100 ;; limit max number of matches displayed for speed
                  helm-ff-skip-boring-files t ;; ignore boring files like .o and .a
                  helm-move-to-line-cycle-in-source nil ;; move to end or beginning of source when reaching top or bottom of source.
                  helm-ff-search-library-in-sexp t ;; search for library in `require' and `declare-function' sexp.
                  helm-scroll-amount 8 ;; scroll 8 lines other window using M-<next>/M-<prior>
                  helm-ff-file-name-history-use-recentf t)

            ;; use silver searcher when available
            (when (executable-find "ag-grep")
              (setq helm-grep-default-command "ag-grep -Hn --no-group --no-color %e %p %f"
                    helm-grep-default-recurse-command "ag-grep -H --no-group --no-color %e %p %f"))

            ;; use curl when available
            (when (executable-find "curl")
              (setq helm-google-suggest-use-curl-p t))

            ;; replace locate with spotlight on Mac
            (when (and (executable-find "mdfind")
                       (equal system-type 'darwin))
              (setq helm-locate-command "mdfind -name %s %s"))

            ;; Via: https://www.reddit.com/r/emacs/comments/3asbyn/new_and_very_useful_helm_feature_enter_search/
            (setq helm-echo-input-in-header-line t)
            (defun helm-hide-minibuffer-maybe ()
              (when (with-helm-buffer helm-echo-input-in-header-line)
                (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
                  (overlay-put ov 'window (selected-window))
                  (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                          `(:background ,bg-color :foreground ,bg-color)))
                  (setq-local cursor-type nil))))
            (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;; rebind tab to run persistent action
            (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ;; make TAB works in terminal
            (define-key helm-map (kbd "C-z") 'helm-select-action) ;; list actions using C-z
            ))

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

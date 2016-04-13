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
              (setq ad-return-value (ad-get-arg 0)))))

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

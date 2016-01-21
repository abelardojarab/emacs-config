;;; setup-cedet.el ---

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

;; Enable Semantic
(add-to-list 'load-path "~/.emacs.d/cedet/lisp/cedet")
(require 'semantic/ia)
(require 'semantic/wisent)
(semantic-mode 1)
(semantic-load-enable-minimum-features)

;; Enable case-insensitive searching
(set-default 'semantic-case-fold t)

;; Faster parsing
(setq semantic-idle-work-parse-neighboring-files-flag nil)
(setq semantic-idle-work-update-headers-flag nil)
(setq semantic-idle-scheduler-idle-time 30)
(setq semantic-idle-scheduler-work-idle-time 1800)
(setq semantic-idle-scheduler-max-buffer-size 1)

;; Default directory
(setq-default semanticdb-default-system-save-directory
              (setq-default semanticdb-default-save-directory "~/.emacs.cache/semanticdb"))
(setq-default ede-project-placeholder-cache-file "~/.emacs.cache/ede-projects.el")

;; Disable Semantics for large files
(add-hook 'semantic--before-fetch-tags-hook
          (lambda () (if (and (> (point-max) 5000)
                              (not (semantic-parse-tree-needs-rebuild-p)))
                         nil
                       t)))

;; This prevents Emacs to become uresponsive
(defun semanticdb-kill-hook ()
  nil)
(defun semanticdb-create-table-for-file-not-in-buffer (arg)
  nil)

;; Auto-complete support
(require 'semantic/analyze/refs)
(defun ac-complete-semantic-self-insert (arg)
  (interactive "p")
  (self-insert-command arg)
  (ac-complete-semantic))

(defun cc-mode-ac-key-bindings ()
  (local-set-key "." 'ac-complete-semantic-self-insert)
  (local-set-key ">" 'ac-complete-semantic-self-insert)
  (local-set-key ":" 'ac-complete-semantic-self-insert))
(add-hook 'c-mode-common-hook 'cc-mode-ac-key-bindings)
(add-hook 'prog-mode-hook 'cc-mode-ac-key-bindings)

;; smart completions
(setq-mode-local prog-mode semanticdb-find-default-throttle
                 '(project system))

;; Put c++-mode as default for *.h files (improves parsing)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; C/C++ style
(defun my/c-mode-init ()
  (c-set-style "k&r")
  (c-toggle-electric-state -1)
  (setq-default c-basic-offset 4))
(add-hook 'c-mode-hook #'my/c-mode-init)
(add-hook 'c++-mode-hook #'my/c-mode-init)

;; Include settings
(require 'semantic/bovine/c)
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/clang)

;; Enable which-function-mode for selected major modes
(setq which-func-modes '(ecmascript-mode emacs-lisp-mode lisp-mode java-mode
                                         c-mode c++-mode makefile-mode sh-mode))
(which-func-mode t)
(mapc (lambda (mode)
        (add-hook mode (lambda () (which-function-mode t))))
      '(prog-mode-hook
        org-mode-hook))

(provide 'setup-cedet)
;;; setup-cedet.el ends here

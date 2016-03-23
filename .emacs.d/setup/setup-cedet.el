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
(add-to-list 'load-path (expand-file-name "cedet/lisp/cedet/" user-emacs-directory))
(semantic-load-enable-minimum-features)
(require 'semantic/wisent)

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
          (lambda () (if (and (> (point-max) 500)
                              (not (semantic-parse-tree-needs-rebuild-p)))
                         nil
                       t)))

;; This prevents Emacs to become uresponsive
(defun semanticdb-kill-hook ()
  nil)
(defun semanticdb-create-table-for-file-not-in-buffer (arg)
  nil)

;; Enable which-function-mode for selected major modes
(setq which-func-modes '(org-mode markdown-mode
                                  ecmascript-mode emacs-lisp-mode lisp-mode java-mode
                                  c-mode c++-mode makefile-mode sh-mode))
(which-func-mode t)
(mapc (lambda (mode)
        (add-hook mode (lambda () (which-function-mode t))))
      '(prog-mode-hook
        org-mode-hook))

(provide 'setup-cedet)
;;; setup-cedet.el ends here

;;; setup-lsp.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019  Abelardo Jara-Berrocal

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

;; lsp-mode:  Emacs client/library for the Language Server Protocol
(use-package lsp-mode
  :demand t
  :commands lsp
  :hook ((c++-mode    . lsp)
         (python-mode . lsp))
  :config (progn
            ;; Prefer flake8, faster than pylint
            (setq-default lsp-pyls-configuration-sources ["flake8"])
            (setq-default lsp-pyls-plugins-pylint-enabled nil)

            ;; `-background-index' requires clangd v8+!
            (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))))

;; Fix bug on dash/lsp
(load (expand-file-name "no-autoloads/dash.el" user-emacs-directory))
(load (expand-file-name "no-autoloads/lsp-mode.el" user-emacs-directory))
(load (expand-file-name "no-autoloads/helm-xref.el" user-emacs-directory))

;; Enable lsp-clients
(use-package lsp-clients
  :demand t
  :after lsp-mode
  :commands lsp-define-stdio-client)

;; make sure we have lsp-imenu everywhere we have LSP
(use-package lsp-imenu
  :hook (lsp-after-open . lsp-enable-imenu))

;; lsp-ui: This contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses.
(use-package lsp-ui
  :custom ((lsp-ui-sideline-enable               nil)
           (lsp-ui-doc-enable                    nil)
           (lsp-ui-flycheck-enable               t)
           (lsp-ui-imenu-enable                  t)
           (lsp-ui-sideline-ignore-duplicate     t )
           (lsp-ui-sideline-show-symbol          nil))
  :hook (lsp-mode . lsp-ui-mode)
  :config (progn
            (if (display-graphic-p)
                (setq lsp-ui-sideline-code-actions-prefix "ℹ "))

            (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
            (define-key lsp-ui-mode-map [remap xref-find-references]  #'lsp-ui-peek-find-references)))

(use-package lsp-metals-treeview
  :disabled t
  :custom (lsp-metals-treeview-show-when-views-received t)
  :config (lsp-metals-treeview-enable t))

(provide 'setup-lsp)
;;; setup-lsp.el ends here

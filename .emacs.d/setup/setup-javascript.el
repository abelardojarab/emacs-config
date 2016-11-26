;;; setup-javascript.el ---

;; Copyright (C) 2016  Abelardo Jara-Berrocal

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

(use-package js2-mode
  :load-path (lambda () (expand-file-name "js2-mode/" user-emacs-directory))
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
            (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

            (add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))
            (add-hook 'js2-mode-hook 'skewer-mode)
            (setq-default js2-allow-rhino-new-expr-initializer nil)
            (setq-default js2-auto-indent-p nil)
            (setq-default js2-enter-indents-newline nil)
            (setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
            (setq-default js2-idle-timer-delay 0.1)
            (setq-default js2-indent-on-enter-key nil)
            (setq-default js2-mirror-mode nil)
            (setq-default js2-strict-inconsistent-return-warning nil)
            (setq-default js2-auto-indent-p t)
            (setq-default js2-include-rhino-externs nil)
            (setq-default js2-include-gears-externs nil)
            (setq-default js2-concat-multiline-strings 'eol)
            (setq-default js2-rebind-eol-bol-keys nil)

            ;; Let flycheck handle parse errors
            (setq-default js2-show-parse-errors nil)
            (setq-default js2-strict-missing-semi-warning nil)
            (setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason
            (add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))))

;; tern
(use-package tern
  :defer t
  :if (and (executable-find "node")
           (executable-find "npm"))
  :commands (tern-mode)
  :diminish tern-mode
  :load-path (lambda () (expand-file-name "tern/emacs/" user-emacs-directory))
  :init (add-hook 'js2-mode-hook 'tern-mode)
  :config (progn
            ;; When using auto-complete
            (when (featurep 'auto-complete)
              (require 'tern-auto-complete)
              (tern-ac-setup))))

;; skewer mode
(use-package skewer-mode
  :defer t
  :commands skewer-mode
  :load-path (lambda () (expand-file-name "skewer-mode/" user-emacs-directory)))

;; web server
(use-package web-server
  :defer t
  :load-path (lambda () (expand-file-name "web-server/" user-emacs-directory)))

;; js2-refactor
(use-package js2-refactor
  :defer t
  :commands js2-refactor-mode
  :diminish js2-refactor-mode
  :load-path (lambda () (expand-file-name "js2-refactor/" user-emacs-directory))
  :init (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c r"))

;; ac-js2
(use-package ac-js2
  :after (auto-complete j2-mode)
  :load-path (lambda () (expand-file-name "ac-js2/" user-emacs-directory))
  :config (progn
            (when (featurep 'auto-complete)
              (add-hook 'js2-mode-hook 'ac-js2-mode)
              (setq-default ac-js2-evaluate-calls t))))

(use-package company-tern
  :after (company tern)
  :load-path (lambda () (expand-file-name "company-tern/" user-emacs-directory))
  :config (progn
            (add-hook 'js2-mode-hook (lambda () (set (make-local-variable 'company-backends) '(company-tern))))
            (setq company-tern-meta-as-single-line t)))

;; json-reformat
(use-package json-reformat
  :load-path (lambda () (expand-file-name "json-reformat/" user-emacs-directory)))

;; json-mode
(use-package json-mode
  :mode "\\.json$"
  :commands json-mode
  :load-path (lambda () (expand-file-name "json-mode/" user-emacs-directory))
  :config (progn
            (add-hook 'json-mode-hook 'js2-minor-mode)
            (setq js-indent-level 4)))

;; json-snatcher
(use-package json-snatcher
  :bind (:map js2-minor-mode-map
              ("C-c C-g" . jsons-print-path))
  :config (bind-key "C-c C-g" 'jsons-print-path js2-mode-map)
  :load-path (lambda () (expand-file-name "json-snatcher/" user-emacs-directory)))

;; Bring node.js to Emacs
(use-package nodejs-repl
  :commands (nodejs-repl
             nodejs-repl-send-buffer
             nodejs-repl-switch-to-repl
             nodejs-repl-send-region
             nodejs-repl-send-last-sexp
             nodejs-repl-execute
             nodejs-repl-load-file)
  :if (executable-find "node")
  :load-path (lambda () (expand-file-name "nodejs-repl/" user-emacs-directory)))

(provide 'setup-javascript)

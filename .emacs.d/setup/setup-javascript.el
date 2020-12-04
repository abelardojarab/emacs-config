;;; setup-javascript.el ---                               -*- lexical-binding: t; -*-

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

(use-package js2-mode
  :demand t
  :mode "\\.js\\'"
  :defines js2-mode-map
  :init (progn
          ;; Setup node.js path
          (setenv "NODE_PATH" (concat (concat (getenv "HOME")
                                              "/node_modules")
                                      ":"
                                      (concat (getenv "HOME")
                                              "/local/lib/node_modules")
                                      ":/usr/local/lib/node_modules:/usr/local/lib/node"
                                      ":" (getenv "NODE_PATH")))

          ;; Check for node access
          (defun check-npm-module (&optional module local)
            (and (executable-find "npm")
                 (= 0 (call-process "npm"  nil nil nil "list"
                                    (if local " " "-g")
                                    (if module module "tern"))))))
  :mode ("\\.js\\'" . js2-mode)
  ;; :ensure-system-package ((typescript-language-server            . "npm install -g typescript-lsp")
  ;;                         (javascript-typescript-language-server . "npm install -g javascript-typescript-language-server")
  ;;                         (tsc                                   . "npm install -g typescript")
  ;;                         (tern                                  . "npm install -g tern")
  ;;                         (flow-bin                              . "npm install -g flow-bin"))
  :commands (js2-mode
             js2-minor-mode)
  :hook (js2-mode . flycheck-mode)
  :custom ((js2-basic-offset                       4)
           (js2-allow-rhino-new-expr-initializer   nil)
           (js2-auto-indent-p                      nil)
           (js2-enter-indents-newline              nil)
           (js2-idle-timer-delay                   0.1)
           (js2-indent-on-enter-key                nil)
           (js2-mirror-mode                        nil)
           (js2-strict-inconsistent-return-warning nil)
           (js2-auto-indent-p                      t  )
           (js2-include-rhino-externs              nil)
           (js2-include-gears-externs              nil)
           (js2-concat-multiline-strings           'eol)
           (js2-rebind-eol-bol-keys                nil)
           (js2-show-parse-errors                  nil)
           (js2-strict-missing-semi-warning        nil)
           (js2-strict-trailing-comma-warning      t))
  :config (progn
            (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
            (setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))))

;; tern
(use-package tern
  :defer t
  :if (and (executable-find "node")
           (executable-find "npm")
           (or (check-npm-module "tern" t)
               (check-npm-module "tern")))
  :diminish tern-mode
  :commands tern-mode
  :after js2-mode
  :hook (js2-mode . tern-mode)
  :config (progn
            (add-to-list 'tern-command "--no-port-file" 'append)

            ;; tern completions
            (use-package company-tern
              :demand t
              :after (company js2-mode)
              :custom ((company-tern-meta-as-single-line t)
                       (company-tern-property-marker     ""))
              :config (progn
                        (add-to-list 'tern-command "--no-port-file" 'append)
                        (add-hook 'js2-mode-hook (lambda ()
                                                   (set (make-local-variable 'company-backends)
                                                        '((company-lsp
                                                           company-tern
                                                           company-files
                                                           :with company-yasnippet
                                                           :with company-capf)))))))))

;; skewer mode
(use-package skewer-mode
  :defer t
  :commands skewer-mode
  :hook (js2-mode . skewer-mode)
  :custom (httpd-port 8079))

;; web server
(use-package web-server
  :defer t)

;; js2-refactor
(use-package js2-refactor
  :defer t
  :commands (js2-refactor-mode
             js2-refactor-hydra/body)
  :functions (js2r-add-keybindings-with-prefix)
  :diminish js2-refactor-mode
  :hook (js2-mode . js2-refactor-mode)
  :bind (:map js2-mode-map
              ("C-k" . js2r-kill))
  :config (js2r-add-keybindings-with-prefix "C-c r")
  :hydra (js2-refactor-hydra (:color blue :hint nil)
                             "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
[_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
[_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp
[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
[_q_]  quit"
                             ("ee" js2r-expand-node-at-point)
                             ("cc" js2r-contract-node-at-point)
                             ("ef" js2r-extract-function)
                             ("em" js2r-extract-method)
                             ("tf" js2r-toggle-function-expression-and-declaration)
                             ("ta" js2r-toggle-arrow-function-and-expression)
                             ("ip" js2r-introduce-parameter)
                             ("lp" js2r-localize-parameter)
                             ("wi" js2r-wrap-buffer-in-iife)
                             ("ig" js2r-inject-global-in-iife)
                             ("ag" js2r-add-to-globals-annotation)
                             ("ev" js2r-extract-var)
                             ("iv" js2r-inline-var)
                             ("rv" js2r-rename-var)
                             ("vt" js2r-var-to-this)
                             ("ao" js2r-arguments-to-object)
                             ("ti" js2r-ternary-to-if)
                             ("sv" js2r-split-var-declaration)
                             ("ss" js2r-split-string)
                             ("uw" js2r-unwrap)
                             ("lt" js2r-log-this)
                             ("dt" js2r-debug-this)
                             ("sl" js2r-forward-slurp)
                             ("ba" js2r-forward-barf)
                             ("k" js2r-kill)
                             ("q" nil)))

;; json-snatcher
(use-package json-snatcher
  :defer t
  :commands jsons-print-path
  :bind (:map js2-mode-map
              ("C-c C-j" . jsons-print-path)
              :map js2-minor-mode-map
              ("C-c C-j" . jsons-print-path)))

;; json-mode
(use-package json-mode
  :defer t
  :mode "\\.json$"
  :commands json-mode
  :custom (js-indent-level 4)
  :hook (json-mode . js2-minor-mode))

;; json-reformat
(use-package json-reformat
  :defer t
  :after json-mode
  :commands json-reformat-region)

;; Bring node.js to Emacs
(use-package nodejs-repl
  :defer t
  :commands (nodejs-repl
             nodejs-repl-send-buffer
             nodejs-repl-switch-to-repl
             nodejs-repl-send-region
             nodejs-repl-send-last-sexp
             nodejs-repl-execute
             nodejs-repl-load-file)
  :if (executable-find "node"))

;; Elm mode
(use-package elm-mode
  :defer t
  :mode "\\.elm\\'"
  :custom (elm-format-on-save t))

;; Typescript mode
(use-package typescript-mode
  :defer t
  :custom (typescript-indent-level 4))

;; Tide
(use-package tide
  :defer t
  :commands tide-setup
  :hook ((typescript-mode js2-mode) . tide-setup))

;; Angular mode
(use-package ng2-mode
  :defer t
  :hook (ng2-ts-mode . typescript-mode)
  :config (flycheck-add-mode 'typescript-tide 'ng2-ts-mode))

(provide 'setup-javascript)

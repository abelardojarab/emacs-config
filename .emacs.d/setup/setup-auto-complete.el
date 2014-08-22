;;; setup-auto-complete.el ---

;; Copyright (C) 2014  abelardo.jara-berrocal

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

;; Auto-Complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(require 'auto-complete)
(ac-config-default)
(setq-default ac-sources '(ac-source-semantic-raw))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
(global-auto-complete-mode t)
(auto-complete-mode 1)
(setq ac-show-menu-immediately-on-auto-complete t)
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; General settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 30
 ac-quick-help-height 30)

;; Make it a bit faster
(setq
 ac-delay 0.5 ;; same as Eclipse
 ac-auto-show-menu 0.5
 ac-quick-help-delay 0.5)

;; this is used for trigger ac actions from org-mode also
(add-to-list 'ac-trigger-commands 'org-self-insert-command)

;; create and add new words to the dictionary on the fly
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories "~/.emacs.cache/ac-dict")
  (setq ac-comphist-file  "~/.emacs.cache/ac-comphist.dat")
  (ac-config-default))

;; Autocomplete with TAGS
(add-to-list 'load-path "~/.emacs.d/auto-complete-etags")
(require 'auto-complete-etags)
(setq ac-etags-use-document t)

;; Let's have snippets and TAGS in the auto-complete dropdown
(defun ac-common-setup ()
  (setq ac-sources (append ac-sources '(ac-source-yasnippet ac-source-etags ac-source-gtags ac-source-semantic ac-source-semantic-raw))))
(add-hook 'auto-complete-mode-hook 'ac-common-setup)

(provide 'setup-auto-complete)
;;; setup-auto-complete.el ends here

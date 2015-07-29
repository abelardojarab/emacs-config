;;; setup-eldoc.el ---

;; Copyright (C) 2014, 2015  abelardo.jara-berrocal

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

;; Eldoc
(require 'eldoc)
(require 'eldoc-extension)
(setq eldoc-echo-area-use-multiline-p t)
(setq eldoc-idle-delay 0)
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)))
(add-hook 'lisp-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)))
(add-hook 'python-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)))
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)
(set-face-attribute 'eldoc-highlight-function-argument nil :underline "red")

;; Eldoc support in minibuffer
(add-to-list 'load-path "~/.emacs.d/eldoc-eval")
(require 'eldoc-eval)
(eldoc-in-minibuffer-mode 1)

;; Setup Eldoc for C
(add-to-list 'load-path "~/.emacs.d/c-eldoc")
(add-to-list 'load-path "~/.emacs.d/deferred")
(require 'deferred)
(when (require 'c-eldoc nil 'noerror)
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode))

(provide 'setup-eldoc)
;;; setup-eldoc.el ends here

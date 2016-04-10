;;; setup-eldoc.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxcj9063.pdx.intel.com>
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

(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

(use-package c-eldoc
  :load-path (lambda () (expand-file-name "c-eldoc/" user-emacs-directory))
  :commands c-turn-on-eldoc-mode
  :config (progn
            (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
            (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
            (setq c-eldoc-buffer-regenerate-time 60)))

(provide 'setup-eldoc)
;;; setup-eldoc.el ends here

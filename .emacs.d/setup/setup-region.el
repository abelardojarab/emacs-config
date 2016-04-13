;;; setup-region.el ---                              -*- lexical-binding: t; -*-

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

;; Move text
(use-package move-text)

;; Region bindings mode
(use-package region-bindings-mode
  :load-path (lambda () (expand-file-name "region-bindings-mode/" user-emacs-directory))
  :diminish region-bindings-mode
  :config (progn
            (region-bindings-mode-enable)
            (define-key region-bindings-mode-map (kbd "C-c") 'kill-ring-save)
            (define-key region-bindings-mode-map (kbd "C-x") 'kill-region)

            ;; extra key bindings
            (define-key region-bindings-mode-map (kbd "d") 'move-text-down)
            (define-key region-bindings-mode-map (kbd "u") 'move-text-up)
            (define-key region-bindings-mode-map (kbd "<") 'decrease-left-margin)
            (define-key region-bindings-mode-map (kbd ">") 'increase-right-margin)
            (define-key region-bindings-mode-map (kbd "c") 'kill-ring-save)
            (define-key region-bindings-mode-map (kbd "x") 'kill-region)))

(provide 'setup-region)
;;; setup-region.el ends here

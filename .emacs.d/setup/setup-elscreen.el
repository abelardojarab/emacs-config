;;; setup-elscreen.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Abelardo Jara-Berrocal

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

;; helm elscreen
(use-package elscreen
  :defer t
  :load-path (lambda () (expand-file-name "elscreen/" user-emacs-directory))
  :config (progn
            (set-face-attribute 'elscreen-tab-background-face nil :inherit 'default :background nil)
            (setq-default elscreen-tab-display-control nil)
            (setq-default elscreen-tab-display-kill-screen nil)
            ;; (elscreen-set-prefix-key "\C-p")
            (elscreen-start)))

(provide 'setup-elscreen)
;;; setup-elscreen.el ends here

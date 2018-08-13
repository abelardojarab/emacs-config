;;; setup-jump.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Abelardo Jara-Berrocal

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

(use-package dumb-jump
  :defer t
  :commands (dumb-jump-go
             dumb-jump-back
             dumb-jump-quick-look
             dumb-jump-go-other-window)
  :config (setq dumb-jump-selector 'ivy
                dumb-jump-aggressive nil))

(use-package smart-jump
  :defer t
  :after dumb-jump
  :commands (smart-jump-go
             smart-jump-back
             smart-jump-references
             smart-jump-simple-find-references)
  :config (progn
            (smart-jump-setup-default-registers)
            (setq smart-jump-selector 'ivy)

            ;; Prefer rtags over ggtags
            (smart-jump-register :modes '(c-mode c++-mode)
                                 :jump-fn 'ggtags-find-tag-dwim
                                 :pop-fn 'ggtags-prev-mark
                                 :refs-fn 'ggtags-find-reference
                                 :should-jump t
                                 :heuristic 'point
                                 :async 500
                                 :order 2)

            (smart-jump-register :modes '(c-mode c++-mode)
                                 :jump-fn 'rtags-find-symbol-at-point
                                 :pop-fn 'rtags-location-stack-back
                                 :refs-fn 'rtags-find-all-references-at-point
                                 :should-jump (lambda ()
                                                (and
                                                 (fboundp 'rtags-executable-find)
                                                 (rtags-executable-find "rc")
                                                 (rtags-is-indexed)))
                                 :heuristic 'point
                                 :async 500
                                 :order 1)))

(provide 'setup-jump)
;;; setup-jump.el ends here

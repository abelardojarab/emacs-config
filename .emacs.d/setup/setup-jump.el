;;; setup-jump.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@ubuntu02>
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
  :bind (("M-g ." . dumb-jump-go)
         ("M-g ," . dumb-jump-back))
  :config (progn
            (setq dumb-jump-selector 'ivy
                  dumb-jump-aggressive nil)))

(use-package smart-jump
  :after dumb-jump
  :config (progn
            (smart-jump-setup-default-registers)

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

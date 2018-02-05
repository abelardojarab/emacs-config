;;; setup-compile.el ---

;; Copyright (C) 2018  Abelardo Jara-Berrocal

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

;; Better compile buffer
(use-package compile
  :demand t
  :config (progn

            ;; make sure ant's output is in a format emacs likes
            (setenv "ANT_ARGS" "-emacs")

            ;; Compilation
            ;; http://www.emacswiki.org/cgi-bin/wiki/ModeCompile
            (setq compilation-context-lines 1
                  compilation-context-lines 1
                  compilation-scroll-output 'first-error      ;; scroll until first error
                  compilation-read-command nil                ;; don't need enter
                  compilation-window-height 12                ;; keep it readable
                  compilation-auto-jump-to-first-error t      ;; jump to first error auto
                  compilation-auto-jump-to-next-error t)      ;; jump to next error

            ;; If the value is first-error, scrolling stops when the first error appears
            (setq compilation-scroll-output 'first-error)))

(provide 'setup-compile)
;;; setup-compile.el ends here

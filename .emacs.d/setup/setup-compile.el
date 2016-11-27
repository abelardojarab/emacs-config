;;; setup-compile.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@Abelardos-MacBook-Pro.local>
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

            ))

(add-hook 'c-mode-common-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -o %s %s %s"
                             (or (getenv "CC") "g++")
                             (file-name-sans-extension file)
                             ;;(or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") " -g -O2")
                             file))))))

;; makefiles
(add-hook 'makefile-mode-hook
          (lambda()
            (setq whitespace-style '(face trailing tabs))
            (whitespace-mode)))

(provide 'setup-compile)
;;; setup-compile.el ends here

;;; setup-compile.el ---

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

;; Close compile buffer if no errors nor warnings
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          **(goto-char 1)**
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; Helper for compilation. Close the compilation window if there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))

;; Compilation
;; http://www.emacswiki.org/cgi-bin/wiki/ModeCompile
(setq
 compilation-exit-message-function 'compilation-exit-autoclose
 compilation-scroll-output 'first-error      ;; scroll until first error
 compilation-read-command nil                  ;; don't need enter
 compilation-window-height 12                ;; keep it readable
 compilation-auto-jump-to-first-error t      ;; jump to first error auto
 compilation-auto-jump-to-next-error t)      ;; jump to next error

;; Make shell scrips executable on save. Good!
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Auto compile *.elc-files on save
(defun auto-byte-recompile ()
  "If the current buffer is in emacs-lisp-mode and there already exists an .elc file corresponding to the current buffer file, then recompile the file on save."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook 'auto-byte-recompile)

;; Better compile buffer
(require 'compile)
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq
             compilation-scroll-output 'first-error  ; scroll until first error
             ;; compilation-read-command nil          ; don't need enter
             compilation-window-height 11)

            (local-set-key (kbd "<M-up>")   'previous-error)
            (local-set-key (kbd "<M-down>") 'next-error)

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
                             file)))))
          ;; (number of things in " " in format must match number of arg. in getenv.)

          ;; This will run Make if there is a Makefile in the same directory as the
          ;; source-file, or it will create a command for compiling a single
          ;; file and name the executable the same name as the file with the extension
          ;; stripped.
          )

;; makefiles
(add-hook 'makefile-mode-hook
          (lambda()
            (setq whitespace-style '(face trailing tabs))
            (whitespace-mode)))

(provide 'setup-compile)
;;; setup-compile.el ends here

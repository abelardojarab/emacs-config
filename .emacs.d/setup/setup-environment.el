;;; setup-environment.el ---                 -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

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

;; These were defined in C code, so use emacs pseudo-package to set them.
(use-package emacs
  :demand t
  :hook ((focus-out                     . garbage-collect)
         (minibuffer-setup              . my/minibuffer-setup)
         (minibuffer-exit               . my/minibuffer-exit)
         (find-file-not-found-functions . make-parent-directory))
  :commands (my/minibuffer-exit
             my/minibuffer-setup
             my/show-init-time)
  :custom ((ad-redefinition-action                'accept)
           (byte-compile-warnings                 nil)
           (load-prefer-newer                     nil)
           (load-file-rep-suffixes                '(""))
           (require-final-newline                 'query)
           (inhibit-startup-message               t)
           (large-file-warning-threshold          nil)
           (enable-recursive-minibuffers          t)
           (visible-bell                          t)
           (ring-bell-function                    'ignore)
           (kill-ring-max                         100)
           (x-select-enable-clipboard             t)
           (select-active-regions                 t)
           (save-interprogram-paste-before-kill   1)
           (yank-pop-change-selection             t)
           (save-interprogram-paste-before-kill   t)
           (recenter-positions                    '(middle top bottom))
           (line-move-visual                      t)
           (next-line-add-newlines                nil)
           (message-truncate-lines                nil)
           (require-final-newline                 t)
           (debug-on-quit                         nil)
           (bidi-display-reordering               nil)
           (no-redraw-on-reenter                  t)
           (column-number-indicator-zero-based    nil)
           (track-eol                             t)
           (minibuffer-eldef-shorten-default      t)
           (register-preview-delay                2)
           (register-separator                    "\n\n")
           (history-delete-duplicates             t)
           (tab-always-indent                     'complete "smart tab behavior - indent or complete"))
  :init (progn
          ;; Allow some header variables
          (put 'encoding 'safe-local-variable (lambda (val) #'stringp))
          (put 'org-src-preserve-indentation 'safe-local-variable (lambda (val) #'booleanp))

          ;; Disable garbage collection messages
          (setq garbage-collection-messages nil)

          ;; Assure cache directory exists
          (if (not (file-exists-p  my/emacs-cache-dir))
              (make-directory my/emacs-cache-dir) t)

          ;; Improve Emacs performance
          (if (boundp 'max-specpdl-size)
              (setq max-specpdl-size (* max-specpdl-size 40)
                    max-lisp-eval-depth (* max-lisp-eval-depth 30)))

          (defun my/disable-garbage-collection ()
            "Disable garbage collection."
            (setq gc-cons-threshold most-positive-fixnum
                  gc-cons-percentage 0.6))

          (defun my/enable-garbage-collection ()
            "Reset garbage collection to small-ish limit."
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1))

          ;; Helm runs with GC disabled
          (add-hook 'minibuffer-setup-hook #'my/disable-garbage-collection)
          (add-hook 'minibuffer-exit-hook  #'my/enable-garbage-collection)

          ;; We disable GC during startup, we re-enable it afterwards.
          (my/disable-garbage-collection)
          (add-hook 'emacs-startup-hook #'my/enable-garbage-collection)

          ;; Perform automatic GC
          (add-hook 'emacs-startup-hook
                    (lambda ()
                      (if (boundp 'after-focus-change-function)
                          (add-function :after after-focus-change-function
                                        (lambda ()
                                          (unless (frame-focus-state)
                                            (garbage-collect))))
                        (add-hook 'after-focus-change-function 'garbage-collect))))

          ;; Features
          (put 'narrow-to-region 'disabled nil)
          (put 'narrow-to-page   'disabled nil)
          (put 'narrow-to-defun  'disabled nil))
  :config (progn
            ;; Disable word wrapping
            (setq-default word-wrap nil)

            ;; Show line-number in the mode line
            (line-number-mode 1)

            ;; Show column-number in the mode line
            (column-number-mode 1)

            ;; Edition of EMACS edition modes
            (setq major-mode 'text-mode)
            (add-hook 'text-mode-hook #'text-mode-hook-identify)

            ;; Make directory on-the-fly if non-existent
            (defun make-parent-directory ()
              "Make sure the directory of `buffer-file-name' exists."
              (make-directory (file-name-directory buffer-file-name) t))

            ;; set high gc limit for minibuffer so doesn't slowdown on helm etc
            (defun my/minibuffer-setup ()
              "Setup minibuffer."
              (setq gc-cons-threshold most-positive-fixnum))

            (defun my/minibuffer-exit ()
              "Undo minibuffer setup."
              (setq gc-cons-threshold (* 64 1024 1024)))

            ;; Printing
            (setq ps-paper-type        'a4
                  ps-font-size         7.0
                  ps-print-header      nil
                  ps-print-color-p     t
                  ps-landscape-mode    nil
                  ps-number-of-columns 1)

            ;; Define preferred shell
            (cond
             ((executable-find "bash")
              (setq shell-file-name "bash"))
             ((executable-find "csh")
              (setq shell-file-name "csh"))
             ((executable-find "cmdproxy")
              (setq shell-file-name "cmdproxy"))
             (t
              (setq shell-file-name "bash")))
            (setq explicit-shell-file-name shell-file-name)

            (cond

             ;; Linux
             ((equal system-type 'gnu/linux)

              ;; Prefer /bin/bash
              (if (executable-find "/bin/bash")
                  (setenv "SHELL" "/bin/bash"))

              (when (display-graphic-p)
                (setq x-select-request-type     '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
                      select-enable-clipboard   t
                      select-enable-primary     t)
                (when (functionp #'x-cut-buffer-or-selection-value)
                  (setq interprogram-paste-function #'x-cut-buffer-or-selection-value))

                (when (executable-find "xsel")
                  (defun xsel-cut-function (text &optional push)
                    (with-temp-buffer
                      (insert text)
                      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
                  (defun xsel-paste-function()

                    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
                      (unless (string= (car kill-ring) xsel-output)
                        xsel-output )))
                  (setq interprogram-cut-function 'xsel-cut-function)
                  (setq interprogram-paste-function 'xsel-paste-function)))

              ;; Default frame appearance
              (setq default-frame-alist
                    '((fringe-mode (quote (28 . 12)) nil (fringe))
                      (fringes-outside-margins nil t)
                      (right-fringe)
                      (left-fringe)
                      (left-fringe-width nil)
                      (right-fringe-width nil)
                      (frame-resize-pixelwise t)
                      (border-color . "black")
                      (menu-bar-lines . 1)))

              ;; Get back font antialiasing
              (push '(font-backend xft x) default-frame-alist)

              ;; Inspired by the windows version. Also used call-process here because
              ;; shell-command-to-string gave me 100% CPU usage by lisp.run until
              ;; kdialog returned.
              (defun kde-open-file ()
                (interactive)
                (let ((file-name
                       (replace-regexp-in-string "kbuildsycoca running..." ""
                                                 (replace-regexp-in-string
                                                  "[\n]+" ""
                                                  (shell-command-to-string "kdialog --getopenurl ~ 2> /dev/null")))))
                  (cond
                   ((string-match "^file://" file-name)
                    ;; Work arround a bug in kioexec, which causes it to delete local
                    ;; files. (See bugs.kde.org, Bug 127894.) Because of this we open the
                    ;; file with `find-file' instead of emacsclient.
                    (let ((local-file-name (substring file-name 7)))
                      (message "Opening local file '%s'" local-file-name)
                      (find-file local-file-name)))
                   ((string-match "^[:space:]*$" file-name)
                    (message "Empty file name given, doing nothing..."))
                   (t
                    (message "Opening remote file '%s'" file-name)
                    (save-window-excursion
                      (shell-command (concat "kioexec emacsclient " file-name "&"))))))))

             ;; Mac OSX
             ((equal system-type 'darwin)
              (setq delete-by-moving-to-trash t
                    trash-directory "~/.Trash/")
              ;; Keep the Option key as Meta
              (setq mac-option-modifier 'meta
                    mac-command-modifier 'meta)
              ;; BSD ls does not support --dired. Use GNU core-utils: brew install coreutils
              (when (executable-find "gls")
                (setq insert-directory-program "gls"))
              ;; Derive PATH by running a shell so that GUI Emacs sessions have access to it
              ;; (exec-path-from-shell-initialize)

              ;; Correctly parse exec-path when PATH delimiter is a space
              (when (equal (file-name-nondirectory (getenv "SHELL")) "fish")
                (setq exec-path (split-string (car exec-path) " "))
                (setenv "PATH" (mapconcat 'identity exec-path ":"))
                (setq eshell-path-env (getenv "PATH")))
              (setenv "PATH" (concat "/usr/local/texlive/2014/bin/x86_64-darwin:/usr/texbin:/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
              (push "/usr/local/bin" exec-path)
              (push "/opt/local/bin" exec-path)
              (push "/usr/texbin" exec-path)

              (defun my/mac-open-file ()
                (interactive)
                (let ((file (do-applescript "try
 POSIX path of (choose file)
 end try")))
                  (if (> (length file) 3)
                      (setq file
                            (substring file 1 (- (length file) 1))))
                  (if (and (not (equal file ""))
                           (file-readable-p file))
                      (find-file file)
                    (beep))))

              (defun my/mac-save-file-as ()
                (interactive)
                (let ((file (do-applescript "try
 POSIX path of (choose file name with prompt \"Save As...\")
 end try")))
                  (if (> (length file) 3)
                      (setq file
                            (substring file 1 (- (length file) 1))))
                  (if (not (equal file ""))
                      (write-file file)
                    (beep))))

              ;; Point Org to LibreOffice executable
              (when (file-exists-p "/Applications/LibreOffice.app/Contents/MacOS/soffice")
                (setq org-export-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")))))

             ;; Windows
             ((equal system-type 'windows-nt)
              (if (or (file-directory-p "c:/cygwin64/bin")
                      (file-directory-p "c:/cygwin64/bin"))
                  (require 'setup-cygwin))

              ;; Custom $PATH
              (when (file-directory-p "c:/cygwin/bin")
                (setenv "PATH" (concat "c:/cygwin/bin:" (getenv "PATH")))
                (add-to-list 'exec-path "c:/cygwin/bin")
                (setq explicit-shell-file-name "c:/cygwin/bin/bash.exe")
                (setq shell-file-name explicit-shell-file-name))
              (when (file-directory-p "c:/cygwin64/bin")
                (setenv "PATH" (concat "c:/cygwin64/bin:c:/cygwin64/usr/local/bin" (getenv "PATH")))
                (add-to-list 'exec-path "c:/cygwin64/bin")
                (add-to-list 'exec-path "c:/cygwin64/usr/local/bin")
                (setq explicit-shell-file-name "c:/cygwin64/bin/bash.exe")
                (setq shell-file-name explicit-shell-file-name))

              (setenv "SHELL" shell-file-name)
              (require 'w32browser-dlgopen)
              (setq dlgopen-executable-path (expand-file-name "elisp/getfile.exe" user-emacs-directory)))

             ;; Measure Emacs startup time
             (defun show-startup-time ()
               "Show Emacs's startup time in the minibuffer"
               (message "Startup time: %s seconds."
                        (emacs-uptime "%s")))
             (add-hook 'emacs-startup-hook #'show-startup-time 'append)

             ;; Benchmark initialization
             (defun my/time-subtract-millis (b a)
               (* 1000.0 (float-time (time-subtract b a))))

             (defvar my/require-times nil
               "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

             (defadvice require (around my/build-require-times (feature &optional filename noerror) activate)
               "Note in `my/require-times' the time taken to require each feature."
               (let* ((already-loaded (memq feature features))
                      (require-start-time (and (not already-loaded) (current-time))))
                 (prog1
                     ad-do-it
                   (when (and (not already-loaded) (memq feature features))
                     (let ((time (my/time-subtract-millis (current-time) require-start-time)))
                       (add-to-list 'my/require-times
                                    (cons feature time)
                                    t))))))

             (defun my/show-init-time ()
               (message "Emacs startup completed in: %.2fms"
                        (my/time-subtract-millis after-init-time before-init-time))))))

;; Exec path from shell in Mac OSX
(use-package exec-path-from-shell
  :defer t
  :if (equal system-type 'darwin)
  :config (exec-path-from-shell-initialize)
  :commands (exec-path-from-shell-initialize)
  :custom (exec-path-from-shell-check-startup-files nil))

;; Display the time in the mode-line
(use-package time
  :demand t
  :custom ((display-time-default-load-average nil)
           (display-time-24hr-format          t)
           (display-time-use-mail-icon        t))
  :config (display-time-mode t))

;; Automatic garbage collection
(use-package gcmh
  :disabled t
  :delight gcmh-mode
  :commands gcmh-mode
  :custom (gcmh-verbose nil)
  :config (gcmh-mode 1))

;; Fix missing maps
(if (not (boundp 'minibuffer-local-must-match-map))
    (defvar minibuffer-local-must-match-map (make-sparse-keymap)))

(if (not (boundp 'minibuffer-local-must-match-filename-map))
    (defvar minibuffer-local-must-match-filename-map (make-sparse-keymap)))

(provide 'setup-environment)
;;; setup-environment.el ends here

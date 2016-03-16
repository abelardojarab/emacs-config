;;; setup-environment.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

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

;; garbage collection
(setq gc-cons-threshold 256000000)

;; ignore byte-compile warnings
(setq byte-compile-warnings nil)

;; Assure .emacs.cache directory exists
(if (not (file-exists-p (expand-file-name "~/.emacs.cache")))
    (make-directory (expand-file-name "~/.emacs.cache") t))

;; Inhibit startup window, very annoying
(setq inhibit-startup-message t)

;; Disable tool-bar
(if window-system
    (tool-bar-mode -1))

;; Printing
(setq ps-paper-type 'a4
      ps-font-size 7.0
      ps-print-header nil
      ps-print-color-p t
      ps-landscape-mode nil    ;; for two pages per page: t
      ps-number-of-columns 1)  ;; for two pages per page: 2

;; Exec path from shell in Mac OSX
(when (equal system-type 'darwin)
  (add-to-list 'load-path "~/.emacs.d/exec-path-from-shell")
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

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

;; GUI-specific thing
(when (window-system)
  (setenv "EMACS_GUI" "t"))

(cond

 ;; Linux
 ((equal system-type 'gnu/linux)

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
  ;; BSD ls does not support --dired. Use GNU core-utils: brew install coreutils
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))
  ;; Derive PATH by running a shell so that GUI Emacs sessions have access to it
  (exec-path-from-shell-initialize)
  ;; Correctly parse exec-path when PATH delimiter is a space
  (when (equal (file-name-nondirectory (getenv "SHELL")) "fish")
    (setq exec-path (split-string (car exec-path) " "))
    (setenv "PATH" (mapconcat 'identity exec-path ":"))
    (setq eshell-path-env (getenv "PATH")))
  (setenv "PATH" (concat "/usr/texbin:/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/bin" exec-path)
  (push "/opt/local/bin" exec-path)
  (push "/usr/texbin" exec-path)

  (defun mac-open-file ()
    (interactive)
    (let ((file (do-applescript "try
 POSIX path of (choose file)
 end try")))
      (if (> (length file) 3)
          (setq file
                (substring file 1 (- (length file) 1))
                ))
      (if (and (not (equal file ""))(file-readable-p file))
          (find-file file)
        (beep))))

  (defun mac-save-file-as ()
    (interactive)
    (let ((file (do-applescript "try
 POSIX path of (choose file name with prompt \"Save As...\")
 end try")))
      (if (> (length file) 3)
          (setq file
                (substring file 1 (- (length file) 1))
                ))
      (if (not (equal file ""))
          (write-file file)
        (beep))))

  ;; Point Org to LibreOffice executable
  (when (file-exists-p "/Applications/LibreOffice.app/Contents/MacOS/soffice")
    (setq org-export-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")))))

 ;; Windows
 ((equal system-type 'windows-nt)

  ;; Custom $PATH
  (when (file-directory-p "c:/cygwin/bin")
    (setenv "PATH" (concat "c:/cygwin/bin:" (getenv "PATH")))
    (add-to-list 'exec-path "c:/cygwin/bin")
    (setq explicit-shell-file-name "C:/cygwin/bin/bash.exe")
    (setq shell-file-name explicit-shell-file-name))
  (when (file-directory-p "c:/cygwin64/bin")
    (setenv "PATH" (concat "c:/cygwin64/bin:" (getenv "PATH")))
    (add-to-list 'exec-path "c:/cygwin64/bin")
    (setq explicit-shell-file-name "C:/cygwin64/bin/bash.exe")
    (setq shell-file-name explicit-shell-file-name))

  (setenv "SHELL" shell-file-name)
  (require 'w32browser-dlgopen)
  (setq dlgopen-executable-path "~/.emacs.d/elisp/getfile.exe")))

;; Trick emacs when opening a file through menu-find-file-existing
(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil)
        (use-dialog-box t))
    ad-do-it))

;; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

;; Garantee utf8 as input-method
(set-input-method nil)
(setq read-quoted-char-radix 10)
(set-language-environment 'utf-8)
(set-locale-environment "en_US.UTF-8")
(setq locale-coding-system 'utf-8-unix)

;; Coding system
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Even so, ansi-term doesn’t obey:
(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

;; update the copyright when present
(add-hook 'before-save-hook 'copyright-update)

;; Emacs is a text editor, make sure your text files end in a newline
(setq require-final-newline 'query)

;; Automatically kill all spawned processes on exit
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                         (save-excursion
                           (untabify (point-min) (point-max)))) nil))

;; Always split horizontally
(setq split-width-threshold 78)
(setq split-height-threshold nil)

;; Helper function for horizontal splitting
(defun split-horizontally-for-temp-buffers ()
  "Split the window horizontally for temp buffers."
  (when (and (one-window-p t)
             (not (active-minibuffer-window)))
    (split-window-horizontally)))
(add-hook 'temp-buffer-setup-hook 'split-horizontally-for-temp-buffers)

;; horizontal splitting - when opening files or buffers with C-x4b or C-x4f
(defun split-window-prefer-horizonally (window)
  "If there's only one window (excluding any possibly active
         minibuffer), then split the current window horizontally."
  (if (and (one-window-p t)
           (not (active-minibuffer-window)))
      (let ((split-height-threshold nil))
        (split-window-sensibly window))
    (split-window-sensibly window)))
(setq split-window-preferred-function 'split-window-prefer-horizonally)

;; Activate highlight in search and replace
(setq search-highlight t)
(setq query-replace-highlight t)

;; Enable tooltips
(tooltip-mode t)
(setq tooltip-use-echo-area t)

;; deleting files goes to OS's trash folder
(setq delete-by-moving-to-trash t)

;; Put something different in the scratch buffer
(setq initial-scratch-message "Start typing...")

;; Bell instead of annoying beep
(setq visible-bell t)

;; Turn off the bell http://www.emacswiki.org/cgi-bin/wiki?AlarmBell
(setq ring-bell-function 'ignore)

;; Do not add empty lines at the end of our file if we press down key
(setq next-line-add-newlines nil)

;; Keep height of the echo area equal to only 1 line
(setq message-truncate-lines t)

;; Makes final line always be a return
(setq require-final-newline t)

;; Set indent to 4 instead of 2
(setq standard-indent 4)

;; Use spaces instead of tab
(setq-default indent-tabs-mode nil)

;; Set tab width
(setq-default default-tab-width 4)

;; auto-indent pasted code
(defadvice yank (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode lisp-mode c-mode c++-mode
                                objc-mode latex-mode plain-tex-mode python-mode java-mode js2-mode))
      (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode lisp-mode c-mode c++-mode
                                objc-mode latex-mode plain-tex-mode python-mode java-mode js2-mode))
      (indent-region (region-beginning) (region-end) nil)))

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; Ignore case when looking for a file
(setq read-file-name-completion-ignore-case t)

;; Edition of EMACS edition modes
(setq major-mode 'text-mode)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook (function
                           (lambda () (ispell-minor-mode))))

;; Pretty diff mode
(autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files-remote "ediff" "Intelligent Emacs interface to diff" t)

;; Change type files
(setq auto-mode-alist
      (append '(("\\.cpp$" . c++-mode)
                ("\\.h$" . c++-mode)
                ("\\.hpp$" . c++-mode)
                ("\\.lsp$" . lisp-mode)
                ("\\.il$" . lisp-mode)
                ("\\.ils$" . lisp-mode)
                ("\\.scm$" . scheme-mode)
                ("\\.pl$" . perl-mode)
                ) auto-mode-alist))

;; Time stamp
(setq
 time-stamp-active t          ;; do enable time-stamps
 time-stamp-line-limit 20     ;; check first 10 buffer lines for Time-stamp:
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ;; date format
(add-hook 'write-file-hooks 'time-stamp) ;; update when saving

;; More exhaustive cleaning of white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make URLs in comments/strings clickable, (emacs > v22)
(add-hook 'find-file-hooks 'goto-address-prog-mode)

;; Marker if the line goes beyond the end of the screen (arrows)
(global-visual-line-mode 1)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
(add-hook 'prog-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))

;; Smoother scrolling
(setq redisplay-dont-pause nil
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;; Optimization
(setq-default bidi-display-reordering nil)

;; Do not redraw entire frame after suspending.
(setq no-redraw-on-reenter t)

;; Modify toggle truncate lines to avoid messages
(defun toggle-truncate-lines (&optional arg)
  "Toggle truncating of long lines for the current buffer.
When truncating is off, long lines are folded.
With prefix argument ARG, truncate long lines if ARG is positive,
otherwise fold them.  Note that in side-by-side windows, this
command has no effect if `truncate-partial-width-windows' is
non-nil."
  (interactive "P")
  (setq truncate-lines
        (if (null arg)
            (not truncate-lines)
          (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update)
  (unless truncate-lines
    (let ((buffer (current-buffer)))
      (walk-windows (lambda (window)
                      (if (eq buffer (window-buffer window))
                          (set-window-hscroll window 0)))
                    nil t)))
  t)

;; Measure Emacs startup time
(defun show-startup-time ()
  "Show Emacs's startup time in the minibuffer"
  (message "Startup time: %s seconds."
           (emacs-uptime "%s")))
(add-hook 'emacs-startup-hook 'show-startup-time 'append)

;; Syntax coloring
(global-font-lock-mode t)
(global-hi-lock-mode nil)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size (* 512 512))
(defun global-font-lock-mode-check-buffers () nil)

;; Lazy font lock
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-chunk-size 5000
      jit-lock-context-time 0.2
      jit-lock-defer-time .1
      jit-lock-stealth-nice 0.5
      jit-lock-stealth-time 16
      jit-lock-stealth-verbose nil)
(setq-default font-lock-multiline t)

;; Do not fontify large files
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 512 256))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;; Put a nice title to the window, including filename
(add-hook 'window-configuration-change-hook
          (lambda ()
            (setq frame-title-format
                  (concat
                   invocation-name "@" system-name ": "
                   (replace-regexp-in-string
                    (concat "/home/" user-login-name) "~"
                    (or buffer-file-name "%b"))))))

;; Scrollbar
(when window-system
  (set-scroll-bar-mode 'right)

  ;; Smart scrollbar
  (defvar regexp-always-scroll-bar '("\\.yes" "\\*Scroll-Bar\\*")
    "Regexp matching buffer names that will always have scroll bars.")

  (defvar regexp-never-scroll-bar '("\\.off" "\\.not")
    "Regexp matching buffer names that will never have scroll bars.")

  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (modify-all-frames-parameters (list (cons 'vertical-scroll-bars nil)))

  (defun lawlist-scroll-bar ()
    (ignore-errors
      (when (window-live-p (get-buffer-window (current-buffer)))
        (redisplay t)
        (cond
         ;; not regexp matches | not narrow-to-region
         ((and
           (not (regexp-match-p regexp-always-scroll-bar (buffer-name)))
           (not (regexp-match-p regexp-never-scroll-bar (buffer-name)))
           (equal (- (point-max) (point-min)) (buffer-size)))
          (cond
           ;; Lines of text are less-than or equal-to window height,
           ;; and scroll bars are present (which need to be removed).
           ((and
             (<= (- (point-max) (point-min)) (- (window-end) (window-start)))
             (equal (window-scroll-bars) `(15 2 right nil)))
            (set-window-scroll-bars (selected-window) 0 'right nil))
           ;; Lines of text are greater-than window height, and
           ;; scroll bars are not present and need to be added.
           ((and
             (> (- (point-max) (point-min)) (- (window-end) (window-start)))
             (not (equal (window-scroll-bars) `(15 2 right nil))))
            (set-window-scroll-bars (selected-window) 15 'right nil))))
         ;; Narrow-to-region is active, and scroll bars are present
         ;; (which need to be removed).
         ((and
           (not (equal (- (point-max) (point-min)) (buffer-size)))
           (equal (window-scroll-bars) `(15 2 right nil)))
          (set-window-scroll-bars (selected-window) 0 'right nil))
         ;; not narrow-to-region | regexp always scroll-bars
         ((and
           (equal (- (point-max) (point-min)) (buffer-size))
           (regexp-match-p regexp-always-scroll-bar (buffer-name)))
          (set-window-scroll-bars (selected-window) 15 'right nil))
         ;; not narrow-to-region | regexp never scroll-bars
         ((and
           (equal (- (point-max) (point-min)) (buffer-size))
           (regexp-match-p regexp-never-scroll-bar (buffer-name)))
          (set-window-scroll-bars (selected-window) 0 'right nil))))))

  (define-minor-mode lawlist-scroll-bar-mode
    "This is a custom scroll bar mode."
    :lighter " sc"
    (if lawlist-scroll-bar-mode
        (progn
          (add-hook 'post-command-hook 'lawlist-scroll-bar nil t))
      (remove-hook 'post-command-hook 'lawlist-scroll-bar t)
      (remove-hook 'change-major-mode-hook 'lawlist-scroll-bar t)
      (remove-hook 'window-configuration-change-hook 'lawlist-scroll-bar t)))

  (define-globalized-minor-mode global-lawlist-scroll-bar-mode
    lawlist-scroll-bar-mode lawlist-scroll-bar-on)

  (defun lawlist-scroll-bar-on ()
    (unless (minibufferp)
      (lawlist-scroll-bar-mode 1)))

  (global-lawlist-scroll-bar-mode))

;; higlight changes in documents
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)

;; Fix highlight bug of marking a file as modified
(defadvice highlight-changes-rotate-faces (around around-rotate-faces)
  (let ((was-modified (buffer-modified-p))
        (buffer-undo-list t))
    ad-do-it
    (unless was-modified
      (set-buffer-modified-p nil))))
(ad-activate 'highlight-changes-rotate-faces)

(provide 'setup-environment)

;;; setup-general.el ---

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

;; Missing function
(when (not (fboundp 'define-error))
  (defun define-error (name message &optional parent)
    "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
    (unless parent (setq parent 'error))
    (let ((conditions
           (if (consp parent)
               (apply #'nconc
                      (mapcar (lambda (parent)
                                (cons parent
                                      (or (get parent 'error-conditions)
                                          (error "Unknown signal `%s'" parent))))
                              parent))
             (cons parent (get parent 'error-conditions)))))
      (put name 'error-conditions
           (delete-dups (copy-sequence (cons name conditions))))
      (when message (put name 'error-message message)))))

;; Missing variable
(defvar scheme-imenu-generic-expression "")

;; Missing variable
(defvar cursor-sensor-inhibit nil)

;; Assure .emacs.cache directory exists
(if (not (file-exists-p (expand-file-name "~/.emacs.cache")))
    (make-directory (expand-file-name "~/.emacs.cache") t))

;; Garbage collection
(setq gc-cons-threshold 120000000)

;; Inhibit startup window, very annoying
(setq inhibit-startup-message t)

;; Disable tool-bar
(if window-system
    (tool-bar-mode -1))

;; Undefined function
(require 'let-alist)

;; Printing
(setq ps-paper-type 'a4
      ps-font-size 7.0
      ps-print-header nil
      ps-print-color-p t
      ps-landscape-mode nil    ; for two pages per page: t
      ps-number-of-columns 1)  ; for two pages per page: 2

;; Namespace implementation
(add-to-list 'load-path "~/.emacs.d/names")
(require 'names)

;; Exec path from shell
(add-to-list 'load-path "~/.emacs.d/exec-path-from-shell")
(require 'exec-path-from-shell)

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

;; Popup, used by auto-complete and other tools
(add-to-list 'load-path "~/.emacs.d/popup")
(require 'popup)
(setq popup-use-optimized-column-computation nil)

;; Fix indent guide issue
(defvar sanityinc/indent-guide-mode-suppressed nil)
(defadvice popup-create (before indent-guide-mode activate)
  "Suspend indent-guide-mode while popups are visible"
  (let ((indent-guide-enabled (and (boundp 'indent-guide-mode) indent-guide-mode)))
    (set (make-local-variable 'sanityinc/indent-guide-mode-suppressed) indent-guide-mode)
    (when indent-guide-enabled
      (indent-guide-mode nil))))
(defadvice popup-delete (after indent-guide-mode activate)
  "Restore indent-guide-mode when all popups have closed"
  (let ((indent-guide-enabled (and (boundp 'indent-guide-mode) indent-guide-mode)))
    (when (and (not popup-instances) sanityinc/indent-guide-mode-suppressed)
      (setq sanityinc/indent-guide-mode-suppressed nil)
      (indent-guide-mode 1))))

;; Manage popup windows
(add-to-list 'load-path "~/.emacs.d/popwin")
(require 'popwin)

;; suspress warnings
(setq warning-minimum-level :error)

;; Automatically save and restore sessions
(require 'desktop)
(setq-default desktop-missing-file-warning nil)
(if (boundp 'desktop-auto-save-timeout)
    (setq desktop-auto-save-timeout (* 60 60)))
(setq desktop-dirname             "~/.emacs.cache/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;; reload tramp paths
      desktop-load-locked-desktop t
      desktop-save 'ask-if-new
      desktop-file-name-format 'absolute
      desktop-restore-frames nil
      desktop-restore-in-current-display t
      desktop-restore-forces-onscreen nil
      desktop-globals-to-save
      '((extended-command-history . 30)
        (file-name-history        . 100)
        (grep-history             . 30)
        (compile-history          . 30)
        (minibuffer-history       . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        (search-ring              . 20)
        (shell-command-history    . 50)
        tags-file-name
        register-alist)
      desktop-locals-to-save
      (nconc '(word-wrap line-move-visual) desktop-locals-to-save))
(setq desktop-files-not-to-save
      "\\(^/[^/:]*:\\|(ftp)$\\)\\|\\(^/tmp/\\)\\|\\(.gpg$\\)")
(setq desktop-buffers-not-to-save
      (concat "\\(" "^nn\\.a[0-9]+\\|\\ECB\\|\\.log\\|(ftp)"
              "\\)$"))

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
(add-to-list 'desktop-modes-not-to-save 'DocView-mode)

;; buffer-display-time is changed when desktop is loaded
(add-to-list 'desktop-locals-to-save 'buffer-display-time-1)
(make-variable-buffer-local 'buffer-display-time-1)
(defun save-buffer-display-time ()
  (mapc (lambda (buf)
          (with-current-buffer buf
            (setq buffer-display-time-1
                  (or buffer-display-time (current-time)))))
        (buffer-list)))
(add-hook 'desktop-save-hook 'save-buffer-display-time)

(defun set-buffer-display-time ()
  (mapc (lambda (buf)
          (with-current-buffer buf
            (setq buffer-display-time buffer-display-time-1)))
        (buffer-list)))
(add-hook 'desktop-after-read-hook 'set-buffer-display-time)
(desktop-save-mode 1)

;; Activate highlight in search and replace
(setq search-highlight t)
(setq query-replace-highlight t)

;; Show-paren-mode: subtle blinking of matching paren (defaults are ugly)
(show-paren-mode 1)

;; Show paren-mode when off-screen
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))

;; Opening bracket to be highlighted when the point is on the closing bracket
(defadvice show-paren-function
    (around show-paren-closing-before
            activate compile)
  (if (eq (syntax-class (syntax-after (point))) 5)
      (save-excursion
        (forward-char)
        ad-do-it)
    ad-do-it))

;; Smartparens
(add-to-list 'load-path "~/.emacs.d/smartparens")
(require 'smartparens-config)
(show-smartparens-global-mode 1)

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

;; Auto-indent mode
(add-to-list 'load-path "~/.emacs.d/auto-indent-mode")
(setq auto-indent-indent-style 'conservative)
(setq auto-indent-on-visit-file t) ;; do not indent when a file is visit
(setq auto-indent-blank-lines-on-move nil)
(setq auto-indent-next-pair-timer-geo-mean (quote ((default 0.0005 0))))
(setq auto-indent-disabled-modes-list (list (quote vhdl-mode)))
(require 'auto-indent-mode)
(auto-indent-global-mode)

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

;; Autosave
(setq auto-save-default nil)
(setq auto-save-interval 500)
(defvar my-auto-save-folder "~/.emacs.cache/auto-save/") ;; folder for auto-saves
(setq auto-save-list-file-prefix "~/.emacs.cache/auto-save/.saves-") ;; set prefix for auto-saves
(setq auto-save-file-name-transforms `((".*" ,my-auto-save-folder t))) ;; location for all auto-save files
(setq tramp-auto-save-directory my-auto-save-folder) ;; auyto-save tramp files in local directory

;; Backups
(setq make-backup-files t
      backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.cache/backups"))
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t)

;; Preserve the owner and group of the file you're editing
(setq backup-by-copying-when-mismatch t)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; Remember the position where we closed a file
(setq save-place-file "~/.emacs.cache/emacs.saveplace")
(setq-default save-place t) ;; activate it for all buffers
(require 'saveplace)

;; Savehist: save some history
(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 120
      savehist-file "~/.emacs.cache/savehist")
(savehist-mode t)

;; filecache: http://www.emacswiki.org/cgi-bin/wiki/FileNameCache
(eval-after-load "filecache"
  '(progn (message "Loading file cache...")
          (file-cache-add-directory "~/")
          (file-cache-add-directory-list '("~/Desktop" "~/Documents" "~/workspace"))))

;; Ignore case when looking for a file
(setq read-file-name-completion-ignore-case t)

;; Search at point
(require 'thingatpt)
(require 'thingatpt+)

;; Better search, similar to vim
(require 'isearch-prop)
(require 'isearch+)
(defun my-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun my-isearch-yank-word-hook ()
  (when (equal this-command 'my-isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                           (progn (skip-syntax-backward "w_") (point))
                           (progn (skip-syntax-forward "w_") (point)))
                          "\\>")))
      (if (and isearch-case-fold-search
               (eq 'not-yanks search-upper-case))
          (setq string (downcase string)))
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))
(add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)

;; Keep the search results in the center in incremental search
(defadvice isearch-repeat-forward (after isearch-repeat-forward-recenter activate)
  (recenter))
(defadvice isearch-repeat-backward (after isearch-repeat-backward-recenter activate)
  (recenter))
(ad-activate 'isearch-repeat-forward)
(ad-activate 'isearch-repeat-backward)

;; Edition of EMACS edition modes
(setq major-mode 'text-mode)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook (function
                           (lambda () (ispell-minor-mode))))

;; Pretty diff mode
(autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files-remote "ediff"
  "Intelligent Emacs interface to diff")

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

;; Turn on subword-mode for non-lispy languages
(add-hook 'c-mode-hook 'subword-mode)
(mapc (lambda (mode)
        (add-hook mode 'subword-mode))
      '(c-common-mode-hook
        python-mode-hook
        js2-mode-hook
        java-mode-hook))

;; Time stamp
(setq
 time-stamp-active t          ;; do enable time-stamps
 time-stamp-line-limit 20     ;; check first 10 buffer lines for Time-stamp:
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ;; date format
(add-hook 'write-file-hooks 'time-stamp) ;; update when saving

;; Uniquify-buffers
(require 'uniquify)  ;; make buffer names more unique
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 uniquify-after-kill-buffer-p t       ;; rename after killing uniquified
 uniquify-ignore-buffers-re "^\\*")  ;; don't muck with special buffers

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

;; imenu list
(add-to-list 'load-path "~/.emacs.d/imenu-list")
(require 'imenu-list)

;; Browse kill ring
(add-to-list 'load-path "~/.emacs.d/browse-kill-ring")
(require 'browse-kill-ring)

;; Autoinsert skeletons and templates
(require 'autoinsert)
(auto-insert-mode t)

;; This turns off the prompt that auto-insert-mode asks before
;; it actually inserts text/code for you
(setq auto-insert-query nil)

;; Line numbers
(require 'linum)
(global-linum-mode t)
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

;; Multiple cursors
(add-to-list 'load-path "~/.emacs.d/multiple-cursors")
(require 'multiple-cursors)

;; Benchmark-init can give us a breakdown of time spent on require and load calls:
(add-to-list 'load-path "~/.emacs.d/benchmark-init")
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;; Measure Emacs startup time
(defun show-startup-time ()
  "Show Emacs's startup time in the minibuffer"
  (message "Startup time: %s seconds."
           (emacs-uptime "%s")))
(add-hook 'emacs-startup-hook 'show-startup-time 'append)

(provide 'setup-general)
;;; setup-general.el ends here

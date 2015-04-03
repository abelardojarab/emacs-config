;;; setup-general.el ---

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

;; Inhibit startup window, very annoying
(setq inhibit-startup-message t)

;; Disable tool-bar
(tool-bar-mode -1)

;; Undefined function
(require 'let-alist)

;; Advice function
(require 'nadvice)

;; GUI-specific thing
(when (window-system)
  (setenv "EMACS_GUI" "t"))

;; Backported function
(when (version< emacs-version "24.4")
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

;; Printing
;; 2 column landscape size 7 prints column 0-78, lines 1 to 70
(setq ps-paper-type 'a4
      ps-font-size 7.0
      ps-print-header nil
      ps-print-color-p t
      ps-landscape-mode nil    ; for two pages per page: t
      ps-number-of-columns 1)  ; for two pages per page: 2

;; Extra stuff
(add-to-list 'load-path "~/.emacs.d/makey")
(add-to-list 'load-path "~/.emacs.d/discover")
(require 'discover)

;; Set path environment depending on OS.
(add-to-list 'load-path "~/.emacs.d/exec-path-from-shell")
(require 'exec-path-from-shell)

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

;; avoid garbage collection up to 20M (default is only 400k)
(setq gc-cons-threshold 20000000)

;; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

;; turn-off info about of pointee for symlinks
(setq dired-listing-switches "-alLF")

;; Make ?, ? and such work
(set-language-environment 'spanish)

;; Garantee utf8 as input-method
(set-input-method nil)
(setq read-quoted-char-radix 10)
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

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

;; Try not to split windows
(setq same-window-regexps '("."))

;; Assure window is splitted horizontally (for compilation buffer)
(setq split-width-threshold nil)

;; Avoid to make a separate frame
(setq display-buffer nil)
(setq display-buffer-reuse-frames t)
(setq pop-up-frames nil)
(setq menu-bar-select-buffer-function 'switch-to-buffer)

;; Popup, used by auto-complete and other tools
(add-to-list 'load-path "~/.emacs.d/popup")
(require 'popup)

;; Avoid popup windows too
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
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop t
      desktop-save 'ask-if-new
      desktop-file-name-format 'absolute
      desktop-restore-frames nil
      desktop-restore-in-current-display t
      desktop-restore-forces-onscreen nil
      desktop-restore-eager t
      desktop-lazy-verbose t
      desktop-lazy-idle-delay 5
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

;; Disable tooltips
(tooltip-mode nil)

;; deleting files goes to OS's trash folder
(setq delete-by-moving-to-trash t)

;; Put something different in the scratch buffer
(setq initial-scratch-message "Start typing...")

;; Automatically reload files after they've been modified
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

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

;; When in text (or related mode) break the lines at 80 chars
(require 'fill-column-indicator)
(setq fill-column 80)

;; Filladapt mode for text files
(require 'filladapt)
(add-hook 'tex-mode-hook 'turn-on-filladapt-mode)
(add-hook 'text-mode-hook 'turn-on-filladapt-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'c-mode-hook 'turn-off-filladapt-mode)

;; Cut the lines at 80 characters; I dont like it but it is a convention
(add-hook 'c++-mode-hook 'turn-on-auto-fill)
(add-hook 'c-mode-hook 'turn-on-auto-fill)
(add-hook 'vhdl-mode-hook 'turn-on-auto-fill)
(add-hook 'python-mode-hook 'turn-on-auto-fill)
(add-hook 'js2-mode-hook 'turn-on-auto-fill)

;; Set indent to 4 instead of 2
(setq standard-indent 4)

;; Use spaces instead of tab
(setq-default indent-tabs-mode nil)

;; Set tab width
(setq-default default-tab-width 4)

;; Auto-indent mode
(add-to-list 'load-path "~/.emacs.d/auto-indent-mode")
(setq auto-indent-on-visit-file t) ;; do not indent when a file is visit
(setq auto-indent-blank-lines-on-move nil)
(setq auto-indent-next-pair-timer-geo-mean (quote ((default 0.0005 0))))
(require 'auto-indent-mode)
(auto-indent-global-mode)

;; auto-indent pasted code
(defadvice yank (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode lisp-mode c-mode c++-mode
                                objc-mode latex-mode plain-tex-mode python-mode js2-mode))
      (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode lisp-mode c-mode c++-mode
                                objc-mode latex-mode plain-tex-mode python-mode))
      (indent-region (region-beginning) (region-end) nil)))

;; Autosave
(setq auto-save-interval 500)
(defvar my-auto-save-folder "~/.emacs.cache/auto-save/") ;; folder for auto-saves
(setq auto-save-list-file-prefix "~/.emacs.cache/auto-save/.saves-") ;; set prefix for auto-saves
(setq auto-save-file-name-transforms `((".*" ,my-auto-save-folder t))) ;; location for all auto-save files
(setq tramp-auto-save-directory my-auto-save-folder) ;; auto-save tramp files in local directory

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

;; https://github.com/kentaro/auto-save-buffers-enhanced
;; `regexp-match-p` function modified by @sds on stackoverflow
;; http://stackoverflow.com/questions/20343048/distinguishing-files-with-extensions-from-hidden-files-and-no-extensions
(defun regexp-match-p (regexps string)
  (and string
       (catch 'matched
         (let ((inhibit-changing-match-data t)) ; small optimization
           (dolist (regexp regexps)
             (when (string-match regexp string)
               (throw 'matched t)))))))

;; Better search, similar to vim
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
(global-set-key (kbd "C-*") 'my-isearch-word-at-point)
(global-set-key [(control kp-multiply)] 'my-isearch-word-at-point)

;; Keep the search results in the center in incremental search
(defadvice isearch-repeat-forward (after isearch-repeat-forward-recenter activate)
  (recenter))

(defadvice isearch-repeat-backward (after isearch-repeat-backward-recenter activate)
  (recenter))

(ad-activate 'isearch-repeat-forward)
(ad-activate 'isearch-repeat-backward)

;; Search at point
(require 'thingatpt)
(require 'thingatpt+)
(define-key isearch-mode-map (kbd "C-*")
  (lambda ()
    "Reset current isearch to a word-mode search of the word under point."
    (interactive)
    (setq isearch-word t
          isearch-string ""
          isearch-message "")
    (isearch-yank-string (word-at-point))))

;; Edition of EMACS edition modes
(setq major-mode 'text-mode)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
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
      '(c-mode-hook
        c++-mode-hook
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
(when (require 'uniquify nil 'noerror)  ;; make buffer names more unique
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"
   uniquify-after-kill-buffer-p t       ;; rename after killing uniquified
   uniquify-ignore-buffers-re "^\\*"))  ;; don't muck with special buffers

;; Ethan whitepsace, the remove trailing whitespace causes problem with buffer-modified-p
(add-to-list 'load-path "~/.emacs.d/whitespace-cleanup-mode")
(require 'whitespace-cleanup-mode)

;; More exhaustive cleaning of white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make URLs in comments/strings clickable, (emacs > v22)
(add-hook 'find-file-hooks 'goto-address-prog-mode)

;; Better help
(require 'help-fns+)

;; Fix longlines in Org
(require 'longlines nil t)
(setq longlines-wrap-follows-window-size t)
(add-hook 'org-mode-hook #'longlines-mode)

(defun longlines-encode-region (beg end &optional _buffer)
  "Replace each soft newline between BEG and END with exactly one space.
Hard newlines are left intact. The optional argument BUFFER exists for
compatibility with `format-alist', and is ignored."
  (save-excursion
    (let ((reg-max (max beg end))
          (mod (buffer-modified-p)))
      (goto-char (min beg end))
      ;; Changed this line to "swallow" indendation when decoding.
      (while (search-forward-regexp " *\\(\n\\) *" reg-max t)
        (let ((pos (match-beginning 1)))
          (unless (get-text-property pos 'hard)
            (goto-char (match-end 0))   ; This line too
            (insert-and-inherit " ")
            (replace-match "" :fixedcase :literal) ; This line too
            (remove-text-properties pos (1+ pos) 'hard))))
      (set-buffer-modified-p mod)
      end)))

(defun longlines-wrap-line ()
  "If the current line needs to be wrapped, wrap it and return nil.
If wrapping is performed, point remains on the line. If the line does
not need to be wrapped, move point to the next line and return t."
  (if (and (bound-and-true-p latex-extra-mode)
           (null (latex/do-auto-fill-p)))
      (progn (forward-line 1) t)
    ;; The conditional above was added for latex equations. It relies
    ;; on the latex-extra package (on Melpa).
    (if (and (longlines-set-breakpoint)
             ;; Make sure we don't break comments.
             (null (nth 4 (parse-partial-sexp
                           (line-beginning-position) (point)))))
        (progn
          ;; This `let' and the `when' below add indentation to the
          ;; wrapped line.
          (let ((indent (save-excursion (back-to-indentation)
                                        (current-column))))
            (insert-before-markers-and-inherit ?\n)
            (backward-char 1)
            (delete-char -1)
            (forward-char 1)
            (when (> indent 0)
              (save-excursion
                (insert (make-string indent ? )))
              (setq longlines-wrap-point
                    (+ longlines-wrap-point indent))))
          nil)
      (if (longlines-merge-lines-p)
          (progn (end-of-line)
                 (if (or (prog1 (bolp) (forward-char 1)) (eolp))
                     (progn
                       (delete-char -1)
                       (if (> longlines-wrap-point (point))
                           (setq longlines-wrap-point
                                 (1- longlines-wrap-point))))
                   (insert-before-markers-and-inherit ?\s)
                   (backward-char 1)
                   (delete-char -1)
                   (forward-char 1)
                   ;; This removes whitespace added for indentation.
                   (while (eq (char-after) ? )
                     (delete-char 1)
                     (setq longlines-wrap-point
                           (1- longlines-wrap-point))))
                 nil)
        (forward-line 1)
        t))))

;; Aggresive indent mode
(add-to-list 'load-path "~/.emacs.d/names")
(add-to-list 'load-path "~/.emacs.d/aggressive-indent-mode")
(require 'names)
(require 'aggressive-indent)
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;; Improved buffer menu
(load "~/.emacs.d/elisp/buff-menu.el")
(require 'buff-menu+)

;; Measure Emacs startup time
(defun nox/show-startup-time ()
  "Show Emacs's startup time in the minibuffer"
  (message "Startup time: %s seconds."
           (emacs-uptime "%s")))
(add-hook 'emacs-startup-hook 'nox/show-startup-time 'append)

;; Benchmark-init can give us a breakdown of time spent on require and load calls:
(add-to-list 'load-path "~/.emacs.d/benchmark-init")
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;; pcache
(add-to-list 'load-path "~/.emacs.d/pcache")
(add-to-list 'load-path "~/.emacs.d/list-utils")
(add-to-list 'load-path "~/.emacs.d/persistent-soft")
(require 'pcache)
(require 'list-utils)
(require 'persistent-soft)
(setq pcache-directory (expand-file-name "~/.emacs.cache"))

;; better windows handling
(add-to-list 'load-path "~/.emacs.d/window-purpose")
(add-to-list 'load-path "~/.emacs.d/imenu-list")
(require 'window-purpose)
(require 'imenu-list)

;; Browse kill ring
(add-to-list 'load-path "~/.emacs.d/browse-kill-ring")
(require 'browse-kill-ring)

(provide 'setup-general)
;;; setup-general.el ends here

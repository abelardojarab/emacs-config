;;; setup-general.el ---

;; Copyright (C) 2014  abelardo.jara-berrocal

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

;; Improve Emacs display engine
(setq redisplay-dont-pause t)

;; Define preferred shell
(setq shell-file-name "bash")
(setq explicit-shell-file-name shell-file-name)

;; avoid garbage collection up to 20M (default is only 400k)
(setq gc-cons-threshold 20000000)

;; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

;; turn-off info about of pointee for symlinks
(setq dired-listing-switches "-alLF")

;; Disable tool-bar
(tool-bar-mode -1)

;; Disable scroll bar
(set-scroll-bar-mode nil)

;; Make ?, ? and such work
(set-language-environment 'spanish)

;; Inhibit startup window, very annoying
(setq inhibit-startup-message t)

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

;; no extra whitespace after lines
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Emacs is a text editor, make sure your text files end in a newline
(setq require-final-newline 'query)

;; Make Emacs ignore the "-e (make-frame-visible)"
;; that it gets passed when started by emacsclientw.
(add-to-list 'command-switch-alist '("(make-frame-visible)" .
                                     (lambda (s))))

;; Automatically kill all spawned processes on exit
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; Try not to split windows
(setq same-window-regexps '("."))

;; Assure window is splitted horizontally (for compilation buffer)
(setq split-width-threshold nil)

;; Avoid to make a separate frame
(setq display-buffer nil)
(setq display-buffer-reuse-frames t)
(setq pop-up-frames nil)

;; Popup, used by auto-complete and other tools
(add-to-list 'load-path "~/.emacs.d/popup")
(require 'popup)

;; Avoid popup windows too
(add-to-list 'load-path "~/.emacs.d/popwin")
(require 'popwin)


;; More general setup


;; Keep session
(require 'desktop)
(desktop-save-mode 1)
(setq desktop-load-locked-desktop t)
(setq-default desktop-missing-file-warning nil)
(setq desktop-path '("~/"))
(setq desktop-dirname "~/")

;; Activate highlight in search and replace
(setq search-highlight t)
(setq query-replace-highlight t)


;; show-paren-mode: subtle blinking of matching paren (defaults are ugly)
(show-paren-mode t)

;; Disable tooltips
(tooltip-mode nil)

;; deleting files goes to OS's trash folder
(setq delete-by-moving-to-trash t)

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Moving cursor down at bottom scrolls only a single line, not half page
(setq
 scroll-margin 0                ;; start scrolling when marker at top/bottom
 scroll-conservatively 100000   ;; marker distance from center (don't jump to center)
 scroll-preserve-screen-position 1) ;; try to keep screen position when PgDn/PgUp

;; These ones are buffer local and thus have to be set up by setq-default
(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)

;; Moving cursor down at bottom scrolls only a single line, not half page
(setq scroll-step 1)
(setq auto-window-vscroll t)

;; Put something different in the scratch buffer
(setq initial-scratch-message
      ";; scratch buffer created -- start typing...\n")

;; Automatically reload files after they've been modified
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; Bell instead of annoying beep
(setq visible-bell t)

;; Turn off the bell http://www.emacswiki.org/cgi-bin/wiki?AlarmBell
(setq ring-bell-function 'ignore)

;; Do not add empty lines at the end of our file if we press down key
(setq next-line-add-newlines nil)

;; Makes final line always be a return
(setq require-final-newline t)

;; When in text (or related mode) break the lines at 80 chars
(setq fill-column 160)
(require 'fill-column-indicator)

;; Marker if the line goes beyond the end of the screen (arrows)
(global-visual-line-mode 1)

;; Save bookmark when you save a file
(setq bookmark-save-flag 1)

;; Set indent to 4 instead of 2
(setq standard-indent 4)

;; Use spaces instead of tab
(setq-default indent-tabs-mode nil)

;; Set tab width
(setq-default tab-width 4)

;; Auto-indent mode
(add-to-list 'load-path "~/.emacs.d/auto-indent-mode")
(setq auto-indent-on-visit-file t)
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

;; Mouse wheel scroll support
(mouse-wheel-mode t)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed t) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Autosave
(setq auto-save-list-file-prefix "~/.emacs.cache/auto-save-list/.saves-")

;; Autosave every 500 typed characters
(setq auto-save-interval 500)

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

;; Insertion of Dates, bind to C-c i
(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; Brackets matching script
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; Abbrevs
(setq abbrev-file-name "~/.emacs.cache/abbrev_defs")
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(add-hook 'kill-emacs-hook
          'write-abbrev-file)

;; Activate template autocompletion
(abbrev-mode t)
(setq default-abbrev-mode t
      save-abbrevs t)

(provide 'setup-general)
;;; setup-general.el ends here

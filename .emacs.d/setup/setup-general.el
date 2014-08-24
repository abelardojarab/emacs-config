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

;; Set path environment depending on OS.
(cond
 ;; Add Homebrew path in Darwin
 ((equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/bin" exec-path)
  (push "/opt/local/bin" exec-path))

 ;; Add Cygwin path in Windows
 ((equal system-type 'windows-nt)
  (when (file-directory-p "c:/cygwin/bin")
    (setenv "PATH" (concat "c:/cygwin/bin:" (getenv "PATH")))
    (add-to-list 'exec-path "c:/cygwin/bin"))
  (if (file-directory-p "c:/cygwin64/bin")
      (setenv "PATH" (concat "c:/cygwin64/bin:" (getenv "PATH")))
    (add-to-list 'exec-path "c:/cygwin64/bin"))))

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

;; End of line
(require 'eol-conversion)

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

;; Filladapt mode for text files
(require 'filladapt)
(add-hook 'text-mode-hook 'turn-on-filladapt-mode)

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

;; Brackets matching script
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; Edition of EMACS edition modes
(setq major-mode 'text-mode)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook (function
                           (lambda () (ispell-minor-mode))))

;; Make a #define be left-aligned
(setq c-electric-pound-behavior (quote (alignleft)))

;; Rainbow delimiters
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(when (require 'rainbow-delimiters nil 'noerror)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Pretty diff mode
(autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files-remote "ediff"
  "Intelligent Emacs interface to diff")

;; Change type files
(setq auto-mode-alist
      (append '(("\\.cpp$" . c++-mode)
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

;; Trick emacs when opening a file through menu-find-file-existing
(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil))
    ad-do-it))

;; Uniquify-buffers
(when (require 'uniquify nil 'noerror)  ;; make buffer names more unique
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"
   uniquify-after-kill-buffer-p t       ;; rename after killing uniquified
   uniquify-ignore-buffers-re "^\\*"))  ;; don't muck with special buffers

(provide 'setup-general)
;;; setup-general.el ends here

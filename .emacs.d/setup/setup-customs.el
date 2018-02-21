;;; setup-customs.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Abelardo Jara-Berrocal

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

(defcustom my/emacs-cache-dir "~/.emacs.cache"
  "Preferred directory to place temporary files"
  :type 'string
  :group 'my/customs)

(defcustom my/bibtex-completion-bibliography "~/workspace/Documents/Bibliography/biblio.bib"
  "Preferred bibliography file"
  :type 'string
  :group 'my/customs)

(defcustom my/bibtex-completion-library-path "~/workspace/Documents/Bibliography/PDFs"
  "Preferred bibliography path for storing PDFs"
  :type 'string
  :group 'my/customs)

(defcustom my/bibtex-completion-notes "~/workspace/Documents/Bibliography/notes.org"
  "Preferred bibliography notes file"
  :type 'string
  :group 'my/customs)

(defcustom my/tab-width 4
  "Default tab width"
  :type 'int
  :group 'my/customs)

(defcustom my/user-full-name  "Abelardo Jara-Berrocal"
  "User full name"
  :group 'my/customs
  :type 'string)

(defcustom my/user-email  "abelardojarab@gmail.com"
  "Preferred user email"
  :group 'my/customs
  :type 'string)

(defcustom my/user-domain "jaraberrocal.readmyblog.org"
  "Preferred user domain"
  :group 'my/customs
  :type 'string)

(defcustom my/mu4e-maildir "~/Mail"
  "Location of the mu4e/mbsync mailbox"
  :group 'my/customs
  :type 'string)

(defcustom my/smtp-accounts
  '((ssl "abelardotomasjb@gmail.com" "smtp.gmail.com"
     587 "abelardojarab@gmail.com"))
  "Availble SMTP accounts"
  :group 'my/customs
  :type 'list)

(defcustom my/preferred-to-language "Spanish"
  "Preferred language to translate to ..."
  :group 'my/customs
  :type 'string)

(defcustom my/preferred-from-language "English"
  "Language languate to translate from ..."
  :group 'my/customs
  :type 'string)

(defcustom my/main-programming-font "Fira Mono"
  "Preferred programming font"
  :type 'string
  :group 'my/customs)

(defcustom my/main-programming-font-size "12"
  "Preferred programming font size"
  :type 'string
  :group 'my/customs)

(defcustom my/main-writing-font "Cousine"
  "Preferred writing font"
  :type 'string
  :group 'my/customs)

(defcustom my/main-writing-font-size "13"
  "Preferred writing font size"
  :type 'string
  :group 'my/customs)

(defcustom my/emacs-theme
  'goose
  "Preferred Emacs theme"
  :group 'my/customs
  :type 'list)

(defcustom my/emacs-theme-console
  'zenburn
  "Preferred Emacs theme"
  :group 'my/customs
  :type 'list)

(defcustom my/ecb-layout-theme "bodil"
  "Preferred ECB layout name"
  :group 'my/customs
  :type 'string)

(defcustom my/preferred-reopen-rw-mode "sudo"
  "Preferred mode for reopen"
  :type 'string
  :group 'my/customs)

(defcustom my/dired-git-after-desktop nil
  "Ask to open a dired buffer every time"
  :group 'my/customs
  :type 'boolean)

(defcustom my/switch-buffer-ignore-dired t
  "If t, ignore dired buffer when calling `my/next-user-buffer' or `my/previous-user-buffer'"
  :group 'my/customs
  :type 'boolean)

(defcustom my/default-closing-char ";"
  "Default closing char, change in my/newline-force-close-alist if needed"
  :group 'my/customs
  :type 'string)

(defcustom my/newline-force-close-alist
  '((html-mode   . " <br>")
    (latex-mode  . " \\newline")
    (org-mode    . " \\newline")
    (python-mode . ":"))
  "Closing char for different modes"
  :group 'my/customs
  :type 'list)

(defcustom my/spell-langs
  '(emacs-lisp-mode-hook
    python-mode-hook
    c-mode-common-hook)
  "Set of programming modes for which I want to enable spelling in comments and strings"
  :group 'my/customs
  :type 'list)

(defcustom my/camelCase-modes
  '(python-mode-hook
    java-mode-hook
    c-mode-common-hook)
  "Modes where camelizing is enabled"
  :group 'my/customs
  :type 'list)

(defcustom my/subword-modes
  '(c-mode-common-hook
    python-mode-hook
    js2-mode-hook
    java-mode-hook)
  "Modes where subword mode is enabled"
  :group 'my/customs
  :type 'list)

(defcustom my/imenu-anywhere-modes
  '(c-mode-common-hook
    python-mode-hook
    reftex-mode-hook
    reftex-load-hook
    org-mode-hook)
  "Modes where imenu anywhere is enabled"
  :group 'my/customs
  :type 'list)

(defcustom my/highlight-blocks-modes
  '(prog-mode-hook)
  "Modes where highlight blocks mode is enabled"
  :group 'my/customs
  :type 'list)

(defcustom my/highlight-sexp-modes
  '(prog-mode-hook)
  "Modes where highlight sexp mode is enabled"
  :group 'my/customs
  :type 'list)

(defcustom my/eldoc-modes
  '(lisp-interaction-mode-hook
    ielm-mode-hook
    emacs-lisp-mode-hook)
  "Modes where eldoc mode is enabled"
  :group 'my/customs
  :type 'list)

(defcustom my/flycheck-modes
  '(prog-mode-hook
    c-mode-common-hook
    ess-mode-hook)
  "Modes where flycheck mode is enabled"
  :group 'my/customs
  :type 'list)

(defcustom my/rainbow-modes
  '(prog-mode-hook
    text-mode-hook
    markdown-mode-hook
    org-mode-hook)
  "Modes where rainbow mode is enabled"
  :group 'my/customs
  :type 'list)

(defcustom my/linum-modes
  '(prog-mode-hook
    c-mode-common-hook)
  "Modes where linum mode is enabled"
  :group 'my/customs
  :type 'list)

(defcustom my/abbrev-modes
  '(prog-mode-hook
    c-mode-common-hook
    markdown-mode-hook
    org-mode-hook
    text-mode-hook)
  "Modes where abbrev mode is enabled"
  :group 'my/customs
  :type 'list)

(defcustom my/flyspell-modes
  '(text-mode-hook
    org-mode-hook
    markdown-mode-hook)
  "Modes where flyspell mode is enabled"
  :group 'my/customs
  :type 'list)

(defcustom my/flyspell-modes-disabled
  '(change-log-mode-hook
    log-edit-mode-hook)
  "Modes where flyspell mode is disabled"
  :group 'my/customs
  :type 'list)

(defcustom my/hideshow-modes
  '(prog-mode-hook
    c-mode-common-hook
    nxml-mode-hook)
  "Modes where hideshow mode is enabled"
  :group 'my/customs
  :type 'list)

(defcustom my/fixme-modes
  '(python-mode-hook
    c-mode-common-hook
    lisp-interaction-mode-hook
    org-mode-hook
    emacs-lisp-mode-hook)
  "Modes for which fixme mode should be enabled"
  :type 'list
  :group 'my/customs)

(defcustom my/desktop-modes-disabled
  '(magit-mode
    magit-log-mode
    dired-mode
    Info-mode
    fundamental-mode
    DocView-mode)
  "Modes where desktop mode is disabled"
  :group 'my/customs
  :type 'list)

(defcustom my/ignored-buffers
  '("*Compile-Log*"
    "^ "
    "rc"
    "*diff-hl*"
    "*tmp status*"
    "Grepint: git-grep"
    "*Messages*"
    "*Completions*"
    "*scratch*"
    "*Python*"
    "*GNU Emacs*"
    "*compilation*"
    "*cmake*"
    "*etags tmp*"
    "TAGS"
    ;; C header files
    "stdint.h"
    "stdbool.h"
    "stdio.h"
    "stdlib.h"
    "confname.h"
    "unistd.h"
    "pthread.h"
    "fcntl.h"
    "cdefs.h"
    ;; C++ header files
    "new"
    "cstdlib"
    "shared_ptr.h"
    "unique_ptr.h"
    "std_mutex"
    "c++config.h")
  "List of buffers to be ignored on tabbar and when switching buffers"
  :group 'my/customs
  :type 'list)

(defcustom my/backend-assoc
  '(('Git . 'magit-status)
    ('Hg  . 'hg-status)
    ('Svn . 'svn-status))
  "Mapping between backend and function"
  :group 'my/customs
  :type 'list)

;; TODO: how do we check if we can check for the existing snippets?
;; TODO: make it smarter, it would be good to accept also a function, in this way it can be made more generic
(defcustom my/auto-header-conses
  '(("setup .py"   . "setup")
    ("\     .sh$"  . "bash")
    ("\     .h$"   . "once")
    ("\     .hpp$" . "once"))
  "Snippets to expand per file extension"
  :group 'my/customs
  :type 'list)

(defcustom my/whitespace-cleanup-enabled nil
  "True if the whitespace cleanup should be enabled, good candidate to use as dir-local variable"
  :type 'boolean
  :group 'my/customs)

(defcustom my/whitespaces-modes-disabled
  '(makefile-mode
    message-mode
    mail-mode)
  "Modes that should not run the whitespace cleanup automatically"
  :type 'list
  :group 'my/customs)

(defcustom my/command-frequency-enabled nil
  "Enable recording the command frequency"
  :type 'boolean
  :group 'my/customs)

(defcustom my/extra-conf-files
  '("\\.pkla\\'" "shorewall" "pylintrc" "\\.spec\\'")
  "Extra list of regexp to enable conf-mode"
  :type 'list
  :group 'my/customs)

(defcustom my/conf-section-regexp
  "\\[.*\\]"
  "Regexp to distinguish the different configuration sections"
  :type 'string
  :group 'my/customs)

(defcustom my/cedet-enabled t
  "Enable cedet"
  :type 'boolean
  :group 'my/customs)

(defcustom my/ecb-enabled t
  "Enable ECB"
  :type 'boolean
  :group 'my/customs)

(defcustom my/cedet-modes
  '(c-mode-common-hook
    emacs-lisp-mode-hook
    makefile-mode-hook)
  "Modes for which cedet should be enabled"
  :type 'list
  :group 'my/customs)

(defcustom erc-autojoin-channels-alist
  '("freenode.net"
    "#mailman" "#pelican"
    "#emacs" "#erc"
    "#python" "#git" "#github" "#c" "#c++" "#conkeror"
    "#android" "#latex" "#org-mode" "#postfix" "#procmail"
    "#android-devel" "#libav-devel" "#archlinux" "#xmonad" "#ledger"
    "#scipy" "#haskell" "#macosx" "#scala" "#ubuntu" "#clojure")
  "List of channels to join automatically"
  :type 'list
  :group 'my/customs)

(defcustom my/anything-requires-pattern 2
  "Number of characters to input before starting to search"
  :type 'int
  :group 'my/customs)

(defcustom my/anything-prune-dir
  '(".git"
    ".svn")
  "Directories to skip"
  :type 'list
  :group 'my/customs)

(defcustom my/compilation-kill-buffer-timeout 10
  "Seconds to wait before kill the compilation buffer"
  :type 'int
  :group 'my/customs)

;; User-specific configuration file
(setq-default custom-file-x (concat (file-name-as-directory
                                     my/emacs-cache-dir) "custom.el"))

(ignore-errors
  (if (file-exists-p custom-file-x)
      (load custom-file-x :noerror :nomessage)
    (write-region "" nil custom-file-x)))
(setq-default custom-file custom-file-x)

;; User information
(setq user-full-name my/user-full-name)
(setq user-mail-address my/user-email)

;; Prompt about unsaved customizations at termination time
(add-hook 'kill-emacs-query-functions
          'custom-prompt-customize-unsaved-options)

;; Settings for currently logged in user
(defvar user-settings-dir (concat (file-name-as-directory
                                   my/emacs-cache-dir) "custom"))
(add-to-list 'load-path user-settings-dir)

(provide 'setup-customs)
;;; setup-customs.el ends here

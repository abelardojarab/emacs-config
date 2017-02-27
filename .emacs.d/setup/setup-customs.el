;;; setup-customs.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Abelardo Jara-Berrocal

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

(defcustom my/tab-width 4
  "Default Emacs tab width"
  :type 'int
  :group 'my/customs)

(defcustom my/user-email  "abelardojarab@gmail.com"
  "Preferred user email"
  :group 'my/customs
  :type 'string)

(defcustom my/user-domain "abelardojarab.dyndns.org"
  "Preferred user domain"
  :group 'my/customs
  :type 'string)

(defcustom my/mu4e-maildir "~/Maildir"
  "Location of the mu4e/mbsync mailbox"
  :group 'my/customs
  :type 'string)

(defcustom my/preferred-to-language "Spanish"
  "Language to translate to"
  :group 'my/customs
  :type 'string)

(defcustom my/preferred-from-language "English"
  "Language to translate from"
  :group 'my/customs
  :type 'string)

(defcustom my/main-programming-font "Roboto Mono for Powerline"
  "Preferred Emacs font"
  :type 'string
  :group 'my/customs)

(defcustom my/main-programming-font-size "12"
  "Preferred Emacs font size"
  :type 'string
  :group 'my/customs)

(defcustom my/main-writing-font "Meslo"
  "Preferred Emacs font"
  :type 'string
  :group 'my/customs)

(defcustom my/main-writing-font-size "13"
  "Preferred Emacs font size"
  :type 'string
  :group 'my/customs)

(defcustom my/emacs-theme
  'monokai
  "Preferred Emacs theme"
  :group 'my/customs
  :type 'list)

(defcustom my/preferred-reopen-rw-mode "sudo"
  "Preferred mode for reopen"
  :type 'string
  :group 'my/customs)

(defcustom my/dired-git-after-desktop
  nil
  "Ask to open a dired buffer every time"
  :group 'my/customs
  :type 'boolean)

(defcustom my/switch-buffer-ignore-dired t
  "If t, ignore dired buffer when calling `my/next-user-buffer' or `my/previous-user-buffer'"
  :group 'my/customs
  :type 'boolean)

(defcustom my/default-closing-char ";"
  "default closing char, change in my/newline-force-close-alist if needed"
  :group 'my/customs
  :type 'string)

(defcustom my/newline-force-close-alist
  '((html-mode . " <br>")
    (latex-mode . " \\newline")
    (org-mode . " \\newline")
    (python-mode . ":"))
  "Closing char for different modes"
  :group 'my/customs
  :type 'list)

(defcustom my/show-battery
  nil
  "show the battery level"
  :group 'my/customs
  :type 'boolean)

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
  "Modes where camelizing is allowed"
  :group 'my/customs
  :type 'list)

(defcustom my/eldoc-modes
  '(lisp-interaction-mode-hook
    ielm-mode-hook
    emacs-lisp-mode-hook)
  "Modes where eldoc eldoc mode is enabled"
  :group 'my/customs
  :type 'list)

(defcustom my/backend-assoc
  '(('Git . 'magit-status)
    ('Hg . 'hg-status)
    ('Svn . 'svn-status))
  "Mapping between backend and function"
  :type 'list)

;; TODO: how do we check if we can check for the existing snippets?
;; TODO: make it smarter, it would be good to accept also a function, in this way it can be made more generic
(defcustom my/auto-header-conses
  '(("setup.py" . "setup")
    ("\.sh$" . "bash")
    ("\.h$"  . "once")
    ("\.hpp$" . "once"))
  "Snippets to expand per file extension"
  :group 'my/customs
  :type 'list)

(defcustom my/whitespace-cleanup-enabled
  nil
  "True if the whitespace cleanup should be enabled, good candidate to use as dir-local variable"
  :type 'boolean
  :group 'my/customs)

(defcustom my/non-whitespaces-modes
  '(makefile-mode
    message-mode
    mail-mode)
  "Modes that should not run the whitespace cleanup automatically"
  :type 'list
  :group 'my/customs)

(defcustom my/command-frequency-enabled
  nil
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

(defcustom my/cedet-modes
  '(c-mode-common-hook
    emacs-lisp-mode-hook
    makefile-mode-hook)
  "Modes for which cedet should be enabled"
  :type 'list
  :group 'my/customs)

(defcustom my/cedet-enabled
  t
  "Enable cedet"
  :type 'boolean
  :group 'my/customs)

(defcustom my/fixme-mode-hooks
  '(python-mode-hook
    c-mode-common-hook
    lisp-interaction-mode-hook
    org-mode-hook
    emacs-lisp-mode-hook)
  "Modes for which fixme-mode should be enabled"
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
  '(".git" ".svn")
  "Directories to skip"
  :type 'list
  :group 'my/customs)

(defcustom my/compilation-kill-buffer-timeout 10
  "Seconds to wait before kill the compilation buffer"
  :type 'int
  :group 'my/customs)

(provide 'setup-customs)
;;; setup-customs.el ends here

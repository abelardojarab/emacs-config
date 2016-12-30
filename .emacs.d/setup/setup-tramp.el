;;; setup-tramp.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2016  Abelardo Jara-Berrocal

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

(use-package tramp
  :init (progn
          ;; Fix SSH agent on UNIX
          (when (not (equal system-type 'windows-nt))
            (defun find-agent ()
              (first (split-string
                      (shell-command-to-string
                       (concat
                        "ls -t1 "
                        "$(find /tmp/ -uid $UID -path \\*ssh\\* -type s 2> /dev/null)"
                        "|"
                        "head -1")))))
            (defun fix-agent ()
              (interactive)
              (let ((agent (find-agent)))
                (setenv "SSH_AUTH_SOCK" agent)
                (message agent))))

          ;; Set control master options before loading tramp
          (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath=~/.emacs.cache/ssh-ControlPath -o ControlPersist=no"))
  :config (progn

            ;; Re-enable the SSH keyring in case Emacs does not refreshes it
            (defun my/ssh-refresh ()
              "Reset the environment variable SSH_AUTH_SOCK"
              (interactive)
              (let (ssh-auth-sock-old (getenv "SSH_AUTH_SOCK"))
                (setenv "SSH_AUTH_SOCK"
                        (car (split-string
                              (shell-command-to-string
                               "ls -t $(find /tmp/ssh-* -user $USER -name 'agent.*' 2> /dev/null)"))))
                (message
                 (format "SSH_AUTH_SOCK %s --> %s"
                         ssh-auth-sock-old (getenv "SSH_AUTH_SOCK")))))
            (if (equal system-type 'gnu/linux)
                (my/ssh-refresh))

            ;; Fix auto save problem
            (setq tramp-persistency-file-name "~/.emacs.cache/tramp")
            (setq tramp-auto-save-directory "~/.emacs.cache/auto-save")

            ;; Tramp configurations
            (setq my-tramp-ssh-completions
                  '((tramp-parse-sconfig "~/.ssh/config")
                    (tramp-parse-shosts "~/.ssh/known_hosts")))
            (mapc (lambda (method)
                    (tramp-set-completion-function method my-tramp-ssh-completions))
                  '("rsync" "scp" "scpc" "scpx" "sftp" "ssh" "plink"))

            ;; have tramp save temps locally
            (unless (string-equal system-type "windows-nt")
              (setq auto-save-file-name-transforms
                    '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
                      ("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" "~/.emacs.cache/auto-save/" t))))))

;; End of line, needed by tramp
(use-package eol-conversion
  :pin manual
  :config (setq inhibit-eol-conversion 't))

(provide 'setup-tramp)
;;; setup-tramp.el ends here

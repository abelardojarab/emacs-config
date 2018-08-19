;;; setup-tramp.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Abelardo Jara-Berrocal

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

(use-package tramp
  :init (progn
          ;; Create an alias
          (defalias 'exit-tramp 'tramp-cleanup-all-connections)

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

            ;; Refresh SSH agent
            (when (executable-find "keychain")
              ;; Re-enable the SSH keyring in case Emacs does not refreshes it
              (defun my/keychain-refresh-environment ()
                "Set ssh-agent and gpg-agent environment variables.
Set the environment variables `SSH_AUTH_SOCK', `SSH_AGENT_PID'
and `GPG_AGENT' in Emacs' `process-environment' according to
information retrieved from files created by the keychain script."
                (interactive)
                (let* ((ssh-auth-sock-old (getenv "SSH_AUTH_SOCK"))
                       (ssh (shell-command-to-string "keychain -q --noask --agents ssh --eval"))
                       (gpg (shell-command-to-string "keychain -q --noask --agents gpg --eval")))
                  (list (and ssh
                             (string-match "SSH_AUTH_SOCK[=\s]\\([^\s;\n]*\\)" ssh)
                             (setenv       "SSH_AUTH_SOCK" (match-string 1 ssh)))
                        (and ssh
                             (string-match "SSH_AGENT_PID[=\s]\\([0-9]*\\)?" ssh)
                             (setenv       "SSH_AGENT_PID" (match-string 1 ssh)))
                        (and gpg
                             (string-match "GPG_AGENT_INFO[=\s]\\([^\s;\n]*\\)" gpg)
                             (setenv "GPG_AGENT_INFO" (match-string 1 gpg))))

                  (message
                   (format "SSH_AUTH_SOCK %s --> %s"
                           ssh-auth-sock-old (getenv "SSH_AUTH_SOCK")))))

              (if (equal system-type 'gnu/linux)
                  (my/keychain-refresh-environment)))

            ;; Fix auto save problem
            (setq tramp-persistency-file-name (concat (file-name-as-directory
                                                       my/emacs-cache-dir)
                                                      "tramp"))
            (setq tramp-auto-save-directory (concat (file-name-as-directory
                                                     my/emacs-cache-dir)
                                                    "auto-save"))

            ;; Tramp configurations
            (setq my/tramp-ssh-completions
                  '((tramp-parse-sconfig "~/.ssh/config")
                    (tramp-parse-shosts "~/.ssh/known_hosts")))
            (mapc (lambda (method)
                    (tramp-set-completion-function method my/tramp-ssh-completions))
                  '("rsync" "scp" "scpc" "scpx" "sftp" "ssh" "plink"))))

;; End of line, needed by tramp
(use-package eol-conversion
  :pin manual
  :config (setq inhibit-eol-conversion 't))

;; helm interface for tramp
(use-package helm-tramp
  :defer t
  :bind ("C-c s" . helm-tramp))

(provide 'setup-tramp)
;;; setup-tramp.el ends here

;;; setup-keychain.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Abelardo Jara-Berrocal

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

;; Secure sockets layer support
(use-package starttls
  :config (progn
            ;; Options
            (if (executable-find "gnutls-cli")
                (setq starttls-use-gnutls      t
                      starttls-gnutls-program  "gnutls-cli"
                      starttls-extra-arguments nil))

            ;; Make gnutls a bit safer
            (setq gnutls-min-prime-bits 4096)))

;; Keychain access
(use-package keychain-environment
  :if (equal system-type 'gnu/linux)
  :load-path (lambda () (expand-file-name "keychain-environment/" user-emacs-directory))
  :commands keychain-refresh-environment
  :defer 60
  :init (keychain-refresh-environment))

;; Keeping Secrets in Emacs with GnuPG & EasyPG
(use-package epg
  :config (progn
            ;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
            (if (executable-find "gpg2")
                (setq epg-gpg-program "gpg2"))

            ;; enable EasyPG handling
            ;; gpg-agent confuses epa when getting passphrase
            (defun my/squash-gpg (&rest ignored-frame)
              "Kill any GPG_AGENT_INFO in our environment."
              (setenv "GPG_AGENT_INFO" nil))

            ;; Fix up the frame so we don't send pinentry to the wrong place
            (defun my/fixup-gpg-agent (&optional frame)
              "Tweak DISPLAY and GPG_TTY environment variables as appropriate to `FRAME'."
              (when (not frame)
                (setq frame (selected-frame)))
              (when (fboundp 'keychain-refresh-environment)
                (keychain-refresh-environment))
              (if (display-graphic-p frame)
                  (setenv "DISPLAY" (terminal-name frame))
                (setenv "GPG_TTY" (terminal-name frame))
                (setenv "DISPLAY" nil)))

            (when (getenv "DISPLAY")
              (add-hook 'after-make-frame-functions 'my/fixup-gpg-agent)
              (add-hook 'focus-in-hook 'my/fixup-gpg-agent))

            ;; https://github.com/stsquad/my-emacs-stuff/blob/master/my-gpg.el
            ;; Disable External Pin Entry
            (setenv "GPG_AGENT_INFO" nil)))

;; Enable encryption/decryption of .gpg files
(use-package epa-file
  :commands epa-file-enable
  :config  (progn
             ;; 'silent to use symmetric encryption
             ;; nil to ask for users unless specified
             ;; t to always ask for a user
             ;; (setq epa-file-select-keys t)

             (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$"
                   epa-armor t)
             (epa-file-name-regexp-update)
             (epa-file-enable)))

;; EasyPG Emacs assistant
(use-package epa
  :defer t
  :config (progn
            (setq epa-popup-info-window nil)

            ;; https://github.com/jwiegley/dot-emacs/blob/master/dot-gnus.el
            (defun epa--key-widget-value-create (widget)
              (let* ((key (widget-get widget :value))
                     (primary-sub-key (car (last (epg-key-sub-key-list key) 3)))
                     (primary-user-id (car (epg-key-user-id-list key))))
                (insert (format "%c "
                                (if (epg-sub-key-validity primary-sub-key)
                                    (car (rassq (epg-sub-key-validity primary-sub-key)
                                                epg-key-validity-alist))
                                  ? ))
                        (epg-sub-key-id primary-sub-key)
                        " "
                        (if primary-user-id
                            (if (stringp (epg-user-id-string primary-user-id))
                                (epg-user-id-string primary-user-id)
                              (epg-decode-dn (epg-user-id-string primary-user-id)))
                          ""))))

            (epa-file-enable)))

;; .authinfo parsing (not longer valid for org2blog, use auth-source)
(use-package netrc)

;; .authinfo parsing
(use-package auth-source
  :config (progn
            (if (file-exists-p "~/.authinfo.gpg")
                (add-to-list 'auth-sources "~/.authinfo.gpg"))

            (if (file-exists-p "~/.authinfo")
                (add-to-list 'auth-sources "~/.authinfo"))))

;; Provide utilities to interact with 'pass' ("the standard Unix password keychain manager")
(use-package password-store
  :demand t
  :if (and (equal system-type 'gnu/linux)
           (executable-find "pass"))
  :load-path (lambda () (expand-file-name "password-store/" user-emacs-directory)))

;; major-mode to manage 'pass'
(use-package pass
  :demand t
  :after password-store
  :if (and (equal system-type 'gnu/linux)
           (executable-find "pass"))
  :load-path (lambda () (expand-file-name "pass/" user-emacs-directory)))

;; 'pass' interface to auth-source,
;; no longer need to store passwords in the .authinfo file
(use-package auth-password-store
  :demand t
  :if (and (equal system-type 'gnu/linux)
           (executable-find "pass"))
  :after pass
  :load-path (lambda () (expand-file-name "auth-password-store/" user-emacs-directory))
  :config (auth-pass-enable))

;; Secrets file
(let ((secrets-file "~/.emacs.cache/.secret.el"))
  (when (file-exists-p secrets-file)
    (load secrets-file)))

(provide 'setup-keychain)
;;; setup-keychain.el ends here
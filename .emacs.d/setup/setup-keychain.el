;;; setup-keychain.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

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
  :demand t
  :if (executable-find "gnutls-cli")
  :custom ((starttls-use-gnutls      t)
           (starttls-gnutls-program  "gnutls-cli")
           (starttls-extra-arguments nil)
           (gnutls-min-prime-bits    4096)
           (gnutls-verify-error      t)
           (tls-checktrust           t))
  :config (let ((trustfile (replace-regexp-in-string
                            "\\\\" "/" (replace-regexp-in-string
                                        "\n" "" (shell-command-to-string "python -m certifi")))))
            (setq tls-program (list
                               (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                                       (if (eq window-system 'w32) ".exe" "") trustfile))
                  gnutls-trustfiles (list trustfile))))

;; Keychain access
(use-package keychain-environment
  :defer t
  :after starttls
  :commands keychain-refresh-environment
  :hook (after-init . keychain-refresh-environment))

;; Keeping secrets in Emacs with GnuPG & EasyPG
(use-package epg
  :demand t
  :after keychain-environment
  :init (progn
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
            (my/squash-gpg)
            (keychain-refresh-environment)
            (if (display-graphic-p frame)
                (setenv "DISPLAY" (terminal-name frame))
              (setenv "GPG_TTY" (terminal-name frame))
              (setenv "DISPLAY" nil))))
  :config (progn
            ;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
            (if (executable-find "gpg2")
                (setq epg-gpg-program "gpg2"))

            (when (getenv "DISPLAY")
              (add-hook 'after-make-frame-functions #'my/fixup-gpg-agent)
              (add-hook 'focus-in-hook              #'my/fixup-gpg-agent))))

;; Prefer gpg
(use-package epg-config
  :demand t
  :custom ((epa-file-cache-passphrase-for-symmetric-encryption t)
           (epg--configurations                                nil)
           (epg-gpg-program                                    "gpg2"))
  :config (add-to-list 'epg-config--program-alist `(OpenPGP epg-gpg-program ("gpg" . ,epg-gpg-minimum-version))))

;; Pinentry (not available in melpa)
(use-package pinentry
  :defer t
  :load-path (lambda () (expand-file-name "pinentry/" user-emacs-directory))
  :commands pinentry-start)

;; Enable encryption/decryption of .gpg files
(use-package epa-file
  :defer t
  :after epg
  :commands epa-file-enable
  :config  (progn
             ;; Unfortunately there is bug in gpg which disabled this
             ;; ~/.gnupg/gpg-agent.conf should contain:
             ;; allow-emacs-pinentry
             ;; allow-loopback-pinentry
             ;; Restart with:
             ;; gpgconf --reload gpg-agent
             ;; (setq epa-pinentry-mode 'loopback)

             (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
             (epa-file-name-regexp-update))
  :hook (after-init . epa-file-enable))

;; EasyPG Emacs assistant
(use-package epa
  :defer t
  :after epa-file
  :custom ((epa-popup-info-window nil)
           (epa-armor             t)
           (epa-pinentry-mode     nil)))

;; .authinfo parsing (not longer valid for org2blog, use auth-source)
(use-package netrc)

;; .authinfo parsing
(use-package auth-source
  :demand t
  :config (progn
            (use-package secrets)
            (if (file-exists-p "~/.authinfo.gpg")
                (add-to-list 'auth-sources "~/.authinfo.gpg"))

            (if (file-exists-p "~/.authinfo")
                (add-to-list 'auth-sources "~/.authinfo"))))

;; Provide utilities to interact with 'pass' ("the standard Unix password keychain manager")
(use-package password-store
  :demand t)

(use-package password-cache
  :if (and (equal system-type 'gnu/linux)
           (executable-find "pass"))
  :custom ((password-cache-expiry nil)
           (password-cache t)))

;; major-mode to manage 'pass'
(use-package pass
  :demand t
  :after password-store
  :if (and (equal system-type 'gnu/linux)
           (executable-find "pass")))

;; 'pass' interface to auth-source,
;; no need to store passwords in the .authinfo file
(use-package auth-source-pass
  :defer t
  :if (and (equal system-type 'gnu/linux)
           (executable-find "pass"))
  :after (password-store auth-source)
  :commands (auth-source-pass-enable)
  :config (setq auth-sources '(password-store))
  :hook (after-init . auth-source-pass-enable))

;; Secrets file
(let ((secrets-file (concat (file-name-as-directory
                             my/emacs-cache-dir)
                            ".secret.el")))
  (when (file-exists-p secrets-file)
    (load secrets-file)))

(provide 'setup-keychain)
;;; setup-keychain.el ends here

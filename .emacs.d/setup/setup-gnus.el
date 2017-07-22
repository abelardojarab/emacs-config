;;; setup-gnus.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017  Abelardo Jara-Berrocal

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

;; Secrets file
(let ((secrets-file "~/.emacs.cache/.secret.el"))
  (when (file-exists-p secrets-file)
    (load secrets-file)))

;; Electric completions of email addresses and the like
(use-package ecomplete)

;; To be able to search within your gmail/imap mail
(use-package nnir
  :config (progn
            (setq nndraft-directory "~/Mail/[Gmail].Drafts"

                  ;; MH Spooling directory
                  nnmh-directory "~/Mail"

                  ;; Directory containing Unix mbox; defaults to message-directory
                  nnfolder-directory "~/Mail"

                  ;; Spool directory; defaults to message-directory
                  nnml-directory "~/Mail")))

;; Gnus
(use-package gnus
  :after (starttls nnir epa)
  :defer 1
  :commands (gnus compose-mail switch-to-gnus activate-gnus)
  :bind (("M-G" . switch-to-gnus)
         :map ctl-x-map
         ("M"   . compose-mail))
  :config (progn

            ;; Start gnus if not available
            (defun activate-gnus ()
              (unless (get-buffer "*Group*") (gnus)))

            ;; http://sachachua.com/blog/2007/12/gnus-multi-pane-tricks-or-i-heart-planet-emacsen/
            (gnus-add-configuration
             '(article
               (horizontal 1.0
                           (vertical 60 (group 1.0))
                           (vertical 1.0
                                     (summary 0.40 point)
                                     (article 1.0)))))

            (gnus-add-configuration
             '(summary
               (horizontal 1.0
                           (vertical 60 (group 1.0))
                           (vertical 1.0 (summary 1.0 point)))))

            ;; You need this to be able to list all labels in gmail
            (setq gnus-ignored-newsgroups ""

                  ;; Asynchronous support
                  gnus-asynchronous t

                  ;; Dont ask question on exit
                  gnus-interactive-catchup nil
                  gnus-interactive-exit nil

                  ;; Headers we wanna see:
                  gnus-visible-headers "^From:\\|^Subject:\\|^X-Mailer:\\|^X-Newsreader:\\|^Date:\\|^To:\\|^Cc:\\|^User-agent:\\|^Newsgroups:\\|^Comments:"

                  ;; And this to configure gmail imap
                  gnus-select-method '(nnimap "gmail"
                                              (nnimap-address "imap.gmail.com")
                                              (nnimap-server-port 993)
                                              (nnimap-stream ssl)
                                              (nnir-search-engine imap))

                  ;; Add Unix mbox'es (mbsync); in case we have local email
                  gnus-secondary-select-methods '((nnmaildir "gmail"
                                                             (directory "~/Mail")
                                                             (directory-files nnheader-directory-files-safe)
                                                             (get-new-mail nil)))

                  ;; Archive outgoing email in Sent folder on imap.gmail.com
                  gnus-message-archive-method '(nnimap "imap.gmail.com")
                  gnus-message-archive-group "[Gmail]/Sent Mail"

                  ;; Display a button for MIME parts
                  gnus-buttonized-mime-types '("multipart/alternative")

                  ;; Gnus news
                  gnus-summary-line-format (concat "%*%0{%U%R%z%d%}"
                                                   "%0{ %}(%2t)"
                                                   "%2{ %}%-23,23n"
                                                   "%1{ %}%1{%B%}%2{%-102,102s%}%-140="
                                                   "\n")

                  ;; A gravatar is an image registered to an e-mail address
                  gnus-treat-from-gravatar t)

            ;; Use gnus for email
            (setq mail-user-agent 'gnus-user-agent)
            (setq read-mail-command 'gnus-user-agent)

            ;; Mode hooks
            (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
            (add-hook 'gnus-group-mode-hook 'hl-line-mode)
            (add-hook 'gnus-summary-mode-hook 'hl-line-mode)

            ;; Truncate lines
            (mapc (lambda (mode)
                    (add-hook mode (lambda ()
                                     (visual-line-mode -1)
                                     (toggle-truncate-lines t)
                                     (setq truncate-lines t))))
                  '(gnus-summary-mode-hook
                    gnus-group-mode-hook
                    gnus-topic-mode-hook))

            ;; Sort email
            (add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)
            (add-hook 'gnus-suspend-gnus-hook 'gnus-group-sort-groups-by-rank)
            (add-hook 'gnus-exit-gnus-hook 'gnus-group-sort-groups-by-rank)

            ;; Get smarter about filtering depending on what I reed or mark.
            ;; I use ! (tick) for marking threads as something that interests me.
            (setq gnus-use-adaptive-scoring 'line
                  gnus-default-adaptive-score-alist
                  '((gnus-dormant-mark (from 20) (subject 100))
                    (gnus-ticked-mark (subject 30))
                    (gnus-read-mark (subject 30))
                    (gnus-del-mark (subject -150))
                    (gnus-catchup-mark (subject -150))
                    (gnus-killed-mark (subject -1000))
                    (gnus-expirable-mark (from -1000) (subject -1000)))
                  gnus-score-decay-constant 1
                  gnus-score-decay-scale 0.03
                  gnus-decay-scores t)

            ;; Save email attachments
            (defun my/gnus-summary-save-parts (&optional arg)
              (interactive "P")
              (let ((directory "~/Downloads"))
                (message "Saving all MIME parts to %s..." directory)
                (gnus-summary-save-parts ".*" directory arg)
                (message "Saving all MIME parts to %s...done" directory)))

            ;; https://www.emacswiki.org/emacs/SwitchToGnus
            (defun switch-to-gnus (&optional arg)
              "Switch to a Gnus related buffer.
    Candidates are buffers starting with
     *mail or *reply or *wide reply
     *Summary or
     *Group*
    Use a prefix argument to start Gnus if no candidate exists."
              (interactive "P")
              (let (candidate
                    (alist '(("^\\*\\(mail\\|\\(wide \\)?reply\\)" t)
                             ("^\\*Group")
                             ("^\\*Summary")
                             ("^\\*Article" nil (lambda ()
                                                  (buffer-live-p gnus-article-current-summary))))))
                (catch 'none-found
                  (dolist (item alist)
                    (let (last
                          (regexp (nth 0 item))
                          (optional (nth 1 item))
                          (test (nth 2 item)))
                      (dolist (buf (buffer-list))
                        (when (and (string-match regexp (buffer-name buf))
                                   (> (buffer-size buf) 0))
                          (setq last buf)))
                      (cond ((and last (or (not test) (funcall test)))
                             (setq candidate last))
                            (optional
                             nil)
                            (t
                             (throw 'none-found t))))))
                (cond (candidate
                       (switch-to-buffer candidate))
                      (arg
                       (gnus))
                      (t
                       (error "No candidate found")))))))

;; To be able to send email with your gmail/smtp mail
(use-package smtpmail
  :after gnus
  :config (progn
            ;; Using smptmail to assure portability; we dont always have postfix
            (setq send-mail-function 'smtpmail-send-it
                  message-send-mail-function 'smtpmail-send-it
                  smtpmail-debug-info t smtpmail-debug-verb t)

            (defun set-smtp (mech server port user password)
              "Set related SMTP variables for supplied parameters."
              (setq smtpmail-smtp-server server
                    smtpmail-smtp-service port
                    smtpmail-auth-credentials (list (list server port user
                                                          password))
                    smtpmail-auth-supported (list mech)
                    smtpmail-starttls-credentials nil)
              (message "Setting SMTP server to `%s:%s' for user `%s'."
                       server port user))

            (defun set-smtp-ssl (server port user password &optional key
                                        cert)
              "Set related SMTP and SSL variables for supplied parameters."
              (setq smtpmail-smtp-server server
                    smtpmail-smtp-service port
                    smtpmail-auth-credentials (list (list server port user
                                                          password))
                    smtpmail-starttls-credentials (list (list
                                                         server port key cert)))
              (message
               "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)" server port user))

            (defun change-smtp ()
              "Change the SMTP server according to the current from line."
              (save-excursion
                (loop with from = (save-restriction
                                    (message-narrow-to-headers)
                                    (message-fetch-field "from"))
                      for (auth-mech address . auth-spec) in my/smtp-accounts
                      when (string-match address from)
                      do (cond
                          ((memq auth-mech '(cram-md5 plain login))
                           (return (apply 'set-smtp (cons auth-mech auth-spec))))
                          ((eql auth-mech 'ssl)
                           (return (apply 'set-smtp-ssl auth-spec)))
                          (t (error "Unrecognized SMTP auth. mechanism: `%s'." auth-mech)))
                      finally (error "Cannot infer SMTP information."))))

            ;; The previous function will complain if you fill the from field with
            ;; an account not present in my/smtp-accounts.
            (defvar %smtpmail-via-smtp (symbol-function 'smtpmail-via-smtp))
            (defun smtpmail-via-smtp (recipient smtpmail-text-buffer)
              (with-current-buffer smtpmail-text-buffer
                (change-smtp))
              (funcall (symbol-value '%smtpmail-via-smtp) recipient
                       smtpmail-text-buffer))))

;; apel
(use-package apel
  :defer t
  :load-path (lambda () (expand-file-name "apel/" user-emacs-directory)))

;; MIME language support
(use-package mml
  :config (progn
            ;; Put attachments at end of buffer
            (defun my/mml-attach-file--go-to-eob (orig-fun &rest args)
              "Go to the end of buffer before attaching files."
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char (point-max))
                  (apply orig-fun args))))
            (advice-add 'mml-attach-file :around #'my/mml-attach-file--go-to-eob)))

;; Message mode
(use-package message
  :config (progn

            ;; My version of gnus in my Mac does not handle html messages
            ;; correctly (the one in the netbook does, I guess it is a different
            ;; version). The following will chose plaintext every time this is
            ;; possible.
            (setq mm-discouraged-alternatives '("text/html" "text/richtext"))


            ;; decode html
            (use-package mm-decode
              :config
              (setq mm-discouraged-alternatives
                    '("text/html" "text/richtext")
                    mm-automatic-display
                    (-difference mm-automatic-display '("text/html" "text/enriched" "text/richtext"))))

            ;; use imagemagick, if available
            (when (and (fboundp 'imagemagick-register-types)
                       (executable-find "import"))
              (imagemagick-register-types))

            ;; Don’t add an empty line when quoting email
            (defun my/message-insert-citation-line ()
              "Insert a simple citation line."
              (when message-reply-headers
                (newline)
                (insert (mail-header-from message-reply-headers) " writes:")
                (newline)))
            (setq message-citation-line-function #'my/message-insert-citation-line)

            ;; We add a copy of the buffer to the kill ring, to make it easy to refer to it later.
            (defun my/copy-buffer-to-kill-ring ()
              "Copy buffer to kill ring."
              (interactive)
              (kill-ring-save (point-min) (point-max)))
            (add-hook 'message-send-hook #'my/copy-buffer-to-kill-ring)

            ;; Use w3m to display HTML mails
            ;; Use w3m to render html
            (if (executable-find "w3m")
                (setq mm-text-html-renderer 'w3m))

            ;; Full sized images
            (setq mm-inline-text-html-with-images t
                  mm-inline-large-images 'resize
                  mm-attachment-file-modes 420)

            ;; store email in ~/Mail directory
            (setq message-directory "~/Mail"

                  ;; message preferences
                  message-generate-headers-first t
                  message-kill-buffer-on-exit t
                  message-signature-file ".signature")

            ;; message mode hooks
            (add-hook 'message-mode-hook
                      (lambda ()
                        ;; Org goodies
                        (orgtbl-mode t)
                        (orgstruct-mode t)
                        (orgstruct++-mode t)

                        ;; Extra modes
                        (footnote-mode t)
                        (auto-fill-mode t)
                        (writegood-mode t)
                        (flyspell-mode t)))

            ;; Enable emacsclient in mutt
            (add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))
            (setq mail-header-separator "")
            (define-key message-mode-map (kbd "C-c C-c")
              '(lambda ()
                 "save and exit quickly"
                 (interactive)
                 (save-buffer)
                 (server-edit)))))

;; Enabling attaching files from dired
(use-package gnus-dired
  :config (progn
            (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

            ;; make the `gnus-dired-mail-buffers' function also work on
            ;; message-mode derived modes, such as mu4e-compose-mode
            (defun gnus-dired-mail-buffers ()
              "Return a list of active message buffers."
              (let (buffers)
                (save-current-buffer
                  (dolist (buffer (buffer-list t))
                    (set-buffer buffer)
                    (when (and (derived-mode-p 'message-mode)
                               (null message-sent-message-via))
                      (push (buffer-name buffer) buffers))))
                (nreverse buffers)))))

;; Flim, wanderlust requirement
(use-package std11
  :defer t
  :load-path (lambda () (expand-file-name "flim/" user-emacs-directory)))

;; All the icons, gnus plugin
(use-package all-the-icons-gnus
  :if (display-graphic-p)
  :after (dired all-the-icons gnus)
  :load-path (lambda () (expand-file-name "all-the-icons-gnus/" user-emacs-directory))
  :commands all-the-icons-gnus-setup
  :init (add-hook 'gnus-group-mode-hook 'all-the-icons-gnus-setup))

(provide 'setup-gnus)
;;; setup-gnus.el ends here

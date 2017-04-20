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

;; Make gnutls a bit safer
(setq gnutls-min-prime-bits 4096)

;; Gnus
(use-package gnus
  :defer 1
  :commands (gnus compose-mail switch-to-gnus)
  :bind (("M-G"   . switch-to-gnus)
         :map ctl-x-map
         ("m" . compose-mail))
  :config (progn
            ;; notmuch search
            (setq notmuch-message-headers '("Subject" "To" "Cc" "Date" "Reply-To"))

            ;; You need this to be able to list all labels in gmail
            (setq gnus-ignored-newsgroups "")

            ;; To be able to search within your gmail/imap mail
            (use-package nnir)

            ;; And this to configure gmail imap
            (setq gnus-select-method '(nnimap "gmail"
                                              (nnimap-address "imap.gmail.com")
                                              (nnimap-server-port 993)
                                              (nnimap-stream ssl)
                                              (nnir-search-engine imap)))

            ;; Setup gnus inboxes
            (setq gnus-secondary-select-methods
                  '((nnmaildir "mail"
                               (directory "~/Maildir")
                               (directory-files nnheader-directory-files-safe)
                               (get-new-mail nil))))

            ;; My version of gnus in my Mac does not handle html messages
            ;; correctly (the one in the netbook does, I guess it is a different
            ;; version). The following will chose plaintext every time this is
            ;; possible.
            (setq mm-discouraged-alternatives '("text/html" "text/richtext"))

            ;; Gnus news
            (setq gnus-summary-line-format "%U%R%z%d %I%(%[ %F %] %s %)\n")

            ;; A gravatar is an image registered to an e-mail address
            (setq gnus-treat-from-gravatar t)

            ;; Get smarter about filtering depending on what I reed or mark.
            ;; I use ! (tick) for marking threads as something that interests me.
            (setq gnus-use-adaptive-scoring t
                  gnus-default-adaptive-score-alist
                  '((gnus-unread-mark)
                    (gnus-ticked-mark (subject 10))
                    (gnus-killed-mark (subject -5))
                    (gnus-catchup-mark (subject -1))))

            ;; Use gnus for email
            (setq mail-user-agent 'gnus-user-agent)
            (setq read-mail-command 'gnus-user-agent)

            ;; Mode hooks
            (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
            (add-hook 'gnus-group-mode-hook 'hl-line-mode)
            (add-hook 'gnus-summary-mode-hook 'hl-line-mode)))

;; To be able to send email with your gmail/smtp mail
(use-package smtpmail
  :config (progn

            ;; Send mail using postfix
            ;; http://pragmaticemacs.com/emacs/using-postfix-instead-of-smtpmail-to-send-email-in-mu4e/
            ;; (setq send-mail-function 'sendmail-send-it
            ;;       message-send-mail-function 'message-send-mail-with-sendmail)

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
              (setq starttls-use-gnutls t
                    starttls-gnutls-program "gnutls-cli"
                    starttls-extra-arguments nil
                    smtpmail-smtp-server server
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

;; Message mode
(use-package message
  :config (progn
            (bind-key "C-c C-x f" #'org-footnote-action message-mode-map)

            ;; decode html
            (use-package mm-decode
              :config
              (setq mm-discouraged-alternatives
                    '("text/html" "text/richtext")
                    mm-automatic-display
                    (-difference mm-automatic-display '("text/html" "text/enriched" "text/richtext"))))

            ;; Use w3m to render html
            (if (executable-find "w3m")
                (setq mm-text-html-renderer 'w3m))

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

            ;; Put attachments at end of buffer
            (defun my/mml-attach-file--go-to-eob (orig-fun &rest args)
              "Go to the end of buffer before attaching files."
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char (point-max))
                  (apply orig-fun args))))
            (advice-add 'mml-attach-file :around #'my/mml-attach-file--go-to-eob)

            ;; We add a copy of the buffer to the kill ring, to make it easy to refer to it later.
            (defun my/copy-buffer-to-kill-ring ()
              "Copy buffer to kill ring."
              (interactive)
              (kill-ring-save (point-min) (point-max)))
            (add-hook 'message-send-hook #'my/copy-buffer-to-kill-ring)

            ;; message preferences
            (setq message-generate-headers-first t
                  message-kill-buffer-on-exit t
                  message-signature-file ".signature")

            ;; message mode hooks
            (add-hook 'message-mode-hook #'flyspell-mode)
            (add-hook 'message-mode-hook #'turn-on-orgstruct)
            (add-hook 'message-mode-hook #'turn-on-orgstruct++)
            (add-hook 'message-mode-hook #'turn-on-orgtbl)
            (add-hook 'message-mode-hook #'typo-mode)
            (add-hook 'message-mode-hook #'footnote-mode)
            (add-hook 'message-mode-hook #'turn-on-auto-fill)

            ;; Enable emacsclient in mutt
            (add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))
            (setq mail-header-separator "")
            (define-key message-mode-map (kbd "C-c C-c")  '(lambda ()
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

(provide 'setup-gnus)
;;; setup-gnus.el ends here

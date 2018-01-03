;;; setup-gnus.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017, 2018  Abelardo Jara-Berrocal

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

;; Electric completions of email addresses and the like
(use-package ecomplete)

;; To be able to search within your gmail/imap mail
(use-package nnir
  :config (progn
            ;; Define email sources
            (setq mail-sources '((maildir :path "~/Mail/INBOX" :subdirs ("cur" "new"))))

                  ;; mailbox settings
            (setq nnmail-expiry-wait 30
                  nnmail-crosspost nil
                  nnmail-extra-headers (quote (To Cc Newsgroups))
                  nnmail-scan-directory-mail-source-once t

                  ;; Directory containing Unix mbox; defaults to message-directory
                  nnfolder-directory "~/Mail")))

;; Gnus
(use-package gnus
  :after (starttls nnir epa)
  :commands (gnus compose-mail)
  :bind (("M-G" . gnus)
         :map ctl-x-map
         ("M"   . compose-mail))
  :config (progn

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

            (setq my/gnus-local '(nnmaildir "local"
                                            (directory "~/Mail")
                                            (directory-files nnheader-directory-files-safe)
                                            (get-new-mail nil)
                                            (nnir-search-engine notmuch))
                  my/gnus-gmail '(nnimap "gmail"
                                         (nnimap-address "imap.gmail.com")
                                         (nnimap-server-port 993)
                                         (nnimap-stream tls)))

            (setq my/gnus-gmane
                  '(nntp "gmane"
                         (nntp-address "news.gmane.org")
                         (nntp-port-number 563)
                         (nntp-open-connection-function nntp-open-tls-stream))
                  my/gnus-aioe
                  '(nntp "aioe"
                         (nntp-address "nntp.aioe.org")
                         (nntp-port-number 563)
                         (nntp-open-connection-function nntp-open-tls-stream)))

            ;; You need this to be able to list all labels in gmail
            (setq gnus-ignored-newsgroups            "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"

                  ;; Asynchronous support
                  gnus-asynchronous                  t

                  ;; Default levels
                  gnus-activate-level                2
                  gnus-group-default-list-level      3

                  ;; Enable checking news groups
                  gnus-check-new-newsgroups          t

                  ;; Dont ask question on exit
                  gnus-interactive-catchup           nil
                  gnus-interactive-exit              nil

                  ;; Add Unix mbox'es (mbsync); in case we have local email
                  gnus-select-method                 my/gnus-local

                  ;; Show email INBOX
                  gnus-permanently-visible-groups    "local"

                  ;; Secondary sources
                  gnus-secondary-select-methods  (quote
                                                  ((nnmaildir "" (directory "~/Mail")
                                                              (directory-files nnheader-directory-files-safe)
                                                              (get-new-mail nil)
                                                              (nnir-search-engine notmuch)
                                                              )))

                  ;; Archive methods
                  gnus-message-archive-method       gnus-select-method

                  ;; Display a button for MIME parts
                  gnus-buttonized-mime-types        '("multipart/alternative")

                  ;; Headers we wanna see:
                  gnus-visible-headers "^From:\\|^Subject:\\|^X-Mailer:\\|^X-Newsreader:\\|^Date:\\|^To:\\|^Cc:\\|^User-agent:\\|^Newsgroups:\\|^Comments:"

                  ;; Gnus news
                  gnus-summary-line-format (concat "%*%0{%U%R%z%d%}"
                                                   "%0{ %}(%2t)"
                                                   "%2{ %}%-23,23n"
                                                   "%1{ %}%1{%B%}%2{%-102,102s%}%-140="
                                                   "\n")

                  ;; A gravatar is an image registered to an e-mail address
                  gnus-treat-from-gravatar           t)

            ;; Add news servers
            (add-to-list 'gnus-secondary-select-methods my/gnus-gmane)

            ;; Use gnus for email
            (setq mail-user-agent             'gnus-user-agent)
            (setq read-mail-command           'gnus-user-agent)

            ;; Mode hooks
            (add-hook 'gnus-group-mode-hook   'gnus-topic-mode)
            (add-hook 'gnus-group-mode-hook   'hl-line-mode)
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
            (add-hook 'gnus-exit-gnus-hook    'gnus-group-sort-groups-by-rank)

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
                (message "Saving all MIME parts to %s...done" directory)))))

;; To be able to send email with your gmail/smtp mail
(use-package smtpmail
  :after gnus
  :config (progn
            ;; Using smptmail to assure portability; we dont always have postfix
            (setq message-send-mail-function 'smtpmail-send-it
                  send-mail-function         'smtpmail-send-it
                  smtpmail-smtp-server       "smtp.gmail.com"
                  smtpmail-smtp-service      465
                  smtpmail-debug-info        t
                  ;; StartTLS is evil.
                  smtpmail-stream-type       'ssl)))

;; apel
(use-package apel
  :defer t
  :load-path (lambda () (expand-file-name "apel/" user-emacs-directory)))

;; MIME language support
(use-package mml
  :demand t
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
  :demand t
  :config (progn

            ;; decode html
            (use-package mm-decode
              :demand t
              :config (progn
                        ;; Use w3m to render html
                        (if (executable-find "w3m")
                            (setq mm-text-html-renderer 'w3m))

                        ;; Full sized images
                        (setq mm-inline-text-html-with-images t
                              mm-inline-large-images 'resize
                              mm-attachment-file-modes 420
                              mm-discouraged-alternatives
                              '("text/html" "text/richtext" "application/msword")
                              mm-automatic-display
                              (-difference mm-automatic-display '("text/html" "application/msword" "text/enriched" "text/richtext")))))

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

            ;; message preferences
            (setq message-generate-headers-first t
                  message-kill-buffer-on-exit    t
                  message-signature-file         ".signature")

            ;; message mode hooks
            (add-hook 'message-mode-hook
                      (lambda ()
                        ;; Org goodies
                        (orgtbl-mode      t)
                        (orgstruct-mode   t)
                        (orgstruct++-mode t)

                        ;; Extra modes
                        (footnote-mode  t)
                        (auto-fill-mode t)
                        (writegood-mode t)
                        (flyspell-mode  t)))

            ;; Turn on PGP
            (add-hook 'message-mode-hook 'epa-mail-mode)

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

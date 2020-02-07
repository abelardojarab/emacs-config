;;; setup-gnus.el ---                         -*- lexical-binding: t; -*-

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

;; Mime support
(use-package mime-w3m
  :defer t
  :custom (mime-edit-split-message nil))

;; Electric completions of email addresses
(use-package ecomplete)

;; To be able to search within your gmail/imap mail
(use-package nnir
  :custom ((nnmail-expiry-wait 30)
           (nnmail-crosspost nil)
           (nnmail-scan-directory-mail-source-once t)
           (nnfolder-directory "~/Mail")))

;; Gnus
(use-package gnus
  :defer t
  :commands (gnus compose-mail)
  :bind (("M-G" . gnus)
         :map ctl-x-map
         ("M"   . compose-mail))
  :custom ((gnus-treat-from-gravatar           t)
           (gnus-asynchronous                  t)
           (gnus-use-demon                     nil)
           (gnus-activate-level                2)
           (gnus-group-default-list-level      3)
           (gnus-check-new-newsgroups          t)
           (gnus-interactive-catchup           nil)
           (gnus-interactive-exit              nil)
           (gnus-select-method                 '(nnnil ""))
           (gnus-permanently-visible-groups    "local")
           (gnus-message-archive-method        gnus-select-method))
  :config (progn
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

            ;; Setup local sources
            (setq my/gnus-local '((nnmaildir "local"
                                             (directory "~/Mail")
                                             (directory-files nnheader-directory-files-safe)
                                             (get-new-mail nil)
                                             (nnir-search-engine notmuch))

                                  ;; News servers
                                  (nntp "news.gwene.org"))
                  my/gnus-gmail '(nnimap "gmail"
                                         (nnimap-address "imap.gmail.com")
                                         (nnimap-server-port 993)
                                         (nnimap-stream tls)
                                         (nnimap-authenticator login)
                                         (nnimap-expunge-on-close 'ask)))
            (setq gnus-secondary-select-methods my/gnus-local)

            ;; You need this to be able to list all labels in gmail
            (setq gnus-ignored-newsgroups            "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"

                  ;; Display a button for MIME parts
                  gnus-inhibit-mime-unbuttonizing    t
                  gnus-buttonized-mime-types         '("multipart/alternative")

                  ;; Headers we wanna see:
                  gnus-visible-headers "^From:\\|^Subject:\\|^X-Mailer:\\|^X-Newsreader:\\|^Date:\\|^To:\\|^Cc:\\|^User-agent:\\|^Newsgroups:\\|^Comments:"

                  ;; Gnus news
                  gnus-summary-line-format (concat "%*%0{%U%R%z%d%}"
                                                   "%0{ %}(%2t)"
                                                   "%2{ %}%-23,23n"
                                                   "%1{ %}%1{%B%}%2{%-102,102s%}%-140="
                                                   "\n"))

            ;; Use gnus for email
            (setq mail-user-agent             'gnus-user-agent)
            (setq read-mail-command           'gnus-user-agent)

            ;; Mode hooks
            (add-hook 'gnus-group-mode-hook   #'gnus-topic-mode)

            ;; Truncate lines
            (mapc (lambda (mode)
                    (add-hook mode (lambda ()
                                     (hl-line-mode t)
                                     (visual-line-mode -1)
                                     (toggle-truncate-lines t)
                                     (setq truncate-lines t))))
                  '(gnus-summary-mode-hook
                    gnus-group-mode-hook
                    gnus-topic-mode-hook))

            ;; Sort email
            (add-hook 'gnus-summary-exit-hook #'gnus-summary-bubble-group)
            (add-hook 'gnus-suspend-gnus-hook #'gnus-group-sort-groups-by-rank)
            (add-hook 'gnus-exit-gnus-hook    #'gnus-group-sort-groups-by-rank)

            ;; Get smarter about filtering depending on what I reed or mark.
            ;; I use ! (tick) for marking threads as something that interests me.
            (setq gnus-default-adaptive-score-alist
                  '((gnus-dormant-mark (from 20) (subject 100))
                    (gnus-ticked-mark (subject 30))
                    (gnus-read-mark (subject 30))
                    (gnus-del-mark (subject -150))
                    (gnus-catchup-mark (subject -150))
                    (gnus-killed-mark (subject -1000))
                    (gnus-expirable-mark (from -1000) (subject -1000)))
                  gnus-use-adaptive-scoring 'line
                  gnus-score-decay-constant 1
                  gnus-score-decay-scale    0.03
                  gnus-decay-scores         t)

            ;; Save email attachments
            (defun my/gnus-summary-save-parts (&optional arg)
              (interactive "P")
              (let ((directory "~/Downloads"))
                (message "Saving all MIME parts to %s..." directory)
                (gnus-summary-save-parts ".*" directory arg)
                (message "Saving all MIME parts to %s...done" directory)))))

;; apel
(use-package apel
  :defer t)

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
  :commands my/message-mode-init
  :hook (message-mode . my/message-mode-init)
  :custom ((message-generate-headers-first t)
           (message-kill-buffer-on-exit    t)
           (message-signature-file         ".signature"))
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

            ;; message mode hooks
            (defun my/message-mode-init ()
              ;; Turn on PGP
              (epa-mail-mode t)

              ;; Org goodies
              (orgtbl-mode      t)
              (orgstruct-mode   t)
              (orgstruct++-mode t)

              ;; Extra modes
              (footnote-mode  t)
              (auto-fill-mode t)
              (writegood-mode t)
              (flyspell-mode  t))

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
  :defer t
  :hook (dired-mode . turn-on-gnus-dired-mode)
  :commands turn-on-gnus-dired-mode
  :config (progn

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
  :defer t)

;; All the icons, gnus plugin
(use-package all-the-icons-gnus
  :if (display-graphic-p)
  :after (dired all-the-icons gnus)
  :commands all-the-icons-gnus-setup
  :hook (gnus-group-mode . all-the-icons-gnus-setup))

;; Easy integration of mbsync with gnus
(use-package mbsync
  :hook (mbsync-exit . gnus-group-get-new-news)
  :bind (:map gnus-group
              ("f" . 'mbsync)))

(provide 'setup-gnus)
;;; setup-gnus.el ends here

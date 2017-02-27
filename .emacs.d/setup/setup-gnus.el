;;; setup-gnus.el ---                                -*- lexical-binding: t; -*-

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

;; Gnus
(use-package gnus
  :defer 1
  :commands (gnus compose-mail)
  :config (progn
            ;; notmuch search
            (setq notmuch-message-headers '("Subject" "To" "Cc" "Date" "Reply-To"))

            ;; Setup gnus inboxes
            (setq gnus-select-method '(nnnil ""))
            (setq gnus-secondary-select-methods
                  '((nntp "news.gmane.org")
                    (nnmaildir "mail"
                               (directory "~/Maildir")
                               (directory-files nnheader-directory-files-safe)
                               (get-new-mail nil))))

            ;; Gnus news
            (setq gnus-summary-line-format "%U%R%z%d %I%(%[ %F %] %s %)\n")

            ;; Get smarter about filtering depending on what I reed or mark.
            ;; I use ! (tick) for marking threads as something that interests me.
            (setq gnus-use-adaptive-scoring t
                  gnus-treat-from-gravatar t)

            (setq gnus-default-adaptive-score-alist
                  '((gnus-unread-mark)
                    (gnus-ticked-mark (subject 10))
                    (gnus-killed-mark (subject -5))
                    (gnus-catchup-mark (subject -1))))


            ;; gnus setup
            ;; (gnus-registry-initialize)

            ))

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

            ;; Send mail using postfix
            ;; http://pragmaticemacs.com/emacs/using-postfix-instead-of-smtpmail-to-send-email-in-mu4e/

            ;; Postfix configuration according to:
            ;; https://www.linode.com/docs/email/postfix/postfix-smtp-debian7 and
            ;; https://www.howtoforge.com/tutorial/configure-postfix-to-use-gmail-as-a-mail-relay/
            ;; https://www.digitalocean.com/community/tutorials/how-to-install-and-configure-postfix-as-a-send-only-smtp-server-on-ubuntu-14-04
            (setq send-mail-function 'sendmail-send-it)
            (setq message-send-mail-function 'message-send-mail-with-sendmail)

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

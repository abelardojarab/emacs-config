;;; setup-email.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@ubuntu-MacBookPro>
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

;; Message mode
(use-package message
  :config (progn
            (bind-key "C-c C-x f" #'org-footnote-action message-mode-map)

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
            (setq message-generate-headers-first t)
            (add-hook 'message-mode-hook #'flyspell-mode)
            (add-hook 'message-mode-hook #'turn-on-orgstruct)
            (add-hook 'message-mode-hook #'turn-on-orgstruct++)
            (add-hook 'message-mode-hook #'turn-on-orgtbl)
            (add-hook 'message-mode-hook #'typo-mode)
            (add-hook 'message-mode-hook #'flyspell-mode)
            (add-hook 'message-mode-hook #'footnote-mode)
            (add-hook 'message-mode-hook #'turn-on-auto-fill)))

;; Attachments are mostly handled using the helm baloo interface below
;; but sometimes we want to send files from a directory
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
  :load-path (lambda () (expand-file-name "flim/" user-emacs-directory)))

;; Wanderlust
(use-package wl
  :defer t
  :init (progn
          (add-to-list 'load-path (expand-file-name "wanderlust/elmo" user-emacs-directory))
          (add-to-list 'load-path (expand-file-name "semi" user-emacs-directory)))
  :commands (wl wl-draft wl-other-frame)
  :load-path (lambda () (expand-file-name "wanderlust/wl/" user-emacs-directory))
  :config (let ((wl-root-dir "~/.emacs.cache/config/"))

            ;; File locations
            (setq wl-init-file (concat wl-root-dir "wl.el")
                  wl-folders-file (concat wl-root-dir "folders"))

            ;; Default look
            (setq wl-message-visible-field-list '("^To" "^Subject" "^From" "^Date" "^Cc"))
            (setq wl-message-ignored-field-list '("^"))

            ;; Mime support
            (setq wl-summary-toggle-mime "mime")
            (use-package mime-w3m)
            (setq mime-edit-split-message nil)
            (setq wl-draft-reply-buffer-style 'full)

            ;; IMAP
            (setq elmo-imap4-default-authenticate-type 'clear)
            (setq elmo-imap4-default-server "imap.gmail.com")
            ;; (setq elmo-imap4-default-user "username@gmail.com")
            (setq elmo-imap4-default-port '993)
            (setq elmo-imap4-default-stream-type 'ssl)
            (setq elmo-imap4-use-modified-utf7 t)

            ;; SMTP
            (setq wl-smtp-connection-type 'starttls)
            (setq wl-smtp-posting-port 587)
            (setq wl-smtp-authenticate-type "plain")
            ;; (setq wl-smtp-posting-user "username")
            (setq wl-smtp-posting-server "smtp.gmail.com")
            (setq wl-local-domain "gmail.com")

            ;; Folders
            (setq wl-default-folder "%Inbox")
            (setq wl-default-spec "%")
            (setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
            (setq wl-trash-folder "%[Gmail]/Trash")
            (setq wl-folder-check-async t)

            ;; Look in zip files as if they are folders
            (setq elmo-archive-treat-file t)

            ;; Expiration policies
            (setq wl-expire-alist
                  '(("^\\+trash$"   (date 14) remove) ;; delete
                    ("^\\+tmp$"     (date 7) trash) ;; re-file to wl-trash-folder
                    ("^\\%inbox"    (date 30) wl-expire-archive-date) ;; archive by year and month (numbers discarded)
                    ))

            ;; Show sent mail by who it was to
            (setq wl-summary-showto-folder-regexp ".*")
            (setq wl-summary-from-function 'wl-summary-default-from)

            ;; Assure we use mu4e
            (if (boundp 'mail-user-agent)
                (setq mail-user-agent 'wl-user-agent))
            (if (fboundp 'define-mail-user-agent)
                (define-mail-user-agent
                  'wl-user-agent
                  'wl-user-agent-compose
                  'wl-draft-send
                  'wl-draft-kill
                  'mail-send-hook))

            ;; Assure we use mu4e
            (setq mail-user-agent 'wl-user-agent)
            (setq read-mail-command 'wl-user-agent)
            (setq gnus-dired-mail-mode 'wl-user-agent)))

;; mu4e (asynchronous email client)
(use-package mu4e
  :defer t
  :if (executable-find "mu")
  :load-path (lambda () (expand-file-name "mu/mu4e/" user-emacs-directory))
  :commands (mu4e/start)
  :init (progn
          ;; Assure .emacs.cache/mu4e exists
          (defvar my/mu4e-maildir "~/.emacs.cache/mu4e" "Location of the mu4e mailbox")
          (if (not (file-exists-p my/mu4e-maildir))
              (make-directory my/mu4e-maildir) t))
  :config (progn
            (setq mu4e-headers-skip-duplicates t
                  mu4e-use-fancy-chars t
                  mu4e-view-show-images t
                  message-kill-buffer-on-exit t
                  mu4e-hide-index-messages t
                  mu4e-auto-retrieve-keys t
                  mu4e-compose-dont-reply-to-self t
                  mu4e-compose-in-new-frame t
                  mu4e-split-view 'horizontal
                  mu4e-headers-visible-columns 122
                  mu4e-headers-visible-lines 16
                  mu4e-context-policy 'pick-first
                  mu4e-compose-context-policy 'ask
                  mu4e-change-filenames-when-moving t
                  mu4e-confirm-quit nil)

            (setq mu4e-use-fancy-chars t
                  mu4e-headers-draft-mark     '("D" . "⚒ ") ; draft
                  mu4e-headers-seen-mark      '("S" . "☑ ")  ; seen
                  mu4e-headers-unread-mark    '("u" . "☐ ")  ; unseen
                  mu4e-headers-new-mark       '("N" .  "")
                  mu4e-headers-seen-mark      '("S" . "")    ; seen
                  mu4e-headers-unread-mark    '("u" . "")    ; unseen
                  mu4e-headers-flagged-mark   '("F" .  "⚵ ") ; flagged
                  mu4e-headers-new-mark       '("N" .  "✉ ")  ; new
                  mu4e-headers-replied-mark   '("R" . "↵ ")  ; replied
                  mu4e-headers-passed-mark    '("P" . "⇉ ")  ; passed
                  mu4e-headers-encrypted-mark '("x" . "⚷ ")   ; encrypted
                  mu4e-headers-signed-mark    '("s" . "✍ ")  ; signed
                  mu4e-headers-empty-parent-prefix '("-" . "○")
                  mu4e-headers-first-child-prefix '("\\" . "▶")
                  mu4e-headers-has-child-prefix '("+" . "●"))

            (setq mu4e-maildir my/mu4e-maildir
                  mu4e-drafts-folder "/[Google Mail]/.Drafts"
                  mu4e-sent-folder   "/[Google Mail]/.Sent Mail"
                  mu4e-trash-folder  "/[Google Mail]/.Bin")

            (setq mu4e-maildir-shortcuts
                  '(("/Inbox"                        . ?i)
                    ("/[Google Mail]/.Drafts"        . ?d)
                    ("/[Google Mail]/.Sent Mail"     . ?s)
                    ("/[Google Mail]/.Bin"           . ?t)
                    ("/royal holloway/.msc projects" . ?m)))

            (setq mu4e-attachment-dir "~/Downloads")

            (add-to-list
             'mu4e-bookmarks
             '("flag:unread NOT flag:trashed AND (flag:list OR from:trac@sagemath.org OR maildir:/bulk OR maildir:/research/.lists)"
               "Unread bulk messages" ?l))

            (add-to-list
             'mu4e-bookmarks
             '("flag:unread NOT flag:trashed AND NOT flag:list AND (maildir:\"/royal holloway\" OR maildir:/INBOX)"
               "Unread messages addressed to me" ?i))

            (add-to-list
             'mu4e-bookmarks
             '("mime:application/* AND NOT mime:application/pgp* AND (maildir:\"/royal holloway\" OR maildir:/INBOX)"
               "Messages with attachments for me." ?d) t)

            (add-to-list
             'mu4e-bookmarks
             '("flag:flagged"
               "Flagged messages" ?f) t)

            (add-to-list
             'mu4e-bookmarks
             '("(maildir:\"/[Google Mail]/.Sent Mail\" OR maildir:\"/royal holloway/.sent\") AND date:7d..now"
               "Sent in last 7 days" ?s) t)

            (setq mu4e-get-mail-command "timelimit -t 180 -T 180 mbsync googlemail-default"
                  mu4e-update-interval nil)

            (use-package mu4e-contrib
              :config (setq mu4e-html2text-command 'mu4e-shr2text))

            ;; Use <TAB> to preview messages and q to close previews.
            (use-package mu4e-view)
            (use-package mu4e-headers)
            (defun my/preview-message ()
              (interactive)
              (mu4e-headers-view-message)
              (sleep-for 0.1) ;; this is a HACK
              (select-window (previous-window)))

            ;; based on (mu4e-select-other-view)
            (defun my/close-message-view ()
              (interactive)
              (let* ((other-buf mu4e~view-buffer)
                     (other-win (and other-buf (get-buffer-window other-buf))))
                (if (window-live-p other-win)
                    (progn
                      (select-window other-win)
                      (sleep-for 0.1)
                      (mu4e~view-quit-buffer))
                  (mu4e~headers-quit-buffer))))
            (bind-key "<tab>" #'my/preview-message mu4e-headers-mode-map)
            (bind-key "q" #'my/close-message-view mu4e-headers-mode-map)
            (bind-key "z" #'my/close-message-view mu4e-headers-mode-map)
            (setq mu4e-headers-fields '((:human-date . 12)
                                        (:flags . 6)
                                        (:mailing-list . 22)
                                        (:from . 32)
                                        (:subject)))

            ;; View emails with width restriction (80 so that HTML crap doesn’t break too easily either)
            (defun my/mu4e-view-mode-hook ()
              (set-fill-column 80)
              (visual-line-mode)
              (visual-fill-column-mode))
            (add-hook 'mu4e-view-mode-hook #'my/mu4e-view-mode-hook)
            (add-to-list 'mu4e-view-actions
                         '("browser" . mu4e-action-view-in-browser) t)
            (bind-key "<home>" #'beginning-of-visual-line mu4e-view-mode-map)
            (bind-key "<end>" #'end-of-visual-line mu4e-view-mode-map)

            ;; My uni likes “Lastname, Firstname (Year)” which is weird, so we fix it.
            ;; Some people like to capitalize their LASTNAME and then write the first name
            ;; Some people like to send incomplete data, so we maintain a local replacement list
            (defcustom my-mu4e-name-replacements nil
              "replacement names from e-mail addresses"
              :type '(alist :key-type string :value-type string)
              :group 'my)

            (defun my/canonicalise-contact-name (name)
              (let* ((case-fold-search nil)
                     (name (or name ""))
                     (email (replace-regexp-in-string "^.* <?\\([^ ]+@[^ ]+\.[^ >]+\\)>?" "\\1" name))
                     ;; look up email address and use entry if found
                     (candidate (cdr (assoc email my-mu4e-name-replacements))))
                (if candidate candidate
                  (progn
                    (setq name (replace-regexp-in-string "^\\(.*\\) [^ ]+@[^ ]+\.[^ ]" "\\1" name)) ;; drop email address
                    (setq name (replace-regexp-in-string "^\"\\(.*\\)\"" "\\1" name)) ;; strip quotes
                    (setq name (replace-regexp-in-string "^\\(\\<[[:upper:]]+\\>\\) \\(.*\\)" "\\2 \\1" name)) ;; deal with YELL’d last names
                    (setq name (replace-regexp-in-string "^\\(.*\\), \\([^ ]+\\).*" "\\2 \\1" name)))))) ;; Foo, Bar becomes Bar Foo

            (defun my/mu4e-contact-rewrite-function (contact)
              (let* ((name (or (plist-get contact :name) ""))
                     (mail (plist-get contact :mail))
                     (case-fold-search nil))
                (plist-put contact :name (my/canonicalise-contact-name name))
                contact))
            (setq mu4e-contact-rewrite-function #'my/mu4e-contact-rewrite-function)

            ;; Useful in email templates
            (defun my/yas-get-names-from-fields (fields)
              (let (names
                    ret
                    name
                    point-end-of-line
                    (search-regexp (mapconcat (lambda (arg)
                                                (concat "^" arg ": "))
                                              fields "\\|"))
                    (case-fold-search nil))
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward search-regexp nil t)
                    (save-excursion
                      (setq point-end-of-line (re-search-forward "$")))
                    (setq name (buffer-substring-no-properties (point) point-end-of-line))
                    (setq name (split-string name "[^ ]+@[^ ]+," t " ")) ;; split on email@address,
                    (setq names (append names name)))
                  (dolist (name names)
                    (setq name (my/canonicalise-contact-name name))
                    (if (string-match "\\([^ ,]+\\)" name)
                        (progn
                          (setq name (capitalize (replace-regexp-in-string "~" " " (match-string 1 name))))
                          (if ret
                              (setq ret (concat ret ", " name))
                            (setq ret name)))))
                  (if ret ret "there"))))

            (defun my/yas-get-names-from-to-fields ()
              (interactive)
              (my/yas-get-names-from-fields '("To")))

            ;; We also add a convenient function to add new replacements.
            (defun my/add-mu4e-name-replacement ()
              (interactive)
              (let* ((email (helm-read-string "Email: " (get-text-property (point) 'email)))
                     (name  (helm-read-string "Name: "
                                              (my/canonicalise-contact-name
                                               (get-text-property (point) 'long)))))
                (add-to-list 'my-mu4e-name-replacements (cons email name) t)
                (customize-save-variable 'my-mu4e-name-replacements my-mu4e-name-replacements)))

            (bind-key "N" #'my/add-mu4e-name-replacement mu4e-view-mode-map)

            ;; Ignore some email addresses when auto completing:
            (setq mu4e-compose-complete-ignore-address-regexp (rx  (or (seq "no" (zero-or-one "-") "reply")
                                                                       (seq "replyto-" (one-or-more char) "@plus.google.com")
                                                                       (seq "@" (one-or-more char) ".twitter.com")
                                                                       (seq "do-not-reply" (zero-or-more char) "@"))))
            ;; Launch dedicated email frame
            (defun mu4e/start ()
              (interactive)
              (select-frame (make-frame-command))
              (sleep-for 0.1) ;; this is a HACK
              (toggle-frame-maximized)
              (sleep-for 0.1) ;; this is a HACK
              (set-frame-size (selected-frame) 200 64)
              (mu4e))

            ;; Kill mu4e frame.
            (defun my/mu4e-quit-session ()
              (interactive)
              (kill-buffer)
              (delete-frame))
            (bind-key "Q" #'my/mu4e-quit-session mu4e-main-mode-map)

            ;; Assure we use mu4e
            (setq mail-user-agent 'mu4e-user-agent)
            (setq read-mail-command 'mu4e-user-agent)
            (setq gnus-dired-mail-mode 'mu4e-user-agent)))

;; Link to mu4e messages and threads.
(use-package org-mu4e
  :if (executable-find "mu")
  :after mu4e
  :load-path (lambda () (expand-file-name "mu/mu4e/" user-emacs-directory))
  :config (setq org-mu4e-link-query-in-headers-mode t))

;; Search mu with helm-mu.
(use-package helm-mu
  :if (executable-find "mu")
  :after mu4e
  :bind (:mu4e-main-mode-map
         ("S" . helm-mu))
  :load-path (lambda () (expand-file-name "helm-mu/" user-emacs-directory)))

(provide 'setup-email)
;;; setup-email.el ends here

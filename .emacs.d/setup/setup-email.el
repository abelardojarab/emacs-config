;;; setup-email.el ---                               -*- lexical-binding: t; -*-

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

;; To be able to send email with your gmail/smtp mail
(use-package smtpmail
  :after gnus
  :custom ((message-send-mail-function         'smtpmail-send-it)
           (send-mail-function                 'smtpmail-send-it)
           (smtpmail-default-smtp-server       "smtp.gmail.com")
           (smtpmail-smtp-server               "smtp.gmail.com")
           (smtpmail-smtp-service              465)
           (smtpmail-debug-info                t)
           (smtpmail-stream-type               'ssl)))

;; mu4e (asynchronous email client)
(use-package mu4e
  :defer t
  :if (executable-find "mu")
  :load-path (lambda () (expand-file-name "mu/mu4e/" user-emacs-directory))
  :commands mu4e
  :init (if (not (file-exists-p my/mu4e-maildir))
            (make-directory my/mu4e-maildir) t)
  :custom ((mu4e-headers-skip-duplicates      t)
           (mu4e-use-fancy-chars              t)
           (mu4e-view-show-images             t)
           (mu4e-hide-index-messages          t)
           (mu4e-auto-retrieve-keys           t)
           (mu4e-compose-dont-reply-to-self   t)
           (mu4e-compose-in-new-frame         t)
           (mu4e-split-view                   'horizontal)
           (mu4e-headers-visible-columns      122)
           (mu4e-headers-visible-lines        16)
           (mu4e-context-policy               'pick-first)
           (mu4e-compose-context-policy       'ask)
           (mu4e-change-filenames-when-moving t)
           (mu4e-confirm-quit                 nil)
           (mu4e-update-interval              nil)
           (mu4e-use-fancy-chars              t)
           (mu4e-compose-format-flowed        nil))
  :config (progn
            (setq mu4e-get-mail-command             "timelimit -t 180 -T 180 mbsync gmail")
            (setq mu4e-headers-draft-mark          '("D"  . "⚒  ") ;; draft
                  mu4e-headers-seen-mark           '("S"  . "☑  ") ;; seen
                  mu4e-headers-unread-mark         '("u"  . "☐  ") ;; unseen
                  mu4e-headers-new-mark            '("N"  . "")
                  mu4e-headers-seen-mark           '("S"  . "") ;;  seen
                  mu4e-headers-unread-mark         '("u"  . "") ;;  unseen
                  mu4e-headers-flagged-mark        '("F"  . "⚵  ") ;; flagged
                  mu4e-headers-new-mark            '("N"  . "✉  ") ;; new
                  mu4e-headers-replied-mark        '("R"  . "↵  ") ;; replied
                  mu4e-headers-passed-mark         '("P"  . "⇉  ") ;; passed
                  mu4e-headers-encrypted-mark      '("x"  . "⚷  ") ;; encrypted
                  mu4e-headers-signed-mark         '("s"  . "✍  ") ;; signed
                  mu4e-headers-empty-parent-prefix '("-"  . "○")
                  mu4e-headers-first-child-prefix  '("\\" . "▶")
                  mu4e-headers-has-child-prefix    '("+"  . "●"))

            (setq mu4e-maildir       my/mu4e-maildir
                  mu4e-refile-folder "/[Gmail].Archive"
                  mu4e-drafts-folder "/[Gmail].Drafts"
                  mu4e-sent-folder   "/[Gmail].Sent Mail"
                  mu4e-trash-folder  "/[Gmail].Bin")

            (setq mu4e-maildir-shortcuts
                  '(("/INBOX"                 . ?i)
                    ("/[Gmail].Drafts"        . ?d)
                    ("/[Gmail].Sent Mail"     . ?s)
                    ("/[Gmail].Bin"           . ?t)))

            (setq mu4e-attachment-dir "~/Downloads")

            (add-to-list
             'mu4e-bookmarks
             '("flag:unread NOT flag:trashed AND (flag:list OR maildir:/bulk)"
               "Unread bulk messages" ?l))

            (add-to-list
             'mu4e-bookmarks
             '("flag:unread NOT flag:trashed AND NOT flag:list AND maildir:/INBOX"
               "Unread messages addressed to me" ?i))

            (add-to-list
             'mu4e-bookmarks
             '("mime:application/* AND NOT mime:application/pgp* AND OR maildir:/INBOX"
               "Messages with attachments for me." ?d) t)

            (add-to-list
             'mu4e-bookmarks
             '("flag:flagged"
               "Flagged messages" ?f) t)

            (add-to-list
             'mu4e-bookmarks
             '("maildir:\"/[Gmail].Sent Mail\" AND date:7d..now"
               "Sent in last 7 days" ?s) t)

            (use-package mu4e-contrib
              :demand t
              :config (setq mu4e-html2text-command 'mu4e-shr2text))

            (use-package mu4e-headers
              :demand t
              ;; Use <TAB> to preview messages and q to close previews.
              :bind (:map mu4e-headers-mode-map
                          ("<tab>"  . my/preview-message)
                          ("q"      . my/close-message-view))
              :config (progn
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
                        (setq mu4e-headers-fields '((:human-date   . 12)
                                                    (:flags        . 6)
                                                    (:mailing-list . 22)
                                                    (:from         . 32)
                                                    (:subject)))))

            (use-package mu4e-view
              :demand t
              :bind (:map mu4e-view-mode-map
                          ("<home>" . beginning-of-visual-line)
                          ("<end>"  . end-of-visual-line)
                          ("N"      . my/add-mu4e-name-replacement))
              :config (progn
                        ;; View emails with width restriction (80 so that HTML crap doesn’t break too easily either)
                        (defun my/mu4e-view-mode-hook ()
                          (set-fill-column 80)
                          (visual-line-mode)
                          (visual-fill-column-mode))
                        (add-hook 'mu4e-view-mode-hook #'my/mu4e-view-mode-hook)
                        (add-to-list 'mu4e-view-actions
                                     '("browser" . mu4e-action-view-in-browser) t)

                        ;; My uni likes “Lastname, Firstname (Year)” which is weird, so we fix it.
                        ;; Some people like to capitalize their LASTNAME and then write the first name
                        ;; Some people like to send incomplete data, so we maintain a local replacement list
                        (defcustom my/mu4e-name-replacements nil
                          "replacement names from e-mail addresses"
                          :type '(alist :key-type string :value-type string)
                          :group 'my)

                        (defun my/canonicalise-contact-name (name)
                          (let* ((case-fold-search nil)
                                 (name (or name ""))
                                 (email (replace-regexp-in-string "^.* <?\\([^ ]+@[^ ]+\.[^ >]+\\)>?" "\\1" name))
                                 ;; look up email address and use entry if found
                                 (candidate (cdr (assoc email my/mu4e-name-replacements))))
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
                            (add-to-list 'my/mu4e-name-replacements (cons email name) t)
                            (customize-save-variable 'my/mu4e-name-replacements my/mu4e-name-replacements)))))

            ;; Assure we use mu4e
            (setq mail-user-agent 'mu4e-user-agent)
            (setq read-mail-command 'mu4e-user-agent)
            (setq gnus-dired-mail-mode 'mu4e-user-agent)

            ;; Link to mu4e messages and threads.
            (use-package org-mu4e
              :if (executable-find "mu")
              :demand t
              :load-path (lambda () (expand-file-name "mu/mu4e/" user-emacs-directory))
              :config (setq org-mu4e-link-query-in-headers-mode t))

            ;; Search mu with helm-mu.
            (use-package helm-mu
              :if (executable-find "mu")
              :demand t
              :bind (:map mu4e-main-mode-map
                          ("S" . helm-mu)))))

(provide 'setup-email)
;;; setup-email.el ends here

;;; setup-desktop.el --- Desktop/session & dashboard  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2025  Abelardo Jara-Berrocal
;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
;; License: GPL-3.0-or-later

;;; Commentary:
;; Desktop/session persistence, savehist/saveplace, and dashboard.

;;; Code:

;; Savehist: persist minibuffer/search histories
(use-package savehist
  :commands savehist-mode
  :init (savehist-mode 1)
  :custom
  ;; FIX: correct variable names (was '(search ring regexp-search-ring))
  (savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
  (savehist-autosave-interval 120)
  :config
  (setq savehist-file (expand-file-name "savehist" my/emacs-cache-dir))
  ;; Strip text properties from kill-ring before saving to avoid bloating
  ;; the savehist file with fonts/overlays (per Doom Emacs).
  (add-hook 'savehist-save-hook
            (lambda ()
              (setq kill-ring
                    (mapcar #'substring-no-properties
                            (cl-remove-if-not #'stringp kill-ring))))))

;; Filecache (disabled)
(use-package file-cache
  :disabled t
  :config
  (message "Loading file cache...")
  (file-cache-add-directory "~/")
  (file-cache-add-directory-list '("~/Desktop" "~/Documents" "~/workspace")))

;; Remember point in files
(use-package saveplace
  :demand t                      ;; ensure the library is loaded now
  :init
  (setq save-place t)            ;; keep compatibility for older Emacs
  :config
  ;; If running on very old Emacs without save-place-mode, fall back gracefully
  (if (fboundp 'save-place-mode)
      (save-place-mode 1)
    (setq-default save-place t)) ;; legacy fallback

  (setq save-place-file
        (expand-file-name "emacs.saveplace" my/emacs-cache-dir)))

;; Desktop: save/restore sessions
(use-package desktop
  :commands (desktop-save-mode desktop-read desktop-save save-buffer-display-time)
  :init
  ;; Save desktop a minute after idle; use cache dir
  (setq-default desktop-missing-file-warning nil)
  (setq desktop-dirname                    (file-name-as-directory my/emacs-cache-dir)
        desktop-base-file-name             "emacs.desktop"
        desktop-base-lock-name             "lock"
        desktop-path                       (list desktop-dirname)
        desktop-load-locked-desktop        t
        desktop-file-name-format           'absolute
        desktop-restore-frames             nil
        desktop-restore-in-current-display t
        desktop-restore-forces-onscreen    nil
        desktop-restore-eager              0
        desktop-auto-save-timeout          60
        ;; Ask before creating a new desktop file
        desktop-save                       'ask-if-new
        desktop-globals-to-save
        '((extended-command-history . 30)
          (file-name-history        . 100)
          (grep-history             . 30)
          (compile-history          . 30)
          (minibuffer-history       . 50)
          (query-replace-history    . 60)
          (read-expression-history  . 60)
          (regexp-history           . 60)
          (regexp-search-ring       . 20)
          (search-ring              . 20)
          (shell-command-history    . 50)
          (ido-buffer-history       . 100)
          (ido-last-directory-list  . 100)
          (ido-work-directory-list  . 100)
          (ido-work-file-list       . 100)
          (magit-read-rev-history   . 50)
          tags-file-name
          register-alist))
  :config
  ;; Don’t save modes you’ve disabled
  (dolist (mode my/desktop-modes-disabled)
    (add-to-list 'desktop-modes-not-to-save mode))

  ;; Persist and restore buffer-display-time
  (add-to-list 'desktop-locals-to-save 'buffer-display-time-1)
  (make-variable-buffer-local 'buffer-display-time-1)

  (defun save-buffer-display-time ()
    (mapc (lambda (buf)
            (with-current-buffer buf
              (setq buffer-display-time-1
                    (or buffer-display-time (current-time)))))
          (buffer-list)))

  (defun set-buffer-display-time ()
    (mapc (lambda (buf)
            (with-current-buffer buf
              (setq buffer-display-time buffer-display-time-1)))
          (buffer-list)))

  (add-hook 'desktop-save-hook #'save-buffer-display-time)
  (add-hook 'desktop-after-read-hook #'set-buffer-display-time)

  (desktop-save-mode 1))

;; Dashboard startup screen (GUI only)
(use-package dashboard
  :if (display-graphic-p)
  :demand t
  :custom
  (dashboard-center-content t)
  (dashboard-icon-type 'all-the-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-set-footer nil)
  (dashboard-projects-backend 'projectile)
  (dashboard-display-icons-p t)
  (dashboard-path-max-length 20)
  (dashboard-page-separator "\n\f\f\n")
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     (agenda   . 5)))
  (dashboard-modify-heading-icons '((recents  . "file-text")
                                    (projects . "code")
                                    (agenda   . "calendar")
                                    (bookmarks . "book")))
  :config
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (toggle-truncate-lines t)
              (setq truncate-lines t)))

  ;; Title with kernel on non-Windows
  (unless (eq system-type 'windows-nt)
    (setq dashboard-banner-logo-title
          (concat "GNU Emacs " emacs-version
                  "  kernel "
                  (car (split-string (shell-command-to-string "uname -r") "-"))
                  "  x86_64")))

  ;; Banner image from user-emacs-directory
  (setq dashboard-startup-banner (expand-file-name "emacs.png" user-emacs-directory))

  (dashboard-setup-startup-hook))

(use-package page-break-lines
  :hook ((dashboard-mode  . page-break-lines-mode)
         (org-agenda-mode . page-break-lines-mode)))

(provide 'setup-desktop)
;;; setup-desktop.el ends here

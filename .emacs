;; -*-mode: Emacs-Lisp; -*-
;; Copyright (C) 1996-2025 Abelardo Jara-Berrocal
;; URL: https://jaraberrocal.readmyblog.org
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-quit t)
(setq debug-on-error t)

;; Defer GC during startup; restore after init.
(defvar my/gc-cons-threshold-default (* 32 1024 1024))
(defvar my/gc-cons-percentage-default 0.1)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/gc-cons-threshold-default
                  gc-cons-percentage my/gc-cons-percentage-default)))

;; Larger read buffer helps lsp/eglot and subprocess-heavy code.
(setq read-process-output-max (* 4 1024 1024))

;; Measure overall startup time
(defvar my/start-time (current-time))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2fs with %d GCs"
                     (float-time (time-subtract (current-time) my/start-time))
                     gcs-done)))

;; Profiled require
(defun my/profile-require (feature)
  "Require FEATURE and print how long it took to load."
  (let ((t0 (current-time)))
    (condition-case err
        (progn
          (require feature)
          (message "Loaded %-20s in %.2fs"
                   feature
                   (float-time (time-subtract (current-time) t0))))
      (error
       (message "Failed %-20s after %.2fs (%s)"
                feature
                (float-time (time-subtract (current-time) t0))
                err)))))

(defmacro require-prof (feature)
  `(my/profile-require ,feature))

(defvar image-load-path nil)
(add-hook 'after-init-hook (lambda ()
                             (setq debug-on-quit nil)
                             (setq debug-on-error nil)))

(if (version< emacs-version "27.1")
    (defconst debian-emacs-flavor 'emacs26
      "A symbol representing the particular debian flavor of emacs running.")
  (defconst debian-emacs-flavor 'emacs27
    "A symbol representing the particular debian flavor of emacs running."))

;; Setup the starting directories
(if (file-exists-p "~/workspace/emacs-config/.emacs.d")
    (setq user-emacs-directory "~/workspace/emacs-config/.emacs.d"))

(setq byte-compile-warnings '(cl-functions))

;; Only utilize local files
(with-no-warnings
  (let ((byte-compile-warnings nil))
    (setq byte-compile-warnings '(cl-functions))

    (defvar my/file-name-handler-alist file-name-handler-alist)
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq file-name-handler-alist my/file-name-handler-alist)))

    ;; Disable needless warnings
    (setq warning-minimum-level :emergency)

    ;; Basics
    (add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))
    (add-to-list 'load-path (expand-file-name "setup/" user-emacs-directory))
    (add-to-list 'load-path (expand-file-name "use-package-2.3/" user-emacs-directory))

    ;; All setup files wrapped with profiling
    (require-prof 'setup-package)
    (require-prof 'setup-functions)
    (require-prof 'setup-customs)
    (require-prof 'setup-environment)
    (require-prof 'setup-general)
    (require-prof 'setup-dabbrev)
    (require-prof 'setup-file)
    (require-prof 'setup-hydra)
    (require-prof 'setup-ediff)
    (require-prof 'setup-search)
    (require-prof 'setup-keychain)
    (require-prof 'setup-tramp)
    (require-prof 'setup-appearance)
    (require-prof 'setup-fonts)
    (require-prof 'setup-font-lock)
    (require-prof 'setup-region)
    (require-prof 'setup-cursor)
    (require-prof 'setup-scroll)
    (require-prof 'setup-themes)
    (require-prof 'setup-parenthesis)
    (require-prof 'setup-indent)
    (require-prof 'setup-ido)
    (require-prof 'setup-highlight)
    (require-prof 'setup-cedet)
    (require-prof 'setup-lsp)
    (require-prof 'setup-c++)
    (require-prof 'setup-doxygen)
    (require-prof 'setup-cmake)
    (require-prof 'setup-spell)
    (require-prof 'setup-gnuplot)
    (require-prof 'setup-calendar)
    (require-prof 'setup-org)
    (require-prof 'setup-latex)
    (require-prof 'setup-modeline)
    (require-prof 'setup-tabbar)
    (require-prof 'setup-smex)
    (require-prof 'setup-web)
    (require-prof 'setup-pandoc)
    (require-prof 'setup-tags)
    (require-prof 'setup-flycheck)
    (require-prof 'setup-compile)
    (require-prof 'setup-gdb)
    (require-prof 'setup-makefile)
    (require-prof 'setup-yasnippet)
    (require-prof 'setup-auto-insert)
    (require-prof 'setup-company)
    (require-prof 'setup-bookmarks)
    (require-prof 'setup-versioning)
    (require-prof 'setup-projectile)
    (require-prof 'setup-eldoc)
    (require-prof 'setup-lisp)
    (require-prof 'setup-python)
    (require-prof 'setup-python-plugins)
    (require-prof 'setup-perl)
    (require-prof 'setup-html)
    (require-prof 'setup-javascript)
    (require-prof 'setup-yaml)
    (require-prof 'setup-java)
    (require-prof 'setup-vhdl)
    (require-prof 'setup-verilog)
    (require-prof 'setup-spice)
    (require-prof 'setup-bison)
    (require-prof 'setup-ess)
    (require-prof 'setup-sqlite)
    (require-prof 'setup-markdown)
    (require-prof 'setup-plantuml)
    (require-prof 'setup-docker)
    (require-prof 'setup-xunit)
    (require-prof 'setup-folding)
    (require-prof 'setup-imenu)
    (require-prof 'setup-windows)
    (require-prof 'setup-eshell)
    (require-prof 'setup-elscreen)
    (require-prof 'setup-iimage)
    (require-prof 'setup-ivy)
    (require-prof 'setup-helm)
    (require-prof 'setup-helm-plugins)
    (require-prof 'setup-dired)
    (require-prof 'setup-dired-plugins)
    (require-prof 'setup-gnus)
    ;; (require-prof 'setup-email)
    (require-prof 'setup-org-blog)
    (require-prof 'setup-post)
    (require-prof 'setup-ecb)
    (require-prof 'setup-mouse)
    (require-prof 'setup-undoandredo)
    (require-prof 'setup-writeroom)
    (require-prof 'setup-regex)
    (require-prof 'setup-menus)
    (require-prof 'setup-keys)
    (require-prof 'setup-keys-extensions)
    (require-prof 'setup-tabkey)
    (require-prof 'setup-jump)
    (require-prof 'setup-ergoemacs)
    (require-prof 'setup-server)
    (require-prof 'setup-nettools)

    ;; User local overrides
    (when (file-exists-p user-settings-dir)
      (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))

    (require-prof 'setup-desktop)
    (require-prof 'setup-recentf)
    ) ;; let
  ) ;; with-no-warnings

(setq debug-on-quit nil)
(setq debug-on-error nil)
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

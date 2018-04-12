;;; setup-server.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Abelardo Jara-Berrocal

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

;; Server configuration
(use-package server
  :demand t
  :bind (:map ctl-x-map
              ("C-c" . my/exit))
  :init (progn

          ;; Monkey patch the server to force it to use ipv4. This is a bug fix that will
          ;; hopefully be in emacs 24: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=6781"
          (defadvice make-network-process (before force-tcp-server-ipv4 activate)
            (if (eq nil (plist-get (ad-get-args 0) :family))
                (ad-set-args 0 (plist-put (ad-get-args 0) :family 'ipv4))))

          ;; Server configuration
          (setq server-use-tcp nil)
          (setq server-port 9999)
          (setq server-auth-dir (concat (file-name-as-directory
                                         my/emacs-cache-dir)
                                        "server"))
          (setq server-socket-dir (concat (file-name-as-directory
                                           my/emacs-cache-dir)
                                          "server"))
          (if (not (file-exists-p server-auth-dir))
              (make-directory server-auth-dir t)))
  :config (progn

            ;; Automatically kill all spawned processes on exit
            (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
              "Prevent annoying \"Active processes exist\" query when you quit Emacs."
              (ignore-errors
                (flet ((process-list ())) ad-do-it)))

            ;; Remove socket directory on emacs exit
            (add-hook 'kill-emacs-hook #'(lambda () (ignore-errors (delete-directory server-socket-dir t))))

            ;; http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
            (defun server-ensure-safe-dir (dir) "Noop" t)

            ;; Ensure there is always at least one visible frame open at all times
            (defadvice server-save-buffers-kill-terminal (around dont-kill-last-client-frame activate)
              (when (< 1 (length (frame-list)))
                ad-do-it))

            ;; Keeping emacs running even when "exiting"
            (defun my/exit ()
              (interactive)
              (message (format "There are currently %d client(s) and %d frame(s)."
                               (length server-clients)
                               (length (frame-list))))

              ;; Check for a server-buffer before closing the server-buffer
              (if server-clients
                  (server-edit))

              ;; Hide the server
              (if (display-graphic-p)
                  (iconify-frame)
                (make-frame-invisible nil t)))

            (defvar my/really-kill-emacs nil)
            (defadvice kill-emacs (around my/really-exit activate)
              "Only kill emacs if a prefix is set"
              (when my/really-kill-emacs
                ;; Kill all remaining clients
                (if (not (my/modified-buffers-exist))
                    (progn
                      (dolist (client server-clients)
                        (server-delete-client client))))
                ad-do-it)
              (my/exit))

            (defun quit-emacs ()
              (interactive)
              (ignore-errors
                (if (bound-and-true-p ergoemacs-mode)
                    (ergoemacs-mode -1))
                (setq my/really-kill-emacs t)
                (kill-emacs)))

            ;; Detect presence of modified buffers
            (defun my/modified-buffers-exist()
              "This function will check to see if there are any buffers
that have been modified.  It will return true if there are
and nil otherwise. Buffers that have buffer-offer-save set to
nil are ignored."
              (let (modified-found)
                (dolist (buffer (buffer-list))
                  (when (and (buffer-live-p buffer)
                             (buffer-modified-p buffer)
                             (not (buffer-base-buffer buffer))
                             (or
                              (buffer-file-name buffer)
                              (progn
                                (set-buffer buffer)
                                (and buffer-offer-save (> (buffer-size) 0)))))
                    (setq modified-found t)))
                modified-found))

            ;; Launch the server
            (unless (server-running-p)
              (if (not (eq 'windows-nt system-type))
                  (server-start)))))

;; emacsclient support
(use-package remote-emacsclient
  :config (progn
            ;; Make Emacs ignore the "-e (make-frame-visible)"
            ;; that it gets passed when started by emacsclientw.
            (add-to-list 'command-switch-alist '("(make-frame-visible)" .
                                                 (lambda (s))))))

(provide 'setup-server)
;;; setup-server.el ends here

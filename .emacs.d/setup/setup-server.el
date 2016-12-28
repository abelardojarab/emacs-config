;;; setup-server.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

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

;; Server configuration
(use-package server
  :init (progn
          (require 'server)

          ;; Monkey patch the server to force it to use ipv4. This is a bug fix that will
          ;; hopefully be in emacs 24: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=6781"
          (defadvice make-network-process (before force-tcp-server-ipv4 activate)
            (if (eq nil (plist-get (ad-get-args 0) :family))
                (ad-set-args 0 (plist-put (ad-get-args 0) :family 'ipv4))))

          ;; Launch the server
          (unless (server-running-p)
            (setq server-use-tcp t)
            (setq server-port 9999)
            (setq server-auth-dir "~/.emacs.cache/server")
            (setq server-socket-dir "~/.emacs.cache/server")
            (if (not (file-exists-p server-auth-dir))
                (make-directory server-auth-dir t))
            (and (>= emacs-major-version 23)
                 (defun server-ensure-safe-dir (dir) "Noop" t))
            (if (not (eq 'windows-nt system-type))
              (server-start))))
  :config (progn

            ;; Ensure there is always at least one visible frame open at all times
            (defadvice server-save-buffers-kill-terminal (around dont-kill-last-client-frame activate)
              (when (< 1 (length (frame-list)))
                ad-do-it))

            ;; Keeping emacs running even when "exiting"
            ;; http://bnbeckwith.com/blog/not-killing-emacs-on-windows.html
            (defun bnb/exit ()
              (interactive)
              ;; message
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
            (global-set-key (kbd "C-x C-c") 'bnb/exit)

            (defvar bnb/really-kill-emacs nil)
            (defadvice kill-emacs (around bnb/really-exit activate)
              "Only kill emacs if a prefix is set"
              (when bnb/really-kill-emacs
                ;; Kill all remaining clients
                (if (not (modified-buffers-exist))
                    (progn
                      (dolist (client server-clients)
                        (server-delete-client client))))
                ad-do-it)
              (bnb/exit))

            (defun bnb/really-kill-emacs ()
              (interactive)
              (setq bnb/really-kill-emacs t)
              (kill-emacs))

            (defun modified-buffers-exist()
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
                modified-found))))

;; emacsclient support
(use-package remote-emacsclient
  :config (progn
            ;; Make Emacs ignore the "-e (make-frame-visible)"
            ;; that it gets passed when started by emacsclientw.
            (add-to-list 'command-switch-alist '("(make-frame-visible)" .
                                                 (lambda (s))))))

(provide 'setup-server)
;;; setup-server.el ends here

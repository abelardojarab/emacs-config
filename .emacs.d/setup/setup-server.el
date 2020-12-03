;;; setup-server.el ---                      -*- lexical-binding: t; -*-

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

;; Server configuration
(use-package server
  :demand t
  :bind (:map ctl-x-map
              ("C-c" . kill-emacs))
  :custom ((server-use-tcp t)
           (server-port    9999))
  :init (progn
          ;; Server configuration
          (setq server-auth-dir   (concat (file-name-as-directory
                                           my/emacs-cache-dir)
                                          "server")
                server-socket-dir (concat (file-name-as-directory
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

            ;; http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
            (defun server-ensure-safe-dir (dir) "Noop" t)

             ;; Launch the server
            (unless (server-running-p)
              (if (not (eq system-type 'windows-nt))
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

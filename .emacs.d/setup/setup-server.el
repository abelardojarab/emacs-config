;;; setup-server.el ---

;; Copyright (C) 2014  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc25288.pdx.intel.com>
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
(load "server")
(defadvice make-network-process (before force-tcp-server-ipv4 activate)
  "Monkey patch the server to force it to use ipv4. This is a bug fix that will
hopefully be in emacs 24: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=6781"
  (if (eq nil (plist-get (ad-get-args 0) :family))
      (ad-set-args 0 (plist-put (ad-get-args 0) :family 'ipv4))))
(unless (server-running-p)
  (setq server-use-tcp t)
  (setq server-port 9999)
  (setq server-auth-dir "~/.emacs.cache/server")
  (if (not (file-exists-p server-auth-dir))
      (make-directory server-auth-dir))
  (and (>= emacs-major-version 23)
       (defun server-ensure-safe-dir (dir) "Noop" t))
  (server-start))

;; Ensure there is always at least one visible frame open at all times
(defadvice server-save-buffers-kill-terminal (around dont-kill-last-client-frame activate)
  (when (< 2 (length (frame-list)))
    ad-do-it))

;; Hide Emacs when using emacsclient
(defvar bnb/really-kill-emacs nil)
(defadvice kill-emacs (around bnb/really-exit activate)
  "Only kill emacs if the variable is true"
  (if bnb/really-kill-emacs
      ad-do-it)
  (bnb/exit))

;; Emacsclient support
(require 'remote-emacsclient)

;; Make Emacs ignore the "-e (make-frame-visible)"
;; that it gets passed when started by emacsclientw.
(add-to-list 'command-switch-alist '("(make-frame-visible)" .
                                     (lambda (s))))

;; Set tramp variables
(setq tramp-default-method "ssh")
(update-tramp-emacs-server-port-forward tramp-default-method)

;; have tramp save temps locally...
(setq auto-save-file-name-transforms
      '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
        ("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" "~/.emacs.cache/backups/" t)))

(provide 'setup-server)
;;; setup-server.el ends here

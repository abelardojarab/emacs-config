;;; setup-server.el ---

;; Copyright (C) 2014, 2015  abelardo.jara-berrocal

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

;; rsync support
(require 'auto-rsync)
(auto-rsync-mode t)
;; (setq auto-rsync-dir-alist
;;       (("/path/to/src1/" . "/path/to/dest1/")
;;        ("/path/to/src2/" . "username@hostname:/path/to/dest2/")))

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
  (setq server-socket-dir "~/.emacs.cache/server")
  (if (not (file-exists-p server-auth-dir))
      (make-directory server-auth-dir t))
  (and (>= emacs-major-version 23)
       (defun server-ensure-safe-dir (dir) "Noop" t))
  (server-start))

;; Ensure there is always at least one visible frame open at all times
(defadvice server-save-buffers-kill-terminal (around dont-kill-last-client-frame activate)
  (when (< 2 (length (frame-list)))
    ad-do-it))

;; Keeping emacs running even when "exiting"
;; http://bnbeckwith.com/blog/not-killing-emacs-on-windows.html
(defun bnb/exit ()
  (interactive)

  (let (new-frame modified-buffers active-clients-or-frames)

    ;; message
    (message (format "There are currently %d client(s), %d buffer clients and %d frames."
                     (- (length server-clients) 0)
                     (- (length server-buffer-clients) 0)
                     (- (length (frame-list)) 0)))

    ;; Check if there are modified buffers or active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames (or (> (length server-clients) 1)
                                      (> (length (frame-list)) 1)))

    ;;  Check for a server-buffer before closing the server-buffer
    (if server-clients
        ;; Disconnect from server
        (server-edit))

    ;; Hide the server
    (make-frame-invisible nil t)))
(global-set-key (kbd "C-x C-c") 'bnb/exit)

(defvar bnb/really-kill-emacs nil)
(defadvice kill-emacs (around bnb/really-exit activate)
  "Only kill emacs if a prefix is set"
  (when bnb/really-kill-emacs

    ;; Save buffers
    (save-buffers-kill-terminal)

    ;; Kill all remaining clients
    (progn
      (dolist (client server-clients)
        (server-delete-client client)))

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
    modified-found))

;; Emacsclient support
(require 'remote-emacsclient)

;; Make Emacs ignore the "-e (make-frame-visible)"
;; that it gets passed when started by emacsclientw.
(add-to-list 'command-switch-alist '("(make-frame-visible)" .
                                     (lambda (s))))

;; Set tramp variables
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))
(update-tramp-emacs-server-port-forward tramp-default-method)

;; Tramp configurations
(setq my-tramp-ssh-completions
      '((tramp-parse-sconfig "~/.ssh/config")
        (tramp-parse-shosts "~/.ssh/known_hosts")))
(mapc (lambda (method)
        (tramp-set-completion-function method my-tramp-ssh-completions))
      '("rsync" "scp" "scpc" "scpx" "sftp" "ssh" "plink"))

;; Fix the auto save problem.
(setq tramp-auto-save-directory "~/.emacs.cache/backups")
(make-directory tramp-auto-save-directory t)

;; have tramp save temps locally...
(setq auto-save-file-name-transforms
      '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
        ("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" "~/.emacs.cache/backups/" t)))

;; End of line
(require 'eol-conversion)
(setq inhibit-eol-conversion 't) ;; do this so tramp doesnt complain about ls

(provide 'setup-server)
;;; setup-server.el ends here

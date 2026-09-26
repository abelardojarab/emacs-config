;;; setup-jetski.el --- Jetski Cascade inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
;; Keywords: tools

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

;; jetski.el, Google's Emacs client for the Jetski Cascade agent.  It talks to
;; a Go bridge on this machine, which drives the Jetski Language Server, which
;; reaches Skydeck over Stubby with your LOAS credentials.
;;
;;   C-c J     `jetski-menu' if Jetski is available here, otherwise a one-line
;;             explanation of what is missing.
;;
;; This file is written to load identically on a corp workstation and on a
;; laptop with no /google at all, so the same config can be checked out
;; anywhere.  Nothing here touches the filesystem at startup.
;;
;; That last point is the whole design, and it is not only about portability.
;; The sources live on a FUSE mount, and a *stale* mount is worse than an
;; absent one: `file-directory-p' against a hung fileserver blocks instead of
;; returning nil, so probing it from your init file can wedge Emacs before it
;; finishes starting.  So availability is resolved the first time you press
;; C-c j, never at load time, and the answer is cached for the session.
;;
;; The bridge is the prebuilt release binary, not a blaze build.  The README
;; tells you to `blaze run ... -- setup', which needs an up-to-date CitC
;; workspace and several minutes; /google/bin/releases/emacs-jetski-bridge is
;; the same binary, already built.

;;; Code:

(defgroup my/jetski nil
  "Local wiring for jetski.el."
  :group 'tools
  :prefix "my/jetski-")

(defcustom my/jetski-elisp-dirs
  '("/google/src/files/head/depot/google3/devtools/editors/emacs/jetski/elisp"
    "~/src/jetski/elisp")
  "Where to look for the jetski.el sources, best first.

The first entry is the read-only HEAD snapshot on a corp machine, which
tracks whatever the Jetski team ships.  Add a local path here if you
sync a copy to a machine that has no /google."
  :type '(repeat directory)
  :group 'my/jetski)

(defcustom my/jetski-bridge-candidates
  '("~/bin/jetski-emacs-bridge"
    "/google/bin/releases/emacs-jetski-bridge/jetski-emacs-bridge")
  "Bridge binaries to try, best first.
A hand-built one in ~/bin wins over the prebuilt release."
  :type '(repeat string)
  :group 'my/jetski)

(defvar my/jetski-state nil
  "Cached availability: nil (unknown), `ready', or a string saying why not.")

(defun my/jetski--probe ()
  "Work out whether Jetski can run here.  Returns `ready' or a reason string.

Deliberately not called at load time; see the Commentary."
  (let ((elisp (seq-find (lambda (d)
                           (file-directory-p (expand-file-name d)))
                         my/jetski-elisp-dirs))
        (bridge (seq-find (lambda (b)
                            (file-executable-p (expand-file-name b)))
                          my/jetski-bridge-candidates)))
    (cond
     ((not elisp)
      (format "jetski.el sources not found (looked in %s)"
              (string-join my/jetski-elisp-dirs ", ")))
     ((not bridge)
      (format "jetski bridge binary not found (looked for %s)"
              (string-join my/jetski-bridge-candidates ", ")))
     (t
      (add-to-list 'load-path (expand-file-name elisp))
      (if (not (require 'jetski nil t))
          "jetski.el found but failed to load"
        (setq jetski-bridge-binary (expand-file-name bridge)
              jetski-bridge-remote-host nil)
        (my/jetski--configure)
        'ready)))))

(defun my/jetski--configure ()
  "Apply local Jetski policy once the package is loaded."
  ;; Ask before running commands.  The agent has this machine and a CitC
  ;; workspace; auto-approving everything is a bad trade for the seconds it
  ;; saves.  Read-only inspection is allowlisted.
  (setq jetski-permissions-allow '("command(ls)"
                                   "command(cat)"
                                   "command(pwd)"
                                   "command(grep)"
                                   "command(rg)"
                                   "command(jj log)"
                                   "command(jj diff)"
                                   "command(jj status)"
                                   "command(blaze build)"
                                   "command(blaze test)")
        ;; Never without being asked, whatever else is allowed.
        jetski-permissions-deny '("command(rm)"
                                  "command(jj piper submit)"
                                  "command(jj piper mail)"
                                  "command(g4 submit)"
                                  "command(hg submit)"
                                  "command(fsubmit)"
                                  "write_file(~/.ssh)")
        jetski-auto-revert-buffers t
        jetski-prompt-in-buffer t))

(defun my/jetski-available-p (&optional recheck)
  "Non-nil when Jetski can run here.  With RECHECK, probe again."
  (when (or recheck (null my/jetski-state))
    (setq my/jetski-state (my/jetski--probe)))
  (eq my/jetski-state 'ready))

;;;###autoload
(defun my/jetski-dispatch (&optional recheck)
  "Open `jetski-menu', or explain why Jetski is unavailable here.
With a prefix argument RECHECK, re-probe rather than trust the cache."
  (interactive "P")
  (if (my/jetski-available-p recheck)
      (call-interactively #'jetski-menu)
    (message "Jetski unavailable: %s" my/jetski-state)))

;;;###autoload
(defun my/jetski-status ()
  "Report whether Jetski is available, probing if necessary."
  (interactive)
  (my/jetski-available-p t)
  (message "Jetski: %s"
           (if (eq my/jetski-state 'ready)
               (format "ready (bridge %s)" jetski-bridge-binary)
             my/jetski-state)))

;; C-c J, not C-c j: `setup-jump' binds C-c j to `hydra-dumb-jump/body' and
;; loads later, so C-c j never reached Jetski.
(global-set-key (kbd "C-c J") #'my/jetski-dispatch)

(provide 'setup-jetski)
;;; setup-jetski.el ends here

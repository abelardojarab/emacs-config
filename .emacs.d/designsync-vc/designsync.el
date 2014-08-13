;;; designsync.el --- loader for DesignSync version-control
;;;                   Loads different code for GNU Emacs-21, 20, XEmacs-21

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author:      kenstir
;; Maintainer:  kenstir

;; $Id: designsync.el.rca 1.3 Thu Dec 11 14:58:25 2003 ken Experimental $

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(cond
 ;; old emacs unsupported
 ((or (not (boundp 'emacs-major-version))
      (< emacs-major-version 20))
  (error "DesignSync VC integration does not work with this version of Emacs"))

 ;; emacs-20 or xemacs-21 -- old VC.
 ;; For now, use old VC for emacs-21 as well.
 ((or (featurep 'xemacs)
      (= emacs-major-version 20)
      (= emacs-major-version 21))

  ;; Set vc-master-templates even tho it is defcustom'd, because in my
  ;; Windows emacs-21.2.1 which I got precompiled, vc-master-templates is
  ;; bound and it is nil!?@
  (setq vc-master-templates
        '(vc-find-sync-master
          vc-find-cvs-master
          ("%sRCS/%s,v" . RCS) ("%s%s,v" . RCS) ("%sRCS/%s" . RCS)))

  (load "vc-hooks")
  (load "vc")

  ;; Make sure our VC got loaded, not the stock VC.
  (if (not (functionp 'vc-find-sync-master))
      (error "Wrong VC loaded!  Put DesignSync VC files first in your load-path"))

  ;; Synchronicity internal adoption: if we have no SYNC_DIR, put
  ;; SYNC_IADIR/bin in PATH.
  (let ((sync_dir (getenv "SYNC_DIR"))
        (sync_iadir (getenv "SYNC_IADIR")))
    (if (and sync_iadir
             (or (not sync_dir)
                 (equal sync_dir "")))
        (progn
          (setenv "SYNC_DIR" sync_iadir)
          (setenv "PATH" (concat sync_iadir "/bin" ":"
                                 (getenv "PATH")))
          ;; Reset exec-path from PATH; exec-path is what lisp programs
          ;; use to spawn subprocesses.  Note that `parse-colon-path'
          ;; causes each element to have a trailing /.  I don't like it,
          ;; but I don't think it'll cause a problem.
          (setq exec-path (parse-colon-path (getenv "PATH")))))))

 ;; emacs 21 -- new VC.
 ((= emacs-major-version 21)
;; disabled until working
;;  (require 'vc)
;;  (setq vc-handled-backends (append '(DesignSync) vc-handled-backends))
;;  (load "vc-designsync")
  )

 ;; What have we here?
 (t
  (error "DesignSync VC integration does not work with this version of Emacs")))

;;; designsync.el ends here

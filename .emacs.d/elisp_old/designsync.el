;;; designsync.el --- 

;; Copyright 2006 Vincent Rayappa
;;
;; Author: vincent.rayappa@intel.com
;; Version: $Id: designsync.el,v 0.0 2006/07/17 22:16:04 vrayapp Exp $
;; Keywords: 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'designsync)

;;; Code:

(provide 'designsync)
(eval-when-compile
  (require 'cl))

(defun ds-exec-cmd (&rest args)
  "Execute dssc commands (sending output to *designsync* buffer) and
   output message representing success/failure."
  (get-buffer-create "*designsync*")
  (let (ret)
	(setq ret (apply 'call-process (append (list "dssc" nil "*designsync*" t) args)))
	(if (equal ret 0)
		(message "DesignSync %s command completed." (car args))
		(message "DesignSync %s command FAILED!" (car args)))))

(defun ds-co ()
  "Checks-out current file from DesignSync."
  (interactive)
  (save-excursion
	(ds-exec-cmd "co" "-lock" "-noc" (buffer-file-name))
	(revert-buffer nil t)))

(defun ds-pop ()
  "Checks-out (for read) current file from DesignSync."
  (interactive)
  (save-excursion
	(ds-exec-cmd "co" "-get" (buffer-file-name))
	(revert-buffer nil t)))

(defun ds-ci (&optional cmt)
  "Checks-in current file from DesignSync."
  (interactive "sEnter a checkin comment: ")
  (save-excursion
	(if (> (length cmt) 0)
		(ds-exec-cmd "ci" "-new" "-com" cmt (buffer-file-name))
		(ds-exec-cmd "ci" "-new" "-noc" (buffer-file-name)))
	(revert-buffer nil t)))

(defun ds-cancel ()
  "Cancels check-out of current file from DesignSync."
  (interactive)
  (save-excursion
	(ds-exec-cmd "cancel" (buffer-file-name))
	(revert-buffer)))

(defun ds-diff ()
  "Diffs current un-ci file against original using tkdiff."
  (interactive)
  (save-excursion
	(ds-exec-cmd "diff" "-gui" (buffer-file-name))))

(defun ds-qa-ready-tag ()
  "Tags file in buffer as QA_READY."
  (interactive)
  (save-excursion
	(ds-exec-cmd "tag" "-replace" "QA_READY" (buffer-file-name))))

(defun ds-prd-ready-tag ()
  "Tags file in buffer as QA_READY."
  (interactive)
  (save-excursion
	(ds-exec-cmd "tag" "-replace" "PRD_READY" (buffer-file-name))))


;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################





;;; designsync.el ends here

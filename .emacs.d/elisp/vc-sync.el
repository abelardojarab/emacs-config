;;; vc-sync.el --- support for DesignSync version-control

(eval-when-compile
  (require 'cl)
  (require 'vc))

;;;
;;; State-querying functions
;;;

(defun vc-sync-registered (file)
  "Check if FILE is SYNC registered."
  (let ((dirname (or (file-name-directory file) ""))
	(basename (file-name-nondirectory file))
        ;; make sure that the file name is searched case-sensitively
        (case-fold-search nil))
    (if (file-readable-p (expand-file-name ".SYNC/Contents" dirname))
	(with-temp-buffer
          (vc-insert-file (expand-file-name ".SYNC/Contents" dirname))
          (goto-char (point-min))
	  (cond
	   ((re-search-forward
	     (concat "^<" (regexp-quote basename) ">") nil t)
	    (beginning-of-line)
	    (when (looking-at "<[^>]+><[^>]+><[^>]+><[^>]+><\\([^>]+\\)>")
	      (vc-file-setprop file 'vc-workfile-version (match-string 1)))
	    t)
	   (t nil)))
      nil)))

(defun vc-sync-state (file)
  "Implementation of `vc-state' for SYNC."
  (with-temp-buffer
    ;; First check if the file is locked (by current user or another user)
    (vc-do-command t 0 "dssc" file "ls" "-report" "UMDVLN" "-locked")
    (goto-char (point-min))
    (cond ((search-forward "Lock " nil t) 'edited)
	  ;; If it shows Cache, it must be locked by another user
	  ((search-forward "Cache" nil t)
	   (beginning-of-line)
	   (if (looking-at "^\\([^\t ]+\\)")
	       (match-string 1)
	     "UNKNOWN"))
	  ;; Now check if the file is up to date or needs to be updated
	  (t (with-temp-buffer
	       (vc-do-command t 0 "dssc" file "ls" "-changed")
	       (goto-char (point-min))
	       (if (search-forward "Cache" nil t)
		   'needs-patch
		 'up-to-date))))))
	       
;; Make a guess on the state.  If the file is a symbolic link, it is
;; unlocked, otherwise it is locked.
(defun vc-sync-state-heuristic (file)
  (if (stringp (car (file-attributes file)))
      'up-to-date
    'edited))

(defun vc-sync-workfile-version (file)
  (vc-sync-registered file)
  (vc-file-getprop file 'vc-workfile-version))

(defun vc-sync-checkout-model (file)
  'locking)

;;;
;;; State-changing functions
;;;

(defun vc-sync-register (file &optional rev comment)
    (let ((switches (list
		     (if (stringp vc-register-switches)
			 (list vc-register-switches)
		       vc-register-switches))))
      ;; A comment is required for initial checkin.  Since this prompt
      ;; can be disabled by the user in Emacs, we have to give a
      ;; generic comment if the user doesn't provide one.
      (if (or (null comment)
	      (zerop (length comment)))
	  (setq comment "initial version"))
      ;; Use the -new option to check in a newly registered file.
      (apply 'vc-do-command nil 0 "dssc" file
	     (append (list "ci" "-new" "-comment" comment)
	     switches))))

;; Look for the .SYNC directory to see if the files are managed by DesignSync
(defun vc-sync-responsible-p (file)
  (file-directory-p (expand-file-name ".SYNC" (file-name-directory file))))

(defun vc-sync-checkin (file rev comment)
  (let ((switches (if (stringp vc-checkin-switches)
		      (list vc-checkin-switches)
		    vc-checkin-switches))
	status)

    (setq status (apply 'vc-do-command nil 1 "dssc" file
			"ci" 
			(append (list "-comment" comment)
			switches)))
    (set-buffer "*vc*")))

(defun vc-sync-checkout (file &optional editable rev workfile)
  "Retrieve a copy of a saved version of FILE into a workfile."
  (let ((filename (or workfile file))
	(file-buffer (get-file-buffer file))
	switches)
    (message "Checking out %s..." filename)
    (save-excursion
      ;; Change buffers to get local value of vc-checkout-switches.
      (if file-buffer (set-buffer file-buffer))
      (setq switches (if (stringp vc-checkout-switches)
			 (list vc-checkout-switches)
		       vc-checkout-switches))
      ;; Save this buffer's default-directory
      ;; and use save-excursion to make sure it is restored
      ;; in the same buffer it was saved in.
      (let ((default-directory default-directory))
	(save-excursion
	  ;; Adjust the default-directory so that the check-out creates
	  ;; the file in the right place.
	  (setq default-directory (file-name-directory filename))
	  ;; If a specific work file is requested, we have to go thru
	  ;; a complex sequence to avoid conflicts with files that may
	  ;; already be checked out
	  (if workfile
	      (progn
		(with-temp-buffer
		  ;; First get the vault URL of the current directory
		  (vc-do-command t 0 "dssc" default-directory "url" "vault")
		  (goto-char (point-min))
		  (search-forward "sync://" nil t)
		  (beginning-of-line)
		  ;; Now create a temporary directory with the same vault URL.
		  (when (looking-at "^\\(.*\\)")
		    (let ((url (match-string 1))
			  (tmp-directory (make-temp-file (concat default-directory "/.vc_sync_tmp") t))
			  tmp-file
			  link-target)
		      (setq tmp-file (concat tmp-directory "/" (file-name-nondirectory file)))
		      (vc-do-command t 0 "dssc" tmp-directory "setvault" url)
		      ;; Finally we can check out the version we want into the temporary directory
		      (vc-do-command t 0 "dssc" tmp-file "co" (if rev "-version") rev)
		      (setq link-target (car (file-attributes tmp-file)))
		      ;; To avoid the cached version from being lost,
		      ;; copy it to the workfile path.
		      (when (stringp link-target)
			(delete-file workfile)
			(copy-file link-target workfile))
		      ;; Now that the version is safely copied, we can
		      ;; delete the temporary directory.
		      (vc-do-command t 0 "dssc" tmp-directory "rmfolder" "-rec")
		      ))))
	    ;; If no workfile is requested, we can just check it out normally.
	    (let (new-version)
	      (apply 'vc-do-command
		     nil 0 "dssc" filename
		     "co"
		     (append (list
			      (if editable "-lock")
			      (if editable "-noc")
			      ;; a literal revision was specified
			      (if (and (stringp rev) (> (length rev) 0)) "-version")
			      (if (stringp rev) rev))
                      switches))
	      ;; determine the new workfile version.  The version is
	      ;; actually named oldver -> newver, but for our purposes
	      ;; the newver will be ignored.  This is more consistent
	      ;; with RCS and CVS.
	      (with-current-buffer "*vc*"
		(setq new-version
		      (vc-parse-buffer "^version \\([0-9.]+\\).*\n" 1)))
	      (vc-file-setprop file 'vc-workfile-version new-version)
	      (message "Checking out %s...done" filename)
	      )))))))

(defun vc-sync-revert (file &optional contents-done)
  "Revert FILE to the version it was based on."
  (vc-do-command nil 0 "dssc" file "cancel" "-force"))

;;;
;;; History functions
;;;

(defun vc-sync-print-log (file)
  "Get change log associated with FILE."
  (vc-do-command nil 0 "dssc" file "vhistory"))

(defun vc-sync-diff (file &optional oldvers newvers)
  (if (not oldvers) (setq oldvers (vc-workfile-version file)))
  (vc-do-command "*vc-diff*" 0 "dssc" 
		 ;; diff only takes one -version argument , but
		 ;; another the syntax file;version can be used to
		 ;; specify a second version if needed.
		 (if newvers (concat file ";" newvers) file) "diff"
		 "-version" oldvers)
  ;; Since the diff command doesn't return a valid exit code, we have
  ;; to manually search for the no difference string.
  (save-excursion
    (set-buffer "*vc-diff*")
    (goto-char (point-min))
    (if (search-forward "No differences\n0 Differences detected" nil t)
	0
      1)))

;;;
;;; Snapshot system
;;;

(defun vc-sync-assign-name (file name)
  (vc-do-command nil 0 "dssc" file "tag" "-repl" name))


;;;
;;; Miscellaneous
;;;


(provide 'vc-sync)

;;; vc-sync.el ends here

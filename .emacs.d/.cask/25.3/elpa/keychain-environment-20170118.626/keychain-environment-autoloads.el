;;; keychain-environment-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "keychain-environment" "keychain-environment.el"
;;;;;;  (23046 44925 26524 788000))
;;; Generated autoloads from keychain-environment.el

(autoload 'keychain-refresh-environment "keychain-environment" "\
Set ssh-agent and gpg-agent environment variables.

Set the environment variables `SSH_AUTH_SOCK', `SSH_AGENT_PID'
and `GPG_AGENT' in Emacs' `process-environment' according to
information retrieved from files created by the keychain script.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; keychain-environment-autoloads.el ends here

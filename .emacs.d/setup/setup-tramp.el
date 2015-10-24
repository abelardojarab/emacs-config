;;; setup-tramp.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Abelardo Jara

;; Author: Abelardo Jara <abelardojara@Abelardos-MacBook-Pro.local>
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

;; Set tramp variables
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))

;; Tramp configurations
(setq my-tramp-ssh-completions
      '((tramp-parse-sconfig "~/.ssh/config")
        (tramp-parse-shosts "~/.ssh/known_hosts")))
(mapc (lambda (method)
        (tramp-set-completion-function method my-tramp-ssh-completions))
      '("rsync" "scp" "scpc" "scpx" "sftp" "ssh" "plink"))

;; Fix the auto save problem.
(setq-default tramp-persistency-file-name "~/.emacs.cache/tramp")
(setq-default tramp-auto-save-directory "~/.emacs.cache/backups")
(make-directory tramp-auto-save-directory t)

;; have tramp save temps locally...
(unless (string-equal system-type "windows-nt")
  (setq auto-save-file-name-transforms
        '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
          ("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" "~/.emacs.cache/backups/" t))))

;; End of line
(require 'eol-conversion)
(setq inhibit-eol-conversion 't) ;; do this so tramp doesnt complain about ls

(provide 'setup-tramp)
;;; setup-tramp.el ends here

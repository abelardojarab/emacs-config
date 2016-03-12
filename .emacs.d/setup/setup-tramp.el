;;; setup-tramp.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2016  Abelardo Jara

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


;; Set control master options before loading tramp
(use-package tramp
  :init (progn
          ;; Set tramp variables
          (if (eq system-type 'windows-nt)
              (setq tramp-default-method "plink")
            (setq tramp-default-method "ssh"))

          (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))
  :config (progn
            ;; Fix auto save problem
            (setq tramp-persistency-file-name "~/.emacs.cache/tramp")
            (setq tramp-auto-save-directory "~/.emacs.cache/auto-save")

            ;; Tramp configurations
            (setq my-tramp-ssh-completions
                  '((tramp-parse-sconfig "~/.ssh/config")
                    (tramp-parse-shosts "~/.ssh/known_hosts")))
            (mapc (lambda (method)
                    (tramp-set-completion-function method my-tramp-ssh-completions))
                  '("rsync" "scp" "scpc" "scpx" "sftp" "ssh" "plink"))

            ;; have tramp save temps locally
            (unless (string-equal system-type "windows-nt")
              (setq auto-save-file-name-transforms
                    '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
                      ("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" "~/.emacs.cache/auto-save/" t))))))

;; End of line, needed by tramp
(use-package eol-conversion
  :pin manual
  :config (setq inhibit-eol-conversion 't))

(provide 'setup-tramp)
;;; setup-tramp.el ends here

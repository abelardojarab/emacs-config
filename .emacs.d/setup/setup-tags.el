;;; setup-tags.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019  Abelardo Jara-Berrocal

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
(use-package etags
  :demand t
  :commands (etags-create-or-update
             etags-tags-completion-table)
  :custom ((tags-revert-without-query          t)
           (tags-always-build-completion-table t)
           (tags-add-tables                    t))
  :init (progn

          ;; Assure .gtags directory exists
          (if (not (file-exists-p "~/.gtags"))
              (make-directory "~/.gtags") t)

          (if (file-exists-p "~/.gtags/TAGS")
              (ignore-errors
                (visit-tags-table "~/.gtags/TAGS" t))
            (with-temp-buffer (write-file "~/.gtags/TAGS")))

          (setq tags-file-name "~/.gtags/TAGS")
          (setq tags-table-list (list tags-file-name)))
  :config (progn
            (unless (fboundp 'push-tag-mark)
              (defun push-tag-mark ()
                "Push the current position to the ring of markers so that
                \\[pop-tag-mark] can be used to come back to current position."
                (interactive)
                (ring-insert find-tag-marker-ring (point-marker))))

            ;; etags creation
            (defun ctags-create-or-update (dir-name)
              "Create tags file."
              (interactive
               (let ((olddir default-directory)
                     (default-directory
                       (read-directory-name
                        "ctags: top of source tree:" (projectile-project-root))))
                 (shell-command
                  (format "find %s -follow -type f -name \"*.[ch][ px][ px]\" | etags - -o %s/TAGS"
                          default-directory
                          default-directory))
                 (message "Created tagfile"))))

            ;; Fix etags bugs (https://groups.google.com/forum/#!msg/gnu.emacs.help/Ew0sTxk0C-g/YsTPVEKTBAAJ)
            (defvar etags--table-line-limit 10)
            (defun etags-tags-completion-table ()
              (let (table
                    (progress-reporter
                     (make-progress-reporter
                      (format "Making tags completion table for %s..." buffer-file-name)
                      (point-min) (point-max))))
                (save-excursion
                  (goto-char (point-min))
                  ;; This regexp matches an explicit tag name or the place where
                  ;; it would start.
                  (while (not (eobp))
                    (if (not (re-search-forward
                              "[\f\t\n\r()=,; ]?\177\\\(?:\\([^\n\001]+\\)\001\\)?"
                              ;; Avoid lines that are too long (bug#20703).
                              (+ (point) etags--table-line-limit) t))
                        (forward-line 1)
                      (push (prog1 (if (match-beginning 1)
                                       ;; There is an explicit tag name.
                                       (buffer-substring (match-beginning 1) (match-end 1))
                                     ;; No explicit tag name.  Backtrack a little,
                                     ;; and look for the implicit one.
                                     (goto-char (match-beginning 0))
                                     (skip-chars-backward "^\f\t\n\r()=,; ")
                                     (prog1
                                         (buffer-substring (point) (match-beginning 0))
                                       (goto-char (match-end 0))))
                              (progress-reporter-update progress-reporter (point)))
                            table))))
                table))))

;; Emacs 25-above xref
(use-package xref
  :if (boundp 'xref-backend-functions)
  :bind (("M-*" . xref-pop-marker-stack)
         ("C-]" . xref-find-definitions))
  :config (setq xref-prompt-for-identifier '(not xref-find-definitions
                                                 xref-find-definitions-other-window
                                                 xref-find-definitions-other-frame
                                                 xref-find-references)))

;; Emacs 25 backend for gtags/xref
(use-package gxref
  :after xref
  :if (and (executable-find "global")
           (boundp 'xref-backend-functions))
  :config (add-to-list 'xref-backend-functions 'gxref-xref-backend))

;; Implementing my own copy of this function since it is required by
;; semantic-ia-fast-jump but this function is not defined in etags.el
;; of GNU emacs
(use-package etags-select
  :defer t
  :after etags
  :commands etags-select-find-tag)

;; Etags table
(use-package etags-table
  :defer t
  :after (projectile etags)
  :custom (etags-table-search-up-depth 2)
  :commands etags-create-or-update-table-tags-table
  :config (progn
            (setq etags-table-alist
                  (list
                   ;; For jumping to standard headers:
                   '(".*\\.\\([ch]\\|cpp\\)" "~/.gtags/TAGS")))

            ;; Below function comes useful when you change the project-root
            ;; symbol to a different value (when switching projects)
            (defun etags-create-or-update-tags-table ()
              "Update `etags-table-alist' based on the current project directory."
              (interactive)
              (add-to-list 'etags-table-alist
                           `(,(concat (projectile-project-root) "TAGS"))
                           t))))

;; Ctags
(use-package ctags
  :defer t
  :if (executable-find "ctags-exuberant")
  :commands (ctags-create-or-update
             ctags-create-or-update-tags-table
             ctags-search)
  :after projectile
  :preface (defun ctags-create-or-update (dir-name)
             "Create tags file."
             (interactive
              (let ((olddir default-directory)
                    (default-directory
                      (read-directory-name
                       "ctags: top of source tree:" (projectile-project-root))))
                (shell-command
                 (format "ctags-exuberant -e -f %s -R %s > /dev/null"
                         (concat default-directory "TAGS")
                         default-directory))
                (message "Created tagfile")))))

;; Gtags
(use-package ggtags
  :defer t
  :if (executable-find "global")
  :after (eldoc projectile)
  :commands (ggtags-mode
             ggtags-find-tag-dwim
             ggtags-eldoc-function
             ggtags-show-definition
             ggtags-create-or-update)
  ;; Taken from https://tuhdo.github.io/c-ide.html
  :bind (:map ggtags-mode-map
              ("M-,"     . pop-tag-mark)
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags))
  :diminish ggtags-mode
  :init (add-hook 'c-mode-common-hook
                  (lambda ()
                    (when (and (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                               (projectile-project-p)
                               (file-exists-p (concat (projectile-project-root)
                                                      "GTAGS")))
                      (ggtags-mode 1))))
  :preface (progn
             ;; Assure .gtags directory exists
             (if (not (file-exists-p "~/.gtags"))
                 (make-directory "~/.gtags") t)

             ;; Descend into GTAGSLIBPATH if definition is not found
             (setenv "GTAGSTHROUGH" "true")
             (setenv "GTAGSLIBPATH" "~/.gtags")

             ;; Use exhuberant ctags format
             (setenv "GTAGSLABEL" "exuberant-ctags")

             ;; Eldoc integration
             (add-hook 'c-mode-common-hook
                       (lambda ()
                         (when (and (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                                    (projectile-project-p)
                                    (file-exists-p (concat (projectile-project-root)
                                                           "GTAGS")))
                           (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
                           (setq-local imenu-create-index-function #'ggtags-build-imenu-index))))

             (defun ggtags-global-output (buffer cmds callback &optional cutoff)
               "Asynchronously pipe the output of running CMDS to BUFFER.
When finished invoke CALLBACK in BUFFER with process exit status."
               (or buffer (error "Output buffer required"))
               (when (get-buffer-process (get-buffer buffer))
                 ;; Notice running multiple processes in the same buffer so that we
                 ;; can fix the caller. See for example `ggtags-eldoc-function'.
                 (message "Warning: detected %S already running in %S; interrupting..."
                          (get-buffer-process buffer) buffer)
                 (interrupt-process (get-buffer-process buffer)))
               (let* ((program (car cmds))
                      (args (cdr cmds))
                      (cutoff (and cutoff (+ cutoff (if (get-buffer buffer)
                                                        (with-current-buffer buffer
                                                          (line-number-at-pos (point-max)))
                                                      0))))
                      (proc (apply #'start-file-process program buffer program args))
                      (filter (lambda (proc string)
                                (and (buffer-live-p (process-buffer proc))
                                     (with-current-buffer (process-buffer proc)
                                       (goto-char (process-mark proc))
                                       (insert string)
                                       (when (and (> (line-number-at-pos (point-max)) cutoff)
                                                  (process-live-p proc))
                                         (interrupt-process (current-buffer)))))))
                      (sentinel (lambda (proc _msg)
                                  (when (memq (process-status proc) '(exit signal))
                                    (ignore-errors
                                      (with-current-buffer (process-buffer proc)
                                        (set-process-buffer proc nil)
                                        (funcall callback (process-exit-status proc))))))))
                 (set-process-query-on-exit-flag proc nil)
                 (and cutoff (set-process-filter proc filter))
                 (set-process-sentinel proc sentinel)
                 proc))

             (defun ggtags-find-project ()
               ;; It is unsafe to cache `ggtags-project-root' in non-file buffers
               ;; whose `default-directory' can often change.
               (unless (equal ggtags-last-default-directory default-directory)
                 (kill-local-variable 'ggtags-project-root))
               (let ((project (gethash ggtags-project-root ggtags-projects)))
                 (if (ggtags-project-p project)
                     (if (ggtags-project-expired-p project)
                         (progn
                           (remhash ggtags-project-root ggtags-projects)
                           (ggtags-find-project))
                       project)
                   (setq ggtags-last-default-directory default-directory)
                   (setq ggtags-project-root
                         (or (ignore-errors
                               (file-name-as-directory
                                (concat (file-remote-p (projectile-project-root))
                                        ;; Resolves symbolic links
                                        (ggtags-process-string "global" "-pr"))))
                             ;; 'global -pr' resolves symlinks before checking the
                             ;; GTAGS file which could cause issues such as
                             ;; https://github.com/leoliu/ggtags/issues/22, so
                             ;; let's help it out.
                             ;;
                             ;; Note: `locate-dominating-file' doesn't accept
                             ;; function for NAME before 24.3.
                             (let ((dir (locate-dominating-file (projectile-project-root) "GTAGS")))
                               ;; `file-truename' may strip the trailing '/' on
                               ;; remote hosts, see http://debbugs.gnu.org/16851
                               (and dir (file-regular-p (expand-file-name "GTAGS" dir))
                                    (file-name-as-directory (file-truename dir))))))
                   (when ggtags-project-root
                     (if (gethash ggtags-project-root ggtags-projects)
                         (ggtags-find-project)
                       (ggtags-make-project ggtags-project-root))))))

             (defun ggtags-create-or-update ()
               "Create or update the GNU-Global tag file"
               (interactive
                (if (zerop (call-process "global" nil nil nil "-p"))
                    ;; case 1: tag file exists: update
                    (progn
                      (shell-command "global -u -q 2> /dev/null")
                      (message "Tagfile updated"))
                  ;; case 2: no tag file yet: create it
                  (when (yes-or-no-p "Create tagfile?")
                    (let ((olddir default-directory)
                          (default-directory
                            (read-directory-name
                             "gtags: top of source tree:" (projectile-project-root))))
                      (shell-command "gtags -i -q 2> /dev/null")
                      (message "Created tagfile"))))))))

(provide 'setup-tags)
;;; setup-tags.el ends here

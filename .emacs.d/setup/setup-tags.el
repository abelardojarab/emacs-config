;;; setup-tags.el ---

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

;; Implementing my own copy of this function since it is required by
;; semantic-ia-fast-jump but this function is not defined in etags.el
;; of GNU emacs
(add-to-list 'load-path "~/.emacs.d/etags-select")
(require 'ctags)
(require 'etags)
(require 'etags-select)
(unless (fboundp 'push-tag-mark)
  (defun push-tag-mark ()
    "Push the current position to the ring of markers so that
    \\[pop-tag-mark] can be used to come back to current position."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))))

;; Tags table
(setq tags-revert-without-query t)
(setq tags-always-build-completion-table t)
(setq tags-file-name (expand-file-name "~/.emacs.cache/TAGS"))
(setq tags-table-list (list tags-file-name))
(setq tags-add-tables t)

;; Etags table
(require 'etags-table)
(setq etags-table-alist
      (list
       ;; For jumping to standard headers:
       '(".*\\.\\([ch]\\|cpp\\)" (expand-file-name "~/.emacs.cache/TAGS"))
        ))
(setq etags-table-search-up-depth 1) ;; Max depth to search up for a tags file.  nil means don't search.

;; Below function comes useful when you change the project-root symbol to a
;; different value (when switching projects)
(defun update-etags-table-then-find-tag ()
  "Update etags-table based on the current value of project-root and then do
tag find"
  (interactive)
  (when (boundp 'project-root) ;; add-to-list if project-root symbol is defined
    (add-to-list 'etags-table-alist
                 `(,(concat project-root "/.*") ,(concat project-root "/TAGS")) t))
  (etags-select-find-tag-at-point))

;; Ctags update
(require 'ctags-update)
(when (executable-find "ctags")
  (setq ctags-update-delay-seconds (* 30 60)) ;; every 1/2 hour
  (autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
  (add-hook 'verilog-mode-hook    'turn-on-ctags-auto-update-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-ctags-auto-update-mode))

;; better searching of tags
(add-to-list 'load-path "~/.emacs.d/ggtags")
(require 'gtags)
(require 'ggtags)

;; Use ggtags instead of gtags
(mapc (lambda (mode)
        (add-hook mode 'gtags-mode)
        (add-hook mode 'ggtags-mode))
      '(c-mode-hook
        c++-mode-hook
        lisp-mode-hook
        python-mode-hook
        js2-mode-hook
        java-mode-hook))

;; Use ido to list tags, but then select via etags-select (best of both worlds!)
(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (find-tag (replace-regexp-in-string "\\\\" "" (ido-completing-read "Tag: " tag-names)))))

;; Helper functions for etags/ctags
(defun create-ctags (dir-name)
  "Create tags file."
  (interactive "Directory: ")
  (shell-command
   (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name))))

(defun create-etags (dir-name)
  "Create tags file."
  (interactive "Directory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

(defun gtags-update ()
  (interactive)
  (let (buffer)
    (save-excursion
      (setq buffer (generate-new-buffer (generate-new-buffer-name "*rootdir*")))
      (set-buffer buffer)
      (call-process "global" nil t nil "-u")
      (kill-buffer buffer))))

(defun gtags-create-or-update ()
  "Create or update the GNU-Global tag file"
  (interactive
   (if (zerop (call-process "global" nil nil nil "-p"))
       ;; case 1: tag file exists: update
       (progn
         (shell-command "global -u -q 2>/dev/null")
         (message "Tagfile updated"))
     ;; case 2: no tag file yet: create it
     (when (yes-or-no-p "Create tagfile?")
       (let ((olddir default-directory)
             (default-directory
               (read-directory-name
                "gtags: top of source tree:" default-directory)))
         (shell-command "gtags -i -q 2>/dev/null")
         (message "Created tagfile"))))))

(add-hook 'prog-mode-hook
          (lambda ()
            ;; ggtags
            (ggtags-mode t)
            (local-set-key (kbd "C-c .") 'ggtags-grep)
            (local-set-key (kbd "M-,")   'ggtags-find-reference)
            (when (fboundp 'gtags-create-or-update)
              (gtags-create-or-update)))) ;; auto-update tags

(provide 'setup-tags)
;;; setup-tags.el ends here

;;; setup-tags.el ---

;; Copyright (C) 2014  abelardo.jara-berrocal

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
(require 'etags)
(require 'etags-select)
(unless (fboundp 'push-tag-mark)
  (defun push-tag-mark ()
    "Push the current position to the ring of markers so that
    \\[pop-tag-mark] can be used to come back to current position."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))))

;; better searching of tags
(add-to-list 'load-path "~/.emacs.d/ggtags")
(require 'gtags)
(require 'ggtags)

;; Get the path of gtags root directory.
(defun gtags-update ()
  (interactive)
  (let (buffer)
    (save-excursion
      (setq buffer (generate-new-buffer (generate-new-buffer-name "*rootdir*")))
      (set-buffer buffer)
      (call-process "global" nil t nil "-u")
      (kill-buffer buffer))))

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

;; Use GNU global instead of normal find-tag, fall back to etags-select
(global-set-key (kbd "M-.") (if (and (fboundp 'ggtags-find-tag-dwim)
                                     (executable-find "global"))
                                'ggtags-find-tag-dwim
                              'etags-select-find-tag))

;; Use Helm instead of 'etags-select-find-tag
(global-set-key (kbd "C-,") 'helm-etags-select)

;; Tags table
(setq tags-always-build-completion-table t)
(setq tag-table-alist
      '(("\\.il$" . "~/workspace/frametools/TAGS")
        ("\\.ils$" . "~/workspace/frametools/TAGS")))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

(defun pm/find-tags-file ()
  "Recursively searches each parent directory for a file named `TAGS'
   and returns the path to that file or nil if a tags file is not found.
   Returns nil if the buffer is not visiting a file.
   (from jds-find-tags-file in the emacs-wiki)"
  (labels ((find-tags-file-r
            (path)
            (let* ((parent (if path (file-name-directory path)
                             default-directory))
                   (possible-tags-file (concat parent "TAGS")))
              (cond
               ((file-exists-p possible-tags-file)
                (shell-command (concat "make -C" parent " TAGS"))
                (throw 'found-it possible-tags-file))
               ((string= "/TAGS" possible-tags-file)
                (error "no tags file found"))
               (t
                (find-tags-file-r (directory-file-name parent)))))))
    (catch 'found-it
      (find-tags-file-r (buffer-file-name)))))

(defadvice find-tag (before pm/before-find-tag activate)
  (setq tags-file-name (pm/find-tags-file)))

(provide 'setup-tags)
;;; setup-tags.el ends here

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

;; better searching of tags
(add-to-list 'load-path "~/.emacs.d/ggtags")
(add-to-list 'load-path "~/.emacs.d/etags-select")
(require 'etags-select)
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
(add-hook 'c-mode-common-hook
          (lambda () (ggtags-mode t)))
(add-hook 'python-mode-hook
          (lambda () (ggtags-mode t)))
(add-hook 'lisp-mode-hook
          (lambda () (ggtags-mode t)))
(add-hook 'js2-mode-hook
          (lambda () (ggtags-mode t)))

;; Use ido to list tags, but then select via etags-select (best of both worlds!)
(defun my-ido-find-tag ()
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
(global-set-key (kbd "C-,") 'my-ido-find-tag)

(provide 'setup-tags)
;;; setup-tags.el ends here

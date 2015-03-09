;;; setup-yasnippet.el ---

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

;; Yasnippet (should be invoked before auto-complete)
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; Remove Yasnippet's default tab key binding (avoid collision with auto-complete)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;; Select a snippet with popup library
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt yas-ido-prompt yas-completing-prompt yas-no-prompt))

;; Tweaking Yasnippet for Org mode
(defun yas--org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas-trigger-key)
            (setq yas-trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas--org-very-safe-expand)
            (define-key yas-keymap [tab] 'yas-next-field)))

;; Provide headers or templates for new files using Yasnippet
(defun yas--expand-by-uuid (mode uuid)
  "Expand snippet template in MODE by its UUID"
  (yas--expand-snippet
   (yas--template-content
    (yas--get-template-by-uuid mode uuid))))

;; Yasnippet templates used in auto-insert mode
(require 'autoinsert)
(auto-insert-mode)
(setq auto-insert-query nil)
(define-auto-insert "\.R"
  '(lambda () (yas--expand-by-uuid 'ess-mode "header")))
(define-auto-insert "\.py"
  '(lambda () (yas--expand-by-uuid 'python-mode "header")))

(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(define-auto-insert "\\.c$"  ["c-auto-insert" autoinsert-yas-expand])
(define-auto-insert "\\.cpp$" ["c++-auto-insert" autoinsert-yas-expand])
(define-auto-insert "\\.cs$" ["csharp-auto-insert" autoinsert-yas-expand])
(define-auto-insert "\\.org$" ["org-auto-insert" autoinsert-yas-expand])

(defadvice auto-insert (around yasnippet-expand-after-auto-insert activate)
  "Expand Content Auto-inserted as yasnippet Templete,
  so That WE could use yasnippet in autoinsert mode "
  (let ((is-new-File (and (not buffer-read-only)
                          (or (eq this-command 'auto-insert)
                              (and auto-insert (bobp) (eobp))))))
    ad-do-it
    (let ((old-point-max (point-max)))
      (when is-new-File
        (goto-char old-point-max)
        (yas-expand-snippet (buffer-substring-no-properties (point-min) (point-max)))
        (delete-region (point-min) old-point-max)
        ))))

;; Get email from Magit if available
(defun yas--magit-email-or-default ()
  "Get email from GIT or use default"
  (if (magit-get-top-dir ".")
      (magit-get "user.email")
    user-mail-address))

;; Fast creation of snippets
(defvaralias 'yas/init-snippet-template 'yas-new-snippet-default)
(defun yas-new-snippet-with-content (s e)
  "Create snippet from region to speed-up snippet development."
  (interactive "r")
  (let ((initial-text (buffer-substring s e))
        (default-directory (file-name-as-directory (car (yas-snippet-dirs)))))
    (yas-new-snippet t)
    (save-excursion
      (when initial-text
        (insert initial-text)
        (goto-char (point-min))
        (while (re-search-forward "[\\$]" nil t)
          (replace-match "\\\\\\&"))))
    (yas-expand-snippet yas/init-snippet-template)))

;; Make snippet placeholders
(defun yas/make-placeholder (s e)
  "Make yasnippet placeholder from region."
  (interactive "r")
  (let ((text (buffer-substring s e)))
    (yas-expand-snippet "\\${$1:`text`}" s e)))
(define-key snippet-mode-map "\C-c\C-n" 'yas/make-placeholder)

;; Auto-complete enhancement
(defun yas/set-ac-modes ()
  "Add modes in `yas-snippet-dirs' to `ac-modes'. Call (yas/set-ac-modes) BEFORE (global-auto-complete-mode 1) or (ac-config-default)."
  (eval-after-load "auto-complete"
    '(setq ac-modes
           (append
            (apply 'append (mapcar (lambda (dir) (mapcar 'intern (directory-files dir nil "-mode$")))
                                   (yas-snippet-dirs)))
            ac-modes))))
(yas/set-ac-modes)

;; Expand snippet synchronously
(defvar yas/recursive-edit-flag nil)
(defun yas-expand-sync ()
  "Execute `yas-expand'. This function exits after expanding snippet."
  (interactive)
  (let ((yas/recursive-edit-flag t))
    (call-interactively 'yas-expand)
    (recursive-edit)))
(defun yas-expand-snippet-sync (content &optional start end expand-env)
  "Execute `yas-expand-snippet'. This function exits after expanding snippet."
  (let ((yas/recursive-edit-flag t))
    (yas-expand-snippet content start end expand-env)
    (recursive-edit)))
(defun yas/after-exit-snippet-hook--recursive-edit ()
  (when yas/recursive-edit-flag
    (throw 'exit nil)))
(add-hook 'yas-after-exit-snippet-hook 'yas/after-exit-snippet-hook--recursive-edit)

;; Yatemplate
(add-to-list 'load-path "~/.emacs.d/yatemplate")
(require 'yatemplate)

(provide 'setup-yasnippet)
;;; setup-yasnippet.el ends here

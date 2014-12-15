;;; setup-eshell.el ---

;; Copyright (C) 2014

;; Author:  <ajaraber@AJARABER-MOBL5>
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

(add-hook 'sh-mode-hook
          (lambda ()
            (show-paren-mode -1)
            (flycheck-mode -1)
            (setq blink-matching-paren nil)))

(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;;When compiling from shell, display error result as in compilation
;;buffer, with links to errors.
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defvar my-local-shells
  '("*shell*"))
(defvar my-remote-shells
  '())
(defvar my-shells (append my-local-shells my-remote-shells))

(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 ;; '(comint-completion-autolist t)     ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 '(comint-buffer-maximum-size 20000)    ; max length of the buffer in lines
 '(comint-prompt-read-only nil)         ; if this is t, it breaks shell-command
 '(comint-get-old-input (lambda () "")) ; what to run when i press enter on a
                                        ; line above the current prompt
 '(comint-input-ring-size 5000)         ; max shell history size
 '(protect-buffer-bury-p nil)
 )

(setenv "PAGER" "cat")

;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(defun make-my-shell-output-read-only (text)
  "Add to comint-output-filter-functions to make stdout read only in my shells."
  (if (member (buffer-name) my-shells)
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer)))))
        (put-text-property comint-last-output-start output-end 'read-only t))))
(add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

(defun my-dirtrack-mode ()
  "Add to shell-mode-hook to use dirtrack mode in my shell buffers."
  (when (member (buffer-name) my-shells)
    (shell-dirtrack-mode 0)
    (set-variable 'dirtrack-list '("^.*[^ ]+:\\(.*\\)>" 1 nil))
    (dirtrack-mode 1)))
(add-hook 'shell-mode-hook 'my-dirtrack-mode)

(defun set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))
(add-hook 'shell-mode-hook 'set-scroll-conservatively)

(defun make-comint-directory-tracking-work-remotely ()
  "Add this to comint-mode-hook to make directory tracking work
while sshed into a remote host, e.g. for remote shell buffers
started in tramp. (This is a bug fix backported from Emacs 24:
http://comments.gmane.org/gmane.emacs.bugs/39082"
  (set (make-local-variable 'comint-file-name-prefix)
       (or (file-remote-p default-directory) "")))
(add-hook 'comint-mode-hook 'make-comint-directory-tracking-work-remotely)

(defun enter-again-if-enter ()
  "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
  (when (and (not isearch-mode-end-hook-quit)
             (equal (this-command-keys-vector) [13])) ; == return
    (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
          ((member (buffer-name) my-shells) (comint-send-input)))))
(add-hook 'isearch-mode-end-hook 'enter-again-if-enter)

(defadvice comint-previous-matching-input
  (around suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
        (progn (fset 'message 'ignore) ad-do-it)
      (fset 'message old-message))))

(provide 'setup-eshell)
;;; setup-eshell.el ends here

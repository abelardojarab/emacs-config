;;; setup-flycheck.el ---

;; Copyright (C) 2016  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@Abelardos-MacBook-Pro.local>
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

;; Flycheck
(use-package flycheck
  :if (not (equal system-type 'windows-nt))
  :load-path (lambda () (expand-file-name "flycheck/" user-emacs-directory))
  :config (progn
            (add-to-list 'display-buffer-alist
                         `(,(rx bos "*Flycheck errors*" eos)
                           (display-buffer-reuse-window
                            display-buffer-in-side-window)
                           (reusable-frames . visible)
                           (side            . bottom)
                           (window-height   . 0.4)))

            ;; Enable flycheck for set of modes
            (mapc (lambda (mode)
                    (add-hook mode (lambda () (flycheck-mode t))))
                  '(prog-mode-hook
                    ess-mode-hook))

            ;; disable flycheck during idle time, if enabled
            (delete 'idle-change flycheck-check-syntax-automatically)

            ;; disable flycheck on newline, if enabled
            (delete 'new-line flycheck-check-syntax-automatically)

            ;; Configuration
            (setq-default flycheck-disabled-checkers '(html-tidy emacs-lisp-checkdoc))

            ;; Highlight whole line with error
            (setq flycheck-highlighting-mode 'lines)

            ;; proselint support
            (when (executable-find "proselint")
              (flycheck-define-checker proselint
                "A linter for prose."
                :command ("proselint" source-inplace)
                :error-patterns
                ((warning line-start (file-name) ":" line ":" column ": "
                          (id (one-or-more (not (any " "))))
                          (message) line-end))
                :modes (text-mode markdown-mode org-mode))
              (add-to-list 'flycheck-checkers 'proselint))

            ;; Display error messages on one line in minibuffer and by new lines
            ;; separated in `flycheck-error-message-buffer'.
            (defun flycheck-diplay-error-messages-one-line (errors)
              (-when-let (messages (-keep #'flycheck-error-message errors))
                (when (flycheck-may-use-echo-area-p)
                  (message (s-join " | " messages))
                  (with-current-buffer (get-buffer-create flycheck-error-message-buffer)
                    (erase-buffer)
                    (insert (s-join "\n\n" messages))))))
            (setq flycheck-display-errors-function
                  'flycheck-diplay-error-messages-one-line)))

;; Flycheck irony
(use-package flycheck-irony
  :after flycheck
  :if (file-exists-p "~/.emacs.cache/irony-server/bin/irony-server")
  :load-path (lambda () (expand-file-name "flycheck-irony/" user-emacs-directory))
  :config (progn
            (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
            (flycheck-add-next-checker 'irony '(warning . c/c++-cppcheck))))

;; Flycheck rtags
(use-package flycheck-rtags
  :after (flycheck rtags)
  :if (executable-find "rdm")
  :load-path (lambda () (expand-file-name "rtags/src/" user-emacs-directory))
  :config (progn
            (defun my/flycheck-rtags-setup ()
              (flycheck-select-checker 'rtags))

            ;; c-mode-common-hook is also called by c++-mode
            (add-hook 'c-mode-common-hook #'my/flycheck-rtags-setup)))

;; Tooltips
(use-package flycheck-tip
  :after flycheck
  :load-path (lambda () (expand-file-name "flycheck-tip/" user-emacs-directory))
  :config (progn
            (setq flycheck-tip-avoid-show-func nil)))

;; Another tooltip using pos-tip
(use-package flycheck-pos-tip
  :if (display-graphic-p)
  :after flycheck
  :commands flycheck-pos-tip-mode
  :load-path (lambda () (expand-file-name "flycheck-pos-tip/" user-emacs-directory))
  :config (progn
            (add-hook 'prog-mode-hook (lambda () (flycheck-pos-tip-mode)))))

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here

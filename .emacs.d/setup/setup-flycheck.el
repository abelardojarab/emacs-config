;;; setup-flycheck.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

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

;; Flycheck
(use-package flycheck
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

            ;; Hide flycheck indicator
            (diminish 'flycheck-mode)

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
  :if (executable-find "rtags")
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

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here

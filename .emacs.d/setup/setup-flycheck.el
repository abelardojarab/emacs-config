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
  :load-path "~/.emacs.d/flycheck"
  :config (progn
            (mapc (lambda (mode)
                    (add-hook mode (lambda () (flycheck-mode t))))
                  '(ess-mode-hook
                    c-mode-hook
                    c++-mode-hook
                    perl-mode-hook
                    python-mode-hook
                    js2-mode-hook))

            ;; disable flycheck during idle time, if enabled
            (delete 'idle-change flycheck-check-syntax-automatically)

            ;; disable flycheck on newline, if enabled
            (delete 'new-line flycheck-check-syntax-automatically)

            ;; Configuration
            (setq-default flycheck-disabled-checkers '(html-tidy emacs-lisp-checkdoc))

            ;; Highlight whole line with error
            (setq flycheck-highlighting-mode 'lines)

            ;; Define a poor c/c++ checker (it fails when errors affect other files,
            ;; not the one being being checked actually)
            (defmacro flycheck-define-clike-checker (name command modes)
              `(flycheck-define-checker ,(intern (format "%s" name))
                 ,(format "A %s checker using %s" name (car command))
                 :command (,@command source-inplace)
                 :error-patterns
                 ((warning line-start (file-name) ":" line ":" column
                           ": warning: " (message) line-end)
                  (error line-start (file-name) ":" line ":" column
                         ": error: " (message) line-end))
                 :modes ',modes))
            (flycheck-define-clike-checker c-gcc
                                           ("gcc" "-fsyntax-only" "-Wall" "-Wextra")
                                           c-mode)
            (add-to-list 'flycheck-checkers 'c-gcc)
            (flycheck-define-clike-checker c++-g++
                                           ("g++" "-fsyntax-only" "-Wall" "-Wextra" "-std=c++11")
                                           c++-mode)
            (add-to-list 'flycheck-checkers 'c++-g++)

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

;; Tooltips
(use-package flycheck-tip
  :load-path "~/.emacs.d/flycheck-tip"
  :config (progn
            (setq flycheck-tip-avoid-show-func nil)))

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here

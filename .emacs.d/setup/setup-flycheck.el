;;; setup-flycheck.el ---

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

;; Flycheck
(add-to-list 'load-path "~/.emacs.d/flycheck")
(add-to-list 'load-path "~/.emacs.d/flycheck-tip")
(require 'f)
(require 'flycheck)
(require 'flycheck-tip)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'js2-mode-hook
          (lambda () (flycheck-mode t)))
(add-hook 'lisp-mode-hook
          (lambda () (flycheck-mode t)))
(add-hook 'python-mode-hook
          (lambda () (flycheck-mode t)))

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
      'flycheck-diplay-error-messages-one-line)

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here

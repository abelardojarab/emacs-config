;;; setup-flycheck.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Abelardo Jara-Berrocal

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

;; Flycheck
(use-package flycheck
  :defer t
  :if (not (equal system-type 'windows-nt))
  :commands (flycheck-add-next-checker
             flycheck-mode)
  :init (mapc (lambda (mode)
                (add-hook mode (lambda () (flycheck-mode t))))
              my/flycheck-modes)
  :config (progn

            (defun my/adjust-flycheck-automatic-syntax-eagerness ()
              "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
              (setq flycheck-idle-change-delay
                    (if flycheck-current-errors 0.3 3.0)))

            ;; Each buffer gets its own idle-change-delay because of the
            ;; buffer-sensitive adjustment above.
            (make-variable-buffer-local 'flycheck-idle-change-delay)

            (add-hook 'flycheck-after-syntax-check-hook
                      'my/adjust-flycheck-automatic-syntax-eagerness)

            ;; Remove newline checks, since they would trigger an immediate check
            ;; when we want the idle-change-delay to be in effect while editing.
            (setq flycheck-check-syntax-automatically '(save
                                                        idle-change
                                                        mode-enabled))

            (defun flycheck-handle-idle-change ()
              "Handle an expired idle time since the last change.
This is an overwritten version of the original
flycheck-handle-idle-change, which removes the forced deferred.
Timers should only trigger inbetween commands in a single
threaded system and the forced deferred makes errors never show
up before you execute another command."
              (flycheck-clear-idle-change-timer)
              (flycheck-buffer-automatically 'idle-change))

            ;; Ubuntu 16.04 shellcheck is too old to understand this
            ;; command-line option
            (setq flycheck-shellcheck-follow-sources nil)

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

            ;; Define fringe indicator / warning levels
            (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
              (vector #b00000000
                      #b00000000
                      #b00000000
                      #b00000000
                      #b00000000
                      #b00000000
                      #b00000000
                      #b00011100
                      #b00111110
                      #b00111110
                      #b00111110
                      #b00011100
                      #b00000000
                      #b00000000
                      #b00000000
                      #b00000000
                      #b00000000))
            (flycheck-define-error-level 'error
              :severity 2
              :overlay-category 'flycheck-error-overlay
              :fringe-bitmap 'flycheck-fringe-bitmap-ball
              :fringe-face 'flycheck-fringe-error)
            (flycheck-define-error-level 'warning
              :severity 1
              :overlay-category 'flycheck-warning-overlay
              :fringe-bitmap 'flycheck-fringe-bitmap-ball
              :fringe-face 'flycheck-fringe-warning)
            (flycheck-define-error-level 'info
              :severity 0
              :overlay-category 'flycheck-info-overlay
              :fringe-bitmap 'flycheck-fringe-bitmap-ball
              :fringe-face 'flycheck-fringe-info)

            ;; Display error messages on one line in minibuffer
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
  :defer t
  :after (flycheck irony)
  :if (or (file-exists-p  (concat (file-name-as-directory
                                   my/emacs-cache-dir)
                                  "irony-server/bin/irony-server"))
          (executable-find "irony-server"))
  :commands flycheck-irony-setup
  :hook (flycheck-mode . flycheck-irony-setup)
  :config (flycheck-add-next-checker 'irony '(warning . c/c++-cppcheck)))

;; Flycheck rtags
(use-package flycheck-rtags
  :defer t
  :after (flycheck rtags)
  :if (executable-find "rdm")
  :load-path (lambda () (expand-file-name "rtags/src/" user-emacs-directory))
  :commands my/flycheck-rtags-setup
  :hook (c-mode-common . my/flycheck-rtags-setup)
  :config (defun my/flycheck-rtags-setup ()
            (flycheck-select-checker 'rtags)))

;; Tooltips
(use-package flycheck-tip
  :after flycheck
  :custom (flycheck-tip-avoid-show-func nil))

;; Another tooltip using pos-tip
(use-package flycheck-pos-tip
  :defer t
  :if (display-graphic-p)
  :after flycheck
  :commands flycheck-pos-tip-mode
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here

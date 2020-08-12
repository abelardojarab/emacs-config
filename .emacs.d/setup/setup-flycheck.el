;;; setup-flycheck.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

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
             flycheck-mode
             my/flycheck-adjust-syntax-eagerness)
  :init (mapc (lambda (mode)
                (add-hook mode (lambda () (flycheck-mode t))))
              my/flycheck-modes)
  :hook ((flycheck-after-syntax-check . my/flycheck-adjust-syntax-eagerness))
  :custom ((flycheck-shellcheck-follow-sources nil)
           (flycheck-disabled-checkers         '(html-tidy emacs-lisp-checkdoc))
           (flycheck-highlighting-mode         'lines)
           (flycheck-checker-error-threshold    5000))
  :config (progn
            (defun my/flycheck-adjust-syntax-eagerness ()
              "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
              (setq flycheck-idle-change-delay
                    (if flycheck-current-errors 0.3 3.0)))

            ;; Each buffer gets its own idle-change-delay because of the
            ;; buffer-sensitive adjustment above.
            (make-variable-buffer-local 'flycheck-idle-change-delay)

            ;; Remove newline checks, since they would trigger an immediate check
            ;; when we want the idle-change-delay to be in effect while editing.
            (setq flycheck-check-syntax-automatically '(save
                                                        idle-change
                                                        mode-enabled))

            (defun flycheck-handle-idle-change ()
              "This is an overwritten version of the original
flycheck-handle-idle-change, which removes the forced deferred."
              (flycheck-clear-idle-change-timer)
              (flycheck-buffer-automatically 'idle-change))

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
  :disabled t
  :after flycheck
  :custom (flycheck-tip-avoid-show-func nil))

;; Another tooltip using pos-tip (does not work)
(use-package flycheck-pos-tip
  :defer t
  :if (display-graphic-p)
  :after flycheck
  :commands flycheck-pos-tip-mode
  :hook (flycheck-mode . flycheck-pos-tip-mode)
  :config (defun flycheck-pos-tip-error-messages (errors)
            "Display ERRORS, using a graphical tooltip on GUI frames."
            (when errors
              (if (display-graphic-p)
                  (let ((message (flycheck-help-echo-all-error-messages errors))
                        (line-height (car (window-line-height))))
                    (flycheck-pos-tip--check-pos)
                    (prin1 message)
                    ;; (pos-tip-show (with-temp-buffer
                    ;;                 (prin1 message)
                    ;;                 (princ "")
                    ;;                 (buffer-string))

                    ;;               nil nil nil flycheck-pos-tip-timeout
                    ;;               flycheck-pos-tip-max-width nil
                    ;;               nil (and line-height (+ line-height 5)))
                    )
                (funcall flycheck-pos-tip-display-errors-tty-function errors)))))

;; posframe integration with flycheck
(use-package flycheck-posframe
  :defer t
  :after flycheck
  :if (and (window-system) (version<= "26.1" emacs-version))
  :custom (flycheck-posframe-border-width 1)
  :commands flycheck-posframe-configure-pretty-defaults
  :hook (flycheck-mode . flycheck-posframe-configure-pretty-defaults)
  :config (progn
            (set-face-attribute 'flycheck-posframe-background-face nil :inherit 'default)
            (set-face-attribute 'flycheck-posframe-border-face nil :foreground "gray50")
            (set-face-attribute 'flycheck-posframe-info-face nil :inherit 'flycheck-error-list-info)
            (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'flycheck-error-list-warning)
            (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'flycheck-error-list-error)
            (add-hook 'flycheck-mode-hook (lambda ()
                                            (if (not (bound-and-true-p lsp-ui-sideline-mode))
                                                (flycheck-posframe-mode))))
            (flycheck-posframe-configure-pretty-defaults)))

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here

;;; setup-ess.el ---

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

(use-package ess-site
  :load-path (lambda () (expand-file-name "ESS/lisp" user-emacs-directory))
  :mode ("\\.[rR]$" . R-mode)
  :commands (R-mode
             ess-eval-function
             ess-eval-line
             ess-eval-buffer
             ess-switch-to-ESS)
  :config (progn
            (setq-default ess-dialect "R")
            (setq-default inferior-R-args " --no-restore-history --no-save ")

            ;; show function arguments in ESS buffers
            (require 'ess-eldoc)

            ;; also show in iESS buffers
            (add-hook 'inferior-ess-mode-hook 'ess-use-eldoc)

            ;; http://www.kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
            (add-hook 'ess-mode-hook
                      '(lambda()
                         (setq comint-scroll-to-bottom-on-input t)
                         (setq comint-scroll-to-bottom-on-output t)
                         (setq comint-move-point-for-output t)))

            ;; http://permalink.gmane.org/gmane.emacs.ess.general/8419
            ;; Script font lock highlight.
            (setq ess-R-font-lock-keywords
                  '((ess-R-fl-keyword:modifiers . t)
                    (ess-R-fl-keyword:fun-defs . t)
                    (ess-R-fl-keyword:keywords . t)
                    (ess-R-fl-keyword:assign-ops . t)
                    (ess-R-fl-keyword:constants . t)
                    (ess-fl-keyword:fun-calls . t)
                    (ess-fl-keyword:numbers . t)
                    (ess-fl-keyword:operators . t)
                    (ess-fl-keyword:delimiters . t)
                    (ess-fl-keyword:= . t)
                    (ess-R-fl-keyword:F&T . t)
                    (ess-R-fl-keyword:%op% . t)))

            ;; Console font lock highlight.
            (setq inferior-R-font-lock-keywords
                  '((ess-S-fl-keyword:prompt . t)
                    (ess-R-fl-keyword:messages . t)
                    (ess-R-fl-keyword:modifiers . t)
                    (ess-R-fl-keyword:fun-defs . t)
                    (ess-R-fl-keyword:keywords . t)
                    (ess-R-fl-keyword:assign-ops . t)
                    (ess-R-fl-keyword:constants . t)
                    (ess-fl-keyword:matrix-labels . t)
                    (ess-fl-keyword:fun-calls . t)
                    (ess-fl-keyword:numbers . t)
                    (ess-fl-keyword:operators . t)
                    (ess-fl-keyword:delimiters . t)
                    (ess-fl-keyword:= . t)
                    (ess-R-fl-keyword:F&T . t)
                    (ess-R-fl-keyword:%op% . t)))

            ;; Enable auto-complete
            (when (featurep 'auto-complete)
              (add-hook 'R-mode-hook  '(lambda ()
                                         (auto-complete-mode)
                                         (setq ess-use-auto-complete t))))

            ;; config
            (setq ess-indent-level 2)
            (setq ess-arg-function-offset-new-line (list ess-indent-level))
            (setq ess-fancy-comments nil)
            (setq ess-loaded-p t)
            (setq ess-ask-for-ess-directory nil)
            (setq ess-eldoc-show-on-symbol t)))

(provide 'setup-ess)

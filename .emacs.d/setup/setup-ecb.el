;;; setup-ecb.el ---                       -*- lexical-binding: t; -*-

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

;; Code Browser
(use-package ecb
  :defer t
  :bind (:map ctl-x-map
              ("0" . my/ecb-activate))
  :init (setq stack-trace-on-error t)
  :commands (ecb-redraw-layout
             my/ecb-activate
             idle-timer-ecb-methods-start)
  :custom ((ecb-truncate-lines                      t)
       (ecb-show-sources-in-directories-buffer  'always)
       (ecb-compile-window-height               nil)
       (ecb-compile-window-width                'edit-window)
       (ecb-compile-window-temporally-enlarge   nil)
       (ecb-eshell-fit-window-to-command-output nil)
       (ecb-windows-width               35)
       (ecb-fix-window-size             'width)
       (ecb-layout-name             my/ecb-layout-theme)
       (ecb-history-make-buckets            'mode)
       (ecb-highlight-tag-with-point-delay      180)
       (ecb-kill-buffer-clears-history      'auto)
       (ecb-tip-of-the-day              nil))
  :config (progn

            (defadvice semanticdb-deep-find-tags-by-name-method (around bar activate)
              (ignore-errors add-do-it))

            (defun my/ecb-activate ()
              (interactive)
              (progn
                (ecb-redraw-layout)
                (ecb-activate)
                (ecb-redraw-layout)))

            ;; Fix error with symboldef sync
            (defmacro ecb-with-readonly-buffer (buffer &rest body)
              "Make buffer BUFFER current but do not display it."
              `(ignore-errors (if (buffer-live-p ,buffer)
                                  (with-current-buffer ,buffer
                                    (unwind-protect
                                        (progn
                                          (setq buffer-read-only nil)
                                          ,@body)
                                      (setq buffer-read-only t)))
                                (ecb-error "Try to set a not existing buffer."))))

            (setq-default ecb-source-path
                          (quote
                           (("~/workspace/Documents"    "Documents")
                            ("~/workspace"              "Local")
                            ("~/workspace_remote"       "Shared"))))

            (setq ecb-create-layout-file  (concat (file-name-as-directory
                                                   my/emacs-cache-dir)
                                                  "ecb-user-layouts.el")
                  ecb-tip-of-the-day-file  (concat (file-name-as-directory
                                                    my/emacs-cache-dir)
                                                   "ecb-tip-of-day.el")
                  ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2
                  ecb-source-file-regexps '( ;; In all folders:
                                            (".*"
                                             ;; Exclude
                                             ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\|pyc\\)$\\)\\)")
                                             ;; Include
                                             ("^\\.\\(emacs\\|gnus\\)$")))
                  semantic-decoration-styles (list
                                              '("semantic-decoration-on-protected-members" . t)
                                              '("semantic-decoration-on-private-members"   . t)
                                              '("semantic-decoration-on-includes"          . t)
                                              '("semantic-tag-boundary"                    . t)))

        (ecb-layout-define "leftright-analyse-y" left-right
          "This function creates the following layout:
   --------------------------------------------------------------
   |              |                               |             |
   |  Directories |                               |  Methods    |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |--------------|             Edit              |-------------|
   |              |                               |             |
   |  Sources     |                               |             |
   |              |                               |             |
   |--------------|                               |  Analyse    |
   |              |                               |             |
   |  History     |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------
If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
          (ecb-set-history-buffer)
          (ecb-split-ver 0.3)
          (ecb-set-sources-buffer)
          (ecb-split-ver 0.5)
          (ecb-set-symboldef-buffer)
          (select-window (next-window (next-window)))
          (ecb-set-methods-buffer)
          (ecb-split-ver 0.5)
          (ecb-set-analyse-buffer)
          (select-window (previous-window (previous-window (selected-window) 0) 0)))

            ;; Reparse files on 'first change' and 'after saving'
            (defun idle-timer-ecb-methods-callback ()
              (interactive)
              (when (bound-and-true-p ecb-minor-mode)
                (ignore-errors
                  ;; this is to get the methods buffer to refresh correctly.
                  ;; semantic idle mode refresh doesn't seem to work all that well.
                  (semantic-force-refresh)
                  (ecb-rebuild-methods-buffer)
                  (if (ecb--semantic-active-p)
                      (ecb-update-methods-buffer--internal nil nil t)
                    (ecb-rebuild-methods-buffer-for-non-semantic))
                  (ecb-window-sync))))

            (add-hook 'after-save-hook    #'idle-timer-ecb-methods-callback)
            (add-hook 'first-change-hook  #'idle-timer-ecb-methods-callback)

            ;; Also reparse files on tabbar change
            (defadvice tabbar-forward-tab (after recompute-semantic activate)
              (idle-timer-ecb-methods-callback))

            (defadvice tabbar-backward-tab (after recompute-semantic activate)
              (idle-timer-ecb-methods-callback))

            (defadvice tabbar-select-tab-callback (after recompute-semantic activate)
              (idle-timer-ecb-methods-callback))

            ;; Speedbar
            (use-package sr-speedbar
              :disabled t
              :demand t
              :defines (sr-speedbar-exist-p)
              :commands (sr-speedbar-open
                         sr-speedbar-toggle
                         sr-speedbar-refresh-turn-on)
              :bind (:map speedbar-mode-map
                          ([S-up]  . speedbar-up-directory)
                          ([right] . speedbar-flush-expand-line)
                          ([left]  . speedbar-contract-line))
              :config (progn
                        (speedbar-set-timer 0.2)
                        (setq speedbar-update-flag-disable          nil
                              speedbar-update-flag                  t
                              speedbar-hide-button-brackets-flag    t
                              speedbar-show-unknown-files           t
                              speedbar-smart-directory-expand-flag  t
                              speedbar-directory-button-trim-method 'trim
                              speedbar-use-images                   t
                              speedbar-indentation-width            2
                              speedbar-use-imenu-flag               t
                              speedbar-file-unshown-regexp          "flycheck-.*"
                              sr-speedbar-width                     40
                              sr-speedbar-width-x                   40
                              sr-speedbar-auto-refresh              t
                              sr-speedbar-skip-other-window-p       t
                              sr-speedbar-right-side                nil)

                        ;; Highlight the current line
                        (add-hook 'speedbar-mode-hook (lambda () (hl-line-mode 1)))

                        ;; Add Javascript
                        (speedbar-add-supported-extension ".js")
                        (add-to-list 'speedbar-fetch-etags-parse-list
                                     '("\\.js" . speedbar-parse-c-or-c++tag))
                        (speedbar-add-supported-extension ".il")
                        (speedbar-add-supported-extension ".ils")

                        ;; Enable speed-bar auto-refresh
                        (sr-speedbar-refresh-turn-on)))

            ;; Enable ECB
            (my/ecb-activate)

            ;; projectile and speedbar integration
            (use-package projectile-speedbar
              :disabled t
              :demand t
              :after (sr-speedbar projectile)
              :commands projectile-speedbar-open-current-buffer-in-tree
              :init (progn
                      (defadvice helm-projectile-find-file (after locate-file activate)
                        (if (sr-speedbar-exist-p)
                            (projectile-speedbar-open-current-buffer-in-tree)))
                      (defadvice speedbar-item-load (after speedbar-highlight-file activate)
                        (projectile-speedbar-open-current-buffer-in-tree))))))

;; Activate ECB in graphical mode
(if (display-graphic-p)
    (my/ecb-activate))

(provide 'setup-ecb)
;;; setup-ecb.el ends here

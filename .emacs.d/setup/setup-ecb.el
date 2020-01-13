;;; setup-ecb.el ---                       -*- lexical-binding: t; -*-

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

;; Code Browser
(use-package ecb
  :defer t
  :bind (:map ctl-x-map
              ("0" . my/ecb-activate))
  :init (setq stack-trace-on-error t)
  :commands (ecb-redraw-layout
             my/ecb-activate
             idle-timer-ecb-methods-start)
  :custom ((stack-trace-on-error                       t)
           (ecb-truncate-lines                         t)
           (ecb-auto-save-before-etags-methods-rebuild nil)
           (ecb-show-sources-in-directories-buffer     'always)
           (ecb-compile-window-height                  nil)
           (ecb-compile-window-width                   'edit-window)
           (ecb-compile-window-temporally-enlarge      nil)
           (ecb-eshell-fit-window-to-command-output    nil)
           (ecb-windows-width                          35)
           (ecb-fix-window-size                        'width)
           (ecb-layout-name                            my/ecb-layout-theme)
           (ecb-history-make-buckets                   'mode)
           (ecb-highlight-tag-with-point-delay         180)
           (ecb-kill-buffer-clears-history             'auto)
           (ecb-tip-of-the-day                         nil))
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
              :hook (speedbar-mode hl-line-mode)
              :custom ((speedbar-update-flag-disable          nil)
                       (speedbar-update-flag                  t)
                       (speedbar-hide-button-brackets-flag    t)
                       (speedbar-show-unknown-files           t)
                       (speedbar-smart-directory-expand-flag  t)
                       (speedbar-directory-button-trim-method 'trim)
                       (speedbar-use-images                   t)
                       (speedbar-indentation-width            2)
                       (speedbar-use-imenu-flag               t)
                       (speedbar-file-unshown-regexp          "flycheck-.*")
                       (sr-speedbar-width                     40)
                       (sr-speedbar-width-x                   40)
                       (sr-speedbar-auto-refresh              t)
                       (sr-speedbar-skip-other-window-p       t)
                       (sr-speedbar-right-side                nil))
              :config (progn
                        (speedbar-set-timer 0.2)

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

;; Activate ECB in console mode
;; (if (not (display-graphic-p))
;;    (my/ecb-activate))

(use-package treemacs
  :demand t
  :commands treemacs
  :init (setq treemacs-persist-file (concat (file-name-as-directory
                                             my/emacs-cache-dir)
                                            "treemacs-persist"))
  :custom ((treemacs-collapse-dirs                 (if (executable-find "python3") 3 0))
           (treemacs-deferred-git-apply-delay      0.5)
           (treemacs-display-in-side-window        t)
           (treemacs-eldoc-display                 t)
           (treemacs-file-event-delay              5000)
           (treemacs-file-follow-delay             0.2)
           (treemacs-follow-after-init             t)
           (treemacs-git-command-pipe              "")
           (treemacs-goto-tag-strategy             'refetch-index)
           (treemacs-indentation                   2)
           (treemacs-indentation-string            " ")
           (treemacs-is-never-other-window         nil)
           (treemacs-max-git-entries               5000)
           (treemacs-missing-project-action        'ask)
           (treemacs-no-png-images                 nil)
           (treemacs-no-delete-other-windows       t)
           (treemacs-project-follow-cleanup        nil)
           (treemacs-position                      'left)
           (treemacs-recenter-distance             0.1)
           (treemacs-recenter-after-file-follow    nil)
           (treemacs-recenter-after-tag-follow     nil)
           (treemacs-recenter-after-project-jump   'always)
           (treemacs-recenter-after-project-expand 'on-distance)
           (treemacs-show-cursor                   nil)
           (treemacs-show-hidden-files             t)
           (treemacs-silent-filewatch              nil)
           (treemacs-silent-refresh                nil)
           (treemacs-sorting                       'alphabetic-asc)
           (treemacs-space-between-root-nodes      t)
           (treemacs-tag-follow-cleanup            t)
           (treemacs-tag-follow-delay              1.5)
           (treemacs-width                         26))
  :config (progn
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t)
            (treemacs-fringe-indicator-mode t)
            (pcase (cons (not (null (executable-find "git")))
                         (not (null (executable-find "python3"))))
              (`(t . t)
               (treemacs-git-mode 'deferred))
              (`(t . _)
               (treemacs-git-mode 'simple)))))

;; Integration with magit
(use-package treemacs-magit
  :defer t
  :after magit)

;; Integration with projectile
(use-package treemacs-projectile
  :defer t
  :after projectile)

;; `lsp-mode' and `treemacs' integration
(use-package lsp-treemacs
  :demand t
  :after (lsp-mode all-the-icons)
  :bind (:map lsp-mode-map
              ("C-<f8>" . lsp-treemacs-errors-list)
              ("<f8>" . lsp-treemacs-symbols))
  :config (progn
            (treemacs-create-theme "centaur-colors"
              :extends "doom-colors"
              :config
              (progn
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
                 :extensions (root))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
                 :extensions (boolean-data))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
                 :extensions (class))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-material "palette" :height 0.95 :v-adjust -0.15))
                 :extensions (color-palette))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.05))
                 :extensions (constant))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "file-text-o" :height 0.95 :v-adjust -0.05))
                 :extensions (document))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-material "storage" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
                 :extensions (enumerator))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
                 :extensions (enumitem))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "bolt" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-orange))
                 :extensions (event))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
                 :extensions (field))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "search" :height 0.95 :v-adjust -0.05))
                 :extensions (indexer))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-material "filter_center_focus" :height 0.95 :v-adjust -0.15))
                 :extensions (intellisense-keyword))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
                 :extensions (interface))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
                 :extensions (localvariable))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
                 :extensions (method))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
                 :extensions (namespace))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15))
                 :extensions (numeric))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-material "control_point" :height 0.95 :v-adjust -0.2))
                 :extensions (operator))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
                 :extensions (property))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
                 :extensions (snippet))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.05))
                 :extensions (string))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
                 :extensions (structure))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
                 :extensions (template))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
                 :extensions (collapsed) :fallback "+")
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
                 :extensions (expanded) :fallback "-")
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9  :v-adjust 0.0 :face 'font-lock-doc-face))
                 :extensions (classfile))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-blue))
                 :extensions (default-folder-opened))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue))
                 :extensions (default-folder))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
                 :extensions (default-root-folder-opened))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
                 :extensions (default-root-folder))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
                 :extensions ("class"))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "file-zip" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
                 :extensions (file-type-jar))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                 :extensions (folder-open))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
                 :extensions (folder))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-orange))
                 :extensions (folder-type-component-opened))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-orange))
                 :extensions (folder-type-component))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
                 :extensions (folder-type-library-opened))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
                 :extensions (folder-type-library))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-pink))
                 :extensions (folder-type-maven-opened))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-pink))
                 :extensions (folder-type-maven))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-type-face))
                 :extensions (folder-type-package-opened))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-type-face))
                 :extensions (folder-type-package))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "plus" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                 :extensions (icon-create))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "list" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                 :extensions (icon-flat))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
                 :extensions (icon-hierarchical))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "link" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                 :extensions (icon-link))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "refresh" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                 :extensions (icon-refresh))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "chain-broken" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                 :extensions (icon-unlink))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-alltheicon "java" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
                 :extensions (jar))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "book" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-green))
                 :extensions (library))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "folder-open" :face 'all-the-icons-lblue))
                 :extensions (packagefolder-open))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
                 :extensions (packagefolder))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
                 :extensions (package))
                (treemacs-create-icon
                 :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
                 :extensions (java-project))))

            (setq lsp-treemacs-theme "centaur-colors")))

(provide 'setup-ecb)
;;; setup-ecb.el ends here

;;; setup-ivy.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023  Abelardo Jara-Berrocal

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
(unless (fboundp 'ensure-list)
  (defun ensure-list (object)
    "Return OBJECT as a list.
If OBJECT is already a list, return OBJECT itself.  If it's
not a list, return a one-element list containing OBJECT."
    (declare (side-effect-free error-free))
    (if (listp object)
        object
      (list object))))

(use-package swiper
  :defer t
  :commands (swiper
             swiper-all
             ivy-mode
             ivy-read
             ivy-completing-read
             ivy-resume
             ivy-switch-buffer
             ivy-switch-buffer-other-window)
  :diminish ivy-mode
  :hook (on-first-input . ivy-mode)
  :defines (projectile-completion-system
            magit-completing-read-function)
  :bind (("C-c C-r" . ivy-resume)
         :map ctl-x-map
         ("s"       . swiper)
         ("b"       . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-v"     . ivy-yank-word)
         ("TAB"     . ivy-alt-done))
  :custom ((ivy-do-completion-in-region      nil)
           (ivy-wrap                         t)
           (ivy-fixed-height-minibuffer      t)
           (ivy-height                       20)
           (ivy-virtual-abbreviate           'full)
           (ivy-initial-inputs-alist         nil)
           (ivy-count-format                 "[%d/%d]")
           (ivy-display-style                'fancy)
           (ivy-format-function              'ivy-format-function-arrow)
           (ivy-use-virtual-buffers          t)
           (ivy-magic-slash-non-match-action nil))
  :config (progn
            (push #'+ivy-yas-prompt yas-prompt-functions)
            (setq completion-in-region-function #'ivy-completion-in-region)

            (ivy-set-actions
             'ivy-switch-buffer
             '(("k"
                (lambda (x)
                  (kill-buffer x)
                  (ivy--reset-state ivy-last))
                "kill")
               ("j"
                ivy--switch-buffer-other-window-action
                "other window")))

            ;; advise swiper to recenter on exit
            (defun my/swiper-recenter (&rest args)
              "recenter display after swiper"
              (recenter))
            (advice-add 'swiper :after #'my/swiper-recenter)))

(use-package counsel
  :defer t
  :commands counsel-mode
  :diminish counsel-mode
  :hook (ivy-mode . counsel-mode)
  :bind (("M-x"                     . counsel-M-x)
         ("C-S-p"                   . counsel-M-x) ;; like sublimetext/vscode
         ("C-o"                     . counsel-find-file)
         ("C-c C-v"                 . counsel-yank-pop)
         ("C-c <xterm-paste>"       . counsel-yank-pop)
         ("C-c C-a"                 . counsel-git-grep)
         ("C-c C-g"                 . counsel-git-checkout)
         ([remap bookmark-jump]     . counsel-bookmark)
         ([remap bookmark-set]      . counsel-bookmark)
         ([remap find-file]         . counsel-find-file)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-function] . counsel-describe-function)
         :map org-mode-map
         ("C-c C-j"                 . counsel-org-goto)
         ("C-c C-t"                 . counsel-org-tag)
         :map ctl-x-map
         ("x"                       . counsel-M-x)
         ("<xterm-paste>"           . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("M-y"                     . ivy-next-line)
         :map read-expression-map
         ("C-r"                     . counsel-expression-history))
  :config (setq counsel-yank-pop-separator
                (concat "\n\n"
                        (concat (apply 'concat (make-list 50 "---")) "\n"))))

;; Use universal ctags to build the tags database for the project.
;; When you first want to build a TAGS database run 'touch TAGS'
;; in the root directory of your project.
(use-package counsel-etags
  :demand t
  :bind (("M-." . counsel-etags-find-tag-at-point)
         ("M-t" . counsel-etags-grep-symbol-at-point)
         ("M-s" . counsel-etags-find-tag))
  :custom (counsel-etags-update-interval 180))

(use-package counsel-gtags
  :if (executable-find "global")
  :hydra (hydra-counsel-gtags (:color blue :columns 4)
                              "GNU GLOBAL"
                              ("d" counsel-gtags-find-definition "Definition")
                              ("r" counsel-gtags-find-reference "Reference")
                              ("s" counsel-gtags-find-symbol "Symbol")
                              ("f" counsel-gtags-find-file "File")
                              ("n" counsel-gtags-go-forward "Next" :color red)
                              ("p" counsel-gtags-go-backward "Previous" :color red)
                              ("c" counsel-gtags-create-tags "Create")
                              ("u" counsel-gtags-update-tags "Update")
                              ("q" nil "Quit")))

;; Select from xref candidates with Ivy
(use-package ivy-xref
  :disabled t ;; prefer helm-xref
  :if (and (executable-find "global")
           (boundp 'xref-backend-functions))
  :custom (xref-show-xrefs-function 'ivy-xref-show-xrefs))

;; Improve ivy-switch-buffer
(use-package ivy-rich
  :defer t
  :after swiper
  :commands ivy-switch-buffer
  :custom (ivy-rich-switch-buffer-align-virtual-buffer nil)
  :config (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

;; Ivy integration with posframe
(use-package ivy-posframe
  :disabled t
  :defer t
  :if (and (window-system) (version<= "26.1" emacs-version))
  :after swiper
  :custom ((ivy-posframe-hide-minibuffer  t)
           (ivy-posframe-border-width     2)
           (ivy-posframe-min-width        80)
           (ivy-posframe-min-height       10)
           (ivy-posframe-width            nil)
           (ivy-posframe-height           nil))
  :hook (ivy-mode . ivy-posframe-mode)
  :config (setq ivy-posframe-display-functions-alist
                '((swiper . ivy-posframe-display-at-window-bottom-left)
                  (t . ivy-posframe-display-at-frame-center)
                  )))

;; Ivy integration with yasnippet
(use-package ivy-yasnippet
  :defer t
  :after (swiper yasnippet)
  :commands ivy-yasnippet
  :bind (:map yas-minor-mode-map
              (([(shift tab)] . ivy-yasnippet)
               ([backtab]     . ivy-yasnippet))
              :map ctl-x-map
              ("y"           . ivy-yasnippet)))

;; Ivy source for 'pass' tool
(use-package ivy-pass
  :custom (password-store-password-length 30)
  :bind ("C-c p" . ivy-pass))

;; counsel-org-clock provides commands for displaying org clock entries
(use-package counsel-org-clock
  :defer t
  :commands (counsel-org-clock-context
             counsel-org-clock-history))

;; Ivy integration with projectile
(use-package counsel-projectile
  :defer t
  :commands counsel-projectile-mode
  :hook (on-first-input . counsel-projectile-mode))

;; Tramp ivy interface
(use-package counsel-tramp
  :defer t
  :commands counsel-tramp)

;; Icons for ivy
(use-package all-the-icons-ivy
  :defer t
  :if (display-grayscale-p)
  :commands all-the-icons-ivy-setup
  :after swiper
  :hook (counsel-mode . all-the-icons-ivy-setup))

;; Ivy integration with lsp
(use-package lsp-ivy
  :commands (lsp-ivy-workspace-symbol
             lsp-ivy-global-workspace-symbol))

;; Use the ~substring~ completion style so calling this from isearch works properly
(defun consult-line-literal ()
  (interactive)
  (let ((completion-styles '(substring))
        (completion-category-defaults nil)
        (completion-category-overrides nil))
    (consult-line)))

(use-package consult-xref
  :commands consult-xref)

(use-package consult-imenu
  :demand t
  :commands consult-imenu)

(use-package consult
  :after projectile
  :defines consult-buffer-sources
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ([remap switch-to-buffer] . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos) ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck) ;; Alternative: consult-flymake
         ("M-g g" . consult-goto-line) ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ;; ("M-s f" . consult-find)
         ;; ("M-s F" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
         ;; ("M-s l" . consult-line-literal)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s m" . consult-multi-occur)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;;("M-s e" . consult-isearch-history)
         :map ctl-x-map
         ("C-r"                     . consult-recent-file)
         :map isearch-mode-map
         ("C-o" . consult-line-literal)
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line-literal) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         )
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-narrow-key "<") ; use this to show different types of things in C-x b

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.4 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   )
  ;; Use projects as a source for consult-buffer
  ;; Works, but hides "file" sources -- use "<" to select other sources
  (projectile-load-known-projects)
  (setq my-consult-source-projectile-projects
        `(:name "Projectile projects"
                :narrow   ?P
                :category project
                :action   ,#'projectile-switch-project-by-name
                :items    ,projectile-known-projects))
  (add-to-list 'consult-buffer-sources my-consult-source-projectile-projects 'append)
  )

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; flycheck integration - nice. ~M-g f~
(use-package consult-flycheck)

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  )

(provide 'setup-ivy)
;;; setup-swiper.el ends here

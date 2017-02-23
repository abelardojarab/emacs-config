;;; setup-helm-plugins.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  Abelardo Jara-Berrocal

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

;; Helm grepint
(use-package helm-grepint
  :defer t
  :commands (helm-grepint-grep)
  :load-path (lambda () (expand-file-name "helm-grepint/" user-emacs-directory))
  :config (progn
            (helm-grepint-set-default-config)))

;; Helm git-grep
(use-package helm-git-grep
  :defer t
  :commands (helm-git-grep)
  :load-path (lambda () (expand-file-name "helm-git-grep/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("g" . helm-git-grep))
  :config (progn
            (setq helm-git-grep-candidate-number-limit nil)))

;; Helm ag/ack
(use-package helm-ag
  :defer t
  :commands (helm-ag helm-do-ag helm-do-ag-this-file helm-do-ag-project-root)
  :bind (:map ctl-x-map
              ("a" . helm-ag))
  :load-path (lambda () (expand-file-name "helm-ag/" user-emacs-directory))
  :config (progn
            ;; Fallback to ack if the silver searcher is not found
            (unless (or (executable-find "ag")
                        (executable-find "pt"))
              (setq helm-ag-base-command "awk --nocolor --nogroup"))))

;; Helm describe modes
(use-package helm-describe-modes
  :defer t
  :commands (helm-describe-modes)
  :load-path (lambda () (expand-file-name "helm-describe-modes/" user-emacs-directory))
  :config (progn
            (global-set-key [remap describe-mode] #'helm-describe-modes)))

;; Helm desc-binds
(use-package helm-descbinds
  :defer t
  :commands (helm-descbinds)
  :load-path (lambda () (expand-file-name "helm-descbinds/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("k" . helm-descbinds))
  :config (progn
            (helm-descbinds-mode 1)))

;; Helm flycheck
(use-package helm-flycheck
  :defer t
  :commands (helm-flycheck)
  :load-path (lambda () (expand-file-name "helm-flycheck/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("e" . helm-flycheck)))

;; Helm flyspell
(use-package helm-flyspell
  :defer t
  :commands (helm-flyspell-correct)
  :load-path (lambda () (expand-file-name "helm-flyspell/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("c" . helm-flyspell-correct)))

;; Helm ls git
(use-package helm-ls-git
  :defer t
  :commands (helm-ls-git-ls helm-browse-project)
  :bind (:map ctl-x-map
              ("C-d" . helm-browse-project))
  :load-path (lambda () (expand-file-name "helm-ls-git/" user-emacs-directory)))

;; Helm bm support
(use-package helm-bm
  :defer t
  :commands (helm-bm)
  :after bm
  :load-path (lambda () (expand-file-name "helm-bm/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("l" . helm-bm))
  :config (setq helm-bookmark-show-location t))

;; Helm etags plus
(use-package helm-etags+
  :defer t
  :commands (helm-etags-select)
  :load-path (lambda () (expand-file-name "helm-etags-plus/" user-emacs-directory))
  :bind ("M-." . helm-etags-select))

;; Helm gtags
(use-package helm-gtags
  :defer t
  :commands (helm-gtags-select
             helm-gtags-dwim
             helm-gtags-mode)
  :if (executable-find "global")
  :load-path (lambda () (expand-file-name "helm-gtags/" user-emacs-directory))
  :bind (("C-." . helm-gtags-dwim)
         :map ctl-x-map
         ("." . helm-gtags-dwim))
  :init (add-hook 'c-mode-common-hook
                  (lambda () (helm-gtags-mode t)))
  :config (setq
           helm-gtags-ignore-case t
           helm-gtags-auto-update t
           helm-gtags-use-input-at-cursor t
           helm-gtags-pulse-at-cursor t
           helm-gtags-prefix-key "\C-cg"
           helm-gtags-suggested-key-mapping t))

;; Helm yasnippet
(use-package helm-c-yasnippet
  :defer t
  :commands (helm-yas-complete)
  :load-path (lambda () (expand-file-name "helm-c-yasnippet/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("y" . helm-yas-complete)))

;; Helm make support
(use-package helm-make
  :defer t
  :commands (helm-make)
  :load-path (lambda () (expand-file-name "helm-make/" user-emacs-directory)))

;; Biblio (helm-bibtex requirement)
;; extensible Emacs package for browsing and fetching references
(use-package biblio
  :defer t
  :load-path (lambda () (expand-file-name "biblio/" user-emacs-directory)))

;; Helm bibtex
(use-package helm-bibtex
  :defer t
  :commands (helm-bibtex)
  :bind (:map ctl-x-map
              ("[" . helm-bibtex))
  :load-path (lambda () (expand-file-name "helm-bibtex/" user-emacs-directory))
  :config (progn
            (setq helm-bibtex-bibliography "~/workspace/Documents/Bibliography/biblio.bib")
            (setq helm-bibtex-library-path "~/workspace/Documents/Bibliography/PDFs")
            (setq helm-bibtex-notes-path "~/workspace/Documents/Bibliography/notes.org")

            ;; open pdf with system pdf viewer (works on mac)
            (setq helm-bibtex-pdf-open-function
                  (lambda (fpath)
                    (start-process "open" "*open*" "open" fpath)))

            (defun helm-bibtex-cite ()
              "Helm command to cite bibliography."
              (interactive)
              (helm-other-buffer
               '(helm-c-source-bibtex)
               "*helm bibtex:"))))

;; Org-Ref
(use-package org-ref
  :after (helm async)
  :load-path (lambda () (expand-file-name "org-ref/" user-emacs-directory))
  :config (progn
            (setq org-ref-bibliography-notes "~/workspace/Documents/Bibliography/notes.org"
                  org-ref-default-bibliography '("~/workspace/Documents/Bibliography/biblio.bib")
                  org-ref-pdf-directory "~/workspace/Documents/Bibliography/PDFs")
            (setq org-ref-insert-cite-key "C-c [")
            (setq org-ref-default-citation-link "autocite")))

;; Helm themes
(use-package helm-themes
  :defer t
  :commands (helm-themes)
  :load-path (lambda () (expand-file-name "helm-themes/" user-emacs-directory)))

;; Helm dash
(use-package helm-dash
  :defer t
  :if (executable-find "sqlite3")
  :commands (helm-dash)
  :load-path (lambda () (expand-file-name "helm-dash/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("d" . helm-dash))
  :config (progn
            (setq helm-dash-enable-debugging nil)
            (setq helm-dash-min-length 2)
            (setq helm-dash-docsets-path (expand-file-name "docsets/" user-emacs-directory))
            (setq helm-dash-common-docsets '("Git"
                                             "Emacs_Lisp"))

            ;; Mode-specific hooks
            (defun dash-load-git ()
              (interactive)
              (setq-local helm-dash-docsets '("Git")))

            (defun dash-load-org ()
              (interactive)
              (setq-local helm-dash-docsets '("Org")))
            (add-hook 'org-mode-hook 'dash-load-org)

            (defun dash-load-cmake ()
              (interactive)
              (setq-local helm-dash-docsets '("CMake")))
            (add-hook 'cmake-mode-hook 'dash-load-cmake)

            (defun dash-load-elisp ()
              (interactive)
              (setq-local helm-dash-docsets '("Emacs_Lisp")))
            (add-hook 'emacs-lisp-mode-hook 'dash-load-elisp)

            (defun dash-load-c ()
              (interactive)
              (setq-local helm-dash-docsets '("C")))
            (add-hook 'c-mode-hook 'dash-load-c)

            (defun dash-load-c++ ()
              (interactive)
              (setq-local helm-dash-docsets '("C++"
                                              "GLib")))
            (add-hook 'c++-mode-hook 'dash-load-c++)

            (defun dash-load-py ()
              (interactive)
              (setq-local helm-dash-docsets '("Python_2"
                                              "Pandas")))
            (add-hook 'python-mode-hook 'dash-load-py)

            (defun dash-load-js ()
              (interactive)
              (setq-local helm-dash-docsets '("JavaScript"
                                              "NodeJS"
                                              "AngularJS")))
            (add-hook 'js2-mode-hook 'dash-load-js)

            (defun dash-load-r ()
              (interactive)
              (setq-local helm-dash-docsets '("R")))
            (add-hook 'ess-mode-hook 'dash-load-r)

            (defun dash-load-bash ()
              (interactive)
              (setq-local helm-dash-docsets '("Bash")))
            (add-hook 'shell-mode-hook 'dash-load-bash)

            (defun dash-load-md ()
              (interactive)
              (setq-local helm-dash-docsets '("Markdown")))
            (add-hook 'markdown-mode-hook 'dash-load-md)

            ))

;; helm company
(use-package helm-company
  :defer t
  :bind (:map ctl-x-map
              (":" . helm-company)
              :map company-mode-map
              ("C-:" . helm-company))
  :after (helm company)
  :load-path (lambda () (expand-file-name "helm-company/" user-emacs-directory))
  :commands (helm-company))

;; helm pages
;; Text is divided into pages delimited by the formfeed character (ASCII code 12, also denoted as ‘control-L’)
(use-package helm-pages
  :after helm
  :load-path (lambda () (expand-file-name "helm-pages/" user-emacs-directory)))

;; helm integration with magit
;; https://github.com/tarao/dotfiles/blob/master/.emacs.d/init/helm-magit.el

(when (featurep 'magit)
  (progn
    ;; https://gist.github.com/mrkuc/f76008ea9145e525f43114b27e4027aa
    (defun magit-mode-setup-return-buffer (mode &rest args)
      "Setup up a MODE buffer using ARGS to generate its content."
      (let ((buffer (magit-mode-get-buffer mode t))
            (section (magit-current-section)))
        (with-current-buffer buffer
          (setq magit-previous-section section)
          (setq magit-refresh-args args)
          (funcall mode))
        (magit-display-buffer buffer)
        (with-current-buffer buffer
          (run-hooks 'magit-mode-setup-hook)
          (magit-refresh-buffer))
        buffer))

    (defun helm-magit-log (revs args files)
      (magit-mode-setup-return-buffer #'magit-log-mode revs args files))

    (defun helm-magit-log-all-candidate-buffer ()
      (interactive)
      (helm-magit-log (if (magit-get-current-branch)
                          (list "--all")
                        (list "HEAD" "--all"))
                      nil nil))

    (setq helm-magit-log-all--source
          (helm-build-in-buffer-source "magit-log-all"
            ;;:data #'helm-magit-log-all-candidate-buffer
            :init (lambda ()
                    (helm-init-candidates-in-buffer 'global
                      (with-current-buffer (helm-magit-log-all-candidate-buffer)
                        (buffer-string))))
            :action (helm-make-actions
                     "Copy" (lambda (line) (kill-new line)))))

    (defun helm-magit:log-all ()
      (interactive)
      (helm
       :sources '(helm-magit-log-all--source)
       :buffer "*helm-magit-log-all*"))

    (define-derived-mode magit-for-each-ref-mode magit-log-mode "Magit Refs"
      "Mode for showing the commit log for each ref."
      :group 'magit-log
      (hack-dir-local-variables-non-file-buffer))

    (define-derived-mode magit-for-each-local-branche-mode magit-for-each-ref-mode
      "Magit Local Branches"
      "Mode for showing the latest commit of local branches."
      :group 'magit-log
      (hack-dir-local-variables-non-file-buffer))

    (define-derived-mode magit-for-each-remote-branche-mode magit-for-each-ref-mode
      "Magit Remote Branches"
      "Mode for showing the latest commit of remote branches."
      :group 'magit-log
      (hack-dir-local-variables-non-file-buffer))

    (define-derived-mode magit-for-each-tag-mode magit-for-each-ref-mode
      "Magit Tags"
      "Mode for showing the latest commit of tags."
      :group 'magit-log
      (hack-dir-local-variables-non-file-buffer))

    (defun magit-for-each-ref-refresh-buffer (refs)
      (eval-and-compile (require 'magit-log))
      (magit-insert-section (logbuf)
        (magit-git-wash (apply-partially #'magit-log-wash-log 'log)
          "for-each-ref"
          "--sort=-authordate"
          "--format=%(objectname:short) (%(refname), %(upstream)) [%(authorname)][%(authordate)]%(contents:subject)"
          refs)))

    (defun magit-for-each-local-branche-refresh-buffer ()
      (magit-for-each-ref-refresh-buffer "refs/heads"))

    (defun magit-for-each-remote-branche-refresh-buffer ()
      (magit-for-each-ref-refresh-buffer "refs/remotes"))

    (defun magit-for-each-tag-refresh-buffer ()
      (magit-for-each-ref-refresh-buffer "refs/tags"))

    (defun magit-refs-local-branches ()
      (interactive)
      (magit-mode-setup #'magit-for-each-local-branche-mode))

    (defun magit-refs-remote-branches ()
      (interactive)
      (magit-mode-setup #'magit-for-each-remote-branche-mode))

    (defun magit-refs-tags ()
      (interactive)
      (magit-mode-setup #'magit-for-each-tag-mode))

    (defmacro helm-magit:init (&rest body)
      `(progn
         (eval-and-compile (require 'magit-log))
         (let ((buffer (progn ,@body
                              (prog1 (current-buffer)
                                (magit-log-bury-buffer 1)))))
           (helm-candidate-buffer buffer))))

    (defun helm-magit:local-branches-init ()
      (helm-magit:init
       (magit-refs-local-branches)))

    (defun helm-magit:remote-branches-init ()
      (helm-magit:init
       (magit-refs-remote-branches)))

    (defun helm-magit:tags-init ()
      (helm-magit:init
       (magit-refs-tags)))

    (defun helm-magit:head-commits-init ()
      (helm-magit:init
       (magit-log-head (remove "--graph" magit-log-arguments))))

    (defun helm-magit:log-transform-candidates (candidates _source)
      (eval-and-compile (require 'magit-section))
      (loop for c in candidates
            unless (string= c "(empty)")
            collect (let* ((section (get-text-property 0 'magit-section c))
                           (commit (magit-section-value section)))
                      (propertize c 'helm-realvalue commit))))

    (defun helm-magit:commit-shortname (commit)
      (eval-and-compile (require 'magit-git))
      (or (magit-name-local-branch commit)
          (magit-name-remote-branch commit)
          (magit-name-tag commit)
          (magit-get-shortname commit)
          commit))

    (defclass helm-magit:source-log (helm-source-in-buffer)
      ((get-line
        :initform #'buffer-substring)
       (fuzzy-match
        :initform t)
       (filtered-candidate-transformer
        :initform #'helm-magit:log-transform-candidates)
       (action
        :initform #'magit-show-commit)
       (persistent-action
        :initform #'magit-show-commit)))

    (defclass helm-magit:source-local-branches (helm-magit:source-log)
      ((init
        :initform #'helm-magit:local-branches-init)))

    (defclass helm-magit:source-remote-branches (helm-magit:source-log)
      ((init
        :initform #'helm-magit:remote-branches-init)))

    (defclass helm-magit:source-tags (helm-magit:source-log)
      ((init
        :initform #'helm-magit:tags-init)))

    (defclass helm-magit:source-head-commits (helm-magit:source-log)
      ((init
        :initform #'helm-magit:head-commits-init)))

    (defun helm-magit:revs (buffer action)
      (helm :sources
            (list (helm-make-source "Local branches"
                      'helm-magit:source-local-branches
                    :action action)
                  (helm-make-source "Remote branches"
                      'helm-magit:source-remote-branches
                    :action action)
                  (helm-make-source "Tags"
                      'helm-magit:source-tags
                    :action action)
                  (helm-make-source "Commits in HEAD"
                      'helm-magit:source-head-commits
                    :action action))
            :buffer "*helm magit diff*"))

    (defun helm-magit:checkout ()
      (interactive)
      (helm-magit:revs
       "*helm magit checkout*"
       #'(lambda (commit)
           (magit-checkout (helm-magit:commit-shortname commit)))))

    (defun helm-magit:diff ()
      (interactive)
      (helm-magit:revs
       "*helm magit diff*"
       #'(lambda (commit)
           (magit-diff (helm-magit:commit-shortname commit)))))))

(provide 'setup-helm-plugins)
;;; setup-helm-plugins.el ends here

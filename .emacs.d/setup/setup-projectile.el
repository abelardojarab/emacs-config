;;; setup-projectile.el ---                          -*- lexical-binding: t; -*-

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

(use-package projectile
  :demand t
  :diminish projectile-mode
  :defines (projectile-ignored-projects
            projectile-enable-caching)
  :commands (projectile-global-mode
             projectile-compile-project
             projectile-find-file
             projectile-project-root
             projectile-mode)
  :init (setq projectile-known-projects-file
              (concat (file-name-as-directory
                       my/emacs-cache-dir) "projectile-bookmarks.eld"))
  :hook (on-first-buffer . projectile-mode)
  :custom ((projectile-mode-line-prefix "")
           (projectile-sort-order       'recentf)
           (projectile-use-git-grep     t))
  :bind (("C-x C-m" . projectile-compile-project)
         ("C-x C-g" . projectile-find-file))
  :config (progn
            (setq projectile-mode-line
                  '(:eval (format " Projectile[%s]"
                                  (projectile-project-name))))

            (add-to-list 'projectile-project-root-files "configure.ac")
            (add-to-list 'projectile-project-root-files ".clang_complete")
            (add-to-list 'projectile-project-root-files ".clang_complete.in")
            (add-to-list 'projectile-project-root-files "AndroidManifest.xml")

            ;; Use the faster searcher to handle project files: ripgrep `rg'.
            (when (and (not (executable-find "fd"))
                       (executable-find "rg"))
              (setq projectile-generic-command
                    (let ((rg-cmd ""))
                      (dolist (dir projectile-globally-ignored-directories)
                        (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
                      (concat "rg -0 --files --color=never --hidden" rg-cmd))))

            ;; Support Perforce project
            (let ((val (or (getenv "P4CONFIG") ".p4config")))
              (add-to-list 'projectile-project-root-files-bottom-up val))

            ;; Jujutsu.  projectile already lists ".jj" and already maps it to
            ;; the `jj' VCS, so root detection needs nothing from us -- but the
            ;; command it runs is stale.  projectile 2.9 ships
            ;; "jj files --no-pager .", and `files' is not a subcommand of
            ;; current jj; it answers
            ;;     error: unrecognized subcommand 'files'
            ;;     tip: a similar subcommand exists: 'file'
            ;; which is what surfaces as "unrecognized subcommand files" from
            ;; helm-projectile.  It is `jj file list' now.
            ;;
            ;; --ignore-working-copy is the second half of the fix: listing
            ;; files has no business snapshotting the working copy, which is
            ;; slow and, in a repo holding large files, prints kilobytes of
            ;; "Refused to snapshot some files" onto stderr.
            (setq projectile-jj-command
                  "jj --no-pager --ignore-working-copy file list . | tr '\\n' '\\0'")

            ;; CitC workspaces need special handling, and getting it wrong
            ;; locks Emacs up hard.
            ;;
            ;; /google/src/cloud is a lazily fetched mount whose jj repo tracks
            ;; all of google3.  Measured: `jj file list' at the root of one had
            ;; not finished after 100 seconds, and `rg --files' walks the tree
            ;; pulling gigabytes on demand.  Either one hangs Emacs inside a
            ;; synchronous call with no way out but killing the child process.
            ;;
            ;; Scoping to the directory you are in makes the common case both
            ;; safe and useful -- one autoscaler subtree lists in 0.7s.  But
            ;; `projectile-switch-project' drops you at the *root*, which is
            ;; precisely the case scoping cannot help, so the root is refused
            ;; outright rather than scoped.  Every listing is also wrapped in
            ;; `timeout' and `head' as a backstop: no command from here can
            ;; run unbounded, whatever the directory turns out to contain.
            (defun my/citc-path-p (path)
              "Non-nil if PATH is inside a CitC mount.
Resolves symlinks: ~/jj_workspaces/foo points into /google/src/cloud,
and comparing the unresolved name would miss every workspace reached
through that directory."
              (and path (string-prefix-p "/google/src/" (file-truename path))))

            (defcustom my/projectile-citc-timeout 15
              "Seconds a CitC file listing may run before it is cut off."
              :type 'integer :group 'projectile)

            (defcustom my/projectile-citc-max-files 40000
              "Most files a CitC listing may return."
              :type 'integer :group 'projectile)

            (defun my/projectile-scope-citc (orig vcs)
              "Scope, bound, or refuse file listing inside a CitC workspace.
ORIG is `projectile-get-ext-command', VCS the detected backend."
              (let ((root (ignore-errors (projectile-project-root))))
                (if (not (and root (my/citc-path-p root) (eq vcs 'jj)))
                    (funcall orig vcs)
                  ;; Both sides through `file-truename', or the symlinked
                  ;; ~/jj_workspaces name never matches the /google/src root
                  ;; and every listing falls back to the whole repo.
                  (let* ((true-root (file-truename root))
                         (here (file-truename
                                (or (and buffer-file-name
                                         (file-name-directory buffer-file-name))
                                    default-directory)))
                         (under (string-prefix-p true-root here))
                         (rel (and under (file-relative-name here true-root))))
                    (if (or (not under) (member rel '("." "./")))
                        (progn
                          (message
                           "projectile: at the CitC root -- not listing all of google3. Open a subdirectory first, or use Code Search.")
                          ;; A command, not nil: projectile will run whatever it
                          ;; gets, and `true' exits 0 with no output.
                          "true")
                      (message "projectile: CitC, listing only %s" rel)
                      (format "timeout %d jj --no-pager --ignore-working-copy file list %s | head -n %d | tr '\\n' '\\0'"
                              my/projectile-citc-timeout
                              (shell-quote-argument rel)
                              my/projectile-citc-max-files))))))
            (advice-add 'projectile-get-ext-command :around
                        #'my/projectile-scope-citc)

            ;; Switching to a CitC project must not immediately try to index
            ;; it.  Land in dired at the root instead; navigate down and
            ;; find-file works scoped from there.
            (defun my/projectile-switch-action ()
              "Dired for CitC projects, the usual find-file elsewhere."
              (if (my/citc-path-p (projectile-project-root))
                  (projectile-dired)
                (projectile-find-file)))
            (setq projectile-switch-project-action #'my/projectile-switch-action)

            ;; ...except `setup-helm' loads after this file and overwrites
            ;; `projectile-switch-project-action' with `helm-projectile', so
            ;; the setq above only applies when helm is absent.  Rather than
            ;; fight over the variable, teach helm-projectile the same rule --
            ;; switch-project is exactly the path that lands you at the root
            ;; and hangs.
            (with-eval-after-load 'helm-projectile
              (defun my/helm-projectile-citc-guard (orig &rest args)
                "Open dired instead of a file list at a CitC project root."
                (let ((root (ignore-errors (projectile-project-root))))
                  (if (and root
                           (my/citc-path-p root)
                           (string= (file-truename default-directory)
                                    (file-truename root)))
                      (progn
                        (message "projectile: CitC root -- opening dired, not a file list")
                        (projectile-dired))
                    (apply orig args))))
              (advice-add 'helm-projectile :around
                          #'my/helm-projectile-citc-guard))

            ;; git-grep is useless in a jj-on-Piper checkout, and in a CitC
            ;; tree it is actively harmful for the reason above.
            (setq projectile-use-git-grep nil)

            (setq projectile-known-projects-file (concat (file-name-as-directory
                                                          my/emacs-cache-dir)
                                                         "projectile-bookmarks.eld")
                  projectile-cache-file          (concat (file-name-as-directory
                                                          my/emacs-cache-dir)
                                                         "projectile.cache")
                  projectile-enable-caching      t
                  projectile-sort-order          'recently-active
                  projectile-indexing-method     'alien
                  projectile-globally-ignored-file-suffixes '("#" "~" ".swp" ".o" ".so" ".exe"
                                                              ".dll" ".elc" ".pyc" ".jar" ".class")
                  projectile-globally-ignored-files '("TAGS" "*.log" "*DS_Store")
                  projectile-globally-ignored-directories '("node_modules"
                                                            "build" ".cache" ".vscode" ".idea" "contrib" "__pycache__"))
            (projectile-global-mode)))

;; Integration with ripgrep
(use-package projectile-ripgrep
  :disabled t
  :if (executable-find "rg")
  :after projectile
  :commands projectile-ripgrep)

(provide 'setup-projectile)
;;; setup-projectile.el ends here

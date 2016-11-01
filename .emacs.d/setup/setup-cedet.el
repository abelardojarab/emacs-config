;;; setup-cedet.el ---

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

;; Assure .emacs.cache/semanticdb directory exists
(if (not (file-exists-p "~/.emacs.cache/semanticdb"))
    (make-directory "~/.emacs.cache/semanticdb") t)

;; Enable Semantic
(add-to-list 'load-path (expand-file-name "cedet/lisp/cedet/" user-emacs-directory))
(semantic-load-enable-minimum-features)

;; To use additional features for names completion, and displaying of information for tags & classes,
;; you also need to load the semantic-ia package. Unfortunately, semantic makes Emacs slow
(require 'semantic/ia)

;; Enable support for parsing additional languages
(require 'semantic/wisent)

;; semantic support for clang
(if (executable-find "clang")
    (require 'semantic/bovine/clang))

;; Enable case-insensitive searching
(set-default 'semantic-case-fold t)

;; Faster parsing
(setq semantic-idle-work-parse-neighboring-files-flag nil)
(setq semantic-idle-work-update-headers-flag nil)
(setq semantic-idle-scheduler-idle-time 60)
(setq semantic-idle-scheduler-work-idle-time 1800) ;; default is 60
(setq semantic-idle-scheduler-max-buffer-size 1)

;; Disable Semantics for large files
(add-hook 'semantic--before-fetch-tags-hook
          (lambda () (if (and (> (point-max) 500)
                         (not (semantic-parse-tree-needs-rebuild-p)))
                    nil
                  t)))

;; etags support
(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))

;; Fixing a bug in semantic, see #22287
(defun semanticdb-save-all-db-idle ()
  "Save all semantic tag databases from idle time.
Exit the save between databases if there is user input."

  ;; save-mark-and-excursion is defined in Emacs 25.1-forward
  (if (fboundp 'save-mark-and-excursion)
      (semantic-safe "Auto-DB Save: %S"
        ;; FIXME: Use `while-no-input'?
        (save-mark-and-excursion ;; <-- added line
         (semantic-exit-on-input 'semanticdb-idle-save
           (mapc (lambda (db)
                   (semantic-throw-on-input 'semanticdb-idle-save)
                   (semanticdb-save-db db t))
                 semanticdb-database-list))))
    (if (fboundp 'save-excursion)
        (save-excursion ;; <-- added line
          (semantic-exit-on-input 'semanticdb-idle-save
            (mapc (lambda (db)
                    (semantic-throw-on-input 'semanticdb-idle-save)
                    (semanticdb-save-db db t))
                  semanticdb-database-list))))))

;; Enable semanticdb, slows down Emacs
;; (require 'semantic/db)
(global-semanticdb-minor-mode nil)

;; This prevents Emacs to become uresponsive
(defun semanticdb-kill-hook ()
  nil)
(defun semanticdb-create-table-for-file-not-in-buffer (arg)
  nil)

;; Default semanticdb directory
(setq-default semanticdb-default-system-save-directory
              (setq-default semanticdb-default-save-directory "~/.emacs.cache/semanticdb"))

;; semanticdb support for global/gtags
(when (executable-find "global")
  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t))

;; Load contrib library
(require 'eassist)

;; EDE project managment, slows down Emacs
(global-ede-mode 1)
(ede-enable-generic-projects)
(setq ede-project-directories t)

;; Default EDE directory
(setq-default ede-project-placeholder-cache-file "~/.emacs.cache/ede-projects.el")

;; Enable the wrapper for compilation database projects
(use-package ede-compdb
  :load-path (lambda () (expand-file-name "ede-compdb/" user-emacs-directory))
  :config (progn
            (defvar my/cmake-build-directories
              '(("cmake_build_dir" . "build")
                ("Debug" . "build.dbg")
                ("Release" . "build.rel")
                ("RelWithDebInfo" . "build.r+d")))

            (defun my/load-cmake-project (dir)
              "Creates a project for the given directory sourced at dir"
              (let ((default-directory dir)
                    (config-and-dir (car (cl-member-if (lambda (c)
                                                         (file-readable-p
                                                          (expand-file-name "compile_commands.json" (concat dir (cdr c)))))
                                                       my/cmake-build-directories))))
                (unless config-and-dir
                  (error "Couldn't determine build directory for project at %s" dir))
                (ede-add-project-to-global-list
                 (ede-compdb-project
                  (file-name-nondirectory (directory-file-name dir))
                  :file (expand-file-name "CMakeLists.txt" dir)
                  :compdb-file (expand-file-name "compile_commands.json" (cdr config-and-dir))
                  :configuration-default (car config-and-dir)
                  :configuration-directories (mapcar #'cdr my/cmake-build-directories)
                  :configurations (mapcar #'car my/cmake-build-directories)
                  :build-command "cmake --build .."))))

            (ede-add-project-autoload
             (ede-project-autoload "generic-cmake"
                                   :name "generic-cmake"
                                   :file 'ede-compdb
                                   :proj-file "CMakeLists.txt"
                                   :proj-root 'cmake-project-find-root-directory
                                   :load-type 'my/load-cmake-project
                                   :class-sym 'ede-compdb-project))))

;; Extensions to Emacs Development Environment for use with CMake-based projects
(use-package ede-cmake
  :load-path (lambda () (expand-file-name "ede-cmake/" user-emacs-directory)))

;; Enable which-function-mode for selected major modes
(setq which-func-modes '(org-mode markdown-mode
                                  ecmascript-mode emacs-lisp-mode lisp-mode java-mode
                                  c-mode c++-mode makefile-mode sh-mode))

;; which-function-mode
(which-func-mode t)
(mapc (lambda (mode)
        (add-hook mode (lambda () (which-function-mode t))))
      '(prog-mode-hook
        org-mode-hook))

(provide 'setup-cedet)
;;; setup-cedet.el ends here

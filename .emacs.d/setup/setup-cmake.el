;;; setup-cmake.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019  Abelardo Jara-Berrocal

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

;; Irony server
(use-package irony
  :defer t
  :commands (irony-mode
             irony-install-server)
  :if (and (executable-find "irony-server")
           (not (executable-find "clangd")))
  :diminish irony-mode
  :load-path (lambda () (expand-file-name "irony-mode/" user-emacs-directory))
  :hook (((c++-mode c-mode objc-mode) . irony-mode))
  :config (progn
            (if (file-exists-p "/usr/local/bin/irony-server")
                (setq irony-server-install-prefix "/usr/local/"))
            (if (file-exists-p (concat (file-name-as-directory
                                        my/emacs-cache-dir)
                                       "irony-server/bin/irony-server"))
                (setq irony-server-install-prefix (concat (file-name-as-directory
                                                           my/emacs-cache-dir)
                                                          "irony-server/")))

            (push "-std=c++11" irony-additional-clang-options)

            (if (not (file-exists-p (concat (file-name-as-directory
                                             my/emacs-cache-dir)
                                            "irony-user-dir")))
                (make-directory (concat (file-name-as-directory
                                         my/emacs-cache-dir)
                                        "irony-user-dir") t))
            (setq irony-user-dir (concat (file-name-as-directory
                                          my/emacs-cache-dir)
                                         "irony-user-dir/"))))

;; Irony json projects
(use-package irony-cdb-json
  :defer t
  :if (not (executable-find "clangd"))
  :commands (irony-cdb-json-add-compile-commands-path
             irony-cdb-autosetup-compile-options)
  :hook ((irony-mode . irony-cdb-autosetup-compile-options))
  :load-path (lambda () (expand-file-name "irony-mode/" user-emacs-directory))
  :init (progn
          (setq irony-user-dir (concat (file-name-as-directory
                                        my/emacs-cache-dir)
                                       "irony-user-dir/"))
          (setq irony-cdb-json--project-alist-file
                (concat irony-user-dir "cdb-json-projects")))
  :config (progn
            (setq irony-user-dir (concat (file-name-as-directory
                                          my/emacs-cache-dir)
                                         "irony-user-dir/"))
            (setq irony-cdb-json--project-alist-file
                  (concat irony-user-dir "cdb-json-projects"))))

;; rtags
;; (LIBCLANG_LLVM_CONFIG_EXECUTABLE=path_to_llvm-config CC=gcc CXX=g++ cmake ../rtags -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=where_to_install_rtags)
(use-package rtags
  :defer t
  :commands (rtags-print-symbol-info
             rtags-find-symbol-at-point
             rtags-start-process-unless-running
             rtags-eldoc-function
             my/c-mode-init-rtags)
  :if (executable-find "rdm")
  :load-path (lambda () (expand-file-name "rtags/src/" user-emacs-directory))
  :bind (:map c++-mode-map
              ("C-c C-i" . rtags-print-symbol-info)
              ("C-c C-s" . rtags-find-symbol-at-point))
  :custom ((rtags-autostart-diagnostics      t)
           (rtags-completions-enabled        t)
           (rtags-display-summary-as-tooltip t)
           (rtags-tooltips-enabled           t)
           (rtags-use-helm                   t)
           (rtags-display-result-backend     'helm)
           (rtags-periodic-reparse-timeout   10)
           (rtags-timeout                    2))
  :hook (kill-emacs . rtags-quit-rdm)
  :config (progn
            (use-package helm-rtags
              :load-path (lambda () (expand-file-name "rtags/src/" user-emacs-directory)))
            (rtags-enable-standard-keybindings)
            (rtags-set-periodic-reparse-timeout 10)

            ;; Integrating rtags with eldoc
            (defun fontify-string (str mode)
              "Return STR fontified according to MODE."
              (with-temp-buffer
                (insert str)
                (delay-mode-hooks (funcall mode))
                (font-lock-default-function mode)
                (font-lock-default-fontify-region
                 (point-min) (point-max) nil)
                (buffer-string)))

            (defun rtags-eldoc-function ()
              (let ((summary (rtags-get-summary-text)))
                (and summary
                     (fontify-string
                      (replace-regexp-in-string
                       "{[^}]*$" ""
                       (mapconcat
                        (lambda (str) (if (= 0 (length str)) "//" (string-trim str)))
                        (split-string summary "\r?\n")
                        " "))
                      major-mode))))

            ;; Enable integration of rtags and eldoc
            (defun my/c-mode-init-rtags ()
              (interactive)
              (when (and (executable-find "rdm")
                         (not (executable-find "clangd"))
                         (projectile-project-p)
                         (not (file-exists-p (concat (projectile-project-root)
                                                     "GTAGS"))))
                  (rtags-start-process-unless-running)
                  (setq-local eldoc-documentation-function #'rtags-eldoc-function)
                  (eldoc-mode t)))))

;; cmake syntax highlighting
(use-package cmake-mode
  :defer t
  :commands cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"          . cmake-mode)))

;; cmake-based IDE
(use-package cmake-ide
  :if (not (equal system-type 'windows-nt))
  :defer t
  :commands (my/cmake-ide-init
             cmake-ide-compile
             cmake-ide-setup
             cmake-ide-run-cmake
             cmake-ide--locate-cmakelists)
  :if (executable-find "cmake")
  :hook (c-mode-common . my/cmake-ide-init)
  :init (if (not (file-exists-p "~/cmake_builds"))
            (make-directory "~/cmake_builds"))
  :config (progn

            ;; Backup
            (use-package cmake-ide-backup
              :demand t)

            ;; Integrate cmake with Emacs standard build system
            (use-package cmake-project
              :defer t
              :commands cmake-project-mode
              :config (progn
                        (defun cmake-project-find-root-directory ()
                          "Find the top-level CMake directory."
                          (interactive)
                          (message "* cmake-project Source code directory set to: %s"
                                   (file-name-directory
                                    (cmake-ide--locate-cmakelists)))
                          (file-name-directory
                           (cmake-ide--locate-cmakelists)))

                        (defun cmake-project-find-build-directory ()
                          "Find location where project will be built."
                          (interactive)
                          (message "* cmake-project Building directory set to: %s" cmake-ide-build-dir)
                          (file-name-as-directory cmake-ide-build-dir))

                        (defun cmake-project-configure-project (build-directory generator &optional flags)
                          "Configure or reconfigure a CMake build tree."
                          (interactive
                           (let ((directory-parts
                                  (when cmake-project-build-directory (cmake-project--split-directory-path
                                                                       cmake-project-build-directory))))
                             (let ((root (car directory-parts))
                                   (directory-name (cdr directory-parts)))
                               (list (read-directory-name
                                      "Configure in directory: " root nil nil directory-name)
                                     (completing-read
                                      "Generator (optional): "
                                      (cmake-project--available-generators) nil t)
                                     (if current-prefix-arg
                                         (read-from-minibuffer "Additional CMake flags (optional): "))))))
                          (let ((source-directory (cmake-project-find-root-directory))
                                (build-directory (file-name-as-directory build-directory)))
                            (unless (file-exists-p build-directory) (make-directory build-directory))
                            (let ((default-directory build-directory))

                              ;; Update the cmake-ide directories
                              (make-local-variable 'cmake-ide-build-dir)
                              (make-local-variable 'cmake-dir)
                              (setq cmake-ide-build-dir build-directory)
                              (setq cmake-dir build-directory)

                              (compilation-start
                               (concat
                                "cd . && "
                                "cd " (shell-quote-argument (expand-file-name build-directory))
                                " && cmake "
                                (unless (string= "" flags) (concat flags " "))
                                (shell-quote-argument
                                 (expand-file-name source-directory))
                                " -DCMAKE_EXPORT_COMPILE_COMMANDS=ON "
                                cmake-ide-cmake-opts
                                " "
                                (if (string= "" generator)
                                    ""
                                  ;; Set the user defined architecture on windows.
                                  (concat " -G " (shell-quote-argument (cmake-project-set-architecture generator)))
                                  )))
                              (cmake-project--changed-build-directory build-directory))))))

            ;; Overwrite get build dir function
            (defun cmake-ide--get-build-dir ()
              "Return the directory name to run CMake in."
              (interactive)
              (let ((build-dir
                     (expand-file-name (or (cmake-ide--build-dir-var)
                                           (cmake-ide--get-build-dir-from-hash))
                                       (cmake-ide--locate-project-dir))))
                (when (not (file-accessible-directory-p build-dir))
                  (cmake-ide--message "Making directory %s" build-dir)
                  (make-directory build-dir))
                (cmake-ide--message "cmake build dir is set to %s" build-dir)
                (file-name-as-directory build-dir)))

            ;; Overwrite run cmake function
            (defun cmake-ide--run-cmake-impl (project-dir cmake-dir)
              "Run the CMake process for PROJECT-DIR in CMAKE-DIR."
              (when project-dir
                (let ((default-directory cmake-dir))
                  (cmake-ide--message "Running cmake for src path %s in build path %s" project-dir cmake-ide-build-dir)
                  (apply 'start-process (append (list "cmake" "*cmake*" cmake-ide-cmake-command)
                                                (split-string cmake-ide-cmake-opts)
                                                (list "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" project-dir))))))

            ;; Initialize cmake-ide
            (defun my/cmake-ide-init ()
              "Initialize cmake-ide"
              (interactive)
              (progn
                (my/cmake-ide-set-build-dir)
                (when (and (cmake-ide--locate-cmakelists)
                           (file-exists-p (cmake-ide--locate-cmakelists)))
                  (cmake-ide-maybe-run-cmake)
                  (when (and (executable-find "rdm")
                             (not (executable-find "clangd")))
                    (cmake-ide-maybe-start-rdm)
                    (my/c-mode-init-rtags))
                  (cmake-project-mode 1)
                  (eldoc-mode)
                  (turn-on-eldoc-mode)
                  (if (executable-find "clangd")
                      (lsp)))))

            ;; Define cmake-ide-build-dir under ~/cmake_builds
            (defun my/cmake-ide-set-build-dir ()
              "Modify cmake-build-dir and make it point to ~/cmake_builds"
              (interactive)
              (let (my/cmake-build-dir my/projectile-build-dir)
                (if (cmake-ide--locate-cmakelists)
                    (progn
                      (if (and (file-exists-p "~/cmake_builds")
                               (projectile-project-name))
                          (progn
                            (make-local-variable 'cmake-project-build-directory)
                            (make-local-variable 'cmake-dir)
                            (make-local-variable 'cmake-ide-build-dir)
                            (make-local-variable 'cmake-ide-cmake-opts)
                            (make-local-variable 'cmake-ide-cmake-opts)

                            (make-local-variable 'projectile-project-compilation-dir)
                            (make-local-variable 'projectile-project-compilation-cmd)

                            ;; Define project build directory
                            (setq my/projectile-build-dir (concat
                                                           "~/cmake_builds/"
                                                           (projectile-project-name)))

                            ;; Create project build directory under ~/cmake_builds
                            (if (not (file-exists-p my/projectile-build-dir))
                                (make-directory my/projectile-build-dir))

                            ;; Define cmake build directory
                            (setq my/cmake-build-dir (expand-file-name
                                                      (file-name-nondirectory
                                                       (directory-file-name
                                                        (expand-file-name
                                                         (file-name-directory
                                                          (cmake-ide--locate-cmakelists)))))
                                                      my/projectile-build-dir))

                            ;; Avoid path repetitions
                            (if (equal (file-name-nondirectory
                                        (directory-file-name
                                         (expand-file-name
                                          (file-name-directory
                                           (cmake-ide--locate-cmakelists)))))
                                       (projectile-project-name))
                                (expand-file-name (setq my/cmake-build-dir (concat
                                                                            "~/cmake_builds/"
                                                                            (projectile-project-name)))))

                            ;; Assure we are not using the last CMakeLists.txt
                            (message "* cmake-ide Building directory set to: %s" my/cmake-build-dir)

                            ;; Create cmake project directory under project directory
                            (if (not (file-exists-p my/cmake-build-dir))
                                (make-directory my/cmake-build-dir))

                            ;; Use Debug as default build
                            (setq cmake-ide-cmake-opts "-DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON")

                            ;; Set cmake-ide-build-dir according to both top project and cmake project names
                            (setq cmake-project-build-directory my/cmake-build-dir)
                            (setq cmake-ide-build-dir my/cmake-build-dir)
                            (setq cmake-dir my/cmake-build-dir)
                            (setq projectile-project-compilation-dir my/cmake-build-dir)
                            (setq projectile-project-compilation-cmd "make -j")))))))))

(provide 'setup-cmake)
;;; setup-cmake.el ends here

;;; setup-cmake.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017  Abelardo Jara-Berrocal

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

;; rtags
;; sudo apt-get install libclang-dev / brew install llvm --with-clang
;; git clone --recursive https://github.com/Andersbakken/rtags.git &
;; (LIBCLANG_LLVM_CONFIG_EXECUTABLE=path_to_llvm-config CC=gcc CXX=g++ cmake ../rtags -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=where_to_install_rtags)
;; cmake --build ./ --target install
(use-package rtags
  :defer t
  :commands (rtags-print-symbol-info
             rtags-find-symbol-at-point
             rtags-start-process-unless-running)
  :if (executable-find "rdm")
  :load-path (lambda () (expand-file-name "rtags/src/" user-emacs-directory))
  :bind (:map c++-mode-map
              ("C-c I" . rtags-print-symbol-info)
              ("C-c S" . rtags-find-symbol-at-point))
  :init (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
  :config (progn
            (use-package rtags-helm)
            (setq rtags-autostart-diagnostics t
                  rtags-completions-enabled t
                  rtags-use-helm t)
            (rtags-enable-standard-keybindings)))

;; cmake syntax highlighting
(use-package cmake-mode
  :commands cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :load-path (lambda () (expand-file-name "cmake-mode/" user-emacs-directory)))

;; cmake-based IDE
(use-package cmake-ide
  :defer t
  :commands (use-cmake-ide cmake-ide--locate-cmakelists)
  :if (executable-find "cmake")
  :load-path (lambda () (expand-file-name "cmake-ide/" user-emacs-directory))
  :init (if (executable-find "cmake")
            (add-hook 'c-mode-common-hook #'use-cmake-ide))
  :config (progn

            ;; Asure cmake_build directory exists
            (if (not (file-exists-p "~/cmake_builds"))
                (make-directory "~/cmake_builds"))

            ;; Define cmake-ide-build-dir under ~/cmake_builds
            (defun use-cmake-ide ()
              (let (my/cmake-build-dir my/projectile-build-dir)
                (cmake-ide-setup)
                (if (cmake-ide--locate-cmakelists)
                    (progn
                      (if (and (file-exists-p "~/cmake_builds")
                               (projectile-project-name))
                          (progn

                            ;; Define project build directory
                            (setq my/projectile-build-dir (concat
                                                        "~/cmake_builds/"
                                                        (projectile-project-name)))

                            ;; Create project build directory under ~/cmake_builds
                            (if (not (file-exists-p my/projectile-build-dir))
                                (make-directory my/projectile-build-dir))

                            ;; Define cmake build directory
                            (setq my/cmake-build-dir (concat
                                                      my/projectile-build-dir
                                                      "/"
                                                      (file-relative-name (cmake-ide--locate-cmakelists)
                                                                          (projectile-project-root))))

                            ;; Create cmake project directory under project directory
                            (if (not (file-exists-p my/cmake-build-dir))
                                (make-directory my/cmake-build-dir))

                            ;; Set cmake-ide-build-dir according to both top project and cmake project names
                            (setq-default cmake-ide-build-dir my/cmake-build-dir)))))))))

;; minor-mode integrating the CMake build process
(use-package cmake-project
  :defer t
  :commands cmake-project-mode
  :if (executable-find "cmake")
  :load-path (lambda () (expand-file-name "cmake-project/" user-emacs-directory))
  :diminish cmake-project-mode
  :init (progn
          (defun cmake-project-hook ()
            (when (cmake-ide--locate-cmakelists)
              (if (not (file-exists-p cmake-project-default-build-dir-name))
                  (make-directory cmake-project-default-build-dir-name) t)
              (cmake-project-mode)))
          (add-hook 'c-common-mode-hook 'cmake-project-hook))
  :config (progn
            (if (cmake-ide--locate-cmakelists)
                (setq-default cmake-project-build-directory (concat
                                                             (cmake-project-find-root-directory)
                                                             "cmake_build_dir/"))
              (setq cmake-project-default-build-dir-name "cmake_build_dir/"))))

;; EDE project managment, slows down Emacs
(use-package ede
  :disabled t
  :config (progn
            (global-ede-mode 1)
            (ede-enable-generic-projects)
            (setq ede-project-directories t)

            ;; Default EDE directory
            (setq-default ede-project-placeholder-cache-file "~/.emacs.cache/ede-projects.el")

            ;; Redefine ede add projects to avoid errors
            (defun ede-add-project-to-global-list (proj)
              "Add the project PROJ to the master list of projects.
On success, return the added project."
              (ignore-errors
                (when (not proj)
                  (error "No project created to add to master list"))
                (when (not (eieio-object-p proj))
                  (error "Attempt to add non-object to master project list"))
                (when (not (obj-of-class-p proj ede-project-placeholder))
                  (error "Attempt to add a non-project to the ede projects list"))
                (if (stringp proj)
                    (add-to-list 'ede-projects proj)))
              proj)))

(provide 'setup-cmake)
;;; setup-cmake.el ends here

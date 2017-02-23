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
  :commands (my/cmake-enable-ide cmake-ide--locate-cmakelists cmake-ide-compile cmake-ide-run-cmake)
  :if (executable-find "cmake")
  :load-path (lambda () (expand-file-name "cmake-ide/" user-emacs-directory))
  :init (add-hook 'c-mode-common-hook 'my/cmake-enable-ide)
  :config (progn

            ;; Asure cmake_build directory exists
            (if (not (file-exists-p "~/cmake_builds"))
                (make-directory "~/cmake_builds"))

            ;; Define cmake-ide-build-dir under ~/cmake_builds
            (defun my/cmake-enable-ide ()
              "Modify cmake-build-dir and make it point to ~/cmake_builds"
              (interactive)
              (let (my/cmake-build-dir my/projectile-build-dir)
                (cmake-ide-setup)
                (if (cmake-ide--locate-cmakelists)
                    (progn
                      (if (and (file-exists-p "~/cmake_builds")
                               (projectile-project-name))
                          (progn
                            (make-local-variable 'cmake-ide-build-dir)

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
                                                       (directory-file-name (cmake-ide--locate-cmakelists)))
                                                      my/projectile-build-dir))
                            (message "* cmake-ide Building directory set to: %s" my/cmake-build-dir)

                            ;; Create cmake project directory under project directory
                            (if (not (file-exists-p my/cmake-build-dir))
                                (make-directory my/cmake-build-dir))

                            ;; Set cmake-ide-build-dir according to both top project and cmake project names
                            (setq cmake-ide-build-dir my/cmake-build-dir)))))))))

(provide 'setup-cmake)
;;; setup-cmake.el ends here

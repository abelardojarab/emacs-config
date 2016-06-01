;;; setup-cmake.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxcj9063.pdx.intel.com>
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

;; cmake syntax highlighting
(use-package cmake-mode
  :defer t
  :commands cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :load-path (lambda () (expand-file-name "cmake-mode/" user-emacs-directory)))

;;;; rtags
;; sudo apt-get install libclang-dev / brew install llvm --with-clang
;; git clone --recursive https://github.com/Andersbakken/rtags.git &
;; (LIBCLANG_LLVM_CONFIG_EXECUTABLE=path_to_llvm-config CC=gcc CXX=g++ cmake ../rtags -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=where_to_install_rtags)
;; cmake --build ./ --target install
(use-package rtags
  :defer 2
  :if (executable-find "rdm")
  :after irony-mode
  :load-path (lambda () (expand-file-name "rtags/src" user-emacs-directory))
  :config (progn
          (setq rtags-use-helm t)
          (setq rtags-autostart-diagnostics t)
          (setq rtags-completions-enabled t)))

;; cmake-based IDE
(use-package cmake-ide
  :defer t
  :after irony-mode
  :commands use-cmake-ide
  :load-path (lambda () (expand-file-name "cmake-ide/" user-emacs-directory))
  :init (if (executable-find "cmake")
            (add-hook 'c-mode-common-hook #'use-cmake-ide))
  :config (progn
            (defun use-cmake-ide ()
              (cmake-ide-setup)
              (when (cmake-ide--locate-cmakelists)
                (setq cmake-ide-dir (concat (cmake-ide--locate-cmakelists) "build/"))))))

;; cmake utilities
(use-package cpputils-cmake
  :defer t
  :commands cppcm-reload-all
  :after cmake-ide
  :if (executable-find "cmake")
  :load-path (lambda () (expand-file-name "cpputils-cmake/" user-emacs-directory))
  :init (progn
          (add-hook 'c-mode-hook 'cppcm-reload-all)
          (add-hook 'c++-mode-hook 'cppcm-reload-all)))

(provide 'setup-cmake)
;;; setup-cmake.el ends here

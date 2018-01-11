;;; setup-cmake.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017, 2018  Abelardo Jara-Berrocal

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
  :commands (irony-mode irony-install-server)
  :if (executable-find "irony-server")
  :diminish irony-mode
  :load-path (lambda () (expand-file-name "irony-mode/" user-emacs-directory))
  :after (ggtags eldoc function-args company)
  :init (add-hook 'c-mode-common-hook #'irony-mode)
  :config (progn
            (if (file-exists-p "/usr/local/bin/irony-server")
                (setq irony-server-install-prefix "/usr/local/"))
            (if (file-exists-p "~/.emacs.cache/irony-server/bin/irony-server")
                (setq irony-server-install-prefix "~/.emacs.cache/irony-server/"))

            (push "-std=c++11" irony-additional-clang-options)

            (if (not (file-exists-p "~/.emacs.cache/irony-user-dir"))
                (make-directory "~/.emacs.cache/irony-user-dir") t)
            (setq irony-user-dir "~/.emacs.cache/irony-user-dir/")

            ;; Irony json projects
            (use-package irony-cdb-json)

            ;; Hooks
            (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)))

;; rtags
;; sudo apt-get install libclang-dev / brew install llvm --with-clang
;; git clone --recursive https://github.com/Andersbakken/rtags.git &
;; (LIBCLANG_LLVM_CONFIG_EXECUTABLE=path_to_llvm-config CC=gcc CXX=g++ cmake ../rtags -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=where_to_install_rtags)
;; cmake --build ./ --target install
(use-package rtags
  :defer t
  :commands (rtags-print-symbol-info
             rtags-find-symbol-at-point
             rtags-start-process-unless-running
             rtags-eldoc-function)
  :if (executable-find "rdm")
  :load-path (lambda () (expand-file-name "rtags/src/" user-emacs-directory))
  :bind (:map c++-mode-map
              ("C-c C-i" . rtags-print-symbol-info)
              ("C-c C-s" . rtags-find-symbol-at-point))
  :init (add-hook 'c-mode-common-hook #'rtags-start-process-unless-running)
  :config (progn
            (use-package helm-rtags
              :load-path (lambda () (expand-file-name "rtags/src/" user-emacs-directory)))
            (setq rtags-autostart-diagnostics      t
                  rtags-completions-enabled        t
                  rtags-display-summary-as-tooltip t
                  rtags-tooltips-enabled           t
                  rtags-use-helm                   t
                  rtags-display-result-backend     'helm)
            (rtags-enable-standard-keybindings)
            (rtags-set-periodic-reparse-timeout 2)

            ;; Kill rdm on Emacs exit
            (add-hook 'kill-emacs-hook #'rtags-quit-rdm)

            ;; Integrating rtags with eldoc
            ;; https://github.com/Andersbakken/rtags/issues/987
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
            (add-hook 'c-mode-common-hook (lambda ()
                                            (when (and (executable-find "rdm")
                                                       (projectile-project-p)
                                                       (not (file-exists-p (concat (projectile-project-root)
                                                                                   "GTAGS"))))
                                              (setq-local eldoc-documentation-function #'rtags-eldoc-function))))))

;; cmake syntax highlighting
(use-package cmake-mode
  :defer t
  :commands cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"          . cmake-mode))
  :load-path (lambda () (expand-file-name "cmake-mode/" user-emacs-directory)))

;; cmake-based IDE
(use-package cmake-ide
  :defer t
  :after (rtags irony-mode cmake-mode)
  :commands (my/cmake-enable-ide
             cmake-ide-compile
             cmake-ide-run-cmake
             cmake-ide--locate-cmakelists)
  :if (executable-find "cmake")
  :load-path (lambda () (expand-file-name "cmake-ide/" user-emacs-directory))
  :init (add-hook 'c-mode-common-hook 'my/cmake-enable-ide)
  :config (progn

            ;; Asure cmake_build directory exists
            (if (not (file-exists-p "~/cmake_builds"))
                (make-directory "~/cmake_builds"))

            (defun cmake-ide--get-build-dir ()
              "Return the directory name to run CMake in."
              ;; build the directory key for the project
              (my/cmake-enable-ide)
              (let ((build-dir
                     (expand-file-name (or (cmake-ide--build-dir-var)
                                           (cmake-ide--get-build-dir-from-hash))
                                       (cmake-ide--locate-project-dir))))
                (when (not (file-accessible-directory-p build-dir))
                  (cmake-ide--message "Making directory %s" build-dir)
                  (make-directory build-dir))
                (setq cmake-ide-build-dir build-dir)
                (file-name-as-directory build-dir)))

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
                                                       (directory-file-name
                                                        (expand-file-name
                                                         (cmake-ide--locate-cmakelists))))
                                                      my/projectile-build-dir))

                            ;; Avoid path repetitions
                            (if (equal (file-name-nondirectory
                                        (directory-file-name
                                         (expand-file-name
                                          (cmake-ide--locate-cmakelists))))
                                       (projectile-project-name))
                                (setq my/cmake-build-dir (concat
                                                          "~/cmake_builds/"
                                                          (projectile-project-name))))
                            (message "* cmake-ide Building directory set to: %s" my/cmake-build-dir)

                            ;; Create cmake project directory under project directory
                            (if (not (file-exists-p my/cmake-build-dir))
                                (make-directory my/cmake-build-dir))

                            ;; Set cmake-ide-build-dir according to both top project and cmake project names
                            (setq cmake-ide-build-dir my/cmake-build-dir)))))))))

(provide 'setup-cmake)
;;; setup-cmake.el ends here

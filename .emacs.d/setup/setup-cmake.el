;;; setup-cmake.el ---                            -*- lexical-binding: t; -*-

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
  :defer t
  :after projectile
  :if (not (equal system-type 'windows-nt))
  :commands (cmake-ide-compile
             cmake-ide-setup
             cmake-ide-run-cmake
             cmake-ide-load-db)
  :if (executable-find "cmake")
  :hook (c-mode-common . my/cmake-ide-find-project)
  :preface (defun my/cmake-ide-find-project ()
             "Finds the directory of the project for cmake-ide."
             (with-eval-after-load 'projectile
               (setq-local cmake-ide-project-dir (projectile-project-root))
               (setq-local cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
             (setq-local cmake-ide-compile-command
                         (concat "cd " cmake-ide-build-dir " && cmake -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES .. && make"))
             (cmake-ide-setup)
             (cmake-ide-load-db))
  :init (progn
          (use-package semantic/bovine/gcc)
          (put 'cmake-ide-build-dir 'safe-local-variable #'stringp)
          (setq cmake-ide-flags-c++ (append '("-std=c++11")
                                            (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c++"))))
          (setq cmake-ide-flags-c (append (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c"))))))

(provide 'setup-cmake)
;;; setup-cmake.el ends here

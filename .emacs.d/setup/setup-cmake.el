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

;; cmake-based IDE
(use-package cmake-ide
  :defer 2
  :commands use-cmake-ide
  :after (rtags flycheck)
  :load-path (lambda () (expand-file-name "cmake-ide/" user-emacs-directory))
  :init (if (executable-find "cmake")
            (add-hook 'c-mode-common-hook #'use-cmake-ide))
  :config (progn
            (defun use-cmake-ide ()
              (cmake-ide-setup)
              (when (cmake-ide--locate-cmakelists)
                (setq cmake-ide-build-dir (concat (cmake-ide--locate-cmakelists) "cmake_build_dir/"))))))

(provide 'setup-cmake)
;;; setup-cmake.el ends here

;;; setup-java.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

;; Author: Abelardo Jara <abelardojara@Abelardos-MacBook-Pro.local>
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

(use-package ajc-java-complete-config
  :after auto-complete
  :load-path (lambda () (expand-file-name "ajc-java-complete/" user-emacs-directory))
  :config (progn
            (add-hook 'java-mode-hook 'ajc-java-complete-mode)
            (setq ajc-tag-file-list (list (expand-file-name "ajc-java-complete/java_base.tag" user-emacs-directory)))))

(provide 'setup-java)
;;; setup-java.el ends here

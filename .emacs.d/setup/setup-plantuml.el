;;; setup-plantuml.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

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

(use-package puml-mode
  :mode ("\\.puml\\'" "\\.plantuml\\'")
  :commands (puml-mode puml-complete-symbol puml-render-buffer)
  :load-path (lambda () (expand-file-name "puml-mode/" user-emacs-directory))
  :init (setq puml-plantuml-jar-path (expand-file-name "jar/plantuml.jar" user-emacs-directory))
  :config (progn
            (defun puml-render-buffer ()
              (interactive)
              (message "PlantUML rendering")
              (shell-command (concat "java -jar "
                                     (expand-file-name "jar/plantuml.jar" user-emacs-directory)
                                     " "
                                     buffer-file-name))
              (message (concat "PlantUML rendered:  " (buffer-name))))))

(provide 'setup-plantuml)
;;; setup-plantuml.el ends here

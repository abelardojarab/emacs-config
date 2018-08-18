;;; setup-writeroom.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Abelardo Jara-Berrocal

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

;; Distraction free
(use-package writeroom-mode
  :defer t
  :bind ("C-c w" . writeroom-mode)
  :config (if (> (frame-width) 200)
              (setq writeroom-width (- (frame-width) 80))
            (setq writeroom-width 120)))

;; Focus on a particular section
(use-package focus
  :defer t
  :commands focus-mode)

;; Insert typographically useful unicode
(use-package typo
  :defer t
  :diminish typo-mode
  :commands typo-mode
  :hook (org-mode . typo-mode)
  :config (setq-default typo-language "English"))

(provide 'setup-writeroom)
;;; setup-writeroom.el ends here

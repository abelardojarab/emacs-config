;;; setup-smex.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023  Abelardo Jara-Berrocal

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

;; Better Alt-x

(use-package smex
  :commands (smex smex-major-mode-commands)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :load-path (lambda ()
               (expand-file-name
                (if (and (= emacs-major-version 24) (= emacs-minor-version 2))
                    "smex2/"
                  "smex/") user-emacs-directory))
  :config (smex-initialize))

(use-package mini-frame
  :commands mini-frame-mode
  :config (progn
			;; Miniframe at the bottom for a nicer display
			(setq mini-frame-show-parameters
				  `((left . 0.5)
					(top . 1.0)
					(width . 1.0)
					(height . 5)
					(left-fringe . 12)
					(right-fringe .12)
					(child-frame-border-width . 0)
					(internal-border-width . 0)))

			(setq mini-frame-resize 'not-set)
			(add-hook 'minibuffer-setup-hook
					  (lambda ()
						(overlay-put (make-overlay (point-min) (+ (point-min) 1))
									 'before-string
									 (propertize "\n" 'face `(:extend t
																	  :height .5)))))))

(provide 'setup-smex)
;;; setup-modeline.el ends here

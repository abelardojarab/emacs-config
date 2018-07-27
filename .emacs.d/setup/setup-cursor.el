;;; setup-cursor.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Abelardo Jara-Berrocal

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

;; Change form/shape of emacs cursor
(setq my/read-only-color "green")
(setq my/read-only-cursor-type 'hbar)
(setq my/overwrite-color "red")
(setq my/overwrite-cursor-type 'box)
(setq my/normal-color "white")
(setq my/normal-cursor-type 'bar)
(defun my/set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."
  (cond
   (buffer-read-only
    (set-cursor-color my/read-only-color)
    (setq cursor-type my/read-only-cursor-type))
   (overwrite-mode
    (set-cursor-color my/overwrite-color)
    (setq cursor-type my/overwrite-cursor-type))
   (t
    (set-cursor-color my/normal-color)
    (setq cursor-type my/normal-cursor-type))))
(add-hook 'post-command-hook
          (lambda () (interactive)
            (unless (member
                     major-mode '(pdf-docs doc-view-mode))
              (my/set-cursor-according-to-mode))))

;; Disable blinking cursor
(blink-cursor-mode 0)

;; Make the cursor the full width of the character at point
(setq x-stretch-cursor t)

;; Blinking cursor
(use-package heartbeat-cursor
  :if (not (equal system-type 'windows-nt))
  :commands heartbeat-cursor-mode
  :init (progn
            (add-hook 'prog-mode-hook #'heartbeat-cursor-mode)
            (add-hook 'org-mode-hook #'heartbeat-cursor-mode)))

;; Multiple cursors
(use-package multiple-cursors
  :defer t
  :bind (:map region-bindings-mode-map
         ("C-c l" . mc/edit-lines)
         ("C-c n" . mc/mark-next-like-this)
         ("C-c p" . mc/mark-previous-like-this)
         ("C-c a" . mc/mark-all-like-this)
         ("C-c m" . mc/mark-more-like-this))
  :load-path (lambda () (expand-file-name "multiple-cursors/" user-emacs-directory)))

(provide 'setup-cursor)
;;; setup-cursor.el ends here

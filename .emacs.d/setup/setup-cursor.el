;;; setup-cursor.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara

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

;; Change form/shape of emacs cursor
(setq djcb-read-only-color "green")
(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color "yellow")
(setq djcb-normal-cursor-type 'bar)
(defun djcb-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."
  (cond
   (buffer-read-only
    (set-cursor-color djcb-read-only-color)
    (setq cursor-type djcb-read-only-cursor-type))
   (overwrite-mode
    (set-cursor-color djcb-overwrite-color)
    (setq cursor-type djcb-overwrite-cursor-type))
   (t
    (set-cursor-color djcb-normal-color)
    (setq cursor-type djcb-normal-cursor-type))))
(add-hook 'post-command-hook
          (lambda () (interactive)
            (unless (member
                     major-mode '(pdf-docs doc-view-mode))
              (djcb-set-cursor-according-to-mode))))

;; Disable blinking cursor
(blink-cursor-mode 0)

;; Blinking cursor
(use-package heartbeat-cursor
  :config (progn
            (add-hook 'prog-mode-hook (lambda () (heartbeat-cursor-mode)))
            (add-hook 'org-mode-hook (lambda () (heartbeat-cursor-mode)))))

(provide 'setup-cursor)
;;; setup-cursor.el ends here

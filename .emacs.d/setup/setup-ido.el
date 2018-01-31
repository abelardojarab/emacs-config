;;; setup-ido.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017, 2018  Abelardo Jara-Berrocal

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

;; ido makes competing buffers and finding files easier
(use-package ido
  :demand t
  :bind (:map minibuffer-local-completion-map
              ;; This tab override shouldn't be necessary given ido's default
              ;; configuration, but minibuffer-complete otherwise dominates the
              ;; tab binding because of my custom tab-completion-everywhere
              ;; configuration.
              ([tab] . ido-complete)
              ([up]  . previous-history-element)
              :map ido-file-dir-completion-map
              ("C-v" . ido-yank))
  :config (progn
            (ido-mode 'both)
            (ido-everywhere 1)
            (setq ido-max-dir-file-cache                 0
                  ido-show-dot-for-dired                 t
                  ido-default-file-method                'samewindow
                  ido-default-buffer-method              'selected-window
                  ido-save-directory-list-file            (concat (file-name-as-directory
                                                                   my/emacs-cache-dir)
                                                                  "ido.last")
                  ido-ignore-buffers                     '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace" "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
                  ido-work-directory-list                '("~/" "~/Desktop" "~/Documents" "~/workspace")
                  ido-case-fold                          t
                  ido-enable-last-directory-history      t
                  ido-auto-merge-work-directories-length -1
                  ido-max-work-directory-list            15
                  ido-max-work-file-list                 10
                  ido-use-filename-at-point              nil
                  ido-use-url-at-point                   nil
                  ido-enable-flex-matching               t
                  ido-enable-prefix                      t
                  ido-max-prospects                      8
                  ido-confirm-unique-completion          t
                  ido-auto-merge-delay-time              0.7
                  ido-auto-merge-work-directories-length -1)

            ;; Paste file name with ctrl-v
            (defun ido-yank ()
              (interactive)
              (let ((path (current-kill 0)))
                (if (file-exists-p path)
                    (progn
                      (let ((dir (file-name-directory path)))
                        (if dir (ido-set-current-directory dir)))
                      (setq ido-exit 'refresh)
                      (setq ido-text-init (if (file-directory-p path) nil (file-name-nondirectory path)))
                      (setq ido-rotate-temp t)
                      (exit-minibuffer))
                  (yank))))

            ;; when using ido, the confirmation is rather annoying...
            (setq confirm-nonexistent-file-or-buffer nil)))

;; Make compiler happy
(use-package ido-hacks
  :demand t
  :config (ido-hacks-mode 1))

;; Ido everywhere
(use-package ido-ubiquitous
  :disabled t
  :demand t
  :load-path (lambda () (expand-file-name "ido-ubiquitous/" user-emacs-directory))
  :config (ido-ubiquitous-mode 1))

;; Use ibuffer
(use-package ibuffer
  :defer t
  :commands ibuffer
  :init (setq ibuffer-inline-columns t)
  :config (progn
            (setq ibuffer-saved-filter-groups
                  '(("Config" (or
                               (filename . "workspace/configfiles/")
                               (filename . ".emacs.d/")))
                    ("Shell"  (or
                               (mode . eshell-mode)
                               (mode . shell-mode)))
                    ("Dired"  (mode . dired-mode))
                    ("Prose"  (or
                               (mode . tex-mode)
                               (mode . plain-tex-mode)
                               (mode . latex-mode)
                               (mode . rst-mode)
                               (mode . markdown-mode)))
                    ("Org"    (mode . org-mode))
                    ("Gnus"   (or
                               (mode . message-mode)
                               (mode . gnus-group-mode)
                               (mode . gnus-summary-mode)
                               (mode . gnus-article-mode)))
                    ("Emacs"  (name . "^\\*.*\\*$"))
                    ("IRC"    (name . "^freenode\.")))
                  ibuffer-show-empty-filter-groups nil
                  ibuffer-expert t)

            (defadvice ibuffer-update-title-and-summary (after remove-column-titles)
              (save-excursion
                (set-buffer "*Ibuffer*")
                (toggle-read-only 0)
                (goto-char 1)
                (search-forward "-\n" nil t)
                (delete-region 1 (point))
                (let ((window-min-height 1))
                  ;; save a little screen estate
                  (shrink-window-if-larger-than-buffer))
                (toggle-read-only)))
            (ad-activate 'ibuffer-update-title-and-summary)

            ;; Use human readable Size column instead of original one
            (define-ibuffer-column size-h
              (:name "Size" :inline t)
              (cond
               ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
               ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
               ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
               (t (format "%8d" (buffer-size)))))

            ;; Modify the default ibuffer-formats
            (setq ibuffer-formats
                  '((mark modified read-only " "
                          (name 18 18 :left :elide)
                          " "
                          (size-h 9 -1 :right)
                          " "
                          (mode 16 16 :left :elide)
                          " "
                          filename-and-process)))))

;; ibuffer versioning-oriented grouping
(use-package ibuffer-vc
  :defer t
  :after ibuffer
  :commands my/ibuffer-apply-filter-groups
  :load-path (lambda () (expand-file-name "ibuffer-vc/" user-emacs-directory))
  :init (add-hook 'ibuffer-hook #'my/ibuffer-apply-filter-groups)
  :config (progn
            (defun my/ibuffer-apply-filter-groups ()
              "Combine my saved ibuffer filter groups with those generated
     by `ibuffer-vc-generate-filter-groups-by-vc-root'"
              (interactive)
              (setq ibuffer-filter-groups
                    (append (ibuffer-vc-generate-filter-groups-by-vc-root)
                            ibuffer-saved-filter-groups))
              (message "ibuffer-vc: groups set")
              (let ((ibuf (get-buffer "*Ibuffer*")))
                (when ibuf
                  (with-current-buffer ibuf
                    (pop-to-buffer ibuf)
                    (ibuffer-update nil t)))))))

(provide 'setup-ido)
;;; setup-ido.el ends here

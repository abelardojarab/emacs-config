;;; setup-folding.el ---                       -*- lexical-binding: t; -*-

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

(use-package folding
  :bind (:map folding-mode-prefix-map
              ("<SPC>" . folding-context-next-action))
  :config (progn
            (defun my/folding-check-folded ()
              "Function to determine if this file is in folded form."
              (let ((folding-re1 "^.?.?.?{{{")
                    (folding-re2 "[\r\n].*}}}"))
                (save-excursion
                  (goto-char (point-min))
                  ;;  If we found both, we assume file is folded
                  (and (assq major-mode folding-mode-marks-alist)
                       (< (point-max) 10000)
                       (re-search-forward folding-re1 nil t)
                       ;; if file is folded, there are \r's
                       (re-search-forward "[\r\n]" nil t)
                       (re-search-forward folding-re2 nil t)))))
            (setq folding-check-folded-file-function 'my/folding-check-folded)
            (folding-mode-add-find-file-hook)

            ;; add keywords to current buffer directly, overwrite the original function in folding.el
            (defun folding-font-lock-support ()
              "Add font lock support."
              (ignore-errors
                (font-lock-add-keywords nil (folding-font-lock-keywords major-mode))))))

;; fold this - folds selected region
(use-package fold-this
  :defer t
  :after region-bindings-mode
  :init (setq fold-this-persistent-folds-file  (concat (file-name-as-directory
                                                        my/emacs-cache-dir)
                                                       "folds-saved"))
  :bind (:map fold-this-keymap
              ;; left-click on ellipsis to unfold
              ("<mouse-1>" . fold-this-unfold-at-point)
              :map region-bindings-mode-map
              ("&"         . fold-this))
  :commands (fold-this fold-this-unfold-at-point)
  :custom (fold-this-persistent-folds t))

;; Cycle code visibility
(use-package hideshow
  :defer t
  :diminish hs-minor-mode
  :hook (prog-mode  . hs-minor-mode))

;; Cycle outline
(use-package outline
  :diminish outline-minor-mode
  :hook ((prog-mode message-mode markdown-mode) . outline-minor-mode))

;; Bring org cycle extensions to outline-minor-mode
(use-package outshine
  :defer t
  :bind (:map outline-minor-mode-map
              ("C-c c" . outshine-cycle-buffer))
  :custom ((outshine-use-speed-commands                t)
           (outshine-org-style-global-cycling-at-bob-p t)))

;; Fast Emacs navigation and control
(use-package navi-mode
  :after outshine
  :commands (navi-search-and-switch)
  :config (setf (cdr (assoc :ALL (cdr (assoc "emacs-lisp" navi-keywords))))
                "^[[:space:]]*(\\(use-package\\|\\(cl-\\)\\{0,1\\}def[a-z]+\\)\\*? "))

;; Outline navigation, similar to outshine-navi
(use-package helm-navi
  :after (helm navi-mode)
  :commands helm-navi)

;; Cycle visibility on outline-minor-mode
(use-package bicycle
  :after outline
  ;; :bind (:map outline-minor-mode-map
  ;;             ("C-c C-c" . bicycle-cycle))
  :commands (bicycle-cycle
             bicycle-cycle-global))

;; org-style folding/unfolding in hideshow
(use-package hideshow-org
  :defer t
  :init (progn
          (setq hs-org/trigger-keys-block (list (kbd "C-c +")))
          (setq hs-org/trigger-keys-all (list (kbd "C-c &"))))
  :commands hs-org/minor-mode)

;; Yet Another Folding - folding code blocks based on indentation
(use-package yafolding
  :defer t
  :commands (yafolding-toggle-element
             yafolding-toggle-all
             yafolding-show-element
             yafolding-show-all
             yafolding-hide-element
             yafolding-hide-all)
  :bind (:map outline-minor-mode-map
              ("C-c +" . yafolding-toggle-element)
              ("C-c -" . yafolding-toggle-all))
  :custom (yafolding-ellipsis-content " ⮷ "))

;; Enable fold dwim (do what i mean)
(use-package fold-dwim
  :defer t
  :after hideshow)

;; Visual hideshow mode
(use-package hideshowvis
  :defer t
  :if (display-graphic-p)
  :after hideshow
  :commands (hideshowvis-minor-mode
             hideshowvis-enable))

;; vim-like fold
(use-package vimish-fold
  :defer t
  :commands (vimish-fold-mode vimish-fold-global-mode)
  :bind (:map vimish-fold-folded-keymap
              ("<tab>" . vimish-fold-unfold)
              :map vimish-fold-unfolded-keymap
              ("<tab>" . vimish-fold-refold))
  :custom (vimish-fold-header-width 78)
  :config (setq-default
           vimish-fold-dir (file-name-as-directory
                            (concat (file-name-as-directory
                                     my/emacs-cache-dir)
                                    "vimish-fold"))))

(use-package origami
  :defer t
  :custom (origami-show-fold-header t)
  :commands (hydra-origami/body
             global-origami-mode
             origami-undo
             origami-redo
             origami-toggle-node
             origami-toggle-all-nodes
             origami-recursively-toggle-node)
  :hook (after-init . global-origami-mode)
  :custom-face
  (origami-fold-replacement-face ((t (:inherit magit-diff-context-highlight))))
  (origami-fold-fringe-face ((t (:inherit magit-diff-context-highlight))))
  :config (progn
            (face-spec-reset-face 'origami-fold-header-face)
            (defhydra hydra-origami (:color blue :hint none)
              "
      _:_: recursively toggle node       _a_: toggle all nodes    _t_: toggle node
      _o_: show only current node        _u_: undo                _r_: redo
      _R_: reset
      "
              (":" origami-recursively-toggle-node)
              ("a" origami-toggle-all-nodes)
              ("t" origami-toggle-node)
              ("o" origami-show-only-node)
              ("u" origami-undo)
              ("r" origami-redo)
              ("R" origami-reset)
              ("q" nil "Quit")
              )))

(provide 'setup-folding)
;;; setup-hideshow.el ends here

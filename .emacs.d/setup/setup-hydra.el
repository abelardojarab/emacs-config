;;; setup-hydra.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017  Abelardo Jara-Berrocal

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

(use-package hydra
  :load-path (lambda () (expand-file-name "hydra/" user-emacs-directory))
  :config (progn

            (defhydra hydra-helm (:hint nil :color pink)
              "
                                                                          ╭──────┐
   Navigation   Other  Sources     Mark             Do             Help   │ Helm │
  ╭───────────────────────────────────────────────────────────────────────┴──────╯
        ^_k_^         _K_       _p_   [_m_] mark         [_v_] view         [_H_] helm help
        ^^↑^^         ^↑^       ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
    _h_ ←   → _l_     _c_       ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
        ^^↓^^         ^↓^       ^↓^    ^ ^               [_y_] yank selection
        ^_j_^         _J_       _n_    ^ ^               [_w_] toggle windows
  --------------------------------------------------------------------------------
        "
              ("<tab>" helm-keyboard-quit "back" :exit t)
              ("<escape>" nil "quit")
              ("\\" (insert "\\") "\\" :color blue)
              ("h" helm-beginning-of-buffer)
              ("j" helm-next-line)
              ("k" helm-previous-line)
              ("l" helm-end-of-buffer)
              ("g" helm-beginning-of-buffer)
              ("G" helm-end-of-buffer)
              ("n" helm-next-source)
              ("p" helm-previous-source)
              ("K" helm-scroll-other-window-down)
              ("J" helm-scroll-other-window)
              ("c" helm-recenter-top-bottom-other-window)
              ("m" helm-toggle-visible-mark)
              ("t" helm-toggle-all-marks)
              ("u" helm-unmark-all)
              ("H" helm-help)
              ("s" helm-buffer-help)
              ("v" helm-execute-persistent-action)
              ("d" helm-persistent-delete-marked)
              ("y" helm-yank-selection)
              ("w" helm-toggle-resplit-and-swap-windows)
              ("f" helm-follow-mode))

            (defhydra my/hydra-toggle-map nil
              "
^Toggle^
^^^^^^^^--------------------
_d_: debug-on-error
_D_: debug-on-quit
_f_: auto-fill-mode
_l_: toggle-truncate-lines
_h_: hl-line-mode
_r_: read-only-mode
_q_: quit
"
              ("d" toggle-debug-on-error :exit t)
              ("D" toggle-debug-on-quit :exit t)
              ("f" auto-fill-mode :exit t)
              ("l" toggle-truncate-lines :exit t)
              ("r" read-only-mode :exit t)
              ("h" hl-line-mode :exit t)
              ("q" nil :exit t))

            (defhydra my/hydra-flycheck (:color blue)
              "
^
^Flycheck^          ^Errors^            ^Checker^
^────────^──────────^──────^────────────^───────^───────────
[_q_] quit          [_c_] check         [_s_] select
[_v_] verify setup  [_n_] next          [_d_] disable
[_m_] manual        [_p_] previous      [_?_] describe
^^                  ^^                  ^^
"
  ("q" nil)
  ("c" flycheck-buffer)
  ("d" flycheck-disable-checker)
  ("m" flycheck-manual)
  ("n" flycheck-next-error :color red)
  ("p" flycheck-previous-error :color red)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup)
  ("?" flycheck-describe-checker))

(defhydra my/hydra-projectile (:color blue)
  "
^
^Projectile^        ^Buffers^           ^Find^              ^Search^
^──────────^────────^───────^───────────^────^──────────────^──────^────────────
[_q_] quit          [_b_] list all      [_d_] directory     [_r_] replace
[_i_] reset cache   [_k_] kill all      [_D_] root          [_s_] ag
^^                  [_S_] save all      [_f_] file          ^^
^^                  ^^                  [_p_] project       ^^
^^                  ^^                  ^^                  ^^
"
  ("q" nil)
  ("b" helm-projectile-switch-to-buffer)
  ("d" helm-projectile-find-dir)
  ("D" projectile-dired)
  ("f" helm-projectile-find-file)
  ("i" projectile-invalidate-cache :color red)
  ("k" projectile-kill-buffers)
  ("p" helm-projectile-switch-project)
  ("r" projectile-replace)
  ("s" helm-projectile-ag)
  ("S" projectile-save-project-buffers :color red))

(defhydra my/hydra-org (:color pink)
  "
^
^Org^               ^Links^             ^Outline^
^───^───────────────^─────^─────────────^───────^───────────
[_q_] quit          [_i_] insert        [_a_] show all
^^                  [_n_] next          [_b_] backward
^^                  [_o_] open          [_f_] forward
^^                  [_p_] previous      [_v_] overview
^^                  [_s_] store         ^^
^^                  ^^                  ^^
"
  ("q" nil)
  ("a" show-all)
  ("b" org-backward-element)
  ("f" org-forward-element)
  ("i" org-insert-link)
  ("n" org-next-link)
  ("o" org-open-at-point)
  ("p" org-previous-link)
  ("s" org-store-link)
  ("v" org-overview))

            (global-set-key (kbd "C-x t") 'my/hydra-toggle-map/body))
  )

(provide 'setup-hydra)
;;; setup-hydra.el ends here

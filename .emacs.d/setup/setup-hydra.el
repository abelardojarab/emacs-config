;;; setup-hydra.el ---                               -*- lexical-binding: t; -*-

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

(use-package hydra
  :defer t
  :commands (defhydra
             hydra-timestamp/body
             hydra-describe/body
             hydra-window/body
             hydra-magit/body
             hydra-toggle/body
             hydra-multiple-cursors/body
             hydra-yasnippet/body
             hydra-helm/body
             hydra-yasnippet/body
             hydra-projectile/body
             hydra-org/body
             hydra-org-clock/body
             hydra-org-template/body
             hydra-flycheck/body
             hydra-ibuffer-main/body
             hydra-ibuffer-action/body
             hydra-ibuffer-filter/body
             hydra-ibuffer-mark/body)
  :config (progn

            ;; Do not use lv it messes up windows layout
            (setq-default hydra-lv nil)

            (defhydra hydra-timestamp (:color blue :hint nil)
              "
Timestamps: (_q_uit)
  Date: _I_SO, _U_S, US With _Y_ear and _D_ashes, US In _W_ords
   Date/Time: _N_o Colons or _w_ith
    Org-Mode: _R_ight Now or _c_hoose
"
              ("q" nil)

              ("I" insert-datestamp)
              ("U" insert-datestamp-us)
              ("Y" insert-datestamp-us-full-year)
              ("D" insert-datestamp-us-full-year-and-dashes)
              ("W" insert-datestamp-us-words)

              ("N" insert-timestamp-no-colons)
              ("w" insert-timestamp)

              ("R" org-time-stamp-with-seconds-now)
              ("c" org-time-stamp))

            (defhydra hydra-describe (:color blue
                                             :hint nil)
              "
Describe Something: (q to quit)
_a_ all help for everything screen
_b_ bindings
_B_ personal bindings
_c_ char
_C_ coding system
_f_ function
_F_ flycheck checker
_i_ input method
_k_ key briefly
_K_ key
_l_ language environment
_L_ mode lineage
_m_ major mode
_M_ minor mode
_n_ current coding system briefly
_N_ current coding system full
_o_ lighter indicator
_O_ lighter symbol
_p_ package
_P_ text properties
_s_ symbol
_t_ theme
_v_ variable
_w_ where is something defined
"
              ("b" describe-bindings)
              ("B" describe-personal-keybindings)
              ("C" describe-categories)
              ("c" describe-char)
              ("C" describe-coding-system)
              ("f" describe-function)
              ("F" flycheck-describe-checker)
              ("i" describe-input-method)
              ("K" describe-key)
              ("k" describe-key-briefly)
              ("l" describe-language-environment)
              ("L" help/parent-mode-display)
              ("M" describe-minor-mode)
              ("m" describe-mode)
              ("N" describe-current-coding-system)
              ("n" describe-current-coding-system-briefly)
              ("o" describe-minor-mode-from-indicator)
              ("O" describe-minor-mode-from-symbol)
              ("p" describe-package)
              ("P" describe-text-properties)
              ("q" nil)
              ("a" help)
              ("s" describe-symbol)
              ("t" describe-theme)
              ("v" describe-variable)
              ("w" where-is))

            (defhydra hydra-window (:color red :hint nil)
              "
 Split: _v_ert _x_:horz
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _s_wap
Frames: _f_rame new  _df_ delete
  Misc: _m_ark _a_ce  _u_ndo  _r_edo"
              ("h" windmove-left)
              ("j" windmove-down)
              ("k" windmove-up)
              ("l" windmove-right)
              ("H" hydra-move-splitter-left)
              ("J" hydra-move-splitter-down)
              ("K" hydra-move-splitter-up)
              ("L" hydra-move-splitter-right)
              ("|" (lambda ()
                     (interactive)
                     (split-window-right)
                     (windmove-right)))
              ("_" (lambda ()
                     (interactive)
                     (split-window-below)
                     (windmove-down)))
              ("v" split-window-right)
              ("x" split-window-below)
                                        ;("t" transpose-frame "'")
              ;; winner-mode must be enabled
              ("u" winner-undo)
              ("r" winner-redo) ;;Fixme, not working?
              ("o" delete-other-windows :exit t)
              ("a" ace-window :exit t)
              ("f" new-frame :exit t)
              ("s" ace-swap-window)
              ("da" ace-delete-window)
              ("dw" delete-window)
              ("db" kill-this-buffer)
              ("df" delete-frame :exit t)
              ("q" nil)
              ("m" headlong-bookmark-jump))

            (defhydra hydra-magit (global-map "C-c g" :color teal :hint nil)
              "
     PROJECTILE: %(projectile-project-root)

     Immuting            Mutating
-----------------------------------------
  _w_: blame line      _b_: checkout
  _a_: annotate file   _B_: branch mgr
  _d_: diff            _c_: commit
  _s_: status          _e_: rebase
  _l_: log
  _t_: time machine

"
              ("w" git-messenger:popup-message)
              ("a" vc-annotate)
              ("b" magit-checkout)
              ("B" magit-branch-manager)
              ("c" vc-next-action)
              ("d" magit-diff-working-tree)
              ("e" magit-interactive-rebase)
              ("s" magit-status)
              ("l" magit-log)
              ("t" git-timemachine))

            (defhydra hydra-multiple-cursors (:hint nil)
              "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
              ("l" mc/edit-lines :exit t)
              ("a" mc/mark-all-like-this :exit t)
              ("n" mc/mark-next-like-this)
              ("N" mc/skip-to-next-like-this)
              ("M-n" mc/unmark-next-like-this)
              ("p" mc/mark-previous-like-this)
              ("P" mc/skip-to-previous-like-this)
              ("M-p" mc/unmark-previous-like-this)
              ("r" mc/mark-all-in-region-regexp :exit t)
              ("q" nil))

            (defhydra hydra-yasnippet (:color blue :hint nil)
              "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
              ("d" yas-load-directory)
              ("e" yas-activate-extra-mode)
              ("i" yas-insert-snippet)
              ("f" yas-visit-snippet-file :color blue)
              ("n" yas-new-snippet)
              ("t" yas-tryout-snippet)
              ("l" yas-describe-tables)
              ("g" yas/global-mode)
              ("m" yas/minor-mode)
              ("a" yas-reload-all))

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

            (defhydra hydra-toggle nil
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

            (defhydra hydra-flycheck (:color blue)
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

            (defhydra hydra-projectile (:color blue)
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

            (defhydra hydra-org (:color pink)
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

            (defhydra hydra-org-clock (:color blue :timeout 12 :columns 4)
              "Org commands"
              ("i" (lambda () (interactive) (org-clock-in '(4))) "Clock in")
              ("o" org-clock-out "Clock out")
              ("q" org-clock-cancel "Cancel a clock")
              ("<f10>" org-clock-in-last "Clock in the last task")
              ("j" (lambda () (interactive) (org-clock-goto '(4))) "Go to a clock")
              ("m" make-this-message-into-an-org-todo-item "Flag and capture this message"))

            (defhydra hydra-org-template (:color blue
                                                 :hint nil)
              "
org-template:  _c_enter        _s_rc          _e_xample           _v_erilog        _t_ext              _I_NCLUDE:
               _l_atex         _h_tml         _V_erse             _m_atlab         _L_aTeX:            _H_TML:
               _a_scii         _q_uote        _E_macs-lisp        _n_im            _i_ndex:            _A_SCII:
               _o_rg           _S_hell        _p_ython            e_X_port         [_bd_] description  [_bn_] note
"
              ("s" (my/org-template-expand "<s")) ;#+begin_src ... #+end_src
              ("E" (my/org-template-expand "<s" "emacs-lisp"))
              ("v" (my/org-template-expand "<s" "systemverilog"))
              ("m" (my/org-template-expand "<s" "matlab"))
              ("n" (my/org-template-expand "<s" "nim"))
              ("o" (my/org-template-expand "<s" "org"))
              ("S" (my/org-template-expand "<s" "shell"))
              ("p" (my/org-template-expand "<s" "python"))
              ("t" (my/org-template-expand "<s" "text"))
              ("bd" (my/org-template-expand "<bd")) ;#+begin_description ... #+end_description (Special block in `ox-hugo')
              ("bn" (my/org-template-expand "<bn")) ;#+begin_note ... #+end_note (Special block in `ox-hugo')
              ("e" (my/org-template-expand "<e")) ;#+begin_example ... #+end_example
              ("x" (my/org-template-expand "<e")) ;#+begin_example ... #+end_example
              ("q" (my/org-template-expand "<q")) ;#+begin_quote ... #+end_quote
              ("V" (my/org-template-expand "<v")) ;#+begin_verse ... #+end_verse
              ("c" (my/org-template-expand "<c")) ;#+begin_center ... #+end_center
              ("X" (my/org-template-expand "<X")) ;#+begin_export ... #+end_export
              ("l" (my/org-template-expand "<X" "latex")) ;#+begin_export latex ... #+end_export
              ("h" (my/org-template-expand "<X" "html")) ;#+begin_export html ... #+end_export
              ("a" (my/org-template-expand "<X" "ascii")) ;#+begin_export ascii ... #+end_export
              ("L" (my/org-template-expand "<L")) ;#+latex:
              ("H" (my/org-template-expand "<H")) ;#+html:
              ("A" (my/org-template-expand "<A")) ;#+ascii:
              ("i" (my/org-template-expand "<i")) ;#+index: line
              ("I" (my/org-template-expand "<I")) ;#+include: line
              ("<" self-insert-command "<")
              ("Q" nil "quit"))

            (defhydra hydra-ibuffer-main (:color pink :hint nil)
              "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
              ("j" ibuffer-forward-line)
              ("RET" ibuffer-visit-buffer :color blue)
              ("k" ibuffer-backward-line)

              ("m" ibuffer-mark-forward)
              ("u" ibuffer-unmark-forward)
              ("*" hydra-ibuffer-mark/body :color blue)

              ("D" ibuffer-do-delete)
              ("S" ibuffer-do-save)
              ("a" hydra-ibuffer-action/body :color blue)

              ("g" ibuffer-update)
              ("s" hydra-ibuffer-sort/body :color blue)
              ("/" hydra-ibuffer-filter/body :color blue)

              ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
              ("q" quit-window "quit ibuffer" :color blue)
              ("." nil "toggle hydra" :color blue))

            (defhydra hydra-ibuffer-mark (:color teal :columns 5
                                                 :after-exit (hydra-ibuffer-main/body))
              "Mark"
              ("*" ibuffer-unmark-all "unmark all")
              ("M" ibuffer-mark-by-mode "mode")
              ("m" ibuffer-mark-modified-buffers "modified")
              ("u" ibuffer-mark-unsaved-buffers "unsaved")
              ("s" ibuffer-mark-special-buffers "special")
              ("r" ibuffer-mark-read-only-buffers "read-only")
              ("/" ibuffer-mark-dired-buffers "dired")
              ("e" ibuffer-mark-dissociated-buffers "dissociated")
              ("h" ibuffer-mark-help-buffers "help")
              ("z" ibuffer-mark-compressed-file-buffers "compressed")
              ("b" hydra-ibuffer-main/body "back" :color blue))

            (defhydra hydra-ibuffer-action (:color teal :columns 4
                                                   :after-exit
                                                   (if (eq major-mode 'ibuffer-mode)
                                                       (hydra-ibuffer-main/body)))
              "Action"
              ("A" ibuffer-do-view "view")
              ("E" ibuffer-do-eval "eval")
              ("F" ibuffer-do-shell-command-file "shell-command-file")
              ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
              ("H" ibuffer-do-view-other-frame "view-other-frame")
              ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
              ("M" ibuffer-do-toggle-modified "toggle-modified")
              ("O" ibuffer-do-occur "occur")
              ("P" ibuffer-do-print "print")
              ("Q" ibuffer-do-query-replace "query-replace")
              ("R" ibuffer-do-rename-uniquely "rename-uniquely")
              ("T" ibuffer-do-toggle-read-only "toggle-read-only")
              ("U" ibuffer-do-replace-regexp "replace-regexp")
              ("V" ibuffer-do-revert "revert")
              ("W" ibuffer-do-view-and-eval "view-and-eval")
              ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
              ("b" nil "back"))

            (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
              "Sort"
              ("i" ibuffer-invert-sorting "invert")
              ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
              ("v" ibuffer-do-sort-by-recency "recently used")
              ("s" ibuffer-do-sort-by-size "size")
              ("f" ibuffer-do-sort-by-filename/process "filename")
              ("m" ibuffer-do-sort-by-major-mode "mode")
              ("b" hydra-ibuffer-main/body "back" :color blue))

            (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
              "Filter"
              ("m" ibuffer-filter-by-used-mode "mode")
              ("M" ibuffer-filter-by-derived-mode "derived mode")
              ("n" ibuffer-filter-by-name "name")
              ("c" ibuffer-filter-by-content "content")
              ("e" ibuffer-filter-by-predicate "predicate")
              ("f" ibuffer-filter-by-filename "filename")
              (">" ibuffer-filter-by-size-gt "size")
              ("<" ibuffer-filter-by-size-lt "size")
              ("/" ibuffer-filter-disable "disable")
              ("b" hydra-ibuffer-main/body "back" :color blue))))

(provide 'setup-hydra)
;;; setup-hydra.el ends here

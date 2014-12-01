;;; setup-modeline.el ---

;; Copyright (C) 2014  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc25288.pdx.intel.com>
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

;; Fix the modeline
(add-to-list 'load-path "~/.emacs.d/rich-minority")
(add-to-list 'load-path "~/.emacs.d/smart-mode-line")
(require 'smart-mode-line)
(sml/setup)

(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 25)
(setq sml/mode-width 'full)

(add-to-list 'sml/hidden-modes " ErgoEmacs")
(add-to-list 'sml/hidden-modes " ErgoEmacs[us]")
(add-to-list 'sml/hidden-modes " my-keys")
(add-to-list 'sml/hidden-modes " iImg")
(add-to-list 'sml/hidden-modes " BufFace")
(add-to-list 'sml/hidden-modes " Ind")
(add-to-list 'sml/hidden-modes " ing")
(add-to-list 'sml/hidden-modes " rsync")
(add-to-list 'sml/hidden-modes " AC")
(add-to-list 'sml/hidden-modes " AI")
(add-to-list 'sml/hidden-modes " SP")
(add-to-list 'sml/hidden-modes " mate")
(add-to-list 'sml/hidden-modes " Plugged")
(add-to-list 'sml/hidden-modes " GG")
(add-to-list 'sml/hidden-modes " Gtags")
(add-to-list 'sml/hidden-modes " Abbrev")
(add-to-list 'sml/hidden-modes " Fill")
(add-to-list 'sml/hidden-modes " Guide")
(add-to-list 'sml/hidden-modes " hs")
(add-to-list 'sml/hidden-modes " yas")
(add-to-list 'sml/hidden-modes " pair")
(add-to-list 'sml/hidden-modes " GitGutter")
(add-to-list 'sml/hidden-modes " Undo-Tree")
(add-to-list 'sml/hidden-modes " MRev")
(add-to-list 'sml/hidden-modes " vl")
(add-to-list 'sml/hidden-modes " ElDoc")
(add-to-list 'sml/hidden-modes " Eldoc-eval")
(add-to-list 'sml/hidden-modes " -Chg")
(add-to-list 'sml/hidden-modes " Projectile")
(add-to-list 'sml/hidden-modes " sc")
(add-to-list 'sml/hidden-modes " WSC")
(add-to-list 'sml/hidden-modes " Spell")
(add-to-list 'sml/hidden-modes " Wrap")
(add-to-list 'sml/hidden-modes " FlyC")
(add-to-list 'sml/hidden-modes " FlyC-")
(add-to-list 'sml/hidden-modes " ll")
(add-to-list 'sml/hidden-modes " FA")
(add-to-list 'sml/hidden-modes " hl-s")

;; Better Alt-x
(if (and (= emacs-major-version 24) (= emacs-minor-version 2))
    ;; Use smex3
    (add-to-list 'load-path "~/.emacs.d/smex2")
  ;; Use smex2
  (add-to-list 'load-path "~/.emacs.d/smex")
  ) ;; if
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Nyan cat
(add-to-list 'load-path "~/.emacs.d/nyan-mode")
(require 'nyan-mode)
(nyan-mode t)

(provide 'setup-modeline)
;;; setup-modeline.el ends here

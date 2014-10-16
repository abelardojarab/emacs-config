;;; setup-hideshow.el ---

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

;; Code folding
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)
(dolist (hook (list 'lisp-mode-hook
                    'python-mode-hook
                    'js2-mode-hook
                    'emacs-lisp-mode-hook
                    'c++-mode-hook))
  (add-hook hook 'hideshowvis-enable))
(global-set-key (kbd "<f7>") 'hs-hide-block)
(global-set-key (kbd "S-<f7>") 'hs-show-block)
(global-set-key (kbd "C-c @ SPC") 'hs-show-block) ; second binding

;; enable `hs-minor-mode' at startup
(add-hook 'emacs-lisp-mode-hook
          (lambda () (hs-minor-mode 1)))
(add-hook 'lisp-mode-hook
          (lambda () (hs-minor-mode 1)))
(add-hook 'python-mode-hook
          (lambda () (hs-minor-mode 1)))
(add-hook 'js2-mode-hook
          (lambda () (hs-minor-mode 1)))

;; Add the following to your .emacs and uncomment it in order to get a + symbol
(define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])

(defcustom hs-fringe-face 'hs-fringe-face
  "*Specify face used to highlight the fringe on hidden regions."
  :type 'face
  :group 'hideshow)

(defface hs-fringe-face
  '((t (:foreground "#999" :box (:line-width 2 :color "grey75" :style released-button))))
  "Face used to highlight the fringe on folded regions"
  :group 'hideshow)

(defcustom hs-face 'hs-face
  "*Specify the face to to use for the hidden region indicator"
  :type 'face
  :group 'hideshow)

(defface hs-face
  '((t (:background "#558" :box t)))
  "Face to hightlight the ... area of hidden regions"
  :group 'hideshow)

(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string))
           (display-string (format "(%d)..." (count-lines
                                              (overlay-start ov) (overlay-end ov)))))
      (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
      (put-text-property 0 marker-length 'display (list 'left-fringe
                                                        'hs-marker 'hs-fringe-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 0 (length display-string) 'face 'hs-face
                         display-string)
      (overlay-put ov 'display display-string))))
(setq hs-set-up-overlay 'display-code-line-counts)

;; Yafolding
(add-to-list 'load-path "~/.emacs.d/yafolding")
(require 'yafolding)
(add-hook 'prog-mode-hook
          (lambda () (yafolding-mode)))
(define-key yafolding-mode-map (kbd "<C-S-return>") nil)
(define-key yafolding-mode-map (kbd "<C-return>") nil)
(define-key yafolding-mode-map (kbd "C-c <C-S-return>") 'yafolding-toggle-all)
(define-key yafolding-mode-map (kbd "C-c <C-return>") 'yafolding-toggle-element)

(provide 'setup-hideshow)
;;; setup-hideshow.el ends here

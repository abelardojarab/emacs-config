;;; setup-spell.el ---

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

;; We need tell emacs to use aspell, and where your custom dictionary is.
(setq ispell-silently-savep t)

;; Use hunspell if available instead
(if (executable-find "hunspell")
    (progn
      (setq ispell-dictionary "american")
      (setq ispell-program-name "hunspell")
      (setq ispell-really-hunspell t)
      (setq ispell-really-aspell nil)
      (setq ispell-extra-args '()) ;; TeX mode "-t"
      (setq ispell-local-dictionary-alist '(
                                            (nil
                                             "[[:alpha:]]"
                                             "[^[:alpha:]]"
                                             "[']"
                                             t
                                             ("-d" "en_US" "-i" "utf-8" "-p" "~/.emacs.d/dictionaries")
                                             nil
                                             utf-8)

                                            ("english"
                                             "[[:alpha:]]"
                                             "[^[:alpha:]]"
                                             "[']"
                                             t
                                             ("-d" "en_US" "-i" "utf-8" "-p" "~/.emacs.d/dictionaries")
                                             nil
                                             utf-8)

                                            ("american"
                                             "[[:alpha:]]"
                                             "[^[:alpha:]]"
                                             "[']"
                                             t
                                             ("-d" "en_US" "-i" "utf-8" "-p" "~/.emacs.d/dictionaries")
                                             nil
                                             utf-8))))
  (progn
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--sug-mode=ultra"))
    (when (eq system-type 'darwin)
      (if (file-executable-p "/usr/local/bin/aspell")
          (progn
            (setq ispell-program-name "/usr/local/bin/aspell")
            (setq ispell-extra-args '("-d" "/Library/Application Support/cocoAspell/aspell6-en-6.0-0/en.multi")))))))

;; Enable ispell at the end
(require 'ispell)

;; change dictionary: "C-c e" = english, "C-c s"=spanish, "C-c w"=turn off flyspell
(add-hook 'text-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c s 2")
                            (lambda () (interactive)
                              (ispell-change-dictionary "american")
                              (flyspell-mode 1)
                              (flyspell-buffer)))
             (local-set-key (kbd "C-c s 1")
                            (lambda () (interactive)
                              (ispell-change-dictionary "spanish")
                              (flyspell-mode 1)
                              (flyspell-buffer)))
             (local-set-key (kbd "C-c s 0")
                            (lambda () (interactive)
                              (flyspell-mode -1)))))

;; Langtool
(add-to-list 'load-path "~/.emacs.d/langtool")
(require 'langtool)
(setq langtool-language-tool-jar (expand-file-name "~/.emacs.d/jar/LanguageTool-2.7/languagetool-commandline.jar")
      langtool-mother-tongue "en"
      langtool-disabled-rules '("WHITESPACE_RULE"
                                "EN_UNPAIRED_BRACKETS"
                                "COMMA_PARENTHESIS_WHITESPACE"
                                "EN_QUOTES"))

;; flyspell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(eval-after-load "flyspell"
  '(defun flyspell-ajust-cursor-point (save cursor-location old-max)
     (when (not (looking-at "\\b"))
       (forward-word))))

;; Fix for right click on Mac OS X
(when (eq system-type 'darwin)
  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined))))

;; Disable flyspell keybindings
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-.") nil))

;; Autocomplete using Aspell
(add-to-list 'load-path "~/.emacs.d/ac-ispell")
(require 'ac-ispell)
(custom-set-variables
 '(ac-ispell-requires 4)
 '(ac-ispell-fuzzy-limit 2))

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setup)

(provide 'setup-spell)
;;; setup-spell.el ends here

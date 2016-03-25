;;; setup-spell.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

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

;; Enable ispell at the end
(use-package ispell
  :config (progn
            ;; General configuration
            (setq ispell-highlight-face 'flyspell-incorrect
                  ispell-silently-savep t
                  ispell-alternate-dictionary (expand-file-name "dictionaries/words.txt" user-emacs-directory))
            (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))

            ;; find aspell and hunspell automatically
            (cond
             ;; try hunspell at first
             ((executable-find "hunspell")
              (setq ispell-dictionary-alist '((nil "[A-Za-z]" "[^A-Za-z]" "[']" t
                                                   ("-d" "en_US" "-i" "utf-8") nil utf-8)

                                              ("english"
                                                   "[[:alpha:]]"
                                                   "[^[:alpha:]]"
                                                   "[']"
                                                   t
                                                   ("-d" "en_US")
                                                   nil
                                                   utf-8)))
              (setq ispell-dictionary "english"
                    ispell-program-name "hunspell"
                    ispell-extra-args (list "-d" (expand-file-name "dictionaries/en_US" user-emacs-directory))))

             ;; if hunspell does not exist, use aspell
             ((executable-find "aspell")
              (setq ispell-dictionary "english"
                    ispell-program-name "aspell"
                    ispell-extra-args '("--sug-mode=ultra"))
              (when (eq system-type 'darwin)
                (if (file-executable-p "/usr/local/bin/aspell")
                    (progn
                      (setq ispell-program-name "/usr/local/bin/aspell")
                      (setq ispell-extra-args '("-d" "/Library/Application Support/cocoAspell/aspell6-en-6.0-0/en.multi")))))))

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
                                          (flyspell-mode -1)))))))

;; flyspell
(use-package flyspell
  :config (progn
            (setq flyspell-issue-message-flag nil
                  flyspell-issue-welcome-flag nil)
            (dolist (hook '(text-mode-hook org-mode-hook markdown-mode-hook))
              (add-hook hook (lambda () (flyspell-mode 1))))
            (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
              (add-hook hook (lambda () (flyspell-mode -1))))
            (add-hook 'prog-mode-hook 'flyspell-prog-mode)

            (defun flyspell-ajust-cursor-point (save cursor-location old-max)
              (when (not (looking-at "\\b"))
                (forward-word)))

            ;; Fix for right click on Mac OS X
            (when (eq system-type 'darwin)
              (progn
                (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
                (define-key flyspell-mouse-map [mouse-3] #'undefined)))

            ;; Disable flyspell keybindings
            (define-key flyspell-mode-map (kbd "C-.") nil)))

;; write good mode
(use-package writegood-mode
  :pin manual
  :load-path (lambda () (expand-file-name "writegood-mode/" user-emacs-directory)))

;; Langtool
(use-package langtool
  :load-path (lambda () (expand-file-name "langtool/" user-emacs-directory))
  :config (progn
            (setq langtool-language-tool-jar (expand-file-name
                                              "jar/LanguageTool-2.7/languagetool-commandline.jar"
                                              user-emacs-directory)
                  langtool-mother-tongue "en"
                  langtool-disabled-rules '("WHITESPACE_RULE"
                                            "EN_UNPAIRED_BRACKETS"
                                            "COMMA_PARENTHESIS_WHITESPACE"
                                            "EN_QUOTES"))))

(provide 'setup-spell)
;;; setup-spell.el ends here

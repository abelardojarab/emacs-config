;;; setup-spell.el ---

;; Copyright (C) 2016  Abelardo Jara-Berrocal

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

;; Enable ispell at the end
(use-package ispell
  :config (progn
            ;; General configuration
            (setq ispell-highlight-face 'flyspell-incorrect
                  ispell-silently-savep t)
            (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))

            ;; find aspell and hunspell automatically
            (cond
             ;; try hunspell at first
             ((executable-find "hunspell")
              (setq ispell-dictionary-alist '((nil
                                               "[A-Za-z]" "[^A-Za-z]" "[']" t
                                               ("-d" "en_US" "-i" "utf-8") nil utf-8)

                                              ("en_US"
                                               "[[:alpha:]]" "[^[:alpha:]]" "[']" t
                                               ("-d" "en_US") nil utf-8)))
              (setq ispell-really-hunspell t
                    ispell-dictionary "en_US"
                    ispell-program-name "hunspell"
                    ispell-extra-args (list "-d" (expand-file-name "dictionaries/en_US" user-emacs-directory))))

             ;; if hunspell does not exist, use aspell
             ((executable-find "aspell")
              (setq ispell-dictionary "english"
                    ispell-program-name "aspell"
                    ispell-extra-args '("--sug-mode=fast")
                    ispell-alternate-dictionary (expand-file-name "dictionaries/words.txt" user-emacs-directory))
              (when (eq system-type 'darwin)
                (setq ispell-dictionary-alist
                      '((nil
                         "[A-Za-z]" "[^A-Za-z]" "[']" nil
                         ("-B" "-d" "en.multi" "--dict-dir"
                          "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
                         nil utf-8)

                        ("english"
                         "[A-Za-z]" "[^A-Za-z]" "[']" nil
                         ("-B" "-d" "en.multi" "--dict-dir"
                          "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
                         nil utf-8)))

                ;; Use Macports aspell if available
                (if (file-executable-p "/opt/local/bin/aspell")
                    (setq ispell-program-name "/opt/local/bin/aspell")))))

            ;; change dictionary: "C-c e" = english, "C-c s"=spanish, "C-c w"=turn off flyspell
            (add-hook 'text-mode-hook
                      '(lambda ()
                         (local-set-key (kbd "C-c s 2")
                                        (lambda () (interactive)
                                          (ispell-change-dictionary "en_US")
                                          (flyspell-mode 1)
                                          (flyspell-buffer)))
                         (local-set-key (kbd "C-c s 1")
                                        (lambda () (interactive)
                                          (ispell-change-dictionary "en_GB")
                                          (flyspell-mode 1)
                                          (flyspell-buffer)))
                         (local-set-key (kbd "C-c s 0")
                                        (lambda () (interactive)
                                          (flyspell-mode -1)))))

            ;; Ignored patterns
            (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
            (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
            (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
            (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))))

;; flyspell
(use-package flyspell
  :diminish flyspell-mode
  :if (not (equal 'system-type 'windows-nt))
  :config (progn
            (setq flyspell-issue-message-flag nil
                  flyspell-issue-welcome-flag nil)
            (dolist (hook '(text-mode-hook org-mode-hook markdown-mode-hook))
              (add-hook hook (lambda () (flyspell-mode 1))))
            (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
              (add-hook hook (lambda () (flyspell-mode -1))))

            (defun flyspell-ajust-cursor-point (save cursor-location old-max)
              (when (not (looking-at "\\b"))
                (forward-word)))

            ;; Fix for right click on Mac OS X
            (when (eq system-type 'darwin)
              (progn
                (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
                (define-key flyspell-mouse-map [mouse-3] #'undefined)))

            ;; 2. ignore message flags
            (setq flyspell-issue-message-flag nil)

            ;; Disable flyspell keybindings
            (define-key flyspell-mode-map (kbd "C-;") nil)
            (define-key flyspell-mode-map (kbd "C-.") nil)))

;; flyspell popup correction
(use-package flyspell-correct-popup
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-word-generic))
  :load-path (lambda () (expand-file-name "flyspell-correct/" user-emacs-directory)))

;; write good mode
(use-package writegood-mode
  :pin manual
  :diminish writegood-mode
  :load-path (lambda () (expand-file-name "writegood-mode/" user-emacs-directory)))

;; Langtool
(use-package langtool
  :if (executable-find "java")
  :load-path (lambda () (expand-file-name "langtool/" user-emacs-directory))
  :config (progn

            ;; tool settings
            (setq langtool-language-tool-jar (expand-file-name
                                              "jar/LanguageTool-3.2/languagetool-commandline.jar"
                                              user-emacs-directory)
                  langtool-mother-tongue "en"
                  langtool-disabled-rules '("WHITESPACE_RULE"
                                            "EN_UNPAIRED_BRACKETS"
                                            "COMMA_PARENTHESIS_WHITESPACE"
                                            "EN_QUOTES"))

            ;; Show LanguageTool report automatically by popup
            ;; This idea come from: http://d.hatena.ne.jp/LaclefYoshi/20150912/langtool_popup
            (defun langtool-autoshow-detail-popup (overlays)
              (when (require 'popup nil t)
                ;; Do not interrupt current popup
                (unless (or popup-instances
                            ;; suppress popup after type `C-g` .
                            (memq last-command '(keyboard-quit)))
                  (let ((msg (langtool-details-error-message overlays)))
                    (popup-tip msg)))))
            (setq langtool-autoshow-message-function
                  'langtool-autoshow-detail-popup)))

(provide 'setup-spell)
;;; setup-spell.el ends here

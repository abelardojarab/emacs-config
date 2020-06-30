;;; setup-spell.el ---                               -*- lexical-binding: t; -*-

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

;; Enable ispell at the end
(use-package ispell
  :custom ((ispell-highlight-face             'flyspell-incorrect)
           (ispell-silently-savep             t)
           (ispell-choices-win-default-height 5))
  :preface (defun my/org-ispell ()
             "Configure `ispell-skip-region-alist' for `org-mode'."
             (make-local-variable 'ispell-skip-region-alist)
             (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
             (add-to-list 'ispell-skip-region-alist '("~" "~"))
             (add-to-list 'ispell-skip-region-alist '("=" "="))
             (add-to-list 'ispell-skip-region-alist '("```" "```"))
             (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
  :hook ((org-mode markdown-mode) . my/org-ispell)
  :config (progn
            ;; Find aspell and hunspell automatically
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

            ;; Redefine ispell minor check such that it ignores errors
            (defun ispell-minor-check ()
              "Check previous word, then continue with the normal binding of this key.
Don't check previous word when character before point is a space or newline.
Don't read buffer-local settings or word lists."
              (interactive "*")
              (let ((ispell-minor-mode nil)
                    (ispell-check-only t)
                    (last-char (char-after (1- (point)))))
                (ignore-errors
                  (if (key-binding (this-command-keys))
                      (command-execute (key-binding (this-command-keys)))
                    (newline-and-indent)))
                (if (not (or (eq last-char ?\ ) (eq last-char ?\n)
                             (and ispell-skip-html (eq last-char ?>))
                             (and ispell-skip-html (eq last-char ?\;))))
                    (ispell-word nil t))))

            ;; Don't send ’ to the subprocess.
            (defun my/replace-apostrophe (args)
              (cons (replace-regexp-in-string
                     "’" "'" (car args))
                    (cdr args)))
            (advice-add #'ispell-send-string :filter-args #'my/replace-apostrophe)

            ;; Convert ' back to ’ from the subprocess.
            (defun my/replace-quote (args)
              (if (not (or (derived-mode-p 'org-mode)
                           (derived-mode-p 'markdown-mode)
                           (derived-mode-p 'rst-mode)
                           (derived-mode-p 'message-mode)))
                  args
                (cons (replace-regexp-in-string
                       "'" "’" (car args))
                      (cdr args))))
            (advice-add #'ispell-parse-output :filter-args #'my/replace-quote)

            ;; Ignored patterns
            (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
            (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
            (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
            (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))))

;; flyspell
(use-package flyspell
  :diminish (flyspell-mode . " ⓢ")
  :if (not (equal 'system-type 'windows-nt))
  :commands (flyspell-mode flyspell-check-next-highlighted-word)
  :custom ((flyspell-delay 1)
           (flyspell-issue-message-flag nil)
           (flyspell-issue-welcome-flag nil))
  :init (progn
          (dolist (hook my/flyspell-modes)
            (add-hook hook (lambda () (flyspell-mode 1))))
          (dolist (hook my/flyspell-modes-disabled)
            (add-hook hook (lambda () (flyspell-mode -1)))))
  :config (progn
            (defun flyspell-ajust-cursor-point (save cursor-location old-max)
              (when (not (looking-at "\\b"))
                (forward-word)))

            (defun flyspell-check-next-highlighted-word ()
              "Custom function to spell check next highlighted word"
              (interactive)
              (flyspell-goto-next-error)
              (ispell-word))

            ;; Fix for right click on Mac OS X
            (when (eq system-type 'darwin)
              (progn
                (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
                (define-key flyspell-mouse-map [mouse-3] #'undefined)))))

;; Profiling revealed flyspell-post-command-hook
;; was responsible for 47% of CPU cycles by itself
(use-package flyspell-lazy
  :disabled t
  :defer t
  :hook (flyspell-mode . flyspell-lazy-mode))

;; flyspell ivy correction
(use-package flyspell-correct-ivy
  :defer t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-word-generic)))

;; write good mode
(use-package writegood-mode
  :defer t
  :pin manual
  :commands writegood-mode
  :diminish writegood-mode)

;; Synonyms search
(use-package powerthesaurus
  :defer t
  :commands powerthesaurus-lookup-word)

;; Langtool
(use-package langtool
  :if (executable-find "java")
  :custom ((langtool-mother-tongue "en")
           (langtool-disabled-rules '("WHITESPACE_RULE"
                                      "EN_UNPAIRED_BRACKETS"
                                      "COMMA_PARENTHESIS_WHITESPACE"
                                      "EN_QUOTES")))
  :config (progn
            ;; tool settings
            (setq langtool-language-tool-jar (expand-file-name
                                              "jar/LanguageTool-3.2/languagetool-commandline.jar"
                                              user-emacs-directory))

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

;;; setup-nettools.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@ubuntu03>
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

;; newsticker
(use-package newsticker
  :commands (newsticker-start newsticker-start-ticker)
  :if (and (not (equal system-type 'windows-nt))
           (internet-up-p))
  :load-path (lambda () (expand-file-name "newsticker/" user-emacs-directory))
  :config (progn
            (require 'newsticker-notify)
            (setq newsticker-dir "~/.emacs.cache/newsticker")
            (setq newsticker-url-list-defaults nil)
            (setq newsticker-automatically-mark-items-as-old t)
            (setq newsticker-automatically-mark-visited-items-as-old t)
            (setq newsticker-retrieval-interval 600)
            (setq newsticker-html-renderer 'w3m-region)
            (setq newsticker-retrieval-method 'extern)
            (setq newsticker-treeview-treewindow-width 40)
            (setq newsticker-treeview-listwindow-height 30)
            (setq newsticker-obsolete-item-max-age (* 30 (* 24 3600)))
            (setq newsticker-ticker-interval 4.3) ;;
            (setq newsticker-display-interval 3.3) ;; 0.3 for scroll-smooth, 15.3 otherwise
            (setq newsticker-scroll-smoothly nil) ;; dont make it t otherwise will start scrolling
            (setq newsticker-wget-arguments '("-q" "-O" "-"
                                              "--user-agent" "testing"))
            (setq newsticker-sort-method (quote sort-by-time))
            (setq newsticker-url-list
                  (quote (("BBC News" "http://www.bbc.co.uk/syndication/feeds/news/ukfs_news/front_page/rss091.xml" nil nil nil)
                          ("Phoronix" "http://www.phoronix.com/rss.php")
                          ("Google News" "http://news.google.com/?output=rss"))))
            (setq newsticker-url-list-defaults
                  (quote (("BBC News" "http://www.bbc.co.uk/syndication/feeds/news/ukfs_news/front_page/rss091.xml" nil nil nil)
                          ("Phoronix" "http://www.phoronix.com/rss.php")
                          ("Google News" "http://news.google.com/?output=rss"))))
            (newsticker-start)
            (newsticker-start-ticker)))

;; StackExchange client for Emacs
(use-package sx
  :defer t
  :load-path (lambda () (expand-file-name "sx/" user-emacs-directory))
  :bind (("C-c a S a" . sx-ask)
       ("C-c a S s" . sx-tab-all-questions)
       ("C-c a S q" . sx-tab-all-questions)
       ("C-c a S f" . sx-tab-all-questions)
       ("C-c a S n" . sx-tab-newest))
  :config (progn
            ;; defines sx-ask
            (use-package sx-interaction)

            ;; defines sx-tab
            (use-package sx-tab)))

;; Write questions/answers for Stack Exchange
(use-package sx-compose
  :after sx
  :defer t
  :load-path (lambda () (expand-file-name "sx/" user-emacs-directory))
  :config (progn
  ;; Don't fill in SX questions/answers, and use visual lines instead.  Plays
  ;; more nicely with the website.
  (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
  (add-hook 'sx-compose-mode-hook #'visual-line-mode)

  ;; Clean up whitespace before sending questions
  (add-hook 'sx-compose-before-send-hook
            (lambda () (whitespace-cleanup) t))))

;; Show Stack
(use-package sx-question-mode
  :after sx
  :defer t
  :load-path (lambda () (expand-file-name "sx/" user-emacs-directory))
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

(provide 'setup-nettools)
;;; setup-nettools.el ends here

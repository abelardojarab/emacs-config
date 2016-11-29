;;; setup-gnus.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

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

;; Gnus
(use-package gnus
  :defer t
  :commands (gnus compose-mail)
  :config (progn
            (setq gnus-select-method
                  '(nnimap "Gmail"
                           (nnimap-address "imap.gmail.com")
                           (nnimap-server-port 993)
                           (nnimap-stream ssl)))

            ;; Gnus news
            (setq gnus-summary-line-format "%U%R%z%d %I%(%[ %F %] %s %)\n")
            (setq gnus-secondary-select-methods '((nntp "news.gmane.org")
                                                  (nntp "news.gwene.org")))

            ;; gnus setup
            ;; (gnus-registry-initialize)
            (setq gnus-treat-from-gravatar t)))

;; apel
(use-package apel
  :defer t
  :load-path (lambda () (expand-file-name "apel/" user-emacs-directory)))

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

(provide 'setup-gnus)
;;; setup-gnus.el ends here

;;; setup-nettools.el ---                            -*- lexical-binding: t; -*-

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

;; newsticker
(use-package newsticker
  :commands (newsticker-start newsticker-start-ticker)
  :if (and (not (equal system-type 'windows-nt))
           (internet-up-p))
  :load-path (lambda () (expand-file-name "newsticker/" user-emacs-directory))
  :config (progn
            (use-package newsticker-notify)

            (setq newsticker-dir (concat (file-name-as-directory
                                          my/emacs-cache-dir)
                                         "newsticker")
                  newsticker-url-list-defaults                       nil
                  newsticker-automatically-mark-items-as-old         t
                  newsticker-automatically-mark-visited-items-as-old t
                  newsticker-retrieval-interval                      600
                  newsticker-html-renderer                           'w3m-region
                  newsticker-retrieval-method                        'extern
                  newsticker-treeview-treewindow-width               40
                  newsticker-treeview-listwindow-height              30
                  newsticker-obsolete-item-max-age                   (* 30 (* 24 3600))
                  newsticker-ticker-interval                         4.3
                  newsticker-display-interval                        3.3
                  newsticker-scroll-smoothly                         nil
                  newsticker-wget-arguments '("-q" "-O" "-"
                                              "--user-agent" "testing")
                  newsticker-sort-method (quote sort-by-time)
                  newsticker-url-list
                  (quote (("Phoronix" "http://www.phoronix.com/rss.php")
                          ("Google News" "http://news.google.com/?output=rss")))
                  newsticker-url-list-defaults
                  (quote (("Phoronix" "http://www.phoronix.com/rss.php")
                          ("Google News" "http://news.google.com/?output=rss"))))
            (newsticker-start)
            (newsticker-start-ticker)))

;; StackExchange client for Emacs
(use-package sx
  :if (and (not (equal system-type 'windows-nt))
           (internet-up-p))
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
  :if (and (not (equal system-type 'windows-nt))
           (internet-up-p))
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
  :if (and (not (equal system-type 'windows-nt))
           (internet-up-p))
  :after sx
  :defer t
  :load-path (lambda () (expand-file-name "sx/" user-emacs-directory))
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

;; Elfeed
(use-package elfeed
  :if (and (not (equal system-type 'windows-nt))
           (internet-up-p))
  :commands (elfeed elfeed-update)
  :load-path (lambda () (expand-file-name "elfeed/" user-emacs-directory))
  :bind (:map elfeed-search-mode-map
              ("a"        . elfeed-search-update--force)
              ("A"        . elfeed-update)
              ("d"        . elfeed-unjam)
              ("o"        . elfeed-search-browse-url)
              ("j"        . next-line)
              ("k"        . previous-line)
              ("g"        . beginning-of-buffer)
              ("G"        . end-of-buffer)
              ("v"        . set-mark-command)
              ("<escape>" . keyboard-quit)
              :map elfeed-show-mode-map
              ("j"        . elfeed-show-next)
              ("k"        . elfeed-show-prev)
              ("o"        . elfeed-show-visit)
              ("<escape>" . keyboard-quit)
              ("SPC"      . scroll-up)
              ("S-SPC"    . scroll-down)
              ("TAB"      . shr-next-link)
              ("S-TAB"    . shr-previous-link))
  :config (progn
            (setq elfeed-use-curl               t
                  elfeed-db-directory           (concat (file-name-as-directory
                                                         my/emacs-cache-dir)
                                                        "elfeed")
                  elfeed-search-filter          "@4-days-old +unread"
                  elfeed-search-title-max-width 100)

            (if (fboundp 'org-add-link-type)
                (with-no-warnings
                  (org-add-link-type "elfeed" #'elfeed-link-open)
                  (add-hook 'org-store-link-functions #'elfeed-link-store-link))
              (with-no-warnings
                (org-link-set-parameters
                 "elfeed"
                 :follow #'elfeed-link-open
                 :store #'elfeed-link-store-link)))

            ;; URLs in no particular order
            (setq elfeed-feeds
                  '(;; News
                    ("http://rss.cnn.com/rss/cnn_topstories.rss" news)
                    ("http://feeds.foxnews.com/foxnews/latest?format=xml" news)

                    ;; Blogs
                    ("http://feeds.feedburner.com/webupd8?format=xml" linux ubuntu)
                    ("http://feeds.feedburner.com/d0od?format=xml" linux ubuntu)

                    ;; Linux
                    ("http://www.phoronix.com/rss.php" linux news)

                    ;; Emacs
                    ("http://planet.emacsen.org/atom.xml" emacs)))))

(provide 'setup-nettools)
;;; setup-nettools.el ends here

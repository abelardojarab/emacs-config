;;; setup-elscreen.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Abelardo Jara-Berrocal

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

;; helm elscreen
(use-package elscreen
  :defer t
  :load-path (lambda () (expand-file-name "elscreen/" user-emacs-directory))
  :init (progn
          ;; Do not set a prefix (conflicts with helm)
          (setq elscreen-prefix-key (kbd "C-a")))
  :bind (:map ctl-x-map
              ("C-0" . elscreen-start)
              ("C-1" . elscreen-previous)
              ("C-2" . elscreen-next)
              ;; Cloning is more useful than fresh creation
              ("C-3" . elscreen-clone)
              ("C-4" . elscreen-kill)
              ("C-5" . elscreen-screen-nickname)
              ("C-6" . elscreen-toggle-display-tab))
  :config (progn
            (set-face-attribute 'elscreen-tab-background-face nil :inherit 'default :background nil)

            (setq
             ;; Do not show tabs to save space
             ;; Can use M-x elscreen-toggle-display-tab
             elscreen-display-tab nil

             ;; No preceding [X] for closing
             elscreen-tab-display-kill-screen nil

             ;; No [<->] at the beginning
             elscreen-tab-display-control nil)

            ;; buffer-dependent naming scheme
            (setq elscreen-buffer-to-nickname-alist
                  '(("^dired-mode$" .
                     (lambda ()
                       (format "Dired(%s)" dired-directory)))
                    ("^Info-mode$"  .
                     (lambda ()
                       (format "Info(%s)" (file-name-nondirectory Info-current-file))))
                    ("^mew-draft-mode$" .
                     (lambda ()
                       (format "Mew(%s)" (buffer-name (current-buffer)))))
                    ("^mew-"        . "Mew")
                    ("^irchat-"     . "IRChat")
                    ("^liece-"      . "Liece")
                    ("^lookup-"     . "Lookup")))

            (setq elscreen-mode-to-nickname-alist
                  '(("[Ss]hell"     . "shell")
                    ("compilation"  . "compile")
                    ("-telnet"      . "telnet")
                    ("dict"         . "OnlineDict")
                    ("*WL:Message*" . "Wanderlust")))

            ;; Use frame-title for tabs
            (defvar *elscreen-tab-truncate-length*
              20 "Number of characters to truncate tab names in frame title")

            (defun elscreen-tabs-as-string ()
              "Return a string representation of elscreen tab names
Set name truncation length in ELSCREEN-TRUNCATE-LENGTH"
              (let* ((screen-list (sort (elscreen-get-screen-list) '<))
                     (screen-to-name-alist (elscreen-get-screen-to-name-alist)))
                ;; mapconcat: mapping and then concate name elements together with separator
                (mapconcat
                 (lambda (screen)
                   (format (if (string-equal "+" (elscreen-status-label screen))
                               ;; Current screen format
                               "[ %d ] %s"
                             ;; Others
                             "(%d) %s")
                           ;; screen number: replaces %d (integer)
                           screen
                           ;; screen name: replaces %s (string)
                           (elscreen-truncate-screen-name
                            ;; Return the value associated with KEY in ALIST
                            (alist-get screen screen-to-name-alist)
                            *elscreen-tab-truncate-length*)))
                 ;; Screen numbers (keys for alist)
                 screen-list
                 ;; Separator
                 " | ")))

            (defvar *elscreen-tabs-as-string*
              "" "Variable to hold curent elscreen tab names as a string")

            (defun update-elscreen-tabs-as-string ()
              "Update *elscreen-tabs-as-string* variable"
              (interactive)
              (setq *elscreen-tabs-as-string* (elscreen-tabs-as-string)))

            ;; Update *elscreen-tabs-as-string* whenever elscreen status updates
            (add-hook 'elscreen-screen-update-hook #'update-elscreen-tabs-as-string)

            ;; Set frame title format as combination of current elscreen tabs and buffer/path
            (setq frame-title-format '(:eval (concat *elscreen-tabs-as-string*
                                                     "    ||    "
                                                     (if buffer-file-name
                                                         (abbreviate-file-name buffer-file-name)
                                                       "%b"))))

            ;; It has to kick in.
            (elscreen-start)))

(provide 'setup-elscreen)
;;; setup-elscreen.el ends here

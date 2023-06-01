;;; setup-tabbar.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023  Abelardo Jara-Berrocal

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

;; Tabbar ruler pre-requisites
(use-package mode-icons
  ;;  :if (display-graphic-p)
  :demand t)

(defadvice mode-icons-set-mode-icon (around bar activate)
  (ignore-errors add-do-it))

;; Tabbar
(use-package tabbar
  :if (version< emacs-version "27.1")
  :hook (after-init . tabbar-mode)
  :preface (push "~/.emacs.d/etc/images/" image-load-path)
  :custom ((tabbar-auto-scroll-flag  t)
           (tabbar-use-images        t)
           (table-time-before-update 0.1))
  :config (progn
            ;; Tabbar
            (global-set-key [C-prior]             'tabbar-backward-tab)
            (global-set-key [C-next]              'tabbar-forward-tab)
            (global-set-key [C-home]              'tabbar-backward-group)
            (global-set-key [C-end]               'tabbar-forward-group)

            ;; Tabbar, now using ctl-x-map
            (global-set-key (kbd "C-x <prior>")   'tabbar-backward-tab)
            (global-set-key (kbd "C-x <next>")    'tabbar-forward-tab)
            (global-set-key (kbd "C-x <home>")    'tabbar-backward-group)
            (global-set-key (kbd "C-x <end>")     'tabbar-forward-group)

            ;; Tabbar Ctrl-x mappings
            (define-key ctl-x-map (kbd "<prior>") 'tabbar-backward-tab)
            (define-key ctl-x-map (kbd "<next>")  'tabbar-forward-tab)
            (define-key ctl-x-map (kbd "<home>")  'tabbar-backward-group)
            (define-key ctl-x-map (kbd "<end>")   'tabbar-forward-group)

            ;; Sort tabbar buffers by name
            (defun tabbar-add-tab (tabset object &optional append_ignored)
              "Add to TABSET a tab with value OBJECT if there isn't one there yet.
 If the tab is added, it is added at the beginning of the tab list,
 unless the optional argument APPEND is non-nil, in which case it is
 added at the end."
              (let ((tabs (tabbar-tabs tabset)))
                (if (tabbar-get-tab object tabset)
                    tabs
                  (let ((tab (tabbar-make-tab object tabset)))
                    (tabbar-set-template tabset nil)
                    (set tabset (sort (cons tab tabs)
                                      (lambda (a b) (string< (buffer-name (car a)) (buffer-name (car b))))))))))

            ;; Reduce tabbar width to enable as many buffers as possible
            (defun tabbar-buffer-tab-label (tab)
              "Return a label for TAB.
That is, a string used to represent it on the tab bar."
              (let ((label  (if tabbar--buffer-show-groups
                                (format "[%s] " (tabbar-tab-tabset tab))
                              (format "%s" (tabbar-tab-value tab)))))
                ;; Unless the tab bar auto scrolls to keep the selected tab
                ;; visible, shorten the tab label to keep as many tabs as possible
                ;; in the visible area of the tab bar.
                (if nil ;; tabbar-auto-scroll-flag
                    label
                  (tabbar-shorten
                   label (max 1 (/ (window-width)
                                   (length (tabbar-view
                                            (tabbar-current-tabset)))))))))

            ;; Tweaking the tabbar
            (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
              (setq ad-return-value
                    (if (and (buffer-modified-p (tabbar-tab-value tab))
                             (buffer-file-name (tabbar-tab-value tab)))
                        (concat "+" (concat ad-return-value ""))
                      (concat "" (concat ad-return-value "")))))

            ;; called each time the modification state of the buffer changed
            (defun my/modification-state-change ()
              (tabbar-set-template tabbar-current-tabset nil)
              (tabbar-display-update))

            ;; first-change-hook is called BEFORE the change is made
            (defun my/on-buffer-modification ()
              (set-buffer-modified-p t)
              (my/modification-state-change))

            ;; Assure switching tabs uses switch-to-buffer
            (defun switch-tabbar (num)
              (let* ((tabs (tabbar-tabs
                            (tabbar-current-tabset)))
                     (tab (nth
                           (if (> num 0) (- num 1) (+ (length tabs) num))
                           tabs)))
                (if tab (switch-to-buffer (car tab)))))))

;; Tabbar ruler
(use-package tabbar-ruler
  :demand t
  :if (and (display-graphic-p)
           (version< emacs-version "27.1"))
  :custom ((tabbar-cycle-scope             'tabs)
           (tabbar-ruler-global-tabbar     t)
           (tabbar-ruler-fancy-close-image nil))
  :config (progn
            (tabbar-ruler-group-by-projectile-project)

            ;; Fix for tabbar under Emacs 24.4
            ;; store tabbar-cache into a real hash,
            ;; rather than in frame parameters
            (defvar tabbar-caches (make-hash-table :test 'equal))

            (defun tabbar-create-or-get-tabbar-cache ()
              "Return a frame-local hash table that acts as a memoization
       cache for tabbar. Create one if the frame doesn't have one yet."
              (or (gethash (selected-frame) tabbar-caches)
                  (let ((frame-cache (make-hash-table :test 'equal)))
                    (puthash (selected-frame) frame-cache tabbar-caches)
                    frame-cache)))))

;; Newer built-in tabbar for Emacs
(use-package tab-line
  :if (not (version< emacs-version "27.0"))
  :preface (push "~/.emacs.d/etc/images/" image-load-path)
  ;; :hook (after-init . global-tab-line-mode)
  :init (tabbar-mode -1)
  :custom ((tab-line-close-button-show t)
           (tab-line-new-button-show   nil)
           (tab-line-separator         ""))
  :config (progn
            (defun tab-line-close-tab (&optional e)
              "Close the selected tab.
If tab is presented in another window, close the tab by using
`bury-buffer` function.  If tab is unique to all existing
windows, kill the buffer with `kill-buffer` function.  Lastly, if
no tabs left in the window, it is deleted with `delete-window`
function."
              (interactive "e")
              (let* ((posnp (event-start e))
                     (window (posn-window posnp))
                     (buffer (get-pos-property 1 'tab (car (posn-string posnp)))))
                (with-selected-window window
                  (let ((tab-list (tab-line-tabs-window-buffers))
                        (buffer-list (flatten-list
                                      (seq-reduce (lambda (list window)
                                                    (select-window window t)
                                                    (cons (tab-line-tabs-window-buffers) list))
                                                  (window-list) nil))))
                    (select-window window)
                    (if (> (seq-count (lambda (b) (eq b buffer)) buffer-list) 1)
                        (progn
                          (if (eq buffer (current-buffer))
                              (bury-buffer)
                            (set-window-prev-buffers window (assq-delete-all buffer (window-prev-buffers)))
                            (set-window-next-buffers window (delq buffer (window-next-buffers))))
                          (unless (cdr tab-list)
                            (ignore-errors (delete-window window))))
                      (and (kill-buffer buffer)
                           (unless (cdr tab-list)
                             (ignore-errors (delete-window window)))))))))

            (defcustom tab-line-tab-min-width 10
              "Minimum width of a tab in characters."
              :type 'integer
              :group 'tab-line)

            (defcustom tab-line-tab-max-width 30
              "Maximum width of a tab in characters."
              :type 'integer
              :group 'tab-line)

            (defun my/tab-line-name-buffer (buffer &rest _buffers)
              "Create name for tab with padding and truncation.
If buffer name is shorter than `tab-line-tab-max-width' it gets
centered with spaces, otherwise it is truncated, to preserve
equal width for all tabs.  This function also tries to fit as
many tabs in window as possible, so if there are no room for tabs
with maximum width, it calculates new width for each tab and
truncates text if needed.  Minimal width can be set with
`tab-line-tab-min-width' variable."
              (with-current-buffer buffer
                (let* ((window-width (window-width (get-buffer-window)))
                       (close-button-size (if tab-line-close-button-show
                                              (length (substring-no-properties tab-line-close-button))
                                            0))
                       (tab-amount (length (tab-line-tabs-window-buffers)))
                       (window-max-tab-width (/ window-width tab-amount))
                       (tab-width (- (cond ((>= window-max-tab-width tab-line-tab-max-width)
                                            tab-line-tab-max-width)
                                           ((< window-max-tab-width tab-line-tab-min-width)
                                            tab-line-tab-min-width)
                                           (t window-max-tab-width))
                                     close-button-size))
                       (buffer-name (string-trim (buffer-name)))
                       (name-width (length buffer-name)))
                  (if (>= name-width (- tab-width 3))
                      (concat  " " (truncate-string-to-width buffer-name (- tab-width 3)) "… ")
                    (let* ((padding (make-string (/ (- tab-width name-width) 2) ?\s))
                           (buffer-name (concat padding buffer-name))
                           (name-width (length buffer-name)))
                      (concat buffer-name (make-string (- tab-width name-width) ?\s)))))))
            (setq tab-line-tab-name-function #'my/tab-line-name-buffer)

            (let ((bg (face-attribute 'default :background))
                  (fg (face-attribute 'default :foreground))
                  (base (face-attribute 'mode-line :background))
                  (box-width (/ (line-pixel-height) 4)))
              (when (and (color-defined-p bg)
                         (color-defined-p fg)
                         (color-defined-p base)
                         (numberp box-width))
                (set-face-attribute 'tab-line-tab nil
                                    :foreground fg
                                    :background bg
                                    :weight 'normal
                                    :inherit nil
                                    :box (list :line-width box-width :color bg))
                (set-face-attribute 'tab-line-tab-inactive nil
                                    :foreground fg
                                    :background base
                                    :weight 'normal
                                    :inherit nil
                                    :box (list :line-width box-width :color base))
                (set-face-attribute 'tab-line-tab-current nil
                                    :foreground fg
                                    :background bg
                                    :weight 'normal
                                    :inherit nil
                                    :box (list :line-width box-width :color bg))))

            (dolist (mode '(ediff-mode
                            process-menu-mode
                            term-mode
                            vterm-mode))
              (add-to-list 'tab-line-exclude-modes mode))

            (defun my/tab-line-drop-caches ()
              "Drops `tab-line' cache in every window."
              (dolist (window (window-list))
                (set-window-parameter window 'tab-line-cache nil)))
            (add-hook 'window-configuration-change-hook #'my/tab-line-drop-caches)

            (if (display-graphic-p)
                (progn
                  (setq tab-line-new-button
                        (propertize " + "
                                    'display `(image :type xpm
                                                     :file ,(image-search-load-path
                                                             "new@2x.xpm")
                                                     :margin (2 . 0)
                                                     :ascent center
                                                     :scale 0.5)
                                    'keymap tab-line-add-map
                                    'mouse-face 'tab-line-highlight
                                    'help-echo "Click to add tab"))

                  (setq tab-line-close-button
                        (propertize " x"
                                    'display `(image :type xpm
                                                     :file ,(image-search-load-path
                                                             "close@2x.xpm")
                                                     :margin (2 . 0)
                                                     :ascent center
                                                     :scale 0.5)
                                    'keymap tab-line-tab-close-map
                                    'mouse-face 'tab-line-close-highlight
                                    'help-echo "Click to close tab"))

                  (setq tab-line-left-button
                        (propertize " <"
                                    'display `(image :type xpm
                                                     :file ,(image-search-load-path
                                                             "left-arrow@2x.xpm")
                                                     :margin (2 . 0)
                                                     :ascent center
                                                     :scale 0.5)
                                    'keymap tab-line-left-map
                                    'mouse-face 'tab-line-highlight
                                    'help-echo "Click to scroll left"))

                  (setq tab-line-right-button
                        (propertize "> "
                                    'display `(image :type xpm
                                                     :file ,(image-search-load-path
                                                             "right-arrow@2x.xpm")
                                                     :margin (2 . 0)
                                                     :ascent center
                                                     :scale 0.5)
                                    'keymap tab-line-right-map
                                    'mouse-face 'tab-line-highlight
                                    'help-echo "Click to scroll right")))
              (progn
                (setq tab-line-right-button (propertize (if (char-displayable-p ?▶) " ▶ " " > ")
                                                        'keymap tab-line-right-map
                                                        'mouse-face 'tab-line-highlight
                                                        'help-echo "Click to scroll right")
                      tab-line-left-button (propertize (if (char-displayable-p ?◀) " ◀ " " < ")
                                                       'keymap tab-line-left-map
                                                       'mouse-face 'tab-line-highlight
                                                       'help-echo "Click to scroll left")
                      tab-line-close-button (propertize (if (char-displayable-p ?×) "× " "x ")
                                                        'keymap tab-line-tab-close-map
                                                        'mouse-face 'tab-line-close-highlight
                                                        'help-echo "Click to close tab"))))))

;; Configuring tabs with centaur-tabs
(use-package centaur-tabs
  :if (not (version< emacs-version "27.0"))
  :custom
  ((centaur-tabs-style       "rounded")
   (centaur-tabs-set-bar     'left)
   (centaur-tabs-height       24)
   (centaur-tabs-cycle-scope 'groups)
   (centaur-tabs-show-navigation-buttons t)
   (centaur-tabs-set-icons    t))
  :init (progn
          (tabbar-mode -1)
          (defun centaur-tabs nil)
          (defun  centaur-tabs-buffer-groups  ()
            "`centaur-tabs-buffer-groups'control buffers'group rules.
    Group centaur-tabs with mode if buffer is derived from `eshell-mode'
    `emacs-lisp-mode'`dired-mode'`org-mode'`magit-mode'.
    All buffer name start with * will group to  \" Emacs \" .
    Other buffer group by `centaur-tabs-get-group-name'with project name."
            (list
             (cond
              ((or (string-equal "*" (substring (buffer-name) 0 1))
                   (memq major-mode '(magit-process-mode
                                      magit-status-mode
                                      magit-diff-mode
                                      magit-log-mode
                                      magit-file-mode
                                      magit-blob-mode
                                      magit-blame-mode
                                      )))
               "Emacs")
              ((derived-mode-p 'prog-mode)
               "Editing")
              ((derived-mode-p 'dired-mode)
               "Dired")
              ((derived-mode-p 'vterm-mode)
               "Terminal")
              ((memq major-mode '(helpful-mode
                                  help-mode))
               "Help")
              ((memq major-mode '(org-mode
                                  org-agenda-clockreport-mode
                                  org-src-mode
                                  org-agenda-mode
                                  org-beamer-mode
                                  org-indent-mode
                                  org-bullets-fashion
                                  org-cdlatex-mode
                                  org-agenda-log-mode
                                  diary-mode))
               "OrgMode")
              (t
               (centaur-tabs-get-group-name  (current-buffer)))))))
  :hook ((recentf-mode . centaur-tabs-local-mode)
         (dashboard-mode .centaur-tabs-local-mode)
         (term-mode . centaur-tabs-local-mode)
         (calendar-mode . centaur-tabs-local-mode)
         (org-agenda-mode . centaur-tabs-local-mode)
         (helpful-mode.centaur -tabs-local-mode))
  :config (progn
            (centaur-tabs-headline-match)
            (centaur-tabs-group-by-projectile-project)
            (centaur-tabs-mode t)

            ;; Tabbar
            (global-set-key [C-prior]             'centaur-tabs-backward-tab)
            (global-set-key [C-next]              'centaur-tabs-forward-tab)
            (global-set-key [C-home]              'centaur-tabs-backward-group)
            (global-set-key [C-end]               'centaur-tabs-forward-group)

            ;; Tabbar, now using ctl-x-map
            (global-set-key (kbd "C-x <prior>")   'centaur-tabs-backward-tab)
            (global-set-key (kbd "C-x <next>")    'centaur-tabs-forward-tab)
            (global-set-key (kbd "C-x <home>")    'centaur-tabs-backward-group)
            (global-set-key (kbd "C-x <end>")     'centaur-tabs-forward-group)

            ;; Tabbar Ctrl-x mappings
            (define-key ctl-x-map (kbd "<prior>") 'centaur-tabs-backward-tab)
            (define-key ctl-x-map (kbd "<next>")  'centaur-tabs-forward-tab)
            (define-key ctl-x-map (kbd "<home>")  'centaur-tabs-backward-group)
            (define-key ctl-x-map (kbd "<end>")   'centaur-tabs-forward-group)))

(provide 'setup-tabbar)
;;; setup-tabbar.el ends here

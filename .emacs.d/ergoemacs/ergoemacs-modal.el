;;; ergoemacs-modal.el --- Modal Editing commands
;; 
;; Filename: ergoemacs-modal.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Sat Sep 28 20:03:23 2013 (-0500)
;; Version:
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL:
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defgroup ergoemacs-modal nil
  "Modal ergoemacs"
  :group 'ergoemacs-mode)

(defcustom ergoemacs-modal-ignored-buffers
  '("^ \\*load\\*" "^[*]e?shell[*]" "^[*]R.*[*]$")
  "Regular expression of bufferst that should come up in
ErgoEmacs state, regardless of if a modal state is currently
enabled."
  :type '(repeat string)
  :group 'ergoemacs-modal)

(defvar ergoemacs-default-cursor nil
  "The default cursor color.
This should be reset every time that the modal cursor changes color.  Otherwise this will be nil
A color string as passed to `set-cursor-color'.")

(defcustom ergoemacs-modal-emacs-state-modes
  '(archive-mode
    bbdb-mode
    bookmark-bmenu-mode
    bookmark-edit-annotation-mode
    browse-kill-ring-mode
    bzr-annotate-mode
    calc-mode
    cfw:calendar-mode
    completion-list-mode
    Custom-mode
    debugger-mode
    delicious-search-mode
    desktop-menu-blist-mode
    desktop-menu-mode
    doc-view-mode
    dvc-bookmarks-mode
    dvc-diff-mode
    dvc-info-buffer-mode
    dvc-log-buffer-mode
    dvc-revlist-mode
    dvc-revlog-mode
    dvc-status-mode
    dvc-tips-mode
    ediff-mode
    ediff-meta-mode
    efs-mode
    Electric-buffer-menu-mode
    emms-browser-mode
    emms-mark-mode
    emms-metaplaylist-mode
    emms-playlist-mode
    etags-select-mode
    fj-mode
    gc-issues-mode
    gdb-breakpoints-mode
    gdb-disassembly-mode
    gdb-frames-mode
    gdb-locals-mode
    gdb-memory-mode
    gdb-registers-mode
    gdb-threads-mode
    gist-list-mode
    gnus-article-mode
    gnus-browse-mode
    gnus-group-mode
    gnus-server-mode
    gnus-summary-mode
    google-maps-static-mode
    ibuffer-mode
    jde-javadoc-checker-report-mode
    magit-commit-mode
    magit-diff-mode
    magit-key-mode
    magit-log-mode
    magit-mode
    magit-reflog-mode
    magit-show-branches-mode
    magit-branch-manager-mode ;; New name for magit-show-branches-mode
    magit-stash-mode
    magit-status-mode
    magit-wazzup-mode
    mh-folder-mode
    monky-mode
    notmuch-hello-mode
    notmuch-search-mode
    notmuch-show-mode
    occur-mode
    org-agenda-mode
    package-menu-mode
    proced-mode
    rcirc-mode
    rebase-mode
    recentf-dialog-mode
    reftex-select-bib-mode
    reftex-select-label-mode
    reftex-toc-mode
    sldb-mode
    slime-inspector-mode
    slime-thread-control-mode
    slime-xref-mode
    shell-mode
    sr-buttons-mode
    sr-mode
    sr-tree-mode
    sr-virtual-mode
    tar-mode
    tetris-mode
    tla-annotate-mode
    tla-archive-list-mode
    tla-bconfig-mode
    tla-bookmarks-mode
    tla-branch-list-mode
    tla-browse-mode
    tla-category-list-mode
    tla-changelog-mode
    tla-follow-symlinks-mode
    tla-inventory-file-mode
    tla-inventory-mode
    tla-lint-mode
    tla-logs-mode
    tla-revision-list-mode
    tla-revlog-mode
    tla-tree-lint-mode
    tla-version-list-mode
    twittering-mode
    urlview-mode
    vc-annotate-mode
    vc-dir-mode
    vc-git-log-view-mode
    vc-svn-log-view-mode
    vm-mode
    vm-summary-mode
    w3m-mode
    wab-compilation-mode
    xgit-annotate-mode
    xgit-changelog-mode
    xgit-diff-mode
    xgit-revlog-mode
    xhg-annotate-mode
    xhg-log-mode
    xhg-mode
    xhg-mq-mode
    xhg-mq-sub-mode
    xhg-status-extra-mode)
  "Modes that should come up in ErgoEmacs state, regardless of if a
modal state is currently enabled."
  :type  '(repeat symbol)
  :group 'ergoemacs-modal)

(defvar ergoemacs-exit-temp-map-var nil)

(defun ergoemacs-minibuffer-setup ()
  "Exit temporary overlay maps."
  ;; (setq ergoemacs-exit-temp-map-var t)
  (ergoemacs-debug-heading "ergoemacs-minibuffer-setup")
  (ergoemacs-debug "emulation-mode-map-alists: %s" emulation-mode-map-alists)
  (ergoemacs-debug "ergoemacs-emulation-mode-map-alist: %s"
                   (mapcar
                    (lambda(x) (nth 0 x))
                    ergoemacs-emulation-mode-map-alist))
  (ergoemacs-debug "minor-mode-map-alist: %s"
                   (mapcar
                    (lambda(x) (nth 0 x))
                    minor-mode-map-alist))
  ;; (setq ergoemacs-shortcut-keys t)
  (ergoemacs-debug "ergoemacs-shortcut-keys: %s" ergoemacs-shortcut-keys)
  (ergoemacs-debug "ergoemacs-shortcut-override-mode: %s" ergoemacs-shortcut-override-mode)
  (ergoemacs-debug "ergoemacs-mode: %s" ergoemacs-mode)
  (ergoemacs-debug "ergoemacs-unbind-keys: %s" ergoemacs-unbind-keys))

(defun ergoemacs-modal-p ()
  "Determine if the command should be modal.
If so return the hash of translation values."
  (if (not ergoemacs-modal-list) nil
    (let* ((type (nth 0 ergoemacs-modal-list))
           (hash (gethash type ergoemacs-translations))
           (always (plist-get hash ':modal-always))
           (ret hash))
      (cond
       ((and (minibufferp)
             (not always))
        (setq ret nil))
       ((and (not always)
             (memq major-mode ergoemacs-modal-emacs-state-modes))
        (setq ret nil))
       ((and (not always)
             (catch 'match-modal
               (mapc
                (lambda(reg)
                  (when (string-match reg (buffer-name))
                    (throw 'match-modal t)))
                ergoemacs-modal-ignored-buffers)
               nil))
        (setq ret nil))
       ((and (not always)
             (lookup-key ergoemacs-modal-ignored-keymap
                         (or ergoemacs-single-command-keys (this-single-command-keys))))
        (setq ret nil)))
      (symbol-value 'ret))))

(defun ergoemacs-modal-default ()
  "The default command for `ergoemacs-mode' modal.
It sends `this-single-command-keys' to `ergoemacs-read-key' with
the translation type defined by `ergoemacs-modal-list' as long as it should."
  (interactive)
  (let* ((type (nth 0 ergoemacs-modal-list))
         (hash (gethash type ergoemacs-translations))
         tmp
         (always (plist-get hash ':modal-always)))
    (when (not (ergoemacs-modal-p))
      (setq type nil))
    ;; Actual call
    (ergoemacs-read-key
     (or ergoemacs-single-command-keys (this-single-command-keys))
     type
     type)
    ;; Fix cursor color and mode-line
    (cond
     ((ergoemacs-modal-p)
      (setq tmp (plist-get hash ':modal-color))
      (if tmp
          (set-cursor-color tmp)
        (when ergoemacs-default-cursor
          (set-cursor-color ergoemacs-default-cursor)))
      (setq tmp (if ergoemacs-modal-list (gethash (nth 0 ergoemacs-modal-list) ergoemacs-translation-text) nil))
      (if tmp
          (ergoemacs-mode-line ;; Indicate Alt+ in mode-line
           (concat " " (nth 5 tmp)))
        (ergoemacs-mode-line)))
     (t
      (when ergoemacs-default-cursor
        (set-cursor-color ergoemacs-default-cursor))
      (ergoemacs-mode-line)))))
(put 'ergoemacs-modal-default 'CUA 'move) ;; Fake movement command

(defvar ergoemacs-modal-save nil)
(defvar ergoemacs-modal nil
  "If ergoemacs modal and what translation is active.")

(defvar ergoemacs-modal-ignored-keymap
  (let ((ret (make-sparse-keymap)))
    (mapc
     (lambda(char)
       (mapc
        (lambda(mod)
          (let ((key (read-kbd-macro (concat mod char))))
            (unless (lookup-key ret key)
              (define-key ret key 'ergoemacs-modal-default))))
        '("" "C-" "C-S-" "M-" "M-S-" "C-M-" "C-M-S-")))
     '("<f1>" 
       "<f2>" 
       "<f3>" 
       "<f4>" 
       "<f5>" 
       "<f6>" 
       "<f7>" 
       "<f8>" 
       "<f9>" 
       "<f10>"
       "<f11>"
       "<f12>"
       "<apps>" "<menu>"
       "RET" "ESC" "DEL" "TAB"
       "<home>" 
       "<next>" 
       "<prior>"
       "<end>"
       "<insert>"
       "<deletechar>"))
    ret)
  "`ergoemacs-mode' keys to ignore the modal translation.
Typically function keys")

(defvar ergoemacs-modal-keymap nil
  "`ergoemacs-mode' modal keymap.  Attempts to capture ALL keystrokes.")

(defun ergoemacs-modal-keymap  (&optional map)
  "Returns the ergoemacs-modal keymap"
  (if ergoemacs-modal-keymap
      (if map
          (make-composed-keymap (list map ergoemacs-modal-keymap))
        ergoemacs-modal-keymap)
    (let ((ret (make-sparse-keymap)))
      (unless ret
        (setq ret (make-sparse-keymap)))
      (mapc
       (lambda(lay)
         (mapc
          (lambda(char)
            (unless (string= char "")
              (mapc
               (lambda(mod)
                 (let ((key (read-kbd-macro
                             (ergoemacs-translate-shifted
                              (concat mod char)))))
                   (unless (lookup-key ret key)
                     (define-key ret key 'ergoemacs-modal-default))))
               '("" "C-" "M-" "C-M-"))))
          (symbol-value (intern (concat "ergoemacs-layout-" lay)))))
       (ergoemacs-get-layouts))
      (mapc
       (lambda(char)
         (mapc
          (lambda(mod)
            (let ((key (read-kbd-macro (concat mod char))))
              (unless (lookup-key ret key)
                (define-key ret key 'ergoemacs-modal-default))))
          '("" "C-" "C-S-" "M-" "M-S-" "C-M-" "C-M-S-")))
       '("<f1>" 
         "<f2>" 
         "<f3>" 
         "<f4>" 
         "<f5>" 
         "<f6>" 
         "<f7>" 
         "<f8>" 
         "<f9>" 
         "<f10>"
         "<f11>"
         "<f12>"
         "<apps>" "<menu>"
         "SPC" "RET" "ESC" "DEL" "TAB"
         "<home>" 
         "<next>" 
         "<prior>"
         "<end>"
         "<insert>"
         "<deletechar>"))
      (setq ergoemacs-modal-keymap ret))
    (ergoemacs-modal-keymap map)))

(defvar ergoemacs-modal-list '())
(defun ergoemacs-modal-toggle (type)
  "Toggle ergoemacs command modes."
  (let* ((x (assq 'ergoemacs-modal ergoemacs-emulation-mode-map-alist))
         (help-list (gethash type ergoemacs-translation-text))
         keymap
         (type type)
         tmp
         (no-ergoemacs-advice t))
    (setq ergoemacs-emulation-mode-map-alist
          (delq x ergoemacs-emulation-mode-map-alist))
    (cond
     ((or (not ergoemacs-modal-list) ;; First time to turn on
          (not (eq (nth 0 ergoemacs-modal-list) type)) ;; New modal 
          )
      (push type ergoemacs-modal-list)
      (setq keymap (make-composed-keymap
                    (list (ergoemacs-local-map type t)
                          (ergoemacs-modal-keymap))))
      (push (cons 'ergoemacs-modal keymap)
            ergoemacs-emulation-mode-map-alist)
      (set-default 'ergoemacs-modal type)
      (setq ergoemacs-modal type)
      (unless ergoemacs-default-cursor
        (setq ergoemacs-default-cursor
              (or (frame-parameter nil 'cursor-color) "black")))
      (let ((hash (gethash type ergoemacs-translations))
            tmp)
        (when hash
          (setq tmp (plist-get hash ':modal-color))
          (when tmp
            (set-cursor-color tmp))))
      (if help-list
          (ergoemacs-mode-line ;; Indicate Alt+ in mode-line
           (concat " " (nth 5 help-list)))
        (ergoemacs-mode-line))
      (let (message-log-max)
        (if help-list
            (message "%s command mode installed" (nth 5 help-list)))))
     (t ;; Turn off.
      (setq tmp (pop ergoemacs-modal-list))
      (when (eq tmp type)
        (if (not ergoemacs-modal-list)
            (setq type nil)
          (setq type (nth 0 ergoemacs-modal-list))))
      (if type
          (progn ;; Turn off current modal, turn on last modal.
            (setq help-list (gethash type ergoemacs-translation-text))
            (setq keymap
                  (make-composed-keymap
                   (list (ergoemacs-local-map type t)
                         (ergoemacs-modal-keymap))))
            (push (cons 'ergoemacs-modal keymap)
                  ergoemacs-emulation-mode-map-alist)
            (set-default 'ergoemacs-modal type)
            (setq ergoemacs-modal type)
            (unless ergoemacs-default-cursor
              (setq ergoemacs-default-cursor
                    (or (frame-parameter nil 'cursor-color) "black")))
            (let ((hash (gethash type ergoemacs-translations))
                  tmp)
              (when hash
                (setq tmp (plist-get hash ':modal-color))
                (when tmp
                  (set-cursor-color tmp))))
            (if help-list
                (ergoemacs-mode-line ;; Indicate Alt+ in mode-line
                 (concat " " (nth 5 help-list)))
              (ergoemacs-mode-line))
            (let (message-log-max)
              (if help-list
                  (message "%s command mode resumed." (nth 5 help-list)))))
        ;; Turn of ergoemacs-modal
        (when ergoemacs-default-cursor
          (set-cursor-color ergoemacs-default-cursor))
        (let (message-log-max)
          (message "Full %s command mode removed." (if help-list (nth 5 help-list) "")))
        (set-default 'ergoemacs-modal nil)
        (setq ergoemacs-modal nil)
        (setq ergoemacs-modal-save nil)
        (ergoemacs-mode-line))))))

(provide 'ergoemacs-modal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-modal.el ends here

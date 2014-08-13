;;; ergoemacs-advices.el --- advices for ErgoEmacs

;; Copyright (C) 2013 Matthew L. Fidler

;; Maintainer: Matthew L. Fidler
;; Keywords: convenience

;; ErgoEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; ErgoEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ErgoEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;; Todo:

;; 

;;; Code:

(defmacro ergoemacs-define-overrides (&rest body)
  "Force the define-keys to work"
  `(let ((ergoemacs-run-mode-hooks t))
     ,@body))

(defadvice add-hook (around ergoemacs-add-hook-advice (hook function &optional append  local))
  "Advice to allow `this-command' to be set correctly before running `pre-command-hook'
If `pre-command-hook' is used and `ergoemacs-mode' is enabled add to `ergoemacs-pre-command-hook' instead."
  (cond
   ((and ergoemacs-mode (eq hook 'pre-command-hook)
         (memq hook ergoemacs-hook-functions))
    (add-hook 'ergoemacs-pre-command-hook function append local))
   (t
    ad-do-it)))
(ad-activate 'add-hook)

(defadvice remove-hook (around ergoemacs-remove-hook-advice (hook function &optional local))
  "Advice to allow `this-command' to be set correctly before running `pre-command-hook'.
If `pre-command-hook' is used and `ergoemacs-mode' is remove from `ergoemacs-pre-command-hook' instead."
  (cond
   ((and ergoemacs-mode (eq hook 'pre-command-hook)
         (memq hook ergoemacs-hook-functions))
    (remove-hook 'ergoemacs-pre-command-hook function local))
   (t
    ad-do-it)))
(ad-activate 'remove-hook)
  

(defadvice define-key (around ergoemacs-define-key-advice (keymap key def))
  "This does the right thing when modifying `ergoemacs-keymap'.
Also adds keymap-flag for user-defined keys run with `run-mode-hooks'."
  (if (and (boundp 'ergoemacs-run-mode-hooks) ergoemacs-run-mode-hooks
           (not (equal keymap (current-global-map)))
           (not (equal keymap ergoemacs-keymap)))
      (let ((no-ergoemacs-advice t)
            (ergoemacs-run-mode-hooks nil)
            (new-key (read-kbd-macro
                      (format "<ergoemacs-user> %s"
                              (key-description key)))))
        (unwind-protect
            (define-key keymap new-key def))))
  (if (and (equal keymap 'ergoemacs-keymap)
           (or (not (boundp 'no-ergoemacs-advice))
               (and (boundp 'no-ergoemacs-advice) (not no-ergoemacs-advice))))
      (progn
        (message "Define Key Advice %s %s" key def)
        (let ((found))
          (set (ergoemacs-get-fixed-layout)
               (mapcar
                (lambda(x)
                  (if (not (or (and (type-of (nth 0 x) 'string)
                                    (string= (key-description
                                              (condition-case err
                                                  (read-kbd-macro (nth 0 x))
                                                (error
                                                 (read-kbd-macro (encode-coding-string (nth 0 x) locale-coding-system)))))
                                             (key-description key)))
                               (and (not (type-of (nth 0 x) 'string))
                                    (string= (key-description (nth 0 x)) (key-description key)))))
                      x
                    (setq found t)
                    `(,(nth 0 x) ,command "")))
                (symbol-value (ergoemacs-get-fixed-layout))))
          (unless found
            (set (ergoemacs-get-variable-layout)
                 (mapcar
                  (lambda(x)
                    (if (not (and (type-of (nth 0 x) 'string)
                                  (string= (key-description (ergoemacs-kbd (nth 0 x) nil (nth 3 x))) (key-description key))))
                        x
                      (setq found t)
                      ;; Assume the complete sequence is translated?
                      `(,(nth 0 x) ,command "")))
                  (symbol-value (ergoemacs-get-variable-layout)))))
          (unless found
            (add-to-list (ergoemacs-get-fixed-layout) `(,(key-description key) ,command ""))))
        (message "Only changed ergoemacs-keybinding for current theme, %s" (or ergoemacs-theme "which happens to be the default key-binding"))
        (when (and (boundp 'ergoemacs-mode) ergoemacs-mode)
          (let ((no-ergoemacs-advice t))
            (define-key ergoemacs-keymap key def))))
    ad-do-it)
  (when (or (equal keymap (current-global-map ))
            (equal keymap global-map))
    (ergoemacs-global-set-key-after key def)))
(ad-activate 'define-key)

(defvar ergoemacs-global-override-keymap (make-sparse-keymap))
;;; Advices enabled or disabled with ergoemacs-mode
(defun ergoemacs-global-set-key-after (key command)
  (add-to-list 'ergoemacs-global-changed-cache (key-description key))
  (when ergoemacs-global-not-changed-cache
    (delete (key-description key) ergoemacs-global-not-changed-cache))
  (let ((no-ergoemacs-advice t))
    ;; Put in the overriding keymap
    (define-key ergoemacs-global-override-keymap key command)
    (when (condition-case err
              (interactive-form (lookup-key ergoemacs-unbind-keymap key))
            (error nil))
      (define-key ergoemacs-unbind-keymap key nil)
      (unless (string-match "^C-[xc]" (key-description key))
        (define-key ergoemacs-shortcut-keymap key nil)))
    (define-key ergoemacs-keymap key nil))
  (let ((x (assq 'ergoemacs-shortcut-keys ergoemacs-emulation-mode-map-alist)))
    (when x
      (setq ergoemacs-emulation-mode-map-alist (delq x ergoemacs-emulation-mode-map-alist)))
    (push (cons 'ergoemacs-shortcut-keys ergoemacs-shortcut-keymap) ergoemacs-emulation-mode-map-alist)))
(defadvice global-set-key (around ergoemacs-global-set-key-advice (key command))
  "This let you use `global-set-key' as usual when `ergoemacs-mode' is enabled."
  ad-do-it
  (ergoemacs-global-set-key-after key command))

(add-to-list 'ergoemacs-advices 'ergoemacs-global-set-key-advice)

(defadvice global-unset-key (around ergoemacs-global-unset-key-advice (key))
  "This let you use `global-unset-key' as usual when `ergoemacs-mode' is enabled."
  ;; the global-unset-key will remove the key from ergoemacs as well.
  ad-do-it
  (ergoemacs-global-set-key-after key nil))

(add-to-list 'ergoemacs-advices 'ergoemacs-global-unset-key-advice)

(defadvice local-set-key (around ergoemacs-local-set-key-advice (key command))
  "This let you use `local-set-key' as usual when `ergoemacs-mode' is enabled."
  (if (and (fboundp 'ergoemacs-mode) ergoemacs-mode)
      (ergoemacs-local-set-key key command)
    ad-do-it))

(add-to-list 'ergoemacs-advices 'ergoemacs-local-set-key-advice)

(defadvice local-unset-key (around ergoemacs-local-unset-key-advice (key))
  "This let you use `local-unset-key' as usual when `ergoemacs-mode' is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-local-unset-key key)
    ad-do-it))

(add-to-list 'ergoemacs-advices 'ergoemacs-local-unset-key-advice)

(eval-after-load "helm"
  '(progn
     ;; (defadvice helm-M-x (around ergoemacs-helm-M-x-keys)
;;        "Translates Helm M-x keys to ergoemacs style bindings."
;;        (flet ((helm-M-x-transformer
;;                (candidates sources)
;;                "filtered-candidate-transformer to show bindings in emacs commands.
;; Show global bindings and local bindings according to current `major-mode'."
;;                (with-helm-current-buffer
;;                  (loop with local-map = (helm-M-x-current-mode-map-alist)
;;                        for cand in candidates
;;                        for local-key  = (car (rassq cand local-map))
;;                        for key        = (substitute-command-keys (format "\\[%s]" cand))
;;                        collect
;;                        (cons (cond ((and (string-match "^M-x" key) local-key)
;;                                     (format "%s (%s)"
;;                                             cand (propertize
;;                                                   (if (and ergoemacs-use-ergoemacs-key-descriptions ergoemacs-mode)
;;                                                       (ergoemacs-pretty-key local-key)
;;                                                     local-key)
;;                                                   'face 'helm-M-x-key)))
;;                                    ((string-match "^M-x" key) cand)
;;                                    (t (format "%s (%s)"
;;                                               cand (propertize
;;                                                     (if (and ergoemacs-use-ergoemacs-key-descriptions ergoemacs-mode)
;;                                                         (ergoemacs-pretty-key key)
;;                                                       key)
;;                                                     'face 'helm-M-x-key))))
;;                              cand) into ls
;;                              finally return
;;                              (sort ls #'helm-command-M-x-sort-fn)))))
;;          ad-do-it))

     ;; (ad-activate 'helm-M-x)
     ))


(defadvice cua-mode (around ergoemacs-activate-only-selection-mode (arg))
  "When `ergoemacs-mode' is enabled, enable `cua-selection-mode' instead of plain `cua-mode'."
  (when ergoemacs-mode
    (setq-default cua-enable-cua-keys nil))
  ad-do-it
  (when ergoemacs-mode
    (customize-mark-as-set 'cua-enable-cua-keys)))

(ad-activate 'cua-mode)

(defadvice icicle-mode (around ergoemacs-icicle-play (arg))
  "Allow `ergoemacs-mode' to play nicely with `icicle-mode'."
  (let ((oee ergoemacs-mode))
    (when oee ;; Remove key bindings
      (ergoemacs-mode -1))
    ad-do-it
    (when oee ;; Add them back.  Now icy-mode should play nice.
      (ergoemacs-mode 1))))

(ad-activate 'icicle-mode)

(defcustom ergoemacs-helm-expand-user-dirs 't
  "Expand user directories under helm.
This makes helm behave more like `ido-find-file'"
  :group 'ergoemacs-mode
  :type 'boolean)

(eval-after-load "helm-files"
  '(progn
    (defadvice helm-ff-auto-expand-to-home-or-root (around ergoemacs-helm-ido-user-dirs)
      "Allow `helm-find-files' to expand user directories.
For example ~ergoemacs/ would expand to /usr/ergoemacs or
whatever that points to...

This require `ergoemacs-mode' to be enabled as well as
`ergoemacs-helm-expand-user-dirs' to be true.
"
      (cond
       ((and ergoemacs-helm-expand-user-dirs
             ergoemacs-mode
             (helm-file-completion-source-p)
             (string-match "/\\(~[^/]*/\\)$" helm-pattern)
             (with-current-buffer (window-buffer (minibuffer-window)) (eolp))
             (not (string-match helm-ff-url-regexp helm-pattern)))
        (let ((input (match-string 1 helm-pattern)))
          (if (file-directory-p input)
              (setq helm-ff-default-directory
                    (setq input (file-name-as-directory input)))
            (setq helm-ff-default-directory (file-name-as-directory
                                             (file-name-directory input))))
          (with-helm-window
            (helm-set-pattern input)
            (helm-check-minibuffer-input))))
       (t
        ad-do-it)))
    (ad-activate 'helm-ff-auto-expand-to-home-or-root)))


(defadvice run-mode-hooks (around ergoemacs-run-hooks)
  "`ergoemacs-mode' run-hooks advice helps user define keys properly.
This assumes any key defined while running a hook is a user-defined hook."
  (let ((ergoemacs-run-mode-hooks t))
    ad-do-it))
(ad-activate 'run-mode-hooks)

(defadvice turn-on-undo-tree-mode (around ergoemacs-undo-tree-mode)
  "Make `ergoemacs-mode' and undo-tree compatible."
  (ergoemacs-with-global
   ad-do-it))
(ad-activate 'turn-on-undo-tree-mode)

(provide 'ergoemacs-advices)
;;;;;;;;;;;;;;;;;;;;;;;;`';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-advices.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:

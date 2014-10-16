;;; setup-js2-mode.el --- tweak js2 settings -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/multiple-cursors")
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(add-to-list 'load-path "~/.emacs.d/js2-refactor")
(add-to-list 'load-path "~/.emacs.d/emacs-web-server")
(add-to-list 'load-path "~/.emacs.d/skewer-mode")
(add-to-list 'load-path "~/.emacs.d/ac-js2")

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))
(add-hook 'js2-mode-hook 'skewer-mode)
(setq ac-js2-evaluate-calls t)

(setq-default js2-allow-rhino-new-expr-initializer nil)
(setq-default js2-auto-indent-p nil)
(setq-default js2-enter-indents-newline nil)
(setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
(setq-default js2-idle-timer-delay 0.1)
(setq-default js2-indent-on-enter-key nil)
(setq-default js2-mirror-mode nil)
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-auto-indent-p t)
(setq-default js2-include-rhino-externs nil)
(setq-default js2-include-gears-externs nil)
(setq-default js2-concat-multiline-strings 'eol)
(setq-default js2-rebind-eol-bol-keys nil)

;; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")

(require 'js2-imenu-extras)
(js2-imenu-extras-setup)

(define-key js2-mode-map (kbd "C-c RET jt") 'jump-to-test-file)
(define-key js2-mode-map (kbd "C-c RET ot") 'jump-to-test-file-other-window)
(define-key js2-mode-map (kbd "C-c RET js") 'jump-to-source-file)
(define-key js2-mode-map (kbd "C-c RET os") 'jump-to-source-file-other-window)
(define-key js2-mode-map (kbd "C-c RET jo") 'jump-between-source-and-test-files)
(define-key js2-mode-map (kbd "C-c RET oo") 'jump-between-source-and-test-files-other-window)

(define-key js2-mode-map (kbd "C-c RET dp") 'js2r-duplicate-object-property-node)

(define-key js2-mode-map (kbd "C-c RET ta") 'toggle-assert-refute)

(defadvice js2r-inline-var (after reindent-buffer activate)
  (cleanup-buffer))

(defun js2-hide-test-functions ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors
      (while (re-search-forward "\"[^\"]+\": function (")
        (js2-mode-hide-element)))))

(define-key js2-mode-map (kbd "C-c t") 'js2-hide-test-functions)

;; js2-mode steals TAB, let's steal it back for yasnippet
(defun js2-tab-properly ()
  (interactive)
  (let ((yas-fallback-behavior 'return-nil))
    (unless (yas-expand)
      (indent-for-tab-command)
      (if (looking-back "^\s*")
          (back-to-indentation)))))

(define-key js2-mode-map (kbd "TAB") 'js2-tab-properly)

;; When renaming/deleting js-files, check for corresponding testfile
(define-key js2-mode-map (kbd "C-x C-r") 'js2r-rename-current-buffer-file)
(define-key js2-mode-map (kbd "C-x C-k") 'js2r-delete-current-buffer-file)

;; Use lambda for anonymous functions
(font-lock-add-keywords
 'js2-mode `(("\\(function\\) *("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u0192")
                        nil)))))

;; Use right arrow for return in one-line functions
(font-lock-add-keywords
 'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u2190")
                        nil)))))

;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;; you can;t have a symbol called "someName:false"
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (when (> (buffer-size) 0)
              (let ((btext (replace-regexp-in-string
                            ": *true" " "
                            (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
                (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                      (split-string
                       (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                       " *, *" t))
                ))))

(require 'json)

;; Tern.JS
(add-to-list 'load-path "~/.emacs.d/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
(when (executable-find "npm")
  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))
(eval-after-load 'auto-complete
  '(eval-after-load 'tern
     '(progn
        (require 'tern-auto-complete)
        (tern-ac-setup))))

(defun my-aget (key map)
  (cdr (assoc key map)))

(defun js2-fetch-autolint-externs (file)
  (let* ((settings (with-temp-buffer
                     (insert-file-literally file)
                     (javascript-mode)
                     (let (kill-ring kill-ring-yank-pointer) (kill-comment 1000))
                     (->> (buffer-substring (point-min) (point-max))
                       (s-trim)
                       (s-chop-prefix "module.exports = ")
                       (s-chop-suffix ";")
                       (json-read-from-string))))
         (predef (->> settings
                   (my-aget 'linterOptions)
                   (my-aget 'predef))))
    (--each (append predef nil)
      (add-to-list 'js2-additional-externs it))))

(defun cjsp--eldoc-innards (beg)
  (save-excursion
    (goto-char beg)
    (search-forward "=")
    (let ((start (point)))
      (search-forward "*/")
      (forward-char -2)
      (buffer-substring-no-properties start (point)))))

(defun cjsp--indentation-of-html-line (html line-number)
  (with-temp-buffer
    (insert html)
    (html-mode)
    (indent-region (point-min) (point-max))
    (goto-line line-number)
    (back-to-indentation)
    (current-column)))

(defun cjsp--line-number-in-eldoc (p beg)
  (save-excursion
    (goto-char p)
    (let ((l (line-number-at-pos)))
      (goto-char beg)
      (- l (line-number-at-pos) -1))))

(defun js2-lineup-comment (parse-status)
  "Indent a multi-line block comment continuation line."
  (let* ((beg (nth 8 parse-status))
         (first-line (js2-same-line beg))
         (p (point))
         (offset (save-excursion
                   (goto-char beg)
                   (cond

                    ((looking-at "/\\*:DOC ")
                     (+ 2 (current-column)
                        (cjsp--indentation-of-html-line
                         (cjsp--eldoc-innards beg)
                         (cjsp--line-number-in-eldoc p beg))))

                    ((looking-at "/\\*")
                     (+ 1 (current-column)))

                    (:else 0)))))
    (unless first-line
      (indent-line-to offset))))

;; json files setup
(add-to-list 'load-path "~/.emacs.d/json-reformat")
(add-to-list 'load-path "~/.emacs.d/json-snatcher")
(add-to-list 'load-path "~/.emacs.d/json-mode")
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json?$" . json-mode))

(defun js-mode-bindings ()
  "Sets a hotkey for using the json-snatcher plugin"
  (when (string-match  "\\.json$" (buffer-name))
    (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
(add-hook 'js-mode-hook 'js-mode-bindings)
(add-hook 'js2-mode-hook 'js-mode-bindings)

(provide 'setup-js2-mode)

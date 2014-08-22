;; -*-mode: Emacs-Lisp; -*-
;; Copyright (C) 1996-2014 Abelardo Jara-Berrocal
;; URL: http://pintucoperu.wordpress.com
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/setup")
(add-to-list 'load-path "~/.emacs.d/elp")
(add-to-list 'load-path "~/.emacs.d/pkg-info")
(add-to-list 'load-path "~/.emacs.d/s")
(add-to-list 'load-path "~/.emacs.d/f")
(add-to-list 'load-path "~/.emacs.d/dash")
(add-to-list 'load-path "~/.emacs.d/use-package")
(add-to-list 'load-path "~/.emacs.d/fringe-helper")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-indent-delete-trailing-whitespace-on-save-file t)
 '(auto-indent-delete-trailing-whitespace-on-visit-file t)
 '(cua-enable-cua-keys nil)
 '(custom-safe-themes (quote ("96ec5305ec9f275f61c25341363081df286d616a27a69904a35c9309cfa0fe1b" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "fb4bf07618eab33c89d72ddc238d3c30918a501cf7f086f2edf8f4edba9bd59f" default)))
 '(ecb-auto-activate t)
 '(ecb-display-image-icons-for-semantic-tags t)
 '(ecb-grep-find-function (quote if))
 '(ecb-highlight-tag-with-point (quote highlight-scroll))
 '(ecb-kill-buffer-clears-history (quote auto))
 '(ecb-layout-name "right1")
 '(ecb-layout-window-sizes (quote (("right1" (ecb-directories-buffer-name 0.26737967914438504 . 0.2711864406779661) (ecb-sources-buffer-name 0.26737967914438504 . 0.3559322033898305) (ecb-methods-buffer-name 0.26737967914438504 . 0.3559322033898305)))))
 '(ecb-methods-menu-sorter (lambda (entries) (let ((sorted (copy-list entries))) (sort sorted (quote string-lessp)))))
 '(ecb-options-version "2.40")
 '(ecb-prescan-directories-for-emptyness t)
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(ecb-process-non-semantic-files t)
 '(ecb-redraw-layout-quickly t)
 '(ecb-sources-perform-read-only-check t)
 '(ecb-sources-sort-method (quote extension))
 '(ecb-tree-buffer-style (quote image))
 '(ecb-use-speedbar-instead-native-tree-buffer nil)
 '(ecb-vc-enable-support t)
 '(ede-locate-setup-options (quote (ede-locate-global ede-locate-locate)))
 '(safe-local-variable-values (quote ((encoding . utf-8-unix))))
 '(semantic-self-insert-show-completion-function (lambda nil (semantic-ia-complete-symbol-menu (point)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jedi:highlight-function-argument ((t (:inherit eldoc-highlight-function-argument)))))

;; cl-lib
(require 'cl-lib)
(defun cl--set-getf (plist tag val)
  (let ((p plist))
    (while (and p (not (eq (car p) tag))) (setq p (cdr (cdr p))))
    (if p (progn (setcar (cdr p) val) plist) (list* tag val plist))))

;; Set path environment depending on OS.
(cond
 ;; Add Homebrew path in Darwin
 ((equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path))

 ;; Add Cygwin path in Windows
 ((equal system-type 'windows-nt)
  (when (file-directory-p "c:/cygwin/bin")
    (setenv "PATH" (concat "c:/cygwin/bin:" (getenv "PATH")))
    (add-to-list 'exec-path "c:/cygwin/bin"))
  (if (file-directory-p "c:/cygwin64/bin")
      (setenv "PATH" (concat "c:/cygwin64/bin:" (getenv "PATH")))
    (add-to-list 'exec-path "c:/cygwin64/bin"))))

;; Setup general
(require 'setup-general)

;; Setup appearance
(require 'setup-appearance)

;; CEDET
(add-to-list 'load-path "~/.emacs.d/cedet-bzr")
(add-to-list 'load-path "~/projects/cedet-bzr/contrib")
(require 'cedet-remove-builtin)
(setq byte-compile-warnings nil)
(load-file "~/.emacs.d/cedet-bzr/cedet-devel-load.el")
(load-file "~/.emacs.d/cedet-bzr/contrib/cedet-contrib-load.el")
(global-ede-mode 1)

;; Setup Cedet
(require 'setup-cedet)

;; Setup Etags
(require 'setup-tags)

;; Setup Flycheck
(require 'setup-flycheck)

;; Setup Compile
(require 'setup-compile)

;; Edition of EMACS edition modes
(setq major-mode 'text-mode)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook (function
                           (lambda ()(ispell-minor-mode))))

;; Enter changes lines and auto-indents the new line
(add-hook 'java-mode-hook
          '(lambda ()
             (define-key java-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'js2-mode-hook
          '(lambda ()
             (define-key c-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'c-mode-hook
          '(lambda ()
             (define-key c-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'c++-mode-hook
          '(lambda ()
             (define-key c++-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'vhdl-mode-hook
          '(lambda ()
             (define-key vhdl-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (define-key lisp-mode-map "\C-m" 'reindent-then-newline-and-indent)))

;; Cut the lines at 80 characters; I dont like it but it is a convention
(add-hook 'c++-mode-hook 'turn-on-auto-fill)
(add-hook 'c-mode-hook 'turn-on-auto-fill)
(add-hook 'vhdl-mode-hook 'turn-on-auto-fill)
(add-hook 'python-mode-hook 'turn-on-auto-fill)
(add-hook 'js2-mode-hook 'turn-on-auto-fill)

;; Make a #define be left-aligned
(setq c-electric-pound-behavior (quote (alignleft)))

;; Rainbow delimiters
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(when (require 'rainbow-delimiters nil 'noerror)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; End of line
(require 'eol-conversion)

;; Pretty diff mode
(autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files-remote "ediff"
  "Intelligent Emacs interface to diff")

;; Change type files
(setq auto-mode-alist
      (append '(("\\.cpp$" . c++-mode)
                ("\\.hpp$" . c++-mode)
                ("\\.lsp$" . lisp-mode)
                ("\\.il$" . lisp-mode)
                ("\\.ils$" . lisp-mode)
                ("\\.scm$" . scheme-mode)
                ("\\.pl$" . perl-mode)
                ) auto-mode-alist))

;; Put file name on clip board
(defun put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; ido makes competing buffers and finding files easier
(add-to-list 'load-path "~/.emacs.d/ido-vertical-mode")
(add-to-list 'load-path "~/.emacs.d/flx")
(require 'ido)
(require 'flx-ido)
(require 'ido-vertical-mode)
(ido-mode 'both) ;; for buffers and files
(ido-everywhere 1)
(flx-ido-mode 1)

(setq
 ido-save-directory-list-file "~/.emacs.cache/ido.last"
 ido-ignore-buffers ;; ignore these guys
 '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
   "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
 ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
 ido-case-fold t
 ido-enable-last-directory-history t
 ido-auto-merge-work-directories-length -1
 ido-max-work-directory-list 30
 ido-max-work-file-list 50
 ido-use-filename-at-point nil
 ido-use-url-at-point nil
 ido-enable-flex-matching t
 ido-max-prospects 8
 ido-confirm-unique-completion t)

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

;; Helm
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)

;; Autopair
(add-to-list 'load-path "~/.emacs.d/autopair")
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
(setq autopair-autowrap t)
(put 'autopair-insert-opening 'delete-selection t)
(put 'autopair-skip-close-maybe 'delete-selection t)
(put 'autopair-insert-or-skip-quote 'delete-selection t)
(put 'autopair-extra-insert-opening 'delete-selection t)
(put 'autopair-extra-skip-close-maybe 'delete-selection t)
(put 'autopair-backspace 'delete-selection 'supersede)
(put 'autopair-newline 'delete-selection t)

;; only needed if you use autopair
(add-hook 'text-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))
(add-hook 'org-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))

;; Eldoc
(require 'eldoc)
(require 'eldoc-extension)
(setq eldoc-echo-area-use-multiline-p t)
(setq eldoc-idle-delay 0)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)))
(add-hook 'lisp-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)))
(add-hook 'python-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)))
(add-hook 'js2-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)))

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)
(set-face-attribute 'eldoc-highlight-function-argument nil :underline "red")

;; Filladapt mode for text files
(require 'filladapt)
(add-hook 'text-mode-hook 'turn-on-filladapt-mode)

;; Pretty lambdas
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'lisp-mode-hook 'pretty-lambdas)
(add-to-list 'load-path "~/.emacs.d/pretty-symbols")
(require 'pretty-symbols)
(add-hook 'lisp-mode-hook 'pretty-symbols-mode)
(add-to-list 'load-path "~/.emacs.d/pretty-mode")
(require 'pretty-mode)
(global-pretty-mode t)

;; Yasnippet (should be invoked before auto-complete)
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; Remove Yasnippet's default tab key binding (avoid collision with auto-complete)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;; Select a snippet with popup library
(setq yas-prompt-functions '(yas-popup-prompt yas-no-prompt))

;; Tweaking Yasnippet for Org mode
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

;; Provide headers or templates for new files using Yasnippet
(defun yas/expand-by-uuid (mode uuid)
  "Exapnd snippet template in MODE by its UUID"
  (yas/expand-snippet
   (yas/template-content
    (yas/get-template-by-uuid mode uuid))))

;; Yasnippet templates used in auto-insert mode
(require 'autoinsert)
(auto-insert-mode)
(setq auto-insert-query nil)
(define-auto-insert "\.R"
  '(lambda () (yas/expand-by-uuid 'ess-mode "header")))

;; Get email from Magit if available
(defun yas--magit-email-or-default ()
  "Get email from GIT or use default"
  (if (magit-get-top-dir ".")
      (magit-get "user.email")
    user-mail-address))

;; Auto-Complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(require 'auto-complete)
(ac-config-default)
(setq-default ac-sources '(ac-source-semantic-raw))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
(global-auto-complete-mode t)
(auto-complete-mode 1)
(setq ac-show-menu-immediately-on-auto-complete t)
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; General settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 30
 ac-quick-help-height 30)

;; Make it a bit faster
(setq
 ac-delay 0.5 ;; same as Eclipse
 ac-auto-show-menu 0.5
 ac-quick-help-delay 0.5)

;; this is used for trigger ac actions from org-mode also
(add-to-list 'ac-trigger-commands 'org-self-insert-command)

;; create and add new words to the dictionary on the fly
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories "~/.emacs.cache/ac-dict")
  (setq ac-comphist-file  "~/.emacs.cache/ac-comphist.dat")
  (ac-config-default))

;; Autocomplete with TAGS
(add-to-list 'load-path "~/.emacs.d/auto-complete-etags")
(require 'auto-complete-etags)
(setq ac-etags-use-document t)

;; Let's have snippets and TAGS in the auto-complete dropdown
(defun ac-common-setup ()
  (setq ac-sources (append ac-sources '(ac-source-yasnippet ac-source-etags ac-source-gtags ac-source-semantic ac-source-semantic-raw))))
(add-hook 'auto-complete-mode-hook 'ac-common-setup)

;; We need tell emacs to use aspell, and where your custom dictionary is.
(require 'ispell)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(if (eq system-type 'darwin)
    (if (file-executable-p "/usr/local/bin/aspell")
        (progn
          (setq ispell-program-name "/usr/local/bin/aspell")
          (setq ispell-extra-args '("-d" "/Library/Application Support/cocoAspell/aspell6-en-6.0-0/en.multi")))))

;; change dictionary: "C-c e" = engelska, "C-c s"=spanish, "C-c w"=turn off flyspell
(add-hook 'text-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c s 2")
                            (lambda () (interactive)
                              (ispell-change-dictionary "american")
                              (flyspell-mode 1)
                              (flyspell-buffer)))
             (local-set-key (kbd "C-c s 1")
                            (lambda () (interactive)
                              (ispell-change-dictionary "spanish")
                              (flyspell-mode 1)
                              (flyspell-buffer)))
             (local-set-key (kbd "C-c s 0")
                            (lambda () (interactive)
                              (flyspell-mode -1)))))

;; flyspell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Markdown
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Yaml mode
(add-to-list 'load-path "~/.emacs.d/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Zoom in/out like feature, with mouse wheel
(global-unset-key (kbd "<C-wheel-up>")) ;; moved to <mode-line>
(global-unset-key (kbd "<C-wheel-down>"))
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase) ;; moved to <mode-line>
(global-set-key  (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Zoom in/out like feature, without mouse wheel
(global-set-key '[C-kp-add] 'text-scale-increase)
(global-set-key '[C-kp-subtract] 'text-scale-decrease)
(global-set-key '[C-+] 'text-scale-increase)

;; Quack
(require 'quack)

;; Fix tab problem in some modes that grab the tab key so auto-complete and yasnipet dont work
(defun iy-ac-tab-noconflict ()
  (let ((command (key-binding [tab]))) ; remember command
    (local-unset-key [tab]) ; unset from (kbd "<tab>")
    (local-set-key (kbd "TAB") command))) ; bind to (kbd "TAB")
(add-hook 'ruby-mode-hook 'iy-ac-tab-noconflict)
(add-hook 'markdown-mode-hook 'iy-ac-tab-noconflict)
(add-hook 'org-mode-hook 'iy-ac-tab-noconflict)

;; Time stamp
(setq
 time-stamp-active t          ;; do enable time-stamps
 time-stamp-line-limit 20     ;; check first 10 buffer lines for Time-stamp:
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ;; date format
(add-hook 'write-file-hooks 'time-stamp) ;; update when saving

;; Code folding
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)
(dolist (hook (list 'lisp-mode-hook
                    'python-mode-hook
                    'js2-mode-hook
                    'emacs-lisp-mode-hook
                    'c++-mode-hook))
  (add-hook hook 'hideshowvis-enable))
(global-set-key (kbd "<f7>") 'hs-hide-block)
(global-set-key (kbd "S-<f7>") 'hs-show-block)
(global-set-key (kbd "C-c @ SPC") 'hs-show-block) ; second binding

;; enable `hs-minor-mode' at startup
(add-hook 'emacs-lisp-mode-hook
          (lambda () (hs-minor-mode 1)))
(add-hook 'lisp-mode-hook
          (lambda () (hs-minor-mode 1)))
(add-hook 'python-mode-hook
          (lambda () (hs-minor-mode 1)))
(add-hook 'js2-mode-hook
          (lambda () (hs-minor-mode 1)))

;; Add the following to your .emacs and uncomment it in order to get a + symbol
(define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])

(defcustom hs-fringe-face 'hs-fringe-face
  "*Specify face used to highlight the fringe on hidden regions."
  :type 'face
  :group 'hideshow)

(defface hs-fringe-face
  '((t (:foreground "#999" :box (:line-width 2 :color "grey75" :style released-button))))
  "Face used to highlight the fringe on folded regions"
  :group 'hideshow)

(defcustom hs-face 'hs-face
  "*Specify the face to to use for the hidden region indicator"
  :type 'face
  :group 'hideshow)

(defface hs-face
  '((t (:background "#558" :box t)))
  "Face to hightlight the ... area of hidden regions"
  :group 'hideshow)

(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string))
           (display-string (format "(%d)..." (count-lines (overlay-start ov) (overlay-end ov)))))
      (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
      (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 0 (length display-string) 'face 'hs-face display-string)
      (overlay-put ov 'display display-string))))
(setq hs-set-up-overlay 'display-code-line-counts)

;; Trick emacs when opening a file through menu-find-file-existing
(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil))
    ad-do-it))

;; Windows-like mouse/arrow movement & selection
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)
(cua-mode 1)
(setq shift-select-mode t)

;; As in Windows, replace after typing a letter
(require 'delsel)
(delete-selection-mode 1)
(setq mouse-drag-copy-region nil)

;; Loads latex auto-complete
(add-to-list 'load-path "~/.emacs.d/ac-math")
(require 'ac-math)
(require 'auto-complete-latex)
(add-to-list 'ac-modes 'latex-mode)
(defun ac-latex-mode-setup ()
  (when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))
    (make-local-variable 'ac-sources)
    (setq ac-sources (append '(ac-source-words-in-same-mode-buffers
                               ac-source-dictionary
                               ac-source-math-unicode
                               ac-source-math-latex) ac-sources))))
(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

;; Org mode
(setq load-path (cons "~/.emacs.d/org/lisp" load-path))
(defvar org-list-allow-alphabetical t)
(defun org-element-bold-successor           (arg))
(defun org-element-code-successor           (arg))
(defun org-element-entity-successor         (arg))
(defun org-element-italic-successor         (arg))
(defun org-element-latex-fragment-successor (arg))
(defun org-element-strike-through-successor (arg))
(defun org-element-subscript-successor      (arg))
(defun org-element-superscript-successor    (arg))
(defun org-element-underline-successor      (arg))
(defun org-element-verbatim-successor       (arg))
(require 'org)
(require 'org-list)
(require 'ox-org)
(require 'ox-md)

(let ((todo "~/workspace/Documents/agenda.org"))
  (when (file-readable-p todo)
    (setq org-agenda-files '("~/workspace/Documents/agenda.org"))
    (setq initial-buffer-choice (lambda ()
                                  (org-agenda nil "n")
                                  (delete-other-windows)
                                  (current-buffer)))))
(add-hook 'org-mode-hook 'flyspell-mode)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'ac-modes 'org-mode)
(setq org-startup-folded 'nofold)
(setq org-startup-indented t)
(setq org-startup-with-inline-images t)
(setq org-startup-truncated t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-use-speed-commands t)
(setq org-default-notes-file "~/workspace/Documents/agenda.org")
(setq org-export-with-sub-superscripts nil)

;; Mouse in Org
(require 'org-mouse)

;; Fonts
(add-hook 'org-mode-hook (lambda () (variable-pitch-mode t)))
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit 'fixed-pitch)
(set-face-attribute 'org-block nil :inherit 'fixed-pitch)

(defface org-block-begin-line
  '((t (:inherit org-meta-line
                 :overline "light grey" :foreground "#008ED1")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-end-line
  '((t (:inherit org-meta-line
                 :underline "light grey" :foreground "#008ED1")))
  "Face used for the line delimiting the end of source blocks.")

;; Beamer support
(require 'ox-beamer)

;; Odt export
(require 'ox-odt)

;; Org Templates
(setq org-capture-templates
      '(("t" "Task" entry (file+headline "" "Tasks") "* TODO %?\n  %u\n  %a")
        ("s" "Simple Task" entry (file+headline "" "Tasks") "* TODO %?\n  %U\n")))

(add-to-list 'org-structure-template-alist '("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC\n"))
(add-to-list 'org-structure-template-alist '("S" "#+BEGIN_SRC shell-script\n?\n#+END_SRC\n"))

;; Fix shift problem in Org mode
(setq org-support-shift-select t)
(eval-after-load "org"
  '(progn
     (eval-after-load "cua-base"
       '(progn
          (defadvice org-call-for-shift-select (before org-call-for-shift-select-cua activate)
            (if (and cua-mode
                     org-support-shift-select
                     (not (use-region-p)))
                (cua-set-mark)))))))

;; Fix on the keys
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map [kp-enter] 'org-return-indent)
            (define-key org-mode-map [enter] 'org-return-indent)
            (define-key org-mode-map (kbd "RET") 'org-return-indent)))

;; define todo states: set time stamps one waiting, delegated and done
(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "IN PROGRESS(p!)"
         "HOLD(h!)"
         "WAITING(w)"
         "|"
         "DONE(d!)"
         "CANCELLED(c)")))
(setq org-todo-keyword-faces
      '(
        ("IN PROGRESS" . 'warning)
        ("HOLD" . 'font-lock-keyword-face)
        ("WAITING" . 'font-lock-builtin-face)
        ("CANCELLED" . 'font-lock-doc-face)))

;; PlantUML
(require 'iimage)
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
(add-to-list 'iimage-mode-image-regex-alist '("@startuml\s+\\(.+\\)" . 1))
(add-to-list 'iimage-mode-image-regex-alist (cons (concat "\[\[file:\(~?" iimage-mode-image-filename-regex "\)\]") 1))
(add-hook 'org-mode-hook '(lambda () (org-turn-on-iimage-in-org)))
(setq org-image-actual-width '(400))

;; Rendering plantuml
(setq org-plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar"))
(defun plantuml-render-buffer ()
  (interactive)
  (message "PLANTUML Start rendering")
  (shell-command (concat "java -jar ~/Downloads/plantuml.jar "
                         buffer-file-name))
  (message (concat "PLANTUML Rendered:  " (buffer-name))))

;; Image reloading
(defun reload-image-at-point ()
  (interactive)
  (message "reloading image at point in the current buffer...")
  (image-refresh (get-text-property (point) 'display)))

;; Image resizing and reloading
(defun resize-image-at-point ()
  (interactive)
  (message "resizing image at point in the current buffer...")
  (let* ((image-spec (get-text-property (point) 'display))
         (file (cadr (member :file image-spec))))
    (message (concat "resizing image..." file))
    (shell-command (format "convert -resize %d %s %s "
                           (* (window-width (selected-window)) (frame-char-width))
                           file file))
    (reload-image-at-point)))

;; Function to setup images for display on load
(defun org-turn-on-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (turn-on-iimage-mode)
  (set-face-underline-p 'org-link nil))

;; Function to toggle images in a org buffer
(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (call-interactively 'iimage-mode))

;; Preview LaTeX equations in buffers by showing images (C-c C-x C-l)
(setq org-latex-create-formula-image-program 'imagemagick)

;; Equations in Org
;; Use C-c C-x C-l to regenerate the images
(defvar text-scale-mode-hook nil
  "Hook run at end of command `text-scale-mode'.")

(defadvice text-scale-mode (after text-scale-mode-hooks nil activate)
  "Run `text-scale-mode-hook' at end of command `text-scale-mode'."
  (if (functionp text-scale-mode-hook)
      (funcall text-scale-mode-hook)
    (loop for hook in text-scale-mode-hook do
          (if (eq hook 't)
              (run-hooks (default-value text-scale-mode-hook))
            (run-hooks hook)))))

(defun org-text-scale-eye ()
  "Scale equation images according to text-scale-mode-amount."
  (when (boundp 'text-scale-mode-amount)
    (let ((relwidth (* (expt text-scale-mode-step text-scale-mode-amount))))
      (loop for ol in (overlays-in (point-min) (point-max)) do
            (when (eq (overlay-get ol 'org-overlay-type) 'org-latex-overlay)
              (unless (overlay-get ol 'org-image-original-width)
                (overlay-put ol 'org-image-original-width (car (image-size (overlay-get ol 'display) t))))
              (let ((ol-disp-plist (cdr (overlay-get ol 'display))))
                (setq ol-disp-plist (plist-put ol-disp-plist :type 'imagemagick))
                (setq ol-disp-plist (plist-put ol-disp-plist :width (round (* relwidth (overlay-get ol 'org-image-original-width)))))
                (overlay-put ol 'display (append '(image) ol-disp-plist))))))
    (force-window-update)))
(add-hook 'org-mode-hook '(lambda () (add-hook 'text-scale-mode-hook 'org-text-scale-eye)))

(defadvice org-format-latex (before set-scale activate)
  "Set :scale in `org-format-latex-options' to the scaling factor resulting from `text-scale-mode' and clear cache."
  (let ((relwidth (expt text-scale-mode-step text-scale-mode-amount)))
    (unless (= (plist-get org-format-latex-options :scale) relwidth)
      (plist-put org-format-latex-options :scale relwidth))))

;; Insert images from files #+BEGIN: image :file "~/Documents/personal/foo.png"
(defun org-dblock-write:image (params)
  (let ((file (plist-get params :file)))
    (clear-image-cache file)
    (insert-image (create-image file) )))

;; Insert screenshots into Org mode, very useful
(defun org-insert-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the same
directory as the org-buffer and insert
a link to this file."
  (interactive)
  (let ((case-fold-search nil))
    (setq tilde-buffer-filename
          (replace-regexp-in-string "/" "\\" (buffer-file-name) t t))
    (setq tilde-buffer-filename
          (replace-regexp-in-string ".org" "" tilde-buffer-filename t t))
    (setq filename
          (concat
           (make-temp-name
            (concat tilde-buffer-filename
                    "_"
                    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
    (setq filename (file-relative-name filename (file-name-directory (buffer-file-name))))
    (setq filename (replace-regexp-in-string "\\\\" "/" filename))
    (if (equal system-type 'windows-nt)
        ;; Windows: Irfanview
        (call-process "C:\\Program Files (x86)\\IrfanView\\i_view32.exe" nil nil nil (concat
                                                                                      "/clippaste /convert=" filename))

      ;; Linux: ImageMagick: (call-process "import" nil nil nil filename)
      (call-process "import" nil nil nil filename)
      ) ;; if
    (insert (concat "[[file:" filename "]]"))
    (org-display-inline-images)))

;; for Tikz image in Org
(setq org-babel-latex-htlatex "htlatex")
(defmacro by-backend (&rest body)
  `(case (if (boundp 'backend) (org-export-backend-name backend) nil) ,@body))

;; for Graphviz image in Org
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

;; for Gnuplot
(add-to-list 'load-path "~/.emacs.d/gnuplot")
(require 'gnuplot)

;; Make Yasnippet work here, but for Org
(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (and (fboundp 'yas-expand) (yas-expand))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-tab-first-hook
                         'yas-org-very-safe-expand)))

;; Configure org-mode so that when you edit source code in an indirect buffer (with C-c '), the buffer is opened in the current window. That way, your window organization isn't broken when switching.
(setq org-src-window-setup 'current-window)

;; Markdown exporter
(require 'ox-md)
(setq org-completion-use-ido t)

;; Tweaks for Latex exporting
(require 'ox-latex)

;; Choose either listings or minted for exporting source code blocks.
(setq org-latex-listings t)

;; Export " to csquotes macros
(setq org-export-latex-quotes
      '(("en" ("\\(\\s-\\|[[(]\\)\"" . "\\enquote{") ("\\(\\S-\\)\"" . "}") ("\\(\\s-\\|(\\)'" . "`"))))

;; Reftex
(require 'reftex-cite)
(require 'dash)
(setq reftex-default-bibliography '("~/workspace/Documents/Bibliography/biblio.bib")) ;; So that RefTeX in Org-mode knows bibliography
(defun org-mode-reftex-setup ()
  (interactive)
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
         ;; Reftex should use the org file as master file. See C-h v TeX-master for infos.
         (setq TeX-master t)
         (turn-on-reftex)
         ;; enable auto-revert-mode to update reftex when bibtex file changes on disk
         (global-auto-revert-mode t) ; careful: this can kill the undo
         ;; history when you change the file
         ;; on-disk.
         (reftex-parse-all)
         ;; add a custom reftex cite format to insert links
         ;; This also changes any call to org-citation!
         (reftex-set-cite-format
          '((?c . "\\citet{%l}") ; natbib inline text
            (?i . "\\citep{%l}") ; natbib with parens
            ))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; Org-Ref
(add-to-list 'load-path "~/.emacs.d/org-ref")
(require 'org-ref)
;; (org-babel-load-file "~/.emacs.d/org-ref/org-ref.org")
(setq org-ref-bibliography-notes "~/workspace/Documents/Bibliography/notes.org"
      org-ref-default-bibliography '("~/workspace/Documents/Bibliography/biblio.bib")
      org-ref-pdf-directory "~/workspace/Documents/Bibliography/bibtex-pdfs/")
(setq org-ref-insert-cite-key "C-c )")
(setq org-ref-default-citation-link "autocite")

;; Add defaults packages to include when exporting.
(add-to-list 'org-latex-packages-alist '("" "graphicx"))
(add-to-list 'org-latex-packages-alist '("" "geometry"))
(add-to-list 'org-latex-packages-alist '("" "hyperref"))
(add-to-list 'org-latex-packages-alist '("" "caption"))
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[10pt,article,oneside]{memoir}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("book"
               "\\documentclass[10pt]{memoir}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("xelatex"
               "\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec,lipsum}
\\usepackage{graphicx}
\\defaultfontfeatures{Mapping=tex-text}
\\setromanfont{Gentium}
\\setromanfont [BoldFont={Gentium Basic Bold},
                ItalicFont={Gentium Basic Italic}]{Gentium Basic}
\\setsansfont{Charis SIL}
\\setmonofont[Scale=0.8]{DejaVu Sans Mono}
\\defaultfontfeatures{Ligatures=TeX}
\\usepackage{geometry}
\\usepackage{listings}
\\usepackage{hyperref}
\\usepackage{caption}
\\usepackage{color}
\\usepackage{tikz}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("pdflatex"
               "\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{geometry}
\\usepackage{listings}
\\usepackage{hyperref}
\\usepackage{caption}
\\usepackage{color}
\\usepackage{mathptmx}
\\usepackage[section]{placeins}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\renewcommand{\\rmdefault}{ptm}
\\pagestyle{empty}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-default-class "pdflatex")

;; Let the exporter use the -shell-escape option to let latex execute external programs.
;; (setq org-latex-pdf-process
;;       '("xelatex -interaction nonstopmode %f"
;;         "bibtex $(basename %b)"
;;         "xelatex -interaction nonstopmode %f"
;;         "xelatex -interaction nonstopmode %f"
;;         )) ;; multipass

(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "bibtex $(basename %b)"
        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        )) ;; multipass

;; Tweak the PDF viewer
(eval-after-load "org"
  '(progn
     ;; .txt files aren't in the list initially, but in case that changes
     ;; in a future version of org, use if to avoid errors
     (if (assoc "\\.txt\\'" org-file-apps)
         (setcdr (assoc "\\.txt\\'" org-file-apps) "gedit %s")
       (add-to-list 'org-file-apps '("\\.txt\\'" . "gedit %s") t))
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "acroread %s")))

;; Automatically refresh inline images that are generated from Babel blocks
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Enable multiple languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (plantuml . t)
   (dot . t)
   (gnuplot . t)
   (sh . t)
   (R . t)
   (perl . t)
   (ruby . t)
   (python . t)
   (js . t)
   (C . t)
   (haskell . t)))

;; Make org do not open other frames
(setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                   (vm-imap . vm-visit-imap-folder-other-frame)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file)
                                   (wl . wl-other-frame))))

;; Add missing function
(defun org-reverse-string (string)
  (apply 'string (reverse (string-to-list string))))

;; The next block makes org-babel aware that a lower-case 'r' in a =src= block header should be processed as R.
(add-to-list 'org-src-lang-modes
             '("r" . ess-mode))

;; Don't ask for confirmation on every =C-c C-c= code-block compile.
(setq org-confirm-babel-evaluate nil)

;; Nice bulleted lists
(add-to-list 'load-path "~/.emacs.d/org-bullets")
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; CSS for the HTML
(setq org-html-style-include-scripts nil
      org-html-style-include-default nil)
(setq org-html-style
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://thomasf.github.io/solarized-css/solarized-light.min.css\" />")

;; Use Org bold, italics, code styles
(defun org-text-wrapper (txt &optional endtxt)
  "Wraps the region with the text passed in as an argument."
  (if (use-region-p)
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min))
        (insert txt)
        (goto-char (point-max))
        (if endtxt
            (insert endtxt)
          (insert txt)))
    (if (looking-at "[A-z]")
        (save-excursion
          (if (not (looking-back "[     ]"))
              (backward-word))
          (progn
            (mark-word)
            (org-text-wrapper txt endtxt)))
      (progn
        (insert txt)
        (let ((spot (point)))
          (insert txt)
          (goto-char spot))))))

(defun org-text-bold () "Wraps the region with asterisks."
  (interactive)
  (org-text-wrapper "*"))

(defun org-text-italics () "Wraps the region with slashes."
  (interactive)
  (org-text-wrapper "/"))

(defun org-text-code () "Wraps the region with equal signs."
  (interactive)
  (org-text-wrapper "="))

;; Gnus
(setq gnus-select-method
      '(nnimap "Gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

;; Gnus news
(setq gnus-summary-line-format "%U%R%z%d %I%(%[ %F %] %s %)\n")
(setq gnus-secondary-select-methods '((nntp "news.gmane.org")
                                      (nntp "news.gwene.org")))
(setq mm-text-html-renderer 'w3m)
(setq message-generate-headers-first t)
(add-hook 'message-mode-hook 'turn-on-auto-fill)

;; Newsticker and w3m
(when (executable-find "w3m")
  (add-to-list 'load-path "~/.emacs.d/newsticker")
  (add-to-list 'load-path "~/.emacs.d/w3m")
  (require 'newsticker)
  (require 'newsticker-notify)
  (require 'w3m)
  (setq newsticker-dir "~/.emacs.cache/Newsticker")
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
  (newsticker-start-ticker))

;; Skill-mode
(load "skill-fn-info.el")

;; Figure out what the function name is
(defun skill-get-fnsym ()
  (let ((p (point))
        (ret nil))
    ;; Don't do anything if current word is inside a string.
    (if (= (or (char-after (1- (point))) 0) ?\")
        nil
      (progn
        (backward-up-list)
        (forward-word)
        (setq ret (thing-at-point 'symbol))))
    (goto-char p)
    ret))

(defun lispdoc-get-arg-index ()
  (save-excursion
    (let ((fn (eldoc-fnsym-in-current-sexp))
          (i 0))
      (unless (memq (char-syntax (char-before)) '(32 39))
        (condition-case err
            (backward-sexp) ;; for safety
          (error 1)))
      (condition-case err
          (while (not (equal fn (eldoc-current-symbol)))
            (setq i (1+ i))
            (backward-sexp))
        (error 1))
      (max 0 i))))

(defun lispdoc-highlight-nth-arg (doc n)
  (cond ((null doc) "")
        ((<= n 0) doc)
        (t
         (let ((i 0))
           (mapconcat
            (lambda (arg)
              (if (member arg '("&optional" "&rest" "@optional" "@key" "@rest"))
                  arg
                (prog2
                    (if (= i (1- n))
                        (put-text-property 0 (length arg) 'face '(:bold t :foreground "yellow") arg))
                    arg
                  (setq i (1+ i)))))
            (split-string doc) " ")))))

;; Function that looks up and return the docstring
(defun skill-eldoc-function ()
  "Returns a documentation string appropriate for the current context or nil."
  (condition-case err
      (let* ((current-fnsym  (skill-get-fnsym))
             (doc (skill-fn-info-get current-fnsym))
             (adviced (lispdoc-highlight-nth-arg doc
                                                 (lispdoc-get-arg-index))))
        adviced)
    ;; This is run from post-command-hook or some idle timer thing,
    ;; so we need to be careful that errors aren't ignored.
    (error (message "eldoc error: %s" err))))

;; Loads Lisp auto-complete
(add-to-list 'ac-modes 'lisp-mode)
(defun my-lisp-mode-common-hook-func ()
  (interactive)
  "Function to be called when entering into c-mode."
  (set (make-local-variable 'eldoc-documentation-function)
       'skill-eldoc-function)
  (when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))
    (auto-complete-mode t)
    (make-local-variable 'ac-sources)
    (setq ac-sources '(ac-source-semantic
                       ac-source-words-in-same-mode-buffers
                       ac-source-gtags
                       ac-source-etags
                       ac-source-dictionary))))
(add-hook 'lisp-mode-hook 'my-lisp-mode-common-hook-func)

;; Better undo
(require 'undo-tree)
(global-undo-tree-mode)

;; Line spacing
(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq-default line-spacing 0.5)
    (setq-default line-spacing nil))
  (redraw-display))

;; Redo
(require 'redo+)
(global-set-key (kbd "C-S-z") 'redo) ; Mac style
(global-set-key (kbd "C-y") 'redo) ; Microsoft Windows style
(setq undo-no-redo t)

;; Python tweaks
(when (featurep 'python) (unload-feature 'python t))
(add-to-list 'load-path "~/.emacs.d/python-mode")
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'auto-complete-mode)

;; Python Hook
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))
(setq-default python-indent 2)
(setq-default python-guess-indent nil)

;; Jedi settings
(add-to-list 'load-path "~/.emacs.d/ctable")
(add-to-list 'load-path "~/.emacs.d/deferred")
(add-to-list 'load-path "~/.emacs.d/epc")
(add-to-list 'load-path "~/.emacs.d/jedi")
(add-to-list 'load-path "~/.emacs.d/python-environment")
(require 'python-environment)
(require 'epc)
(require 'jedi)
(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)))
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(defun jedi:ac-direct-matches ()
  (mapcar
   (lambda (x)
     (destructuring-bind (&key word doc description symbol)
         x
       (popup-make-item word
                        :symbol symbol
                        :document (unless (equal doc "") doc))))
   jedi:complete-reply))

(eval-after-load 'jedi
  '(progn
     (custom-set-faces
      '(jedi:highlight-function-argument ((t (:inherit eldoc-highlight-function-argument)))))

     (setq jedi:tooltip-method nil)
     (defun jedi-eldoc-documentation-function ()
       (deferred:nextc
         (jedi:call-deferred 'get_in_function_call)
         #'jedi-eldoc-show)
       nil)

     (defun jedi-eldoc-show (args)
       (when args
         (let ((eldoc-documentation-function
                (lambda ()
                  (apply #'jedi:get-in-function-call--construct-call-signature args))))
           (eldoc-print-current-symbol-info))))))

;; Jedi Eldoc
(require 'jedi-eldoc)
(set-face-attribute 'jedi-eldoc:highlight-function-argument nil
                    :foreground "green")
(add-hook 'python-mode-hook 'jedi-eldoc-mode)

;; Javascript mode
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
(require 'setup-js2-mode)

;; Ergoemacs
(add-to-list 'load-path "~/.emacs.d/ergoemacs")
(add-to-list 'load-path "~/.emacs.d/ergoemacs-keybindings")
(setq ergoemacs-keyboard-layout "us")
(require 'ergoemacs-mode)
(ergoemacs-mode 1)

;; Remove the trailing spaces
(defun remove-trailing-spaces ()
  "Remove trailing spaces in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d Trailing spaces removed from buffer." remove-count))))))

;; Unfill
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the reverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the reverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

;; From Ergoemacs
(global-set-key (kbd "<C-f3>") 'ergoemacs-compact-uncompact-block)

;; Bookmarks
(add-to-list 'load-path "~/.emacs.d/bm")
(setq bm-restore-repository-on-load t)
(require 'bm)
(setq bm-highlight-style 'bm-highlight-line-and-fringe)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;; make bookmarks persistent as default
(setq-default bm-buffer-persistence t)

;; Loading the repository from file when on start up.
(add-hook 'after-init-hook 'bm-repository-load)

;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)

;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)

;; Saving the repository to file when on exit.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))

;; Designsync versioning control
(require 'vc-sync)
(defun dired-sync-symlink-filter ()
  (save-excursion
    ;; Goto the beginning of the buffer
    (goto-char (point-min))
    ;; For each matching symbolic link with sync_cache or sync/mirrors in the path name...
    (while (re-search-forward "\\(-> .*/sync\\(_cache\\|/mirrors\\)/.*$\\)" nil t)
      ;; Create an overlay that masks out everything between the -> and the end of line
      (let ((o (make-overlay (match-beginning 1) (progn (end-of-line) (point)))))
        (overlay-put o 'invisible t)
        (overlay-put o 'evaporate t)))))
(add-hook 'dired-after-readin-hook 'dired-sync-symlink-filter)

;; psvn
(add-to-list 'load-path "~/.emacs.d/diff-hl")
(require 'psvn)
(require 'diff-hl)
(defadvice svn-status-update-modeline (after svn-update-diff-hl activate)
  (diff-hl-update))

;; git
(add-to-list 'load-path "~/.emacs.d/git-modes")
(add-to-list 'load-path "~/.emacs.d/git-emacs")
(add-to-list 'load-path "~/.emacs.d/git-gutter-plus")
(add-to-list 'load-path "~/.emacs.d/git-gutter-fringe-plus")
(add-to-list 'load-path "~/.emacs.d/git-timemachine")
(add-to-list 'load-path "~/.emacs.d/magit")
(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/magit/")))

(require 'magit)
(require 'setup-magit)
(require 'git-emacs)
(require 'git-gutter-fringe+)
(require 'git-timemachine)
(global-git-gutter+-mode t)
(set-face-foreground 'git-gutter-fr+-modified "white")
(set-face-foreground 'git-gutter-fr+-added    "green")
(set-face-foreground 'git-gutter-fr+-deleted  "orange")

;; Please adjust fringe width if your own sign is too big.
(setq-default left-fringe-width 20)

(fringe-helper-define 'git-gutter-fr+-added nil
  ".XXXXXX."
  "XXxxxxXX"
  "XX....XX"
  "XX....XX"
  "XXXXXXXX"
  "XXXXXXXX"
  "XX....XX"
  "XX....XX")

(fringe-helper-define 'git-gutter-fr+-deleted nil
  "XXXXXX.."
  "XXXXXXX."
  "XX...xXX"
  "XX....XX"
  "XX....XX"
  "XX...xXX"
  "XXXXXXX."
  "XXXXXX..")

(fringe-helper-define 'git-gutter-fr+-modified nil
  "XXXXXXXX"
  "XXXXXXXX"
  "Xx.XX.xX"
  "Xx.XX.xX"
  "Xx.XX.xX"
  "Xx.XX.xX"
  "Xx.XX.xX"
  "Xx.XX.xX")

;; Stop Org splitting window vertically
(setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                   (vm-imap . vm-visit-imap-folder-other-frame)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file)
                                   (wl . wl-other-frame))))

;; Code folding
(defun toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))
(global-set-key [f1] 'toggle-selective-display)

;; Commands to make my programming environment nice
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\C-l" 'goto-line)
(global-set-key (kbd "") 'other-window)
(global-set-key [f5] 'compile)
(global-set-key [f6] 'next-error)
(global-set-key [f11] 'djcb-full-screen-toggle)
(global-set-key [C-tab] 'comment-or-uncomment-region)
(global-set-key [kp-prior] 'scroll-down)
(global-set-key [prior] 'scroll-down)
(global-set-key [kp-next] 'scroll-up)
(global-set-key [next] 'scroll-up)
(global-set-key "\M-g" 'goto-line)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [(meta delete)] '(lambda () (interactive) (backward-or-forward-kill-word -1)))
(global-set-key [(alt delete)] '(lambda () (interactive) (backward-or-forward-kill-word -1)))
(global-set-key [M-up] 'enlarge-window)
(global-set-key [M-down] 'shrink-window)
(global-set-key [(control o)] 'find-file)
(global-set-key [(control n)] 'find-file-other-frame)
(global-set-key [(control s)] 'save-buffer)
(global-set-key [(meta s)] 'write-file)
(global-set-key [(control q)] 'save-buffers-kill-emacs)
(global-set-key [(meta q)] 'kill-this-buffer)
(global-set-key [(control t)] 'ispell-buffer)
(global-set-key [(control r)] 'replace-string)
(global-set-key [(control z)] 'undo)
(global-set-key "\C-a" 'mark-whole-buffer)
(global-set-key (kbd "\C-c \C-c") 'kill-ring-save)

;; search forward with Ctrl-f
(global-set-key [(control f)] 'isearch-forward)
(define-key isearch-mode-map [(control f)] (lookup-key isearch-mode-map "\C-f"))
(define-key minibuffer-local-isearch-map [(control f)]
  (lookup-key minibuffer-local-isearch-map "\C-s"))

;; search backward with Alt-f
(global-set-key [(meta f)] 'isearch-backward)
(define-key isearch-mode-map [(meta f)] (lookup-key isearch-mode-map "\C-r"))
(define-key minibuffer-local-isearch-map [(meta f)]
  (lookup-key minibuffer-local-isearch-map "\C-r"))

;; Cancel minibuffer operation if you click outside
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Scroll with the mouse
(defun smooth-scroll (number-lines increment)
  (if (= 0 number-lines)
      t
    (progn
      (sit-for (* 0.01 number-lines))
      (scroll-up increment)
      (smooth-scroll (- number-lines 1) increment))))

(global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 8 1)))
(global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll 8 -1)))

;; to get the scroll wheel work
(global-set-key [(shift button5)] '(lambdas () (interactive) (scroll-up-line)))
(global-set-key [(shift button4)] '(lambda () (interactive) (scroll-down-line)))
(global-set-key [(control button5)] 'text-scale-decrease)
(global-set-key [(control button4)] 'text-scale-increase)

(global-set-key [(shift mouse-5)] '(lambda () (interactive) (scroll-up-line)))
(global-set-key [(shift mouse-4)] '(lambda () (interactive) (scroll-down-line)))
(global-set-key [(control mouse-5)] 'text-scale-decrease)
(global-set-key [(control mouse-4)] 'text-scale-increase)

;; higlight changes in documents
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)

;; toggle visibility
(global-set-key (kbd "<f6>") 'highlight-changes-visible-mode) ;; changes

;; remove the change-highlight in region
(global-set-key (kbd "S-<f6>") 'highlight-changes-remove-highlight)

;; if you're not already using it for something else...
(global-set-key (kbd "<M-next>") 'highlight-changes-next-change)
(global-set-key (kbd "<M-prior>")  'highlight-changes-previous-change)
(set-face-foreground 'highlight-changes nil)
(set-face-background 'highlight-changes "#882020")
(set-face-foreground 'highlight-changes-delete nil)
(set-face-background 'highlight-changes-delete "#916868")

;; toggle truncate lines
(global-set-key (kbd "<f8>") 'toggle-truncate-lines)

;; Refresh file on F9
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t))
(global-set-key [f9] 'refresh-file)

;; Show guide for shortcuts
(add-to-list 'load-path "~/.emacs.d/guide-key")
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(guide-key-mode 1) ;; Enable guide-key-mode

;; Recentf stuff
(require 'recentf)
(setq recentf-max-menu-items 25)
(setq recentf-exclude (append recentf-exclude '("/usr*")))
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(setq recentf-filename-handlers '(abbreviate-file-name))
(setq recentf-save-file "~/.emacs.cache/recentf")
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; iMenu
(set-default 'imenu-auto-rescan t)
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'imenu-example--create-lisp-index)
            (setq imenu-generic-expression scheme-imenu-generic-expression)))

(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)
(add-hook 'lisp-mode-hook 'imenu-add-menubar-index)
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
(add-hook 'latex-mode-hook 'imenu-add-menubar-index)
(add-hook 'org-mode-hook 'imenu-add-menubar-index)
(add-hook 'python-mode-hook 'imenu-add-menubar-index)

;; Enable which-function-mode for selected major modes
(setq which-func-modes '(ecmascript-mode python-mode emacs-lisp-mode lisp-mode
                                         c-mode c++-mode makefile-mode sh-mode))
(which-function-mode t)
(add-hook 'js2-mode-hook
          (lambda () (which-function-mode t)))
(add-hook 'python-mode-hook
          (lambda () (which-function-mode t)))
(add-hook 'c-mode-common-hook
          (lambda () (which-function-mode t)))
(add-hook 'lisp-mode-hook
          (lambda () (which-function-mode t)))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (which-function-mode t)))

;; Project management
(add-to-list 'load-path "~/.emacs.d/ack-and-a-half")
(add-to-list 'load-path "~/.emacs.d/projectile")
(add-to-list 'load-path "~/.emacs.d/perspective")
(require 'ack-and-a-half)
;; (require 'perspective)
(require 'projectile)
(projectile-global-mode t)
(setq projectile-cache-file "~/.emacs.cache/projectile.cache")
(setq projectile-known-projects-file "~/.emacs.cache/projectile-bookmarks.eld")
(setq projectile-enable-caching t)
(setq projectile-keymap-prefix (kbd "C-c C-p"))

;; Code Browser
(setq stack-trace-on-error t)
(setq after-find-file-from-revert-buffer t)
(add-to-list 'load-path "~/.emacs.d/ecb")
(require 'ecb)
(setq ecb-show-sources-in-directories-buffer 'always)
(set-face-foreground 'ecb-default-general-face "#ffffff")
(setq ecb-tip-of-the-day nil)
(setq ecb-auto-compatibility-check nil)
(if (ecb--semantic-active-p)
    (ecb-update-methods-buffer--internal nil nil t)
  (ecb-rebuild-methods-buffer-for-non-semantic))

;; Fix the modeline
(add-to-list 'load-path "~/.emacs.d/smart-mode-line")
(require 'smart-mode-line)
(sml/setup)

(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 25)
(setq sml/mode-width 'full)

(add-to-list 'sml/hidden-modes " AC")
(add-to-list 'sml/hidden-modes " AI")
(add-to-list 'sml/hidden-modes " SP")
(add-to-list 'sml/hidden-modes " mate")
(add-to-list 'sml/hidden-modes " Plugged")
(add-to-list 'sml/hidden-modes " Gtags")
(add-to-list 'sml/hidden-modes " Abbrev")
(add-to-list 'sml/hidden-modes " Fill")
(add-to-list 'sml/hidden-modes " Guide")
(add-to-list 'sml/hidden-modes " hs")
(add-to-list 'sml/hidden-modes " yas")
(add-to-list 'sml/hidden-modes " pair")
(add-to-list 'sml/hidden-modes " GitGutter")
(add-to-list 'sml/hidden-modes " Undo-Tree")
(add-to-list 'sml/hidden-modes " MRev")
(add-to-list 'sml/hidden-modes " vl")
(add-to-list 'sml/hidden-modes " ElDoc")
(add-to-list 'sml/hidden-modes " -Chg")

;; Better Alt-x
(add-to-list 'load-path "~/.emacs.d/smex")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Right click mouse
(global-unset-key (kbd "<mouse-3>"))
(require 'mouse3)
(defalias 'mouse3-region-popup-menu 'mouse3-popup-menu)
(global-set-key (kbd "<mouse-3>") 'mouse3-region-popup-menu)

;; Enable the menu bar
(menu-bar-mode t)

;; Server configuration
(load "server")
(defadvice make-network-process (before force-tcp-server-ipv4 activate)
  "Monkey patch the server to force it to use ipv4. This is a bug fix that will
hopefully be in emacs 24: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=6781"
  (if (eq nil (plist-get (ad-get-args 0) :family))
      (ad-set-args 0 (plist-put (ad-get-args 0) :family 'ipv4))))
(unless (server-running-p)
  (setq server-use-tcp t)
  (setq server-port 9999)
  (setq server-auth-dir "~/.emacs.cache/server")
  (if (not (file-exists-p server-auth-dir))
      (make-directory server-auth-dir))
  (and (>= emacs-major-version 23)
       (defun server-ensure-safe-dir (dir) "Noop" t))
  (server-start))

;; Emacsclient support
(require 'remote-emacsclient)
(update-tramp-emacs-server-port-forward tramp-default-method)

;; End of customizations

;; Utilities

;; dos2unix
(defun dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))

;; Remove or add ending chars
(defun compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)
  (let (currentStateIsCompact (bigFillColumnVal 4333999) (deactivate-mark nil))
    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ) )

      (if (region-active-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))) )
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil)) ) )

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)) ) ) )

;; Beautify (poner bonito) tabulaciones en nuestro pograma en C/C++
(defun beautify-region (beg end)
  (interactive "r")
  (setq end (save-excursion (goto-char end) (point-marker)))
  (indent-region beg end nil))

(defun beautify-buffer ()
  "Beautify buffer by applying indentation, whitespace fixup, alignment, and
case fixing to entire buffer. Calls `vhdl-beautify-region' for the entire
buffer."
  (interactive)
  (beautify-region (point-min) (point-max))
  (when noninteractive (save-buffer)))
(global-unset-key "\C-b")
(global-set-key "\C-b" 'beautify-buffer)

;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                         (save-excursion
                           (untabify (point-min) (point-max)))) nil))

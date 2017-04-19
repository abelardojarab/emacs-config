;;; gtags.el --- gtags facility for Emacs

;; Copyright (C) 1997-2000, 2006-2011  Tama Communications Corporation
;; Copyright (C) 2011 Leo Liu

;; Author: Tama Communications Corporation
;; Maintainer: Leo Liu <sdl.web@gmail.com>
;; Keywords: tools

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; GLOBAL home page is at: http://www.gnu.org/software/global/
;; Author: Tama Communications Corporation
;; Required version: GLOBAL 5.9 or later

;; Gtags-mode is implemented as a minor mode so that it can work with any
;; other major modes. Gtags-select mode is implemented as a major mode.
;;
;; Please copy this file into emacs lisp library directory or place it in
;; a directory (for example "~/.emacs.d") and write $HOME/.emacs like this.
;;
;;	(add-to-list 'load-path "~/.emacs.d")
;;	(autoload 'gtags-mode "gtags" "" t)
;;
;; If you hope gtags-mode is on in c-mode then please add c-mode-hook
;; to your $HOME/.emacs like this.
;;
;; (add-hook 'c-mode-hook (lambda () (gtags-mode 1)))

;; There are two hooks, gtags-mode-hook and gtags-select-mode-hook.
;; The usage of the hook is shown as follows.

;; * Setting to reproduce old 'Gtags mode'

;; (add-hook 'gtags-mode-hook
;;   '(lambda ()
;;      (setq gtags-pop-delete t)
;;      (setq gtags-path-style 'absolute)))

;; * Setting to make 'Gtags select mode' easy to see

;; (add-hook 'gtags-select-mode-hook
;;   '(lambda ()
;;      (setq hl-line-face 'underline)
;;      (hl-line-mode 1)))

;;; Code

;;;
;;; Customizing gtags-mode
;;;
(defgroup gtags nil
  "Minor mode for GLOBAL source code tag system."
  :group 'tools
  :prefix "gtags-")

(defcustom gtags-path-style 'root
  "Controls the style of path in [GTAGS SELECT MODE]."
  :type '(choice (const :tag "Relative from the root of the current project" root)
                 (const :tag "Relative from the current directory" relative)
                 (const :tag "Absolute" absolute))
  :group 'gtags)

(defcustom gtags-read-only nil
  "Gtags read only mode"
  :type 'boolean
  :group 'gtags)

(defcustom gtags-pop-delete nil
  "If non-nil, gtags-pop will delete the buffer."
  :group 'gtags
  :type 'boolean)

(defcustom gtags-select-buffer-single nil
  "If non-nil, gtags select buffer is single."
  :group 'gtags
  :type 'boolean)

(defcustom gtags-disable-pushy-mouse-mapping nil
  "If non-nil, mouse key mapping is disabled."
  :group 'gtags
  :type 'boolean)

(defcustom gtags-suggested-key-mapping nil
  "If non-nil, suggested key mapping is enabled."
  :group 'gtags
  :type 'boolean)

;; Variables
(defvar gtags-current-buffer nil
  "Current buffer.")

(defvar gtags-buffer-stack nil
  "Stack for tag browsing.")

(defvar gtags-point-stack nil
  "Stack for tag browsing.")

(defvar gtags-history nil
  "Gtags history list.")

(defconst gtags-symbol-regexp "[A-Za-z_][A-Za-z_0-9]*"
  "Regexp matching tag name.")

(defconst gtags-definition-regexp "#[ \t]*define[ \t]+\\|ENTRY(\\|ALTENTRY("
  "Regexp matching tag definition name.")

(defvar gtags-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-*"   'gtags-pop-stack)
    (define-key m "\M-."   'gtags-find-tag)
    (define-key m "\C-x4." 'gtags-find-tag-other-window)
    m)
  "Keymap used in gtags mode.")

(defvar gtags-rootdir nil
  "Root directory of source tree.")

(defvar gtags-select-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-*" 'gtags-pop-stack)
    (define-key m "\^?"  'scroll-down)
    (define-key m " "    'scroll-up)
    (define-key m "\C-b" 'scroll-down)
    (define-key m "\C-f" 'scroll-up)
    (define-key m "k"    'previous-line)
    (define-key m "j"    'next-line)
    (define-key m "p"    'previous-line)
    (define-key m "n"    'next-line)
    (define-key m "q"    'gtags-pop-stack)
    (define-key m "u"    'gtags-pop-stack)
    (define-key m "\C-t" 'gtags-pop-stack)
    (define-key m "\C-m" 'gtags-select-tag)
    (define-key m "\C-o" 'gtags-select-tag-other-window)
    (define-key m "\M-." 'gtags-select-tag)
    m)
  "Keymap used in gtags select mode.")

;;
;; utility
;;
;; Return a default tag to search for, based on the text at point.
(defun gtags-current-token ()
  (save-excursion
    (if (looking-back "[0-9A-Za-z_]")
        (skip-chars-backward "0-9A-Za-z_" (line-beginning-position))
      (skip-chars-forward " \t\n"))
    (when (looking-at gtags-definition-regexp)
      (goto-char (match-end 0)))
    (when (looking-at gtags-symbol-regexp)
      (match-string 0))))

;; push current context to stack
(defun gtags-push-context ()
  (push (current-buffer) gtags-buffer-stack)
  (push (point) gtags-point-stack))

;; pop context from stack
(defun gtags-pop-context ()
  (when gtags-buffer-stack
    (list (pop gtags-buffer-stack) (pop gtags-point-stack))))

;; if the buffer exist in the stack
(defun gtags-exist-in-stack (buffer)
  (memq buffer gtags-buffer-stack))

(defun gtags-completing (flag string predicate code)
  "FLAG is either symbol, file or tag."
  ;; The purpose of using the -n option for the -P command is to
  ;; exclude dependence on the execution directory.
  (let ((option (cond ((eq flag 'file)   "-Pon")
                      ((eq flag 'symbol) "-cs")
                      (t                 "-c")))
        (complete-list (make-vector 67 0)))
    ;; build completion list
    (with-temp-buffer
      (call-process "global" nil t nil option string)
      (goto-char (point-min))
      ;;
      ;; The specification of the completion for files is different
      ;; from that for symbols. The completion for symbols matches
      ;; only to the head of the symbol. But the completion for files
      ;; matches any part of the path.
      ;;
      (if (eq flag 'file)
          ;; Extract input string and the following part.
          (let ((regexp (if (equal "" string)
                            "\./\\(.*\\)"
                          (concat ".*\\(" string ".*\\)"))))
            (while (re-search-forward regexp nil t)
              (intern (match-string 1) complete-list)))
        (while (re-search-forward gtags-symbol-regexp nil t)
          (intern (match-string 0) complete-list))))
    ;; execute completion
    (cond ((eq code nil)
           (try-completion string complete-list predicate))
          ((eq code t)
           (all-completions string complete-list predicate))
          ((eq code 'lambda)
           (and (intern-soft string complete-list) t)))))

(defun gtags-read-tagname (flag type)
  "See `gtags-completing' for the meaning of FLAG."
  (let* ((default (gtags-current-token))
         (prompt (if default
                     (format "Find %s: (default %s) " type default)
                   (format "Find %s: " type))))
    (completing-read prompt (lambda (string predicate code)
                              (gtags-completing flag string predicate code))
                     nil nil nil 'gtags-history default)))

;; get the path of gtags root directory.
(defun gtags-get-rootpath ()
  (with-temp-buffer
    (when (zerop (call-process "global" nil t nil "-pr"))
      (goto-char (point-min))
      (file-name-as-directory
       (buffer-substring (line-beginning-position) (line-end-position))))))

;;
;; interactive command
;;
(defun gtags-visit-rootdir ()
  "Tell tags commands the root directory of source tree."
  (interactive)
  (let* ((default (or (gtags-get-rootpath) gtags-rootdir default-directory))
         (root (read-directory-name "Visit root directory: " default nil t)))
    (setq gtags-rootdir (expand-file-name root))
    (setenv "GTAGSROOT" gtags-rootdir)))

(defun gtags-find-tag (tagname &optional other-win)
  "Input tag name and move to the definition."
  (interactive (list (gtags-read-tagname 'tag "tag")))
  (gtags-push-context)
  (gtags-goto-tag tagname "" other-win))

(defun gtags-find-tag-other-window (tagname)
  "Input tag name and move to the definition in other window."
  (interactive (list (gtags-read-tagname 'tag "tag")))
  (gtags-find-tag tagname t))

(defun gtags-find-rtag (tagname)
  "Input tag name and move to the referenced point."
  (interactive (list (gtags-read-tagname 'tag "reference")))
  (gtags-push-context)
  (gtags-goto-tag tagname "r"))

(defun gtags-find-symbol (tagname)
  "Input symbol and move to the locations."
  (interactive (list (gtags-read-tagname 'symbol "symbol")))
  (gtags-push-context)
  (gtags-goto-tag tagname "s"))

(defun gtags-find-pattern ()
  "Input pattern, search with grep(1) and move to the locations."
  (interactive)
  (gtags-find-with-grep))

(defun gtags-find-with-grep ()
  "Input pattern, search with grep(1) and move to the locations."
  (interactive)
  (gtags-find-with "g"))

(defun gtags-find-with-idutils ()
  "Input pattern, search with idutils(1) and move to the locations."
  (interactive)
  (gtags-find-with "I"))

(defun gtags-find-file (tagname)
  "Input pattern and move to the top of the file."
  (interactive (list (gtags-read-tagname 'file "files")))
  (gtags-push-context)
  (gtags-goto-tag tagname "Po"))

(defun gtags-parse-file ()
  "Input file name and show the list of tags in it."
  (interactive)
  (let (tagname prompt input)
    (setq prompt "Parse file: ")
    (setq input (read-file-name prompt buffer-file-name buffer-file-name t))
    (if (or (equal "" input) (not (file-regular-p input)))
        (message "Please specify an existing source file.")
      (setq tagname input)
      (gtags-push-context)
      (gtags-goto-tag tagname "f"))))

(defun gtags-find-tag-from-here ()
  "Get the expression as a tagname around here and move there."
  (interactive)
  (when (gtags-current-token)
    (gtags-push-context)
    (gtags-goto-tag (gtags-current-token) "C")))

; This function doesn't work with mozilla.
; But I will support it in the near future.
(defun gtags-display-browser ()
  "Display current screen on hypertext browser."
  (interactive)
  (call-process "gozilla"  nil nil nil
                (format "+%d" (line-number-at-pos)) buffer-file-name))

(defun gtags-find-tag-by-event (event)
  "Get the expression as a tagname around here and move there."
  (interactive "e")
  (let (tagname flag)
    (if (zerop (count-lines (point-min) (point-max)))
        (progn (setq tagname "main")
               (setq flag ""))
      (select-window (posn-window (event-end event)))
      (set-buffer (window-buffer (posn-window (event-end event))))
      (goto-char (posn-point (event-end event)))
      (setq tagname (gtags-current-token))
      (setq flag "C"))
    (when tagname
      (gtags-push-context)
      (gtags-goto-tag tagname flag))))

(defun gtags-select-tag (&optional other-win)
  "Select a tag in [GTAGS SELECT MODE] and move there."
  (interactive)
  (gtags-push-context)
  (gtags-select-it nil other-win))

(defun gtags-select-tag-other-window ()
  "Select a tag in [GTAGS SELECT MODE] and move there in other window."
  (interactive)
  (gtags-select-tag t))

(defun gtags-select-tag-by-event (event)
  "Select a tag in [GTAGS SELECT MODE] and move there."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (set-buffer (window-buffer (posn-window (event-end event))))
  (goto-char (posn-point (event-end event)))
  (gtags-push-context)
  (gtags-select-it nil))

(defun gtags-pop-stack ()
  "Move to previous point on the stack."
  (interactive)
  (let (delete context buffer)
    (if (and (not (equal gtags-current-buffer nil))
             (not (equal gtags-current-buffer (current-buffer))))
        (switch-to-buffer gtags-current-buffer)
      ;; By default, the buffer of the referred file is left.
      ;; If gtags-pop-delete is set to t, the file is deleted.
      ;; Gtags select mode buffer is always deleted.
      (if (and (or gtags-pop-delete (equal mode-name "Gtags-Select"))
               (not (gtags-exist-in-stack (current-buffer))))
          (setq delete t))
      (setq context (gtags-pop-context))
      (if (not context)
	  (message "The tags stack is empty.")
        (if delete
	    (kill-buffer (current-buffer)))
        (switch-to-buffer (nth 0 context))
        (setq gtags-current-buffer (current-buffer))
        (goto-char (nth 1 context))))))

;;
;; common function
;;

;; find with grep or idutils.
(defun gtags-find-with (flag)
  (gtags-push-context)
  (gtags-goto-tag (gtags-read-tagname 'tag "pattern") flag))

;; goto tag's point
(defun gtags-goto-tag (tagname flag &optional other-win)
  (let (option context save prefix buffer lines flag-char)
    (setq save (current-buffer))
    (setq flag-char (string-to-char flag))
    ;; Use always ctags-x format.
    (setq option "-x")
    (if (char-equal flag-char ?C)
        (setq context (format "--from-here=%d:%s" (line-number-at-pos) buffer-file-name))
      (setq option (concat option flag)))
    (cond
     ((char-equal flag-char ?C)
      (setq prefix "(CONTEXT)"))
     ((char-equal flag-char ?P)
      (setq prefix "(P)"))
     ((char-equal flag-char ?f)
      (setq prefix "(F)")
      (setq option (concat option "q")))
     ((char-equal flag-char ?g)
      (setq prefix "(GREP)"))
     ((char-equal flag-char ?I)
      (setq prefix "(IDUTILS)"))
     ((char-equal flag-char ?s)
      (setq prefix "(S)"))
     ((char-equal flag-char ?r)
      (setq prefix "(R)"))
     (t (setq prefix "(D)")))
    ;; load tag
    (when gtags-select-buffer-single
      ;; delete "*GTAGS SELECT*" buffer info from gtags-buffer-stack and gtags-point-stack
      (let (now-gtags-buffer-stack now-buffer now-gtags-point-stack now-point)
        (setq now-gtags-buffer-stack (reverse gtags-buffer-stack))
        (setq now-gtags-point-stack (reverse gtags-point-stack))
        (setq gtags-buffer-stack nil)
        (setq gtags-point-stack nil)
        (while now-gtags-buffer-stack
          (setq now-buffer (car now-gtags-buffer-stack))
          (setq now-point (car now-gtags-point-stack))
          (when (and (buffer-name now-buffer) (not (string-match "*GTAGS SELECT*" (buffer-name now-buffer))))
            (setq gtags-buffer-stack (cons now-buffer gtags-buffer-stack))
            (setq gtags-point-stack (cons now-point gtags-point-stack)))
          (setq now-gtags-buffer-stack (cdr now-gtags-buffer-stack))
          (setq now-gtags-point-stack (cdr now-gtags-point-stack))))
                                        ; kill "*GTAGS SELECT*" buffer
      (let (now-buffer-list now-buffer)
        (setq now-buffer-list (buffer-list))
        (while now-buffer-list
          (setq now-buffer (car now-buffer-list))
          (and (string-match "*GTAGS SELECT*" (buffer-name now-buffer))
               (kill-buffer now-buffer))
          (setq now-buffer-list (cdr now-buffer-list)))))
    (setq buffer (generate-new-buffer (generate-new-buffer-name (concat "*GTAGS SELECT* " prefix tagname))))
    (set-buffer buffer)
    ;;
    ;; Path style is defined in gtags-path-style:
    ;;   root: relative from the root of the project (Default)
    ;;   relative: relative from the current directory
    ;;	absolute: absolute (relative from the system root directory)
    ;;
    (cond
     ((equal gtags-path-style 'absolute)
      (setq option (concat option "a")))
     ((equal gtags-path-style 'root)
      (let (rootdir)
        (if gtags-rootdir
            (setq rootdir gtags-rootdir)
          (setq rootdir (gtags-get-rootpath)))
        (if rootdir (cd rootdir)))))
    (message "Searching %s ..." tagname)
    (if (not (= 0 (if (equal flag "C")
                      (call-process "global" nil t nil option "--encode-path=\" \t\"" context tagname)
                    (call-process "global" nil t nil option "--encode-path=\" \t\"" tagname))))
	(progn (message (buffer-substring (point-min)(1- (point-max))))
               (gtags-pop-context))
      (goto-char (point-min))
      (setq lines (count-lines (point-min) (point-max)))
      (cond
       ((= 0 lines)
        (cond
         ((char-equal flag-char ?P)
          (message "%s: path not found" tagname))
         ((char-equal flag-char ?g)
          (message "%s: pattern not found" tagname))
         ((char-equal flag-char ?I)
          (message "%s: token not found" tagname))
         ((char-equal flag-char ?s)
          (message "%s: symbol not found" tagname))
         (t
          (message "%s: tag not found" tagname)))
	(gtags-pop-context)
	(kill-buffer buffer)
	(set-buffer save))
       ((= 1 lines)
	(message "Searching %s ... Done" tagname)
	(gtags-select-it t other-win))
       (t
        (if (null other-win)
            (switch-to-buffer buffer)
          (switch-to-buffer-other-window buffer))
	(gtags-select-mode))))))

;; select a tag line from lines
(defun gtags-select-it (delete &optional other-win)
  (let (line file)
    ;; get context from current tag line
    (beginning-of-line)
    (if (not (looking-at "[^ \t]+[ \t]+\\([0-9]+\\)[ \t]\\([^ \t]+\\)[ \t]"))
        (gtags-pop-context)
      (setq line (string-to-number (match-string 1)))
      ;; decode path name
      ;; The path is encoded by global(1) with the --encode-path="..." option.
      (setq file (url-unhex-string (match-string 2)))
      ;;
      ;; Why should we load new file before killing current-buffer?
      ;;
      ;; If you kill current-buffer before loading new file, current directory
      ;; will be changed. This might cause loading error, if you use relative
      ;; path in [GTAGS SELECT MODE], because emacs's buffer has its own
      ;; current directory.
      ;; 
      (let ((prev-buffer (current-buffer)))
        ;; move to the context
        (if gtags-read-only 
	    (if (null other-win) (find-file-read-only file) 
	      (find-file-read-only-other-window file))
	  (if (null other-win) (find-file file)
	    (find-file-other-window file)))
        (if delete (kill-buffer prev-buffer)))
      (setq gtags-current-buffer (current-buffer))
      (goto-char (point-min))
      (forward-line (1- line))
      (gtags-mode 1))))

;; make complete list (do nothing)
(defun gtags-make-complete-list ()
  "Make tag name list for completion."
  (interactive)
  (message "gtags-make-complete-list: Deprecated. You need not call this command any longer."))

;;;###autoload
(define-minor-mode gtags-mode
  "Toggle Gtags mode, a minor mode for browsing source code using GLOBAL.

Specify the root directory of project.
	\\[gtags-visit-rootdir]
Input tag name and move to the definition.
	\\[gtags-find-tag]
Input tag name and move to the definition in other window.
        \\[gtags-find-tag-other-window]
Input tag name and move to the referenced point.
	\\[gtags-find-rtag]
Input symbol and move to the locations.
	\\[gtags-find-symbol]
Input pattern, search with grep(1) and move to the locations.
	\\[gtags-find-with-grep]
Input pattern, search with idutils(1) and move to the locations.
	\\[gtags-find-with-idutils]
Input pattern and move to the top of the file.
	\\[gtags-find-file]
Input pattern and show the list of definitions of the file.
	\\[gtags-parse-file]
Get the expression as a tagname around here and move there.
	\\[gtags-find-tag-from-here]
Display current screen on hypertext browser.
	\\[gtags-display-browser]
Get the expression as a tagname around here and move there.
	\\[gtags-find-tag-by-event]
Move to previous point on the stack.
	\\[gtags-pop-stack]

Key definitions:
\\{gtags-mode-map}
Turning on Gtags mode calls the value of the variable `gtags-mode-hook'
with no args, if that value is non-nil."
  :lighter " Gtags"
  ;; Suggested key mapping
  (when gtags-mode
    (when gtags-suggested-key-mapping
      (define-key gtags-mode-map "\M-h" 'gtags-display-browser)
      (define-key gtags-mode-map "\C-]" 'gtags-find-tag-from-here)
      (define-key gtags-mode-map "\C-t" 'gtags-pop-stack)
      (define-key gtags-mode-map "\M-P" 'gtags-find-file)
      (define-key gtags-mode-map "\M-f" 'gtags-parse-file)
      (define-key gtags-mode-map "\M-g" 'gtags-find-with-grep)
      (define-key gtags-mode-map "\M-I" 'gtags-find-with-idutils)
      (define-key gtags-mode-map "\M-s" 'gtags-find-symbol)
      (define-key gtags-mode-map "\M-r" 'gtags-find-rtag)
      (define-key gtags-mode-map "\M-t" 'gtags-find-tag)
      (define-key gtags-mode-map "\M-v" 'gtags-visit-rootdir))
    ;; Mouse key mapping
    (unless gtags-disable-pushy-mouse-mapping
      (define-key gtags-mode-map [mouse-3] 'gtags-pop-stack)
      (define-key gtags-mode-map [mouse-2] 'gtags-find-tag-by-event))))

;; make gtags select-mode
(define-derived-mode gtags-select-mode nil "Gtags-Select"
  "Major mode for choosing a tag from tags list.

Select a tag in tags list and move there.
	\\[gtags-select-tag]
Move to previous point on the stack.
	\\[gtags-pop-stack]

Key definitions:
\\{gtags-select-mode-map}
Turning on Gtags-Select mode calls the value of the variable
`gtags-select-mode-hook' with no args, if that value is non-nil."
  :abbrev-table nil
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq gtags-current-buffer (current-buffer))
  (goto-char (point-min))
  (message "[GTAGS SELECT MODE] %d lines" (count-lines (point-min) (point-max)))
  ;; Mouse key mapping
  (unless gtags-disable-pushy-mouse-mapping
    (define-key gtags-select-mode-map [mouse-3] 'gtags-pop-stack)
    (define-key gtags-select-mode-map [mouse-2] 'gtags-select-tag-by-event)))

(provide 'gtags)

;;; gtags.el ends here

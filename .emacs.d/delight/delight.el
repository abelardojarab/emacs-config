;;; delight.el --- A dimmer switch for your lighter text.
;;
;; Author: Phil S.

;; Commentary:
;;
;; Enables you to customise the mode names displayed in the mode line.
;;
;; For major modes, the buffer-local `mode-name' variable is modified.
;; For minor modes, the associated value in `minor-mode-alist' is set.
;;
;; Example usage:
;;
;; (require 'delight)
;;
;; (delight 'abbrev-mode " Abv" "abbrev")
;;
;; (delight '((abbrev-mode " Abv" "abbrev")
;;            (smart-tab-mode " \\t" "smart-tab")
;;            (eldoc-mode nil "eldoc")
;;            (rainbow-mode)
;;            (emacs-lisp-mode "Elisp" "lisp-mode")))
;;
;; Important note:
;;
;; Although strings are common, any mode-line construct is permitted
;; as the value (for both minor and major modes); so before you
;; override a value you should check the existing one, as you may
;; want to replicate any structural elements in your replacement
;; if it turns out not to be a simple string.
;;
;; For major modes, M-: mode-name
;; For minor modes, M-: (cadr (assq 'MODE minor-mode-alist))
;; for the minor MODE in question.
;;
;; Conversely, you may incorporate additional mode-line constructs in
;; your replacement values, if you so wish. e.g.:
;;
;; (delight 'emacs-lisp-mode
;;          '("Elisp" (lexical-binding ":Lex" ":Dyn"))
;;          'lisp-mode)
;;
;; See `mode-line-format' for information about mode-line constructs,
;; and M-: (info "(elisp) Mode Line Format") for further details.
;;
;; Also bear in mind that some modes may dynamically update these
;; values themselves (for instance dired-mode updates mode-name if
;; you change the sorting criteria) in which cases this library may
;; prove inadequate.

;;; Code:

(defvar delighted-modes ()
  "List of specs for modifying the display of mode names in the mode line.

See `delight'.")

;;;###autoload
(defun delight (spec &optional value file)
  "Modify the lighter value displayed in the mode line for the given mode SPEC
if and when the mode is loaded.

SPEC can be either a mode symbol, or a list containing multiple elements of
the form (MODE VALUE FILE).

For minor modes, VALUE is the replacement lighter value (or nil to disable)
to set in the `minor-mode-alist' variable. For major modes VALUE is the
replacement buffer-local `mode-name' value to use when a buffer changes to
that mode.

In both cases VALUE is commonly a string, but may in fact contain any valid
mode-line construct. See `mode-line-format' for details.

The FILE argument is passed through to `eval-after-load'. If FILE is nil then
the mode symbol is passed as the required feature."
  (add-hook 'after-change-major-mode-hook 'delight-major-mode)
  (let ((glum (if (consp spec) spec (list (list spec value file)))))
    (while glum
      (destructuring-bind (mode &optional value file) (pop glum)
        (assq-delete-all mode delighted-modes)
        (add-to-list 'delighted-modes (list mode value file))
        (eval-after-load (or file mode)
          `(let ((minor-delight (assq ',mode minor-mode-alist)))
             (when minor-delight
               (setcar (cdr minor-delight) ',value))))))))

(defun delight-major-mode ()
  "Delight the 'pretty name' of the current buffer's major mode
when displayed in the mode-line.

When `mode-name' is displayed in other contexts (such as in the
`describe-mode' help buffer), its original value will be used."
  (let ((major-delight (assq major-mode delighted-modes)))
    (when major-delight
      (setq mode-name `(inhibit-mode-name-delight
                        ,mode-name ;; glum
                        ,(cadr major-delight)))))) ;; delighted

(defadvice format-mode-line (around delighted-modes-are-glum activate)
  "Delighted modes should exhibit their original `mode-name' when
`format-mode-line' is called. See `delight-major-mode'."
  (let ((inhibit-mode-name-delight t))
    ad-do-it))

(provide 'delight)

;;; delight.el ends here

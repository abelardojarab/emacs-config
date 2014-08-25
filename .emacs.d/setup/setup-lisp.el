;;; setup-lisp.el ---

;; Copyright (C) 2014  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc25288.pdx.intel.com>
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

;; Rainbow delimiters
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(when (require 'rainbow-delimiters nil 'noerror)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Quack
(require 'quack)

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

(provide 'setup-lisp)
;;; setup-lisp.el ends here

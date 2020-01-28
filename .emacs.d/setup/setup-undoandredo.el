;;; setup-undoandredo.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

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

;; Redo
(use-package redo+
  :bind ("C-y" . redo))

;; Better undo
(use-package undo-tree
  :bind (("C-S-z"    . undo-tree-redo)
         ("C-z"      . undo-tree-undo)
         ("<undo>"   . undo-tree-undo)
         :map ctl-x-map
         ("u"        . undo-tree-undo))
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode)
  :commands global-undo-tree-mode
  :custom ((undo-no-redo                        t)
           (undo-tree-visualizer-diff           t)
           (undo-tree-visualizer-timestamps     t)
           (undo-tree-auto-save                 t)
           (undo-tree-enable-undo-in-region     t)
           (undo-tree-auto-save-history         t))
  :config (progn
            (setq undo-tree-history-directory-alist (list
                                                     (cons "."
                                                           (concat my/emacs-cache-dir
                                                                   "/undo-tree-hist/"))))

            ;; Remove text properties from history
            ;; https://emacs.stackexchange.com/questions/32194/undo-tree-history-file-without-text-properties
            (cl-defstruct (copy-tree*
                           (:constructor copy-tree*-mem (&optional stack stack-new (hash (make-hash-table)))))
              stack stack-new hash)

            (defmacro copy-tree*--push (el el-new mem &optional hash)
              (let ((my-el (make-symbol "my-el"))
                    (my-el-new (make-symbol "my-el-new"))) ; makes sure `el' is only evaluated once
                (append `(let ((,my-el ,el)
                               (,my-el-new ,el-new))
                           (push ,my-el (copy-tree*-stack ,mem))
                           (push ,my-el-new (copy-tree*-stack-new ,mem)))
                        (and hash
                             `((puthash ,my-el ,my-el-new (copy-tree*-hash ,mem))))
                        (list my-el-new))))

            (defmacro copy-tree*--pop (el el-new mem)
              `(setq ,el (pop (copy-tree*-stack ,mem))
                     ,el-new (pop (copy-tree*-stack-new mem))))

            (defun copy-tree*--copy-node (node mem vecp)
              (if (or (consp node)
                      (and vecp (vectorp node)))
                  (let ((existing-node (gethash node (copy-tree*-hash mem))))
                    (if existing-node
                        existing-node
                      (copy-tree*--push node (if (consp node)
                                                 (cons nil nil)
                                               (make-vector (length node) nil))
                                        mem t)))
                node))

            (defun copy-tree* (tree &optional vecp)
              "Structure preserving version of `cl-copy-tree'."
              (if (or (consp tree)
                      (and vecp (vectorp tree)))
                  (let* ((tree-new (if (consp tree) (cons nil nil)
                                     (make-vector (length tree) nil)))
                         (mem (copy-tree*-mem))
                         next
                         next-new)
                    (copy-tree*--push tree tree-new mem t)
                    (while (copy-tree*--pop next next-new mem)
                      (cond
                       ((consp next)
                        (setcar next-new (copy-tree*--copy-node (car next) mem vecp))
                        (setcdr next-new (copy-tree*--copy-node (cdr next) mem vecp)))
                       ((and vecp (vectorp next))
                        (cl-loop for i from 0 below (length next) do
                                 (aset next-new i (copy-tree*--copy-node (aref next i) mem vecp))))))
                    tree-new)
                tree))


            (defun undo-tree-seq-unprop (x)
              "Remove text properties on all strings of seq."
              (cond
               ((stringp x)
                (substring-no-properties x))
               ((consp x)
                (cons (undo-tree-seq-unprop (car x)) (undo-tree-seq-unprop (cdr x))))
               ((null x)
                nil)
               ((seqp x)
                (apply (type-of x) (seq-map (lambda (x) (undo-tree-seq-unprop x)) x)))
               (t x)))

            (defun undo-tree-string (&optional filename)
              "Return undo-tree as a string.
If FILENAME is a string save this string in file with name FILENAME.
Ask for the file name in the interactive case."
              (interactive "FFile name: ") ;; just to ease testing
              (when (eq buffer-undo-list t)
                (user-error "No undo information in this buffer"))
              (undo-list-transfer-to-tree)
              (when (and buffer-undo-tree (not (eq buffer-undo-tree t)))
                (condition-case nil
                    (progn
                      (undo-tree-clear-visualizer-data buffer-undo-tree)
                      (undo-tree-kill-visualizer))
                  (error (undo-tree-clear-visualizer-data buffer-undo-tree)))
                (let ((buff (current-buffer))
                      tree
                      return-string)
                  (unwind-protect
                      (progn
                        ;; transform undo-tree into non-circular structure, and make
                        ;; temporary copy
                        (undo-tree-decircle buffer-undo-tree)
                        (setq tree (copy-tree* buffer-undo-tree t))
                        ;; discard undo-tree object pool before saving
                        (setf (undo-tree-object-pool tree) nil)
                        (undo-tree-mapc (lambda (node)
                                          (setf (undo-tree-node-undo node) (undo-tree-seq-unprop (undo-tree-node-undo node)))
                                          (setf (undo-tree-node-redo node) (undo-tree-seq-unprop (undo-tree-node-redo node)))
                                          )
                                        (undo-tree-root tree))
                        (with-auto-compression-mode
                          (with-temp-buffer
                            (prin1 (sha1 buff) (current-buffer))
                            (terpri (current-buffer))
                            (let ((print-circle t)) (prin1 tree (current-buffer)))
                            (setq return-string (buffer-string))
                            (when filename
                              (write-file filename)))))
                    ;; restore circular undo-tree data structure
                    (undo-tree-recircle buffer-undo-tree))
                  return-string)))))

(provide 'setup-undoandredo)
;;; setup-undoandredo.el ends here

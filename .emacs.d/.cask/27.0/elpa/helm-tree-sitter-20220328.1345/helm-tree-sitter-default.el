;;; helm-tree-sitter-default.el --- Helm interface for tree-sitter -*- lexical-binding: t -*-

;; Copyright (C) 2021 ~ 2022 Giedrius Jonikas <giedriusj1@gmail.com>

;; Author: Giedrius Jonikas <giedriusj1@gmail.com>
;; Version: 0.1.0
;; URL: https://gitlab.com/giedriusj1/helm-tree-sitter

;; Package-Requires: ((emacs "25.1") (helm "3.6.2") (tree-sitter "0.16.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Provides default function for dealing with code we don't have specific
;; handlers for.

;;; Code:

(defun helm-tree-sitter-default-fn (elem)
  "Simple helm-tree-sitter handler for when we don't know how to
handle this specific mode.
Argument ELEM is `helm-tree-sitter-core-elem' representing the node."

  (unless (helm-tree-sitter-core-elem-p elem)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-core-elem-p elem)))

  (concat
   (helm-tree-sitter-utils-prepend-depth-if-needed elem)
   (propertize (symbol-name (helm-tree-sitter-core-elem-node-type elem))
               'face 'italic)
   ": "
   (s-truncate 90 (s-replace "\n" " "
                             (helm-tree-sitter-utils-get-node-text (helm-tree-sitter-core-elem-node elem))))))


(provide 'helm-tree-sitter-default)
;;; 'helm-tree-sitter-default.el ends here

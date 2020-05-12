;;; competitive-programming-snippets.el --- Competitive Programming snippets for Yasnippet -*- lexical-binding: t -*-

;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; Version: 1.1.3
;; URL: https://github.com/sei40kr/competitive-programming-snippets
;; Package-Requires: ((emacs "26") (yasnippet "0.8.0"))
;; Keywords: tools

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Competitive Programming snippets for Yasnippet.

;;; Code:

(require 'yasnippet)

(defconst competitive-programming-snippets-dir
  (expand-file-name
   "snippets"
   (file-name-directory
    (cond
     (load-in-progress load-file-name)
     ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
      byte-compile-current-file)
     (:else (buffer-file-name))))))

;;;###autoload
(defun competitive-programming-snippets-initialize ()
  "Load the `competitive-programming-snippets' snippets directory."
  ;; NOTE: we add the symbol `competitive-programming-snippets-dir' rather than
  ;; its value, so that yasnippet will automatically find the directory
  ;; after this package is updated (i.e., moves directory).
  (add-to-list 'yas-snippet-dirs 'competitive-programming-snippets-dir t)
  (yas-load-directory competitive-programming-snippets-dir t))

(provide 'competitive-programming-snippets)

;;; competitive-programming-snippets.el ends here

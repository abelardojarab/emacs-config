;;; jedi-eldoc.el --- Eldoc with emacs-jedi

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-jedi-eldoc
;; Version: 0.01

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

;;; Code:

(require 'jedi)
(require 'eldoc)

(defgroup jedi-eldoc nil
  "Eldoc for jedi"
  :group 'jedi
  :prefix "jedi-eldoc:")

(defface jedi-eldoc:highlight-function-argument
  '((t (:inherit eldoc-highlight-function-argument)))
  "Face of current function argument"
  :group 'jedi-eldoc)

(defun* jedi-eldoc:format--for-eldoc (&key params index call_name)
  (let ((current-arg (nth index params)))
    (if current-arg
        (setf (nth index params)
              (propertize current-arg
                          'face 'jedi-eldoc:highlight-function-argument)))
    (concat call_name "(" (mapconcat #'identity params ", ") ")")))

(defun jedi-eldoc:format-for-eldoc (args)
  (when args
    (eldoc-message (apply #'jedi-eldoc:format--for-eldoc args))))

(defun jedi-eldoc:documentation-function ()
  (deferred:nextc
    (jedi:call-deferred 'get_in_function_call)
    #'jedi-eldoc:format-for-eldoc)
  nil)

(defun jedi-eldoc:init ()
  (setq jedi:get-in-function-call--d t) ;; disable jedi's popup documentation
  (set (make-local-variable 'eldoc-documentation-function)
       #'jedi-eldoc:documentation-function)
  (turn-on-eldoc-mode))

(define-minor-mode jedi-eldoc-mode
  "Jedi Eldoc mode."
  :group 'jedi-eldoc
  (if jedi-eldoc-mode
      (jedi-eldoc:init)))

(provide 'jedi-eldoc)

;;; jedi-eldoc.el ends here

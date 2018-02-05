;;; setup-org-html.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Abelardo Jara-Berrocal

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

;; CSS for the HTML
(setq org-html-style-include-scripts nil
      org-html-style-include-default nil
      org-html-htmlize-output-type 'css)

(setq org-html-style
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://raw.githubusercontent.com/thi-ng/org-spec/master/css/style.css\"/>")

;; Org export compatible with Twitter Bootstrap
(use-package ox-twbs
  :load-path (lambda () (expand-file-name "ox-twbs/" user-emacs-directory))
  :after org
  :commands org-twbs-publish-to-html)

(provide 'setup-org-html)
;;; setup-org-html.el ends here

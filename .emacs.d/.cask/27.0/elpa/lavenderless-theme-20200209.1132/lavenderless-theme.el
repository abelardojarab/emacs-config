;;; lavenderless-theme.el --- A colorless theme inspired by Lavender

;; Copyright (C) 2019–2020 Thomas Letan
;;
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
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;; Author: Thomas Letan <contact@thomasletan.fr>
;; URL: https://git.sr.ht/~lthms/colorless-themes.el
;; Package-Version: 20200209.1132
;; Version: 0.1
;; Package-Requires: ((colorless-themes "0.1"))
;; License: GPL-3
;; Keywords: faces theme

;;; Commentary:
;; The main source of inspiration of this theme is Lavender.
;;
;; https://github.com/emacsfodder/emacs-lavender-theme/

;;; Code:
(require 'colorless-themes)

(colorless-themes-make lavenderless
                       "#29222E"    ; bg
                       "#362145"    ; bg+
                       "#3b3341"    ; current-line
                       "#51415C"    ; fade
                       "#E0CEED"    ; fg
                       "#d2c2d7"    ; fg+
                       "#68d3a7"    ; primary
                       "#cc3333"    ; red
                       "#FF6200"    ; orange
                       "#F4DC97"    ; yellow
                       "#A6E22E")   ; green

(provide 'lavenderless-theme)
;;; lavenderless-theme.el ends here

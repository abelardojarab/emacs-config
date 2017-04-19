;;; hexrgb.el --- Functions to manipulate RGB hex strings
;;
;; Filename: hexrgb.el
;; Description: Manipulate RGB hex strings
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2004-2007, Drew Adams, all rights reserved.
;; Created: Mon Sep 20 22:58:45 2004
;; Version: 21.0
;; Last-Updated: Sun Jan 21 15:09:36 2007 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 478
;; URL: http://www.emacswiki.org/cgi-bin/wiki/hexrgb.el
;; Keywords: number, hex, rgb, color, background, frames, display
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Functions to manipulate RGB hex strings.
;;
;;  An RGB hex string, such as used as a frame background-color
;;  property, is a string of 3 * n + 1 characters, the first of which
;;  is "#".  The other characters are hex digits, in three groups
;;  representing (from the left): red, green, and blue hex codes.
;;
;;  The functions here manipulate such strings.
;;
;;  Constants defined here:
;;
;;    `hexrgb-defined-colors', `hexrgb-defined-colors-alist'.
;;
;;  Commands defined here:
;;
;;    `hexrgb-blue', `hexrgb-complement', `hexrgb-green',
;;    `hexrgb-hue', `hexrgb-read-color', `hexrgb-red',
;;    `hexrgb-saturation', `hexrgb-value'.
;;
;;  Non-interactive functions defined here:
;;
;;    `hexrgb-approx-equal', `hexrgb-color-name-to-hex',
;;    `hexrgb-color-values-to-hex', `hexrgb-hex-char-to-integer',
;;    `hexrgb-hex-to-hsv', `hexrgb-hex-to-rgb', `hexrgb-hsv-to-hex',
;;    `hexrgb-hex-to-int', `hexrgb-hsv-to-rgb',
;;    `hexrgb-increment-blue', `hexrgb-increment-equal-rgb',
;;    `hexrgb-increment-green', `hexrgb-increment-hex',
;;    `hexrgb-increment-red', `hexrgb-int-to-hex',
;;    `hexrgb-rgb-hex-string-p', `hexrgb-rgb-to-hex',
;;    `hexrgb-rgb-to-hsv'.
;;
;;
;;  Add this to your initialization file (~/.emacs or ~/_emacs):
;;
;;    (require 'hexrgb)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2007/01/21 dadams
;;    hexrgb-read-color: Error if empty string (and not allow-empty-name-p).
;; 2006/06/06 dadams
;;    Added: hexrgb-defined-colors(-alist).  Use instead of (x-defined-colors).
;;    hexrgb-(red|green|blue): Added interactive specs.
;; 2006/06/04 dadams
;;    hexrgb-read-color: Added optional arg allow-empty-name-p.
;; 2006/06/02 dadams
;;    Added: hexrgb-rgb-hex-string-p.  Used it.
;; 2006/05/30 dadams
;;    Added: hexrgb-hex-to-(hsv|rgb), hexrgb-hsv-to-hex, hexrgb-color-name-to-hex,
;;           hexrgb-complement, hexrgb-read-color, hexrgb-hue, hexrgb-saturation,
;;           hexrgb-value, hexrgb-red, hexrgb-blue, hexrgb-green.
;;    approx-equal: Add optional fuzz factor arguments.  Changed the algorithm.
;;    Renamed: approx-equal to hexrgb-approx-equal.
;;    hexrgb-rgb-to-hsv: Changed test from < to <=: (when (<= hue 0.0)...).
;;    hexrgb-hsv-to-rgb: Treat hue = 0.0 (int 0) the same as hue = 1.0 (int 6).
;;    hexrgb-rgb-to-hex, hexrgb-increment-hex: Corrected doc strings.
;; 2006/05/22 dadams
;;    Added: hexrgb-hsv-to-hex, hexrgb-rgb-to-hex.  Require cl.el when byte-compile.
;; 2005/08/09 dadams
;;    hexrgb-rgb-to-hsv: Side-stepped Emacs-20 bug in comparing NaN.
;;    hexrgb-increment-*: Added optional arg wrap-p.
;;    hexrgb-increment-hex: Prevent wrap if not wrap-p.
;; 2005/08/02 dadams
;;    hexrgb-rgb-to-hes: Bug fix: If delta is zero, then so are hue and saturation.
;; 2005/06/24 dadams
;;    hexrgb-rgb-to-hsv: Bug fix: test for NaN (e.g. on divide by zero).
;; 2005/02/08 dadams
;;    hexrgb-hsv-to-rgb: Bug fix (typo: p, q -> pp, qq; added ww).
;; 2005/01/09 dadams
;;    hexrgb-int-to-hex: Fixed bug in hexrgb-int-to-hex: nb-digits not respected.
;;    Added: hexrgb-hsv-to-rgb, hexrgb-rgb-to-hsv, approx-equal.
;;    Renamed old hexrgb-increment-value to hexrgb-increment-equal-rgb.
;; 2005/01/05 dadams
;;    hexrgb-int-to-hex: Used a suggestion from Juri Linkov.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; case; plus, for Emacs < 20: when, unless

;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Not used here, but put here to be available to libraries that use `hexrgb.el'.
;;;###autoload
(defconst hexrgb-defined-colors (eval-when-compile (x-defined-colors))
  "List of all supported colors.")

;;;###autoload
(defconst hexrgb-defined-colors-alist (eval-when-compile (mapcar #'list (x-defined-colors)))
  "Alist of all supported colors, for use in completion.")

;;;###autoload
(defun hexrgb-read-color (&optional convert-to-RGB-p allow-empty-name-p prompt)
  "Read a color name or RGB hex value: #RRRRGGGGBBBB.
Completion is available for color names, but not for RGB hex strings.
If the user inputs an RGB hex string, it must have the form
#XXXXXXXXXXXX or XXXXXXXXXXXX, where each X is a hex digit.  The
number of Xs must be a multiple of 3, with the same number of Xs for
each of red, green, and blue.  The order is red, green, blue.

Input is checked to be sure it represents a valid color.  If not, an
error is raised (but see exception for empty input with non-nil
ALLOW-EMPTY-NAME-P).

Interactively, or with optional arg CONVERT-TO-RGB-P non-nil, an input
color name is converted to an RGB hex string.

Optional arg ALLOW-EMPTY-NAME-P controls what happens if the user
enters an empty color name (that is, just hits `RET').  If non-nil,
then an empty color name, \"\", is returned.  If nil, then empty input
raises an error.  Programs must test for \"\" if ALLOW-EMPTY-NAME-P is
non-nil.  They can then perform an appropriate action in case of empty
input.

Optional arg PROMPT is the prompt; a default prompt is used if nil."
  (interactive "p")                     ; Always convert to RGB interactively.
  (let* ((completion-ignore-case t)
	 (color (completing-read (or prompt "Color (name or #R+G+B+): ")
                                 hexrgb-defined-colors-alist))
         (hex-string (hexrgb-rgb-hex-string-p color t)))
    (if (and allow-empty-name-p (string= "" color))
        ""
      (when (and hex-string (not (eq 0 hex-string)))
        (setq color (concat "#" color))) ; No #; add it.
      (unless hex-string
        (when (or (string= "" color)
                  (not (if (fboundp 'test-completion) ; Not defined in Emacs 20.
                           (test-completion color hexrgb-defined-colors-alist)
                         (try-completion color hexrgb-defined-colors-alist))))
          (error "No such color: %S" color))
        (when convert-to-RGB-p (setq color (hexrgb-color-name-to-hex color))))
      (when (interactive-p) (message "Color: `%s'" color))
      color)))

;;;###autoload
(defun hexrgb-rgb-hex-string-p (color &optional laxp)
  "Non-nil if COLOR is an RGB string #XXXXXXXXXXXX.
Each X is a hex digit.  The number of Xs must be a multiple of 3, with
the same number of Xs for each of red, green, and blue.

Non-nil optional arg LAXP means that the initial `#' is optional.  In
that case, for a valid string of hex digits: when # is present 0 is
returned; otherwise, t is returned."
  (or (string-match "^#\\([a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)+$" color)
      (and laxp (string-match "^\\([a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)+$" color) t)))

;;;###autoload
(defun hexrgb-complement (color)
  "Return the color that is the complement of COLOR."
  (interactive (list (hexrgb-read-color)))
  (setq color (hexrgb-color-name-to-hex color))
  (let ((red (hexrgb-red color))
        (green (hexrgb-green color))
        (blue (hexrgb-blue color)))
    (setq color (hexrgb-rgb-to-hex (- 1.0 red) (- 1.0 green) (- 1.0 blue))))
  (when (interactive-p) (message "Complement: `%s'" color))
  color)

;;;###autoload
(defun hexrgb-hue (color)
  "Return the hue component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\"."
  (interactive (list (hexrgb-read-color)))
  (setq color (hexrgb-color-name-to-hex color))    
  (car (hexrgb-rgb-to-hsv (hexrgb-red color) (hexrgb-green color) (hexrgb-blue color))))

;;;###autoload
(defun hexrgb-saturation (color)
  "Return the saturation component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\"."
  (interactive (list (hexrgb-read-color)))
  (setq color (hexrgb-color-name-to-hex color))
  (cadr (hexrgb-rgb-to-hsv (hexrgb-red color) (hexrgb-green color) (hexrgb-blue color))))

;;;###autoload
(defun hexrgb-value (color)
  "Return the value component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\"."
  (interactive (list (hexrgb-read-color)))
  (setq color (hexrgb-color-name-to-hex color))
  (caddr (hexrgb-rgb-to-hsv (hexrgb-red color) (hexrgb-green color) (hexrgb-blue color))))

;;;###autoload
(defun hexrgb-red (color)
  "Return the red component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\"."
  (interactive (list (hexrgb-read-color)))
  (setq color (hexrgb-color-name-to-hex color))
  (/ (hexrgb-hex-to-int (substring color 1 (1+ (/ (1- (length color)) 3))))
     (expt 16.0 (/ (1- (length color)) 3.0))))
    
;;;###autoload
(defun hexrgb-green (color)
  "Return the green component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\"."
  (interactive (list (hexrgb-read-color)))
  (setq color (hexrgb-color-name-to-hex color))
  (let* ((len (/ (1- (length color)) 3))
         (start (1+ len)))
    (/ (hexrgb-hex-to-int (substring color start (+ start len)))
       (expt 16.0 (/ (1- (length color)) 3.0)))))
    
;;;###autoload
(defun hexrgb-blue (color)
  "Return the blue component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\"."
  (interactive (list (hexrgb-read-color)))
  (setq color (hexrgb-color-name-to-hex color))
  (let* ((len (/ (1- (length color)) 3))
         (start (+ 1 len len)))
    (/ (hexrgb-hex-to-int (substring color start (+ start len)))
       (expt 16.0 (/ (1- (length color)) 3.0)))))

;;;###autoload
(defun hexrgb-rgb-to-hsv (red green blue)
  "Convert RED, GREEN, BLUE components to HSV (hue, saturation, value).
Each input component is 0.0 to 1.0, inclusive.
Returns a list of HSV components of value 0.0 to 1.0, inclusive."
  (let* ((min (min red green blue))
         (max (max red green blue))
         (value max)
         (delta (- max min))
         hue saturation)
    (if (hexrgb-approx-equal 0.0 delta)
        (setq hue 0.0 saturation 0.0) ; Gray scale - no color; only value.
      (if (and (condition-case nil
                   (setq saturation (/ delta max))
                 (arith-error nil))
               (or (< emacs-major-version 21) ; Emacs 20 bug makes next test fail falsely.
                   (not (equal 0.0e+NaN saturation)))) ; Must be a number, not NaN.
          (if (hexrgb-approx-equal 0.0 saturation)
              (setq hue 0.0 saturation 0.0) ; Again, no color; only value.
            ;; Color
            (if (hexrgb-approx-equal red max)
                (setq hue (/ (- green blue) delta)) ; Between yellow & magenta.
              (if (hexrgb-approx-equal green max)
                  (setq hue (+ 2.0 (/ (- blue red) delta))) ; Between cyan & yellow.
                (setq hue (+ 4.0 (/ (- red green) delta))))) ; Between magenta & cyan.
            (setq hue (/ hue 6.0))
            (when (<= hue 0.0)(setq hue (+ hue 1.0))))
        (setq saturation 0.0 hue 0.0))) ; Div by zero (max=0): H:=0, S:=0. (Hue undefined.)
    (list hue saturation value)))

;;;###autoload
(defun hexrgb-hsv-to-rgb (hue saturation value)
  "Convert HUE, SATURATION, VALUE components to RGB (red, green, blue).
Each input component is 0.0 to 1.0, inclusive.
Returns a list of RGB components of value 0.0 to 1.0, inclusive."
  (let (red green blue int-hue fract pp qq tt ww)
    (if (hexrgb-approx-equal 0.0 saturation)
        (setq red value green value blue value) ; Gray
      (setq hue (* hue 6.0)             ; Sectors: 0 to 5
            int-hue (floor hue)
            fract (- hue int-hue)
            pp (* value (- 1 saturation))
            qq (* value (- 1 (* saturation fract)))
            ww (* value (- 1 (* saturation (- 1 (- hue int-hue))))))
      (case int-hue
        ((0 6) (setq red value green ww blue pp))
        (1 (setq red qq green value blue pp))
        (2 (setq red pp green value blue ww))
        (3 (setq red pp green qq blue value))
        (4 (setq red ww green pp blue value))
        (otherwise (setq red value green pp blue qq))))
    (list red green blue)))

;;;###autoload
(defun hexrgb-hsv-to-hex (hue saturation value)
  "Return the hex RBG color string for inputs HUE, SATURATION, VALUE.
The inputs are each in the range 0 to 1.
The output string is of the form \"#RRRRGGGGBBBB\"."
  (hexrgb-color-values-to-hex
   (mapcar (lambda (x) (floor (* x 65535.0))) (hexrgb-hsv-to-rgb hue saturation value))))

;;;###autoload
(defun hexrgb-rgb-to-hex (red green blue)
  "Return the hex RBG color string for inputs RED, GREEN, BLUE.
The inputs are each in the range 0 to 1.
The output string is of the form \"#RRRRGGGGBBBB\"."
  (hexrgb-color-values-to-hex
   (mapcar (lambda (x) (floor (* x 65535.0))) (list red green blue))))

;;;###autoload
(defun hexrgb-hex-to-hsv (color)
  "Return a list of HSV (hue, saturation, value) color components.
Each component is a value from 0.0 to 1.0, inclusive.
COLOR is a color name or a hex RGB string that starts with \"#\" and
is followed by an equal number of hex digits for red, green, and blue
components."
  (let ((rgb-components (hexrgb-hex-to-rgb color)))
    (apply #'hexrgb-rgb-to-hsv rgb-components)))

;;;###autoload
(defun hexrgb-hex-to-rgb (color)
  "Return a list of RGB (red, green, blue) color components.
Each component is a value from 0.0 to 1.0, inclusive.
COLOR is a color name or a hex RGB string that starts with \"#\" and
is followed by an equal number of hex digits for red, green, and blue
components."
  (unless (hexrgb-rgb-hex-string-p color) (setq color (hexrgb-color-name-to-hex color)))
  (let ((len (/ (1- (length color)) 3)))
    (list (/ (hexrgb-hex-to-int (substring color 1 (1+ len))) 65535.0)
          (/ (hexrgb-hex-to-int (substring color (1+ len) (+ 1 len len))) 65535.0)
          (/ (hexrgb-hex-to-int (substring color (+ 1 len len))) 65535.0))))

;;;###autoload
(defun hexrgb-color-name-to-hex (color)
  "Return the RGB hex string for the COLOR name, starting with \"#\".
If COLOR is already a string starting with \"#\", then just return it."
  (let ((components (x-color-values color)))
    (unless components (error "No such color: %S" color))
    (unless (hexrgb-rgb-hex-string-p color)
      (setq color (hexrgb-color-values-to-hex components))))
  color)

;; Just hard-code 4 as the number of hex digits, since `x-color-values'
;; seems to produce appropriate integer values for this value.
;;
;; Color "components" would be better in the name than color "value"
;; but this name follows the Emacs tradition (e.g. `x-color-values',
;; 'ps-color-values', `ps-e-x-color-values').
;;;###autoload
(defun hexrgb-color-values-to-hex (values)
  "Convert list of rgb color VALUES to a hex string, #XXXXXXXXXXXX.
Each X in the string is a hexadecimal digit.
Input VALUES is as for the output of `x-color-values'."
  (concat "#"
          (hexrgb-int-to-hex (nth 0 values) 4) ; red
          (hexrgb-int-to-hex (nth 1 values) 4) ; green
          (hexrgb-int-to-hex (nth 2 values) 4))) ; blue

;;;###autoload
(defun hexrgb-increment-red (hex nb-digits increment &optional wrap-p)
  "Increment red value of rgb string HEX by INCREMENT.
String HEX starts with \"#\".  Each color is NB-DIGITS hex digits long.
If optional arg WRAP-P is non-nil, then the result wraps around zero.
For example, incrementing \"#FFFFFFFFF\" by 1 will cause it to wrap
around to \"#000000000\"."
  (concat "#"
          (hexrgb-increment-hex (substring hex 1 (1+ nb-digits)) increment nb-digits wrap-p)
          (substring hex (1+ nb-digits) (1+ (* nb-digits 2)))
          (substring hex (1+ (* nb-digits 2)))))

;;;###autoload
(defun hexrgb-increment-green (hex nb-digits increment &optional wrap-p)
  "Increment green value of rgb string HEX by INCREMENT.
String HEX starts with \"#\".  Each color is NB-DIGITS hex digits long.
For example, incrementing \"#FFFFFFFFF\" by 1 will cause it to wrap
around to \"#000000000\"."
  (concat
   "#" (substring hex 1 (1+ nb-digits))
   (hexrgb-increment-hex (substring hex (1+ nb-digits) (1+ (* nb-digits 2)))
                         increment
                         nb-digits
                         wrap-p)
   (substring hex (1+ (* nb-digits 2)))))

;;;###autoload
(defun hexrgb-increment-blue (hex nb-digits increment &optional wrap-p)
  "Increment blue value of rgb string HEX by INCREMENT.
String HEX starts with \"#\".  Each color is NB-DIGITS hex digits long.
For example, incrementing \"#FFFFFFFFF\" by 1 will cause it to wrap
around to \"#000000000\"."
  (concat "#" (substring hex 1 (1+ (* nb-digits 2)))
          (hexrgb-increment-hex (substring hex (1+ (* nb-digits 2)))
                                increment
                                nb-digits
                                wrap-p)))

;;;###autoload
(defun hexrgb-increment-equal-rgb (hex nb-digits increment &optional wrap-p)
  "Increment each color value (r,g,b) of rgb string HEX by INCREMENT.
String HEX starts with \"#\".  Each color is NB-DIGITS hex digits long.
For example, incrementing \"#FFFFFFFFF\" by 1 will cause it to wrap
around to \"#000000000\"."
  (concat
   "#" (hexrgb-increment-hex (substring hex 1 (1+ nb-digits)) increment nb-digits wrap-p)
   (hexrgb-increment-hex (substring hex (1+ nb-digits) (1+ (* nb-digits 2)))
                         increment
                         nb-digits
                         wrap-p)
   (hexrgb-increment-hex (substring hex (1+ (* nb-digits 2))) increment nb-digits wrap-p)))

;;;###autoload
(defun hexrgb-increment-hex (hex increment nb-digits &optional wrap-p)
  "Increment HEX number (a string NB-DIGITS long) by INCREMENT.
For example, incrementing \"FFFFFFFFF\" by 1 will cause it to wrap
around to \"000000000\"."
  (let* ((int (hexrgb-hex-to-int hex))
         (new-int (+ increment int)))
    (if (or wrap-p
            (and (>= int 0)             ; Not too large for the machine.
                 (>= new-int 0)         ; For the case where increment < 0.
                 (<= (length (format (concat "%X") new-int)) nb-digits))) ; Not too long.
        (hexrgb-int-to-hex new-int nb-digits) ; Use incremented number.
      hex)))                                  ; Don't increment.

;;;###autoload
(defun hexrgb-hex-to-int (hex)
  "Convert HEX string argument to an integer.
The characters of HEX must be hex characters."
  (let* ((factor 1)
         (len (length hex))
         (indx (1- len))
         (int 0))
    (while (>= indx 0)
      (setq int (+ int (* factor (hexrgb-hex-char-to-integer (aref hex indx)))))
      (setq indx (1- indx))
      (setq factor (* 16 factor)))
    int))

;; From `hexl.el'.  This is the same as `hexl-hex-char-to-integer' defined there.
;;;###autoload
(defun hexrgb-hex-char-to-integer (character)
  "Take a CHARACTER and return its value as if it were a hex digit."
  (if (and (>= character ?0) (<= character ?9))
      (- character ?0)
    (let ((ch (logior character 32)))
      (if (and (>= ch ?a) (<= ch ?f))
	  (- ch (- ?a 10))
	(error "Invalid hex digit `%c'" ch)))))

;; Originally, I used the code from `int-to-hex-string' in `float.el'.
;; This version is thanks to Juri Linkov <juri@jurta.org>.
;;
;;;###autoload
(defun hexrgb-int-to-hex (int &optional nb-digits)
  "Convert integer argument INT to a #XXXXXXXXXXXX format hex string.
Each X in the output string is a hexadecimal digit.
NB-DIGITS is the number of hex digits.  If INT is too large to be
represented with NB-DIGITS, then the result is truncated from the
left.  So, for example, INT=256 and NB-DIGITS=2 returns \"00\", since
the hex equivalent of 256 decimal is 100, which is more than 2 digits."
  (setq nb-digits (or nb-digits 4))
  (substring (format (concat "%0" (int-to-string nb-digits) "X") int) (- nb-digits)))

;; Inspired by Elisp Info manual, node "Comparison of Numbers".
;;;###autoload
(defun hexrgb-approx-equal (x y &optional rfuzz afuzz)
  "Return non-nil if numbers X and Y are approximately equal.
RFUZZ is a relative fuzz factor.  AFUZZ is an absolute fuzz factor.
RFUZZ defaults to 1.0e-8.  AFUZZ defaults to (/ RFUZZ 10).
The algorithm is:
 (< (abs (- X Y)) (+ AFUZZ (* RFUZZ (+ (abs X) (abs Y)))))."
  (setq rfuzz (or rfuzz 1.0e-8) afuzz (or afuzz (/ rfuzz 10)))
  (< (abs (- x y)) (+ afuzz (* rfuzz (+ (abs x) (abs y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hexrgb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hexrgb.el ends here


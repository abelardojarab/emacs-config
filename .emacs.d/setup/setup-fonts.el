;;; setup-fonts.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2024  Abelardo Jara-Berrocal

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

(use-package faces
  :demand t
  :custom ((inhibit-compacting-font-caches t)
		   (redisplay-skip-fontification-on-input t))
  :commands fontify-frame
  :hook ((org-mode markdown-mode TeX-mode message-mode mu4e-view-mode) . variable-pitch-mode)
  :init (unless (fboundp 'set-default-font)
          (defun set-default-font (font frame)
            (interactive "Font Name-Size: ")
            (set-face-attribute 'default nil :font font)))
  :config (progn
			;; if gui do something in whatver type of emacs instance we are using
			(defun apply-if-gui (&rest action)
			  "Do specified ACTION if we're in a gui regardless of daemon or not."
			  (if (daemonp)
				  (add-hook 'after-make-frame-functions
							(lambda (frame)
							  (select-frame frame)
							  (if (display-graphic-p frame)
								  (apply action))))
				(if (display-graphic-p)
					(apply action))))

			;; Default font (cant be font with hyphen in the name like Inconsolata-g)
			(setq initial-frame-alist '((font . "MesloLGMDZ Nerd Font Mono-12")))
			(setq default-frame-alist '((font . "MesloLGMDZ Nerd Font Mono-12")))

			;; Emoji: 😄, 🤦, 🏴, ,  ;; should render as 3 color emojis and 2 glyphs
			(defun styling/set-backup-fonts()
			  "Set the emoji and glyph fonts."
			  ;; Default font (cant be font with hyphen in the name like Inconsolata-g)
			  (setq initial-frame-alist '((font . "MesloLGMDZ Nerd Font Mono-12")))
			  (setq default-frame-alist '((font . "MesloLGMDZ Nerd Font Mono-12")))

			  (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend)
			  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'prepend)
			  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'prepend)
			  (set-fontset-font t 'symbol "UbuntuMono Nerd Font" nil 'prepend))

			;; respect default terminal fonts
			;; if we're in a gui set the fonts appropriately
			;; for daemon sessions and and nondaemons
			(apply-if-gui 'styling/set-backup-fonts)

            ;; Prefer user choices
            (if (find-font (font-spec :name my/main-programming-font))
                (if (not (find-font (font-spec :name my/main-writing-font)))
                    (setq my/main-writing-font my/main-programming-font)))

            ;; Dynamic font adjusting based on monitor resolution, using Android fonts
            (defun fontify-frame (&optional frame)
              (interactive)
              (let ()
                (when (and (find-font (font-spec :name my/main-programming-font))
                           (find-font (font-spec :name my/main-writing-font)))

                  ;; I like italic comment face as long as the actual font supports it
                  ;; (which Hack does)
                  (set-face-italic font-lock-comment-face t)

                  ;; Adjust text size based on the active display.
                  ;; Bucket by display height (pixels) and adjust per platform:
                  ;; macOS reports logical pixels with Retina scaling, so its
                  ;; sizes are inherently larger; Linux/Windows report physical
                  ;; pixels so the same height needs a smaller point size.
                  (let* ((h (or (ignore-errors (display-pixel-height)) 1080))
                         (w (or (ignore-errors (display-pixel-width)) 1920))
                         (mm-h (or (ignore-errors (display-mm-height)) 0))
                         (dpi (if (and (numberp mm-h) (> mm-h 0))
                                  (/ (* h 25.4) mm-h)
                                96))
                         (mac-p (eq system-type 'darwin))
                         ;; Coarse buckets that work across HiDPI variants.
                         (base
                          (cond
                           ;; 5K / 6K Retina (Pro Display, iMac 27" 5K)
                           ((or (>= h 2500) (>= w 5000))   (if mac-p 16 15))
                           ;; 4K (3840x2160) or 2880x1800 Retina laptop
                           ((or (>= h 1900) (>= w 3500))   (if mac-p 14 13))
                           ;; QHD / Retina-equivalent (2560x1440)
                           ((or (>= h 1400) (>= w 2400))   (if mac-p 13 12))
                           ;; FHD (1920x1080)
                           ((or (>= h 1000) (>= w 1800))   (if mac-p 15 13))
                           ;; Smaller laptops, half-screen splits
                           (t                              (if mac-p 13 11))))
                         ;; Very high DPI on non-mac platforms — nudge up.
                         (base (if (and (not mac-p) (> dpi 140))
                                   (+ base 1) base)))
                    (setq my/main-programming-font-size (number-to-string base)
                          my/main-writing-font-size     (number-to-string (1+ base))))

                  ;; Apply fonts
                  (add-to-list 'default-frame-alist (cons 'font
                                                          (concat
                                                           my/main-programming-font
                                                           "-"
                                                           my/main-programming-font-size)))
                  (set-frame-font (concat my/main-programming-font
                                          "-"
                                          my/main-programming-font-size) t)
                  (set-face-attribute 'default nil
                                      :font (concat my/main-programming-font
                                                    "-"
                                                    my/main-programming-font-size)
                                      :weight 'regular
                                      :width  'semi-condensed)
                  (set-face-attribute 'fixed-pitch nil
                                      :font (concat my/main-programming-font
                                                    "-"
                                                    my/main-programming-font-size)
                                      :weight 'regular
                                      :width  'semi-condensed)
                  (set-face-attribute 'variable-pitch nil
                                      :font (concat my/main-writing-font
                                                    "-"
                                                    my/main-writing-font-size)
                                      :weight 'normal))

                ;; Use mathematical symbols
                (when (and (display-graphic-p)
                           (find-font (font-spec :name "MesloLGMDZ Nerd Font Mono")))
                  (let ((utf8-font "MesloLGMDZ Nerd Font Mono"))
                    (set-fontset-font "fontset-startup" '(#x000000 . #x3FFFFF) utf8-font)
                    (set-fontset-font "fontset-default" '(#x000000 . #x3FFFFF) utf8-font)
                    (set-fontset-font "fontset-standard" '(#x000000 . #x3FFFFF) utf8-font)))

                ;; Specify fonts for all unicode characters
                (when (and (member "MesloLGMDZ Nerd Font Mono" (font-family-list))
                           (display-graphic-p))
                  (set-fontset-font t 'unicode "MesloLGMDZ Nerd Font Mono" nil 'prepend))))

            ;; Fontify frame only for graphical mode
            (when (display-graphic-p)
              ;; Fontify current frame
              (fontify-frame nil)
              (let ((frame (selected-frame)))
                (fontify-frame frame))

              ;; Fontify any future frames for emacsclient
              (add-hook 'after-make-frame-functions #'fontify-frame)

              ;; hook for setting up UI when not running in daemon mode
              (add-hook 'emacs-startup-hook (lambda () (fontify-frame (selected-frame))))

              ;; The early fontify calls above can run before the frame is on
              ;; its final monitor / fully maximized, so `display-pixel-height'
              ;; reports the wrong resolution and we pick a smaller font bucket.
              ;; Recompute once the frame has settled so startup matches the
              ;; size we get after a theme switch.
              (run-with-idle-timer 0.5 nil
                                   (lambda ()
                                     (when (display-graphic-p)
                                       (fontify-frame (selected-frame))))))))

;; Fixed pitch for HTML
(defun fixed-pitch-mode ()
  (buffer-face-mode -1))
(add-hook 'html-mode-hook #'fixed-pitch-mode)
(add-hook 'nxml-mode-hook #'fixed-pitch-mode)

;; Pretty mode
(use-package pretty-mode
  :defer t
  :commands pretty-mode)

;; Prettier symbols
(use-package pretty-symbols-mode
  :defer t
  :if (and (fboundp 'global-prettify-symbols-mode)
           (display-graphic-p))
  :commands (prettify-symbols-mode
             global-prettify-symbols-mode)
  :hook (org-mode . my/org-mode/load-prettify-symbols)
  :custom (prettify-symbols-unprettify-at-point 'right-edge)
  :init (defun my/org-mode/load-prettify-symbols ()
          (interactive)
          (setq prettify-symbols-alist
                '(("[ ]" .  "☐")
				  ("[X]" . "☑" )
				  ("[-]" . "❍" )
				  ("lambda" .   "λ")
                  ("|>" .   "▷")
                  ("<|" .   "◁")
                  ("->>" .  "↠")
                  ("->"  .   "→")
                  ("<-"  .   "←")
                  ("=>"  .   "⇒")
                  ("<="  .   "≤")
                  (">="  .   "≥")))

          (prettify-symbols-mode 1)))

(defun set-icon-fonts (CODE-FONT-ALIST)
  "Utility to associate many unicode points with specified fonts."
  (--each CODE-FONT-ALIST
    (-let (((font . codes) it))
      (--each codes
        (set-fontset-font t `(,it . ,it) font)))))

;; Fallback font for non-ascii glyphs
(use-package unicode-fonts
  :defer t
  :if (display-graphic-p)
  :commands unicode-fonts-setup
  :config (progn
			:config
			;; Common math symbols
			(dolist (unicode-block '("Mathematical Alphanumeric Symbols"))
			  (push "JuliaMono" (cadr (assoc unicode-block unicode-fonts-block-font-mapping))))))

;; For Iosevka
(set-char-table-range composition-function-table ?+ '(["\\(?:++++\\)" 0 font-shape-gstring]))

;; font-utils - Utilities for Unicode characters
(use-package font-utils
  :demand t
  :custom
  (font-utils-less-feedback nil)
  (font-utils-use-persistent-storage "font-utils")
  (font-utils-use-memory-cache t))

;; ucs-utils - Utilities for Unicode characters
(use-package ucs-utils
  :demand t
  :custom
  (ucs-utils-trade-memory-for-speed t)
  (ucs-utils-use-persistent-storage "ucs-utils")
  (ucs-utils-hide-numbered-cjk-ideographs t))

(provide 'setup-fonts)
;;; setup-fonts.el ends here

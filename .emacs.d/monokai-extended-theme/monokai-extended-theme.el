;;; monokai-extended-theme.el --- Monokai Color Theme for Emacs.

;; Copyright (C) 2015, 2016 Katherine Whitlock
;;
;; Author: Katherine Whitlock <toroidalcode@gmail.com
;; Maintainer: Katherine Whitlock
;; URL: https://github.com/toroidal-code/monokai-extended.el
;; Version: 0.0.1
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 of the License.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.

;;; Commentary:

;; This theme builds on the great work done by lvillani and milouse at
;; https://github.com/milouse/el-monokai-theme
;; and hbin's molokai theme at https://github.com/hbin/molokai.
;;

;;; Code:

(deftheme monokai-extended)


(let* ((class '((class color) (min-colors 89)))
      ;; Monokai palette
      (monokai-bg             "#272822")
      (monokai-fg             "#F8F8F2")
      (monokai-fg2            "#75715E")
      (monokai-base1          "#141411")
      (monokai-base1-2        "#333330")
      (monokai-base2          "#383830")
      (monokai-base3          "#595959")
      (monokai-base4          "#E6E6E6")
      (monokai-base5          "#595959")
      (monokai-grapefruit     "#F92672")
      (monokai-red            "#DC322F")
      (monokai-orange+20      "#Cf3919")
      (monokai-red-true       "#ff0000")
      (monokai-orange+5       "#ef5939")
      (monokai-orange-5       "#FFA746")

      (monokai-orange         "#FD971F")
      (monokai-orange2        "#EC850E")
      (monokai-yellow         "#FFFF00")
      (monokai-goldenrod      "#E6DB74")
      (monokai-wheat          "#c4be89")
      (monokai-olive          "#808000")
      (monokai-green          "#008000")
      (monokai-green2         "#95C515")
      (monokai-green3         "#88AE24")
      (monokai-chartreuse     "#A6E22E")
      (monokai-lime           "#00ff00")
      (monokai-aqua           "#00ffff")
      (monokai-cyan           "#66D9EF")
      (monokai-cyan2          "#69C4EA")
      (monokai-cyan3          "#48ACD5")
      (monokai-blue           "#89BDFF")
      (monokai-slateblue      "#7070f0")
      (monokai-teal           "#008080")
      (monokai-purple         "#AE81FF")
      (monokai-purple2        "#9670E0")
      (monokai-purple3        "#7C61BD")
      (monokai-magenta        "#FD5FF1")
      (monokai-palevioletred  "#d33682")
      (monokai-maroon         "#960050")
      (monokai-darkmaroon     "#800000")
      (monokai-darkwine       "#1e0010")
      (monokai-dodgerblue     "#13354a")
      (monokai-white          "#ffffff")
      (monokai-black          "#000000")

      (monokai-grey-2         "#bcbcbc")
      (monokai-grey-1         "#8f8f8f")
      (monokai-grey           "#808080")
      (monokai-grey+2         "#403d3d")
      (monokai-grey+3         "#4c4745")
      (monokai-grey+5         "#232526")
      (monokai-grey+10        "#080808")
      (monokai-base01         "#465457")
      (monokai-base02         "#455354")
      (monokai-base03         "#293739")

      )
  (custom-theme-set-faces
   'monokai-extended
   ;; Base
   `(default ((t (:foreground ,monokai-fg :background ,monokai-bg))))
   `(cursor ((t (:background ,monokai-fg :foreground ,monokai-bg :inverse-video t))))
   `(mouse ((t (:background ,monokai-fg :foreground ,monokai-grapefruit :inverse-video t))))
   `(fringe ((t (:background ,monokai-bg))))
   `(highlight ((t (:background ,monokai-base2))))
   `(hl-line ((t (:background ,monokai-base1))))
   `(hl-sexp-background-color ((t (:background ,monokai-base1))))
   `(region ((t (:background ,monokai-base2))
             (t (:inverse-video t))))
   `(warning ((t (:foreground ,monokai-palevioletred :weight bold))))

   `(minibuffer-prompt ((t (:foreground ,monokai-orange :bold t))))  ; was fg2
   `(modeline ((t (:background ,monokai-base5 :foreground ,monokai-base4))))


   `(show-paren-match-face ((t (:background ,monokai-base5))))
   `(scroll-bar ((t (:foreground ,monokai-base4 :background ,monokai-bg))))
   `(popup-scroll-bar-background-face ((t (:background ,monokai-bg))))
   `(popup-scroll-bar-foreground-face ((t (:foreground ,monokai-base4))))
   `(punctuation ((t (:foreground ,monokai-grapefruit))))
   `(link ((t (:foreground ,monokai-cyan :underline t))))
   `(link-visited ((t (:foreground ,monokai-cyan :underline t :slant italic))))

   ;; Font Lock
   `(font-lock-builtin-face ((t (:foreground ,monokai-chartreuse))))
   `(font-lock-comment-face ((t (:foreground ,monokai-fg2))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,monokai-fg2))))
   `(font-lock-constant-face ((t (:foreground ,monokai-purple))))
   `(font-lock-doc-string-face ((t (:foreground ,monokai-goldenrod))))
   `(font-lock-function-name-face ((t (:foreground ,monokai-chartreuse))))
   `(font-lock-keyword-face ((t (:foreground ,monokai-grapefruit))))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,monokai-goldenrod))))
   `(font-lock-type-face ((t (:foreground ,monokai-cyan :italic t))))
   `(font-lock-variable-name-face ((t (:foreground ,monokai-orange)))) ; was green
   `(font-lock-warning-face ((t (:bold t :foreground ,monokai-chartreuse))))

   ;; Company
   `(company-tooltip ((t (:foreground ,monokai-base4 :background ,monokai-base2))))
   `(company-tooltip-selection ((t (:foreground ,monokai-orange :background "#333333"))))
   `(company-tooltip-annotation ((t (:foreground ,monokai-cyan :background ,monokai-base2))))
   `(company-tooltip-common ((t (:foreground ,monokai-chartreuse :background ,monokai-base2))))
   `(company-tooltip-common-selection ((t (:foreground ,monokai-orange :background ,monokai-base2))))
   `(company-preview ((t (:foreground ,monokai-chartreuse :inherit 'company-tooltip))))
   `(company-preview-common ((t (:foreground ,monokai-orange+5 :background ,monokai-bg))))

   ;; CUA
   `(cua-rectangle ((t (:background ,monokai-base1))))

   ;; ECB
   `(ecb-default-highlight-face ((t (:foreground ,monokai-orange+5))))

   ;; Enh-ruby
   `(enh-ruby-op-face ((t (:foreground ,monokai-orange+5))))
   `(enh-ruby-regexp-delimiter-face ((t (:foreground ,monokai-orange+20))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,monokai-orange-5) :inherit unspecified))
      ;;(,t (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))
      ))

   ;; helm
   `(helm-source-header ((t (:foreground ,monokai-purple :background ,monokai-base1 :height 1.2))))
   `(helm-visible-mark ((t (nil))))
   `(helm-header ((t (nil))))
   `(helm-candidate-number ((t (:underline t :foreground ,monokai-purple :background nil))))
   `(helm-selection ((t (:foreground ,monokai-green :background ,monokai-base2))))
   `(helm-separator ((t (:background ,monokai-base1))))
   `(helm-action ((t (nil))))

   ;; helm find file
   `(helm-ff-prefix ((t (nil))))
   `(helm-ff-executable ((t (:foreground ,monokai-grapefruit))))
   `(helm-ff-directory ((t (:foreground ,monokai-cyan :background nil))))
   `(helm-ff-symlink ((t (:foreground ,monokai-magenta))))
   `(helm-ff-invalid-symlink ((t (:foreground nil :background ,monokai-goldenrod))))
   `(helm-ff-file ((t (:inherit default))))

   ;; IDO
   `(ido-first-match ((t (:foreground ,monokai-purple :bold t))))
   `(ido-only-match ((t (:foreground ,monokai-grapefruit))))
   `(ido-subdir ((t (:foreground ,monokai-chartreuse))))

   ;; linum-mode
   `(linum ((t (:foreground ,monokai-grey-2 :background ,monokai-grey+5))))

   ;; Merlin
   `(merlin-type-face ((t (:background ,monokai-base2 :bold t))))

   ;; mode line
   `(mode-line ((t (:foreground ,monokai-fg :background ,monokai-base2 :box nil))))
   `(mode-line-buffer-id ((t (:bold t))))
   `(mode-line-inactive ((t (:foreground ,monokai-fg :background ,monokai-base1 :box nil))))

   ;; neotree
   `(neo-banner-face ((t (:foreground ,monokai-goldenrod))))
   `(neo-header-face ((t (:foreground ,monokai-chartreuse))))
   `(neo-root-dir-face ((t (:foreground ,monokai-grapefruit :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,monokai-chartreuse))))
   `(neo-expand-btn-face ((t (:foreground ,monokai-orange :weight bold))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,monokai-cyan3))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,monokai-purple3))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,monokai-green3))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,monokai-orange2))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,monokai-cyan3))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,monokai-purple3))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,monokai-green3))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,monokai-orange2))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,monokai-cyan3))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,monokai-purple3))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,monokai-green3))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,monokai-orange2))))
   `(rainbow-delimiters-unmatched-face
     ((t (:foreground ,monokai-fg, :background ,monokai-base3 :inverse-video t))))

      ;; search
   `(isearch ((t (:foreground ,monokai-black :background ,monokai-wheat :bold t))))
   `(isearch-fail ((t (:foreground ,monokai-orange+20 :background ,monokai-darkwine))))

   ;; tuareg
   `(tuareg-font-lock-governing-face ((t (:foreground ,monokai-grapefruit))))
   `(tuareg-font-lock-operator-face ((t (:foreground ,monokai-orange+5))) )
   `(tuareg-font-lock-constructor-face ((t (:foreground ,monokai-cyan))) )

   ;; Whitespace
   `(whitespace-space ((t (:foreground ,monokai-base3))))
   `(trailing-whitespace ((t (:foreground ,monokai-magenta))))
   ;; Yasnippet
   `(yas/field-highlight-face ((t (:background ,monokai-grey))))
   ;; zencoding uses this
   `(tooltip ((t (:background ,monokai-fg :foreground ,monokai-fg2))))

   ))

;;;###autoload
(when (and load-file-name
           (boundp 'custom-theme-load-path))
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide-theme 'monokai-extended)

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; indent-tabs-mode: nil
;; fill-column: 95
;; End:

;;; monokai-extended-theme.el ends here

;;; vs-dark-theme.el --- Visual Studio IDE dark theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2025 Shen, Jen-Chieh

;; Author: Jen-Chieh Shen
;; URL: https://github.com/emacs-vs/vs-dark-theme
;; Package-Version: 20250725.1905
;; Package-Revision: c3a6d1545600
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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
;;
;; Visual Studio IDE dark theme.
;;

;;; Code:

(deftheme vs-dark
  "Visual Studio IDE dark theme.")

(defconst vs-dark-theme-graphic-p (display-graphic-p)
  "Non-nil if graphic mode.")

(custom-theme-set-faces
 'vs-dark
 `(default                      ((t (:background "#1E1E1E" :foreground "#D2D2D2"))))
 `(font-lock-builtin-face       ((t (:foreground "light steel blue"))))
 `(font-lock-comment-face       ((t (:foreground "olive drab"))))
 `(font-lock-negation-char-face ((t (:foreground "#38EFCA"))))
 `(font-lock-reference-face     ((t (:foreground "#38EFCA"))))
 `(font-lock-constant-face      ((t (:foreground "#38EFCA"))))
 `(font-lock-doc-face           ((t (:foreground "olive drab"))))
 `(font-lock-function-name-face ((t (:foreground "#D2D2D2"))))
 `(font-lock-keyword-face       ((t (:foreground "#17A0FB"))))
 `(font-lock-preprocessor-face  ((t (:foreground "#8D9B99"))))
 `(font-lock-string-face        ((t (:foreground "#D69D78"))))
 `(font-lock-type-face          ((t (:foreground "#38EFCA"))))
 `(font-lock-variable-name-face ((t (:foreground "#D2D2D2"))))

 `(header-line ((t (:background "#383838" :foreground "#FAFAFA"))))

 `(mode-line          ((t :box ( :line-width -1 :style released-button))))
 `(mode-line-inactive ((t :box ( :line-width -1 :style released-button))))

 `(cursor  ((t :background "#909090")))
 `(hl-line ((t :background ,(if vs-dark-theme-graphic-p "#2E2E2E" "#363636"))))
 `(region  ((t :background "#264F78")))
 `(fringe  ((t :background "#333333")))

 `(completions-annotations ((t :inherit (shadow))))
 `(completions-common-part ((t :foreground "#72A4FF" :weight bold)))

 `(highlight ((t :background ,(if vs-dark-theme-graphic-p "#264F78" "#363636"))))

 `(line-number              ((t ( :background "#252525" :foreground "#2B9181"))))
 `(line-number-current-line ((t ( :background "#252525"
                                  :foreground ,(if vs-dark-theme-graphic-p
                                                   "#2B9181"
                                                 "#00FFD5")))))

 `(fill-column-indicator ((t :foreground "#AA4242")))

 `(show-paren-match ,(if vs-dark-theme-graphic-p
                         `((t :box ( :line-width (-1 . -1) :style released-button
                                     :color "#464646")))
                       `((t :background "#72A4FF"))))

 `(show-eof-mode-marker-face ((t :background "#252525" :foreground "#888581")))

 `(diredfl-number      ((t :foreground "#B5CEA8")))
 `(diredfl-dir-name    ((t :foreground "#FFD767")))
 `(diredfl-date-time   ((t :foreground "#787A81")))
 `(diredfl-file-name   ((t :foreground "#D4D4D8")))
 `(diredfl-file-suffix ((t :foreground "#D4D4D8")))
 `(diredfl-no-priv     ((t :background "#2C2C2C2C2C2C")))
 `(diredfl-dir-priv    ((t :foreground "#FFD767")))
 `(diredfl-read-priv   ((t :background "#751D3A")))
 `(diredfl-write-priv  ((t :background "#134F16")))
 `(diredfl-exec-priv   ((t :background "#3B2A14")))

 `(highlight-indent-guides-odd-face             ((t :foreground "#414141")))
 `(highlight-indent-guides-even-face            ((t :foreground "#414141")))
 `(highlight-indent-guides-character-face       ((t :foreground "#414141")))
 `(highlight-indent-guides-top-odd-face         ((t :foreground "#414141")))
 `(highlight-indent-guides-top-even-face        ((t :foreground "#414141")))
 `(highlight-indent-guides-top-character-face   ((t :foreground "#414141")))
 `(highlight-indent-guides-stack-odd-face       ((t :foreground "#414141")))
 `(highlight-indent-guides-stack-even-face      ((t :foreground "#414141")))
 `(highlight-indent-guides-stack-character-face ((t :foreground "#414141")))

 `(highlight-doxygen-comment    ((t :background "#1E1E1E")))
 `(highlight-doxygen-code-block ((t :background "grey40")))
 `(highlight-doxygen-command    ((t :foreground "SlateGray")))
 `(highlight-doxygen-type       ((t :foreground "SteelBlue")))
 `(highlight-doxygen-variable   ((t :foreground "gold4")))

 `(tree-sitter-hl-face:function              ((t :foreground "#D2D2D2")))
 `(tree-sitter-hl-face:function.call         ((t :foreground "#D2D2D2")))
 `(tree-sitter-hl-face:function.builtin      ((t :foreground "#808080")))
 `(tree-sitter-hl-face:function.special      ((t :foreground "#808080")))
 `(tree-sitter-hl-face:function.macro        ((t :foreground "#808080")))
 `(tree-sitter-hl-face:method                ((t :foreground "#D2D2D2")))
 `(tree-sitter-hl-face:method.call           ((t :foreground "#D2D2D2")))
 `(tree-sitter-hl-face:type                  ((t :foreground "#38EFCA")))
 `(tree-sitter-hl-face:type.parameter        ((t :foreground "#D2D2D2")))
 `(tree-sitter-hl-face:type.argument         ((t :foreground "#D2D2D2")))
 `(tree-sitter-hl-face:type.builtin          ((t :foreground "#17A0FB")))
 `(tree-sitter-hl-face:type.super            ((t :foreground "#17A0FB")))
 `(tree-sitter-hl-face:constructor           ((t :foreground "#D2D2D2")))
 `(tree-sitter-hl-face:variable              ((t :foreground "#D2D2D2")))
 `(tree-sitter-hl-face:variable.parameter    ((t :foreground "#7F7F7F")))
 `(tree-sitter-hl-face:variable.builtin      ((t :foreground "light steel blue")))
 `(tree-sitter-hl-face:variable.special      ((t :foreground "#B363BE")))
 `(tree-sitter-hl-face:property              ((t :foreground "#B5CEA8")))
 `(tree-sitter-hl-face:property.definition   ((t :foreground "#B5CEA8")))
 `(tree-sitter-hl-face:comment               ((t :foreground "olive drab")))
 `(tree-sitter-hl-face:doc                   ((t :foreground "olive drab")))
 `(tree-sitter-hl-face:string                ((t :foreground "#D69D78")))
 `(tree-sitter-hl-face:string.special        ((t :foreground "#D69D78")))
 `(tree-sitter-hl-face:escape                ((t :foreground "#D69D78")))
 `(tree-sitter-hl-face:embedded              ((t :foreground "#D69D78")))
 `(tree-sitter-hl-face:keyword               ((t :foreground "#17A0FB")))
 `(tree-sitter-hl-face:operator              ((t :foreground "#B4B4B3")))
 `(tree-sitter-hl-face:label                 ((t :foreground "#808080")))
 `(tree-sitter-hl-face:constant              ((t :foreground "#B363BE")))
 `(tree-sitter-hl-face:constant.builtin      ((t :foreground "#17A0FB")))
 `(tree-sitter-hl-face:number                ((t :foreground "#B5CEA8")))
 `(tree-sitter-hl-face:boolean               ((t :foreground "#17A0FB")))
 `(tree-sitter-hl-face:repeat                ((t :foreground "#17A0FB")))
 `(tree-sitter-hl-face:conditional           ((t :foreground "#17A0FB")))
 `(tree-sitter-hl-face:conditional.ternary   ((t :foreground "#17A0FB")))
 `(tree-sitter-hl-face:exception             ((t :foreground "#17A0FB")))
 `(tree-sitter-hl-face:punctuation           ((t :foreground "#B4B4B3")))
 `(tree-sitter-hl-face:punctuation.bracket   ((t :foreground "#B4B4B3")))
 `(tree-sitter-hl-face:punctuation.delimiter ((t :foreground "#B4B4B3")))
 `(tree-sitter-hl-face:punctuation.special   ((t :foreground "#B4B4B3")))
 `(tree-sitter-hl-face:tag                   ((t :foreground "#569CD6")))
 `(tree-sitter-hl-face:attribute             ((t :foreground "#8D9B99")))
 `(tree-sitter-hl-face:noise                 ((t :foreground "#8D9B99")))

 `(ts-fold-replacement-face ((t :foreground "#808080" :box (:line-width (-1 . -1) :style pressed-button))))
 `(ts-fold-fringe-face      ((t :foreground "#B9B9B9")))

 `(treesit-fold-replacement-face ((t :foreground "#808080" :box (:line-width (-1 . -1) :style pressed-button))))
 `(treesit-fold-fringe-face      ((t :foreground "#B9B9B9")))

 `(company-tooltip-annotation       ((t :foreground "#96A2AA")))
 `(company-fuzzy-annotation-face    ((t :foreground "#7BABCA")))
 `(company-preview                  ((t :foreground "dark gray" :underline t)))
 `(company-preview-common           ((t (:inherit company-preview))))
 `(company-tooltip                  ((t :background "#252526" :foreground "#BEBEBF")))
 `(company-tooltip-selection        ((t :background "#062F4A" :foreground "#BEBEBF")))
 `(company-tooltip-common           ((((type x)) (:inherit company-tooltip :weight bold))
                                     (t (:background "#252526" :foreground "#0096FA"))))
 `(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold))
                                     (t (:background "#062F4A" :foreground "#0096FA"))))
 `(company-scrollbar-bg             ((t :background "#3E3E42")))
 `(company-scrollbar-fg             ((t :background "#686868")))

 `(corfu-default     ((t :background "#252526" :foreground "#BEBEBF")))
 `(corfu-current     ((t :background "#062F4A" :foreground "#BEBEBF")))
 `(corfu-bar         ((t :background "#062F4A" :foreground "#BEBEBF")))
 `(corfu-annotations ((t :foreground "#96A2AA")))
 `(corfu-popupinfo   ((t :background "#2A2D38" :foreground "#F1F1F1")))

 `(popup-tip-face ((t :background "#2A2D38" :foreground "#F1F1F1")))

 `(flx-highlight-face ((t :foreground "#72a4ff" :weight bold)))

 `(ahs-plugin-default-face           ((t :background "#123E70" :box (:line-width (-1 . -1) :style pressed-button :color "#525D68"))))
 `(ahs-plugin-default-face-unfocused ((t :background "#0E3056" :box (:line-width (-1 . -1) :style pressed-button :color "#525D68"))))
 `(ahs-face                          ((t :background "#123E70" :box (:line-width (-1 . -1) :style pressed-button :color "#525D68"))))
 `(ahs-definition-face               ((t :background "#123E70" :box (:line-width (-1 . -1) :style pressed-button :color "#525D68"))))
 `(ahs-face-unfocused                ((t :background "#0E3056" :box (:line-width (-1 . -1) :style pressed-button :color "#525D68"))))
 `(ahs-definition-face-unfocused     ((t :background "#0E3056" :box (:line-width (-1 . -1) :style pressed-button :color "#525D68"))))

 `(tab-line     ((t :background "#292929")))
 `(tab-line-tab ((t :background "#292929")))

 `(centaur-tabs-display-line               ((t :background "#292929" :box nil :overline nil :underline nil)))
 `(centaur-tabs-default                    ((t :background "#292929")))
 `(centaur-tabs-unselected                 ((t :background "#3D3C3D" :foreground "grey50")))
 `(centaur-tabs-selected                   ((t :background "#31343E" :foreground "white")))
 `(centaur-tabs-unselected-modified        ((t :background "#3D3C3D" :foreground "grey50")))
 `(centaur-tabs-selected-modified          ((t :background "#31343E" :foreground "white")))
 `(centaur-tabs-modified-marker-unselected ((t :background "#3D3C3D" :foreground "grey50")))
 `(centaur-tabs-modified-marker-selected   ((t :background "#31343E" :foreground "white")))

 `(dashboard-text-banner       ((t :foreground "white")))
 `(dashboard-banner-logo-title ((t :foreground "cyan1")))
 `(dashboard-heading           ((t :foreground "#17A0FB"
                                   :box (:line-width (-1 . 5) :color "#1E1E1E"))))
 `(dashboard-items-face        ((t :foreground "light steel blue")))

 `(yascroll:thumb-fringe    ((t :background "#686868" :foreground "#686868")))
 `(yascroll:thumb-text-area ((t :background "#686868" :foreground "#686868")))

 `(region-occurrences-highlighter-face ((t :background "#113D6F")))

 `(whitespace-indentation ((t :background "grey20" :foreground "aquamarine3")))
 `(whitespace-trailing    ((t :background "grey20" :foreground "red")))

 `(highlight-numbers-number ((t :foreground "#9BCEA3")))

 `(modablist-select-face ((t :box (:line-width (-1 . -1) :color "#65A7E2" :style nil))))
 `(modablist-insert-face ((t :background "#565136" :box (:line-width (-1 . -1) :color "#65A7E2" :style nil))))

 `(lsp-flycheck-info-unnecessary-face    ((t :inherit flycheck-info    :foreground "#B0B0B0")))
 `(lsp-flycheck-warning-unnecessary-face ((t :inherit flycheck-warning :foreground "#B0B0B0")))
 `(lsp-flycheck-error-unnecessary-face   ((t :inherit flycheck-error   :foreground "#B0B0B0")))

 `(lsp-inlay-hint-face           ((t :background "#252525" :foreground "#888581")))
 `(lsp-inlay-hint-type-face      ((t :background "#252525" :foreground "#888581")))
 `(lsp-inlay-hint-parameter-face ((t :background "#252525" :foreground "#888581")))

 `(dap-ui-breakpoint-verified-fringe ((t :foreground "#E71F2D")))
 `(breakpoint-disabled               ((t :foreground "#C55159")))

 `(diff-hl-insert ((t :background "#107C10")))
 `(diff-hl-delete ((t :background "#B01414")))
 `(diff-hl-change ((t :background "#0077D4")))

 `(rjsx-tag              ((t (:foreground "#87CEFA"))))
 `(rjsx-attr             ((t (:foreground "#EEDD82"))))
 `(rjsx-text             ((t (:inherit default))))
 `(rjsx-tag-bracket-face ((t (:inherit web-mode-html-attr-name-face))))

 `(markdown-markup-face           ((t :foreground "#7EA728" :background "#2B2B2B")))
 `(markdown-code-face             ((t :foreground "#D2D2D2" :background "#2B2B2B" :extend t :inherit nil)))
 `(markdown-list-face             ((t :foreground "gold3")))
 `(markdown-table-face            ((t :foreground "#87CEFA" :background "#1E1E1E")))
 `(markdown-header-face           ((t :foreground "#B5CCEB" :background "#1E1E1E")))
 `(markdown-header-delimiter-face ((t :foreground "#B5CCEB" :background "#1E1E1E")))
 `(markdown-metadata-key-face     ((t :foreground "#17A0FB")))
 `(markdown-metadata-value-face   ((t :foreground "#D2D2D2")))

 `(org-block    ((t :foreground "#D2D2D2" :background "#2B2B2B" :extend t :inherit nil)))
 `(org-level-1  ((t :foreground "#4ec9b0")))
 `(org-level-2  ((t :foreground "#9cdcfe")))
 `(org-level-3  ((t :foreground "#569cd6")))
 `(org-level-4  ((t :foreground "#dcdcaa")))
 `(org-level-5  ((t :foreground "#c586c0")))
 `(org-level-6  ((t :foreground "#ce9178")))
 `(org-level-7  ((t :foreground "#d7ba7d")))
 `(org-level-8  ((t :foreground "#d16969")))
 `(org-ellipsis ((t :foreground "#808080" :box (:line-width (-1 . -1) :style pressed-button))))

 `(web-mode-doctype-face            ((t :foreground "Pink3")))
 `(web-mode-comment-face            ((t :foreground "olive drab")))
 `(web-mode-block-comment-face      ((t :foreground "olive drab")))
 `(web-mode-html-tag-bracket-face   ((t :foreground "#80765E")))
 `(web-mode-html-tag-face           ((t :foreground "#569CD6")))
 `(web-mode-html-attr-name-face     ((t :foreground "#6CCAFE")))
 `(web-mode-html-attr-equal-face    ((t :foreground "#B4B4B4")))
 `(web-mode-html-attr-value-face    ((t :foreground "#D69D78")))
 `(web-mode-css-selector-tag-face   ((t :foreground "#D7BA5F")))
 `(web-mode-css-selector-class-face ((t :foreground "#D7BA5F")))
 `(web-mode-css-property-name-face  ((t :foreground "#68CDFE")))

 `(nxml-processing-instruction-target    ((t :foreground "#569CD6")))
 `(nxml-processing-instruction-delimiter ((t :foreground "#80765E")))
 `(nxml-namespace-attribute-xmlns        ((t :foreground "#6CCAFE")))
 `(nxml-namespace-attribute-prefix       ((t :foreground "#6CCAFE")))
 `(nxml-element-local-name               ((t :foreground "#569CD6")))
 `(nxml-attribute-local-name             ((t :foreground "#6CCAFE")))
 `(nxml-tag-delimiter                    ((t :foreground "#80765E")))
 `(nxml-text                             ((t :foreground "#D2D2D2")))

 `(define-it-pop-tip-color ((t :background "#2A2D38")))

 `(preview-it-background ((t :background "#2A2D38")))
 )

(custom-theme-set-variables
 'vs-dark
 `(centaur-tabs-background-color "#292929")
 ;; coverlay overlays
 `(coverlay:tested-line-background-color   "#E1FFE1")
 `(coverlay:untested-line-background-color "LavenderBlush")
 `(jcs-poptip-background-color "#2A2D38")
 `(jcs-poptip-foreground-color "#F1F1F1")
 `(cogru-tip-background-color "#2A2D38")
 `(cogru-tip-foreground-color "#F1F1F1"))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun vs-dark-theme ()
  "Load Visual Studio dark theme."
  (interactive)
  (load-theme 'vs-dark t))

(provide-theme 'vs-dark)

(provide 'vs-dark-theme)
;;; vs-dark-theme.el ends here

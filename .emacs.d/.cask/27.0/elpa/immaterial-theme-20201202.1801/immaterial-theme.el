;;; immaterial-theme.el --- A flexible theme based on material design principles

;; Copyright (C) 2019-2020 Peter Gardfjäll

;; Author: Peter Gardfjäll
;; Keywords: themes
;; URL: https://github.com/petergardfjall/emacs-immaterial-theme
;; Version: 0.6.2
;; Package-Requires: ((emacs "25"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; To use the theme, put any of the following two lines in your Emacs
;; configuration file:
;;
;;   (load-theme 'immaterial-dark t)    ;; dark variant
;;   (load-theme 'immaterial-light t)   ;; light variant
;;
;; Requirements: Emacs 25.
;;

;;; Code:

(defface immaterial-small-face
  '((t :height 0.95))
  "Face that can be used via :inherit on faces that should have a smaller font size."
  :group 'immaterial-faces)

(defvar immaterial-color-override-alist
  '(())
  "Values provided here will override values in immaterial-color-alist.
The material color tool https://material.io/resources/color/ is
recommended for constructing primary and secondary color
schemes.")

(defvar immaterial-color-alist nil
  "Color alist to be used to define the different faces of the theme variant.
Initialized when either of the theme variants is loaded.")


;; Tip: enable rainbow-mode to preview the colors.
(defun immaterial-create-color-alist (variant)
  "Create a color palette for a given VARIANT of the theme.
VARIANT can either be 'dark of 'light.  Values can be overridden
via immaterial-color-override-alist).  The palette was created
using the https://material.io/resources/color/ tool."
  `(("background-primary"    . ,(if (eq variant 'dark) "#012027" "#fdfdfa"))
    ("background-off"        . ,(if (eq variant 'dark) "#001b21" "#fbfbf8"))
    ("background-on"         . ,(if (eq variant 'dark) "#01343f" "#f5f5e7"))
    ("foreground-primary"    . ,(if (eq variant 'dark) "#dddddd" "#333333"))
    ("foreground-secondary"  . ,(if (eq variant 'dark) "#c8c8c8" "#606060"))
    ("foreground-tertiary"   . ,(if (eq variant 'dark) "#b0b0b0" "#8e8e8e"))
    ("primary"               . ,(if (eq variant 'dark) "#9fa8da" "#7e57c2"))
    ("primary-light"         . ,(if (eq variant 'dark) "#d1d9ff" "#b085f5"))
    ("primary-dark"          . ,(if (eq variant 'dark) "#6f79a8" "#4d2c91"))
    ("secondary"             . ,(if (eq variant 'dark) "#c5e1a5" "#689f38"))
    ("secondary-light"       . ,(if (eq variant 'dark) "#f8ffd7" "#99d066"))
    ("secondary-dark"        . ,(if (eq variant 'dark) "#94af76" "#387002"))
    ("tertiary"              . ,(if (eq variant 'dark) "#90caf9" "#1565c0"))
    ("tertiary-light"        . ,(if (eq variant 'dark) "#c3fdff" "#5e92f3"))
    ("tertiary-dark"         . ,(if (eq variant 'dark) "#5d99c6" "#003c8f"))

    ("error"                 . ,(if (eq variant 'dark) "#ff5555" "#b0003a"))
    ("warning"               . ,(if (eq variant 'dark) "#ff9800" "#ff9800"))
    ("discrete"              . ,(if (eq variant 'dark) "#777777" "#999999"))
    ("vertical-border"       . ,(if (eq variant 'dark) "#012830" "#dddddd"))
    ("cursor"                . ,(if (eq variant 'dark) "#64d8cb" "#64d8cb"))
    ("modeline-active-fg"    . ,(if (eq variant 'dark) "#ffffff" "#ffffff"))
    ("modeline-active-bg"    . ,(if (eq variant 'dark) "#005662" "#9575cd"))
    ("modeline-inactive-fg"  . ,(if (eq variant 'dark) "#777777" "#9e9e9e"))
    ("modeline-inactive-bg"  . ,(if (eq variant 'dark) "#001017" "#ede7f6"))
    ;; various task-specific colors
    ("diff-added"            . ,(if (eq variant 'dark) "#0e3a19" "#e6ffed"))
    ("diff-added-refined"    . ,(if (eq variant 'dark) "#1a8736" "#acf2bd"))
    ("diff-removed"          . ,(if (eq variant 'dark) "#460b0e" "#ffeef0"))
    ("diff-removed-refined"  . ,(if (eq variant 'dark) "#b51a22" "#fdb8c0"))
    ("diff-changed"          . ,(if (eq variant 'dark) "#07275a" "#e1f0fe"))
    ("diff-changed-refined"  . ,(if (eq variant 'dark) "#2674ed" "#a8d3ff"))
    ))


(defun immaterial-color (color-name)
  "Retrieves the hex color value registered for a ´COLOR-NAME´.
The overrides in immaterial-color-override-alist take precedence
over the default ones defined in immaterial-color-alist."
  (let ((colmap (append immaterial-color-override-alist immaterial-color-alist)))
    (cdr (assoc color-name colmap))))


(defun immaterial-color-lighten (hex-color percent)
  "Determines a brighter/darker shade of a hex color.
For a HEX-COLOR (such as `#3cb878`) return the hex color that is
PERCENT percent brighter (or darker if percent value is
negative)."
  (let ((color-transform-fn (if (> percent 0)
				'color-lighten-hsl
			      'color-darken-hsl))
	(percent-unsigned (abs percent)))
     (apply 'color-rgb-to-hex
	    (append
	     (apply 'color-hsl-to-rgb
		    (apply color-transform-fn
			   (append
			    (apply 'color-rgb-to-hsl (color-name-to-rgb hex-color))
			    (list percent-unsigned))))
	     (list 2)))))



(defun immaterial-create-theme (name variant)
  "Initialize immaterial-theme for the given NAME and VARIANT.
NAME and VARIANT should be symbols."
  (progn
    (setq immaterial-color-alist (immaterial-create-color-alist variant))
    ;; not sure why this isn't a custom face in lsp-ui-doc
    (setq lsp-ui-doc-border (immaterial-color "modeline-active-bg"))
    (let ((class '((class color) (min-colors 89)))
	  (fg1                  (immaterial-color "foreground-primary"))
	  (fg2                  (immaterial-color "foreground-secondary"))
	  (fg3                  (immaterial-color "foreground-tertiary"))
	  (bg-prim              (immaterial-color "background-primary"))
	  (bg-on                (immaterial-color "background-on"))
	  (bg-off               (immaterial-color "background-off"))
	  (prim                 (immaterial-color "primary"))
	  (prim-light           (immaterial-color "primary-light"))
	  (prim-dark            (immaterial-color "primary-dark"))
	  (sec                  (immaterial-color "secondary"))
	  (sec-light            (immaterial-color "secondary-light"))
	  (sec-dark             (immaterial-color "secondary-dark"))
	  (tert                 (immaterial-color "tertiary"))
	  (tert-light           (immaterial-color "tertiary-light"))
	  (tert-dark            (immaterial-color "tertiary-dark"))
	  (discrete             (immaterial-color "discrete"))

	  (keyword              (immaterial-color "primary"))
	  (builtin              (immaterial-color "primary"))
	  (const                (immaterial-color "primary"))
	  (type                 (immaterial-color "secondary"))
	  (var                  (immaterial-color "foreground-primary"))
	  (func                 (immaterial-color "secondary-dark"))
	  (str                  (immaterial-color "tertiary"))
	  (comment              (immaterial-color "discrete"))
	  (negation             (immaterial-color "warning"))
	  (warning              (immaterial-color "warning"))
	  (error                (immaterial-color "error"))
	  (cursor               (immaterial-color "cursor"))

	  (v-border             (immaterial-color "vertical-border"))
	  (modeline-active-bg   (immaterial-color "modeline-active-bg"))
	  (modeline-active-fg   (immaterial-color "modeline-active-fg"))
	  (modeline-inactive-bg (immaterial-color "modeline-inactive-bg"))
	  (modeline-inactive-fg (immaterial-color "modeline-inactive-fg"))

	  (diff-added           (immaterial-color "diff-added"))
	  (diff-added-refined   (immaterial-color "diff-added-refined"))
	  (diff-changed         (immaterial-color "diff-changed"))
	  (diff-changed-refined (immaterial-color "diff-changed-refined"))
	  (diff-removed         (immaterial-color "diff-removed"))
	  (diff-removed-refined (immaterial-color "diff-removed-refined")))

      (custom-theme-set-faces
       name
       `(default ((,class (:background ,bg-prim :foreground ,fg1))))
       ;;
       ;; Syntax higlighting/font-lock minor mode. (syntax rules are provided by
       ;; the particular major-mode).
       ;;

       ;; for a keyword with special syntactic significance, like ‘if’.
       `(font-lock-keyword-face ((,class (:bold t :foreground ,keyword))))
       ;; for the names of built-in functions.
       `(font-lock-builtin-face ((,class (:foreground ,builtin))))
       ;; for the names of constants, like ‘NULL’ in C.
       `(font-lock-constant-face ((,class (:foreground ,const))))
       ;; for string literals.
       `(font-lock-string-face ((,class (:foreground ,str))))

       ;; for the names of user-defined data types.
       `(font-lock-type-face ((,class (:foreground ,type))))
       ;; for the name of a variable being defined or declared.
       `(font-lock-variable-name-face ((,class (:foreground ,var))))
       ;; for the name of a function being defined or declared.
       `(font-lock-function-name-face ((,class (:foreground ,func ))))

       ;; for comments
       `(font-lock-comment-face ((,class (:foreground ,comment))))
       ;; for comment delimiters, like ‘/*’ and ‘*/’ in C.
       `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
       ;; for documentation strings in the code.
       `(font-lock-doc-face ((,class (:foreground ,comment))))

       ;; for easily-overlooked negation characters.
       `(font-lock-negation-char-face ((,class (:foreground ,negation))))
       ;; for a construct that is peculiar, or that greatly changes the meaning of
       ;; other text, like ‘;;;###autoload’ in Emacs Lisp and ‘#error’ in C.
       `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg-on))))

       ;;
       ;; Buttons and links
       ;;
       `(button ((,class (:foreground ,str :weight bold :underline t))))
       `(link ((,class (:foreground ,str :weight bold :underline t))))
       `(link-visited ((,class (:foreground ,str :weight bold :underline t))))

       ;;
       ;; region selection
       ;;
       `(region ((,class (:background ,bg-on :foreground ,fg2))))
       ;; used for secondary selections and selected date/time in org-mode
       `(secondary-selection ((,class (:background ,bg-on :foreground ,sec-dark))))
       ;; face used for text highlighting in various contexts (e.g. ivy search)
       `(highlight ((,class (:background ,bg-on :foreground ,fg2 :extend t))))
       ;; hl-line-mode background
       `(hl-line ((,class (:background ,bg-on :extend t))))
       ;; linum-mode column
       `(linum ((t (:foreground ,discrete :background ,bg-prim :height 1.0 :weight normal))))
       ;; display-line-numbers-mode (emacs26+)
       `(line-number ((t (:foreground ,discrete :background ,bg-prim :height 1.0 :weight normal))))
       `(line-number-current-line ((t (:foreground ,fg1 :background ,bg-prim :height 1.0 :weight normal))))
       `(fringe ((,class (:background ,bg-prim))))
       `(cursor ((,class (:background ,cursor))))
       ;; show-paren-mode: how to highlight matching/mismatching parenthesis
       `(show-paren-match ((,class (:weight bold :background ,bg-on :foreground ,warning))))
       `(show-paren-mismatch ((,class (:background ,error))))
       ;; current match of an on-going incremental search (isearch-forward)
       `(isearch ((,class (:foreground ,warning :weight semi-bold))))
       ;; other matches for the search string that are visible on display
       `(lazy-highlight ((,class (:background ,bg-on :foreground ,warning))))
       ;;
       ;; mode-line
       ;;
       ;; mode-line of the active buffer (e.g. in case of split window)
       `(mode-line ((,class (:background ,modeline-active-bg :foreground ,modeline-active-fg))))
       ;; mode-line of the inactive buffer (e.g. in case of split window)
       `(mode-line-inactive  ((,class (:background ,modeline-inactive-bg :foreground ,modeline-inactive-fg))))
       `(mode-line-buffer-id ((,class (:weight bold))))

       ;;
       ;; powerline
       ;;
       ;; for active buffer in the frame
       `(powerline-active1 ((,class (:background ,modeline-active-bg :foreground ,modeline-active-fg))))
       `(powerline-active2 ((,class (:background ,modeline-active-bg :foreground ,modeline-active-fg))))
       ;; for inactive buffers in the frame
       `(powerline-inactive1 ((,class (:background ,modeline-inactive-bg :foreground ,modeline-inactive-fg))))
       `(powerline-inactive2 ((,class (:background ,modeline-inactive-bg :foreground ,modeline-inactive-fg))))

       ;; the vertical line that separates windows in a frame
       `(vertical-border ((,class (:foreground ,v-border))))
       `(minibuffer-prompt ((,class (:bold t :foreground ,prim))))
       `(default-italic ((,class (:italic t))))
       `(link ((,class (:foreground ,prim-dark :underline t))))

       `(gnus-header-content ((,class (:foreground ,prim))))
       `(gnus-header-from ((,class (:foreground ,sec-dark))))
       `(gnus-header-name ((,class (:foreground ,sec))))
       `(gnus-header-subject ((,class (:foreground ,sec-dark :bold t))))
       `(warning ((,class (:foreground ,warning))))
       `(ac-completion-face ((,class (:underline t :foreground ,prim))))
       `(info-quoted-name ((,class (:foreground ,prim-light))))
       `(info-string ((,class (:foreground ,prim))))
       `(icompletep-determined ((,class :foreground ,prim-light)))
       ;;
       ;; undo-tree
       ;;
       `(undo-tree-visualizer-current-face ((,class :foreground ,prim-light)))
       `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
       `(undo-tree-visualizer-unmodified-face ((,class :foreground ,sec-dark)))
       `(undo-tree-visualizer-register-face ((,class :foreground ,sec)))

       `(slime-repl-inputed-output-face ((,class (:foreground ,sec))))
       `(trailing-whitespace ((,class :foreground nil :background ,warning)))
       ;;
       ;; ansi-term/term: set up colors that work well with the theme at large
       ;;
       `(term-default-fg-color ((,class (:foreground ,fg1, :background ,bg-prim))))
       `(term-default-bg-color ((,class (:foreground ,fg1 :background ,bg-prim))))
       `(term-color-red        ((,class (:foreground ,error :background ,bg-prim))))
       `(term-color-blue       ((,class (:foreground ,prim-dark))))
       `(term-color-yellow     ((,class (:foreground ,prim))))
       `(term-color-magenta    ((,class (:foreground ,prim-light))))
       `(term-color-black      ((,class (:foreground ,sec-dark))))
       `(term-color-green      ((,class (:foreground ,sec))))
       `(term-color-cyan       ((,class (:foreground ,sec-light))))
       `(term-color-white      ((,class (:foreground ,fg1))))
       ;;
       ;; company -- "complete any" completion engine
       ;;
       ;; Face used for the common part of completions in the echo area
       `(company-echo-common ((,class (:foreground ,fg1 :background ,bg-on))))
       ;; display (single remaining) suggestion while typing
       `(company-preview ((,class (:background ,bg-on :foreground ,fg1))))
       `(company-preview-common ((,class (:background ,bg-on :foreground ,fg1))))
       `(company-preview-search ((,class (:foreground ,bg-on :background ,fg1))))
       ;; scrollbar style in company tooltip
       `(company-scrollbar-bg ((,class (:background ,bg-off))))
       `(company-scrollbar-fg ((,class (:background ,bg-on))))
       ;; general style of tooltip popup
       `(company-tooltip ((,class (:foreground ,fg1 :background ,bg-on :bold t))))
       ;; annotation appearance (could be the return-type of a function)
       `(company-tooltip-annotation ((,class (:weight normal :foreground ,fg1 :background ,bg-on))))
       ;; annotation appearance for the selected item in the completion list
       `(company-tooltip-annotation-selection ((,class (:weight normal :inherit company-tooltip-selection))))
       `(company-tooltip-search ((,class (:weight normal :inherit company-tooltip-selection))))
       ;; the highlight style to use when typing and showing common search prefix
       `(company-tooltip-common ((,class (:foreground ,prim))))
       `(company-tooltip-common-selection ((,class (:foreground ,prim))))
       ;; style for item mouse is hovering over
       `(company-tooltip-mouse ((,class (:inherit company-tooltip-selection))))
       `(company-tooltip-selection ((,class (:background ,bg-off :foreground ,fg3))))
       `(company-tooltip-selection ((,class (:weight bold :foreground ,fg3 :background ,bg-off))))
       ;;
       ;; sh-mode
       ;;
       `(sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
       `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-function-name-face))))
       ;;
       ;; neotree
       ;;
       `(neo-dir-link-face ((,class (:foreground ,prim :inherit bold))))
       `(neo-expand-btn-face ((,class (:foreground ,fg1))))
       `(neo-file-link-face ((,class (:foreground ,fg1))))
       `(neo-root-dir-face ((,class (:foreground ,sec-dark :inherit bold))))
       ;;
       ;; markdown-mode
       ;;
       ;; face to use for leading #:s
       `(markdown-header-delimiter-face ((,class (:foreground ,prim :weight bold))))
       `(markdown-header-face-1 ((,class (:foreground ,prim :weight bold))))
       `(markdown-header-face-2 ((,class (:foreground ,prim :weight bold))))
       `(markdown-header-face-3 ((,class (:foreground ,prim :weight bold))))
       `(markdown-header-face-4 ((,class (:foreground ,prim :weight bold))))
       `(markdown-header-face-5 ((,class (:foreground ,prim :weight bold))))
       `(markdown-header-face-6 ((,class (:foreground ,prim :weight bold))))
       `(markdown-code-face ((,class (:foreground ,sec))))
       `(markdown-table-face ((,class (:foreground ,sec))))
       `(markdown-list-face ((,class (:foreground ,sec))))
       `(markdown-link-face ((,class (:foreground ,tert))))
       `(markdown-reference-face ((,class (:foreground ,tert))))
       `(markdown-blockquote-face ((,class (:inherit font-lock-doc-face))))
       `(markdown-html-tag-face ((,class (:foreground ,sec))))
       `(markdown-url-face ((,class (:foreground ,tert))))
       `(markdown-plain-url-face ((,class (:foreground ,tert))))

       ;;
       ;; treemacs
       ;;
       `(treemacs-root-face ((,class (:foreground ,sec-dark :inherit bold))))
       `(treemacs-directory-face ((,class (:foreground ,sec-dark))))
       `(treemacs-file-face ((,class (:inherit immaterial-small-face))))
       `(treemacs-term-node-face ((,class (:foreground ,sec-dark :weight bold))))
       `(treemacs-git-modified-face ((,class (:background ,diff-changed :box (:line-width 1 :color ,diff-changed-refined :style nil)))))
       `(treemacs-git-added-face ((,class (:background ,diff-added :box (:line-width 1 :color ,diff-added-refined :style nil)))))
       `(treemacs-git-renamed-face ((,class (:background ,diff-changed :box (:line-width 1 :color ,diff-changed-refined :style nil) :italic t))))
       `(treemacs-git-ignored-face ((,class (:foreground ,discrete))))
       `(treemacs-git-untracked-face ((,class (:foreground ,discrete))))
       `(treemacs-git-conflict-face ((,class (:background ,diff-removed :box (:line-width 1 :color ,diff-removed-refined :style nil) :italic t))))

       ;;
       ;; lsp-ui
       ;;
       ;; ui-doc popup
       `(lsp-ui-doc-background ((,class (:background ,(immaterial-color-lighten bg-off -2)))))

       ;;
       ;; lsp-ui-peek
       ;;
       ;; face to use for the file being peeked (to the left)
       `(lsp-ui-peek-peek ((,class (:background ,bg-off))))
       ;; face to use for the peek file listing (to the right)
       `(lsp-ui-peek-list ((,class (:background ,bg-off :foreground ,fg2))))
       ;; face for current selection in peek file listing (to the right)
       `(lsp-ui-peek-selection ((,class (:inherit lsp-ui-peek-list :weight bold :foreground ,warning))))
       ;; face for file names in file listing (to the right)
       `(lsp-ui-peek-filename ((,class (:foreground ,prim))))
       ;; face for the type/object being peeked at in listing to the right
       `(lsp-ui-peek-highlight ((,class (:foreground ,warning))))
       ;; face for line numbers in listing to the right
       `(lsp-ui-peek-line-number ((,class (:foreground ,discrete))))
       ;; face for header line above entire peek frame
       `(lsp-ui-peek-header ((,class (:background ,modeline-active-bg :foreground ,modeline-active-fg :weight bold))))
       ;; face for footer line below entire peek frame
       `(lsp-ui-peek-footer ((,class (:background ,modeline-active-bg :foreground ,modeline-active-fg :weight bold))))

       ;;
       ;; ido
       ;;
       `(ido-first-match ((,class (:weight bold))))
       `(ido-only-match ((,class (:weight bold))))
       `(ido-subdir ((,class (:foreground ,sec-dark))))

       ;;
       ;; ivy/swiper
       ;;
       `(ivy-current-match ((,class (:background ,bg-on :extend t))))
       ;; how to highlight the matching part of the search expression on presented
       ;; search candidates in the minibuffer.
       `(ivy-minibuffer-match-face-1 ((,class (:inherit isearch))))
       `(ivy-minibuffer-match-face-2 ((,class (:inherit isearch))))
       `(ivy-minibuffer-match-face-3 ((,class (:inherit isearch))))
       `(ivy-minibuffer-match-face-4 ((,class (:inherit isearch))))
       ;; ivy information for grep-like searches (such as counsel-ag)
       `(ivy-grep-info ((,class (:foreground ,sec-dark))))
       `(ivy-grep-line-number ((,class (:foreground ,sec-dark))))
       ;; how to highlight the matching part of the search expression on presented
       ;; search candidates in the buffer itself.
       `(swiper-match-face-1 ((,class (:inherit isearch))))
       `(swiper-match-face-2 ((,class (:inherit isearch))))
       `(swiper-match-face-3 ((,class (:inherit isearch))))
       `(swiper-match-face-4 ((,class (:inherit isearch))))

       ;;
       ;; ivy-posframe
       ;;
       `(ivy-posframe ((,class (:background ,bg-off))))
       `(ivy-posframe-border ((,class (:background ,discrete))))

       ;;
       ;; org-mode
       ;;
       ;; face to use for #+TITLE: document info keyword
       `(org-document-title ((,class (:foreground ,prim :weight bold))))
       ;; face to use for value following #+DATE:, #+AUTHOR:, #+EMAIL:
       `(org-document-info ((,class (:foreground ,prim))))
       ;; face to use for keywords #+DATE:, #+AUTHOR:, #+EMAIL:
       `(org-document-info-keyword ((,class (:foreground ,prim))))
       ;; face for lines starting with "#+"
       `(org-meta-line ((,class (:foreground ,discrete))))
       ;; face used for headlines at different levels
       `(org-level-1 ((,class (:weight bold :foreground ,prim))))
       `(org-level-2 ((,class (:weight bold :foreground ,prim))))
       `(org-level-3 ((,class (:weight bold :foreground ,prim))))
       `(org-level-4 ((,class (:weight bold :foreground ,prim))))
       `(org-level-5 ((,class (:weight bold :foreground ,prim))))
       `(org-level-6 ((,class (:weight bold :foreground ,prim))))
       `(org-level-7 ((,class (:weight bold :foreground ,prim))))
       `(org-level-8 ((,class (:weight bold :foreground ,prim))))
       ;; face for the ellipsis in folded text
       `(org-ellipsis ((,class (:foreground ,prim))))
       ;; face to use for TODO keyword
       ;; `(org-todo ((,class (:weight bold :foreground ,prim-light :background ,bg-on))))
       `(org-todo ((,class (:weight bold :foreground ,tert-dark :box (:line-width -1 :color ,bg-on)))))
       ;; face to use for DONE keyword
       `(org-done ((,class (:weight bold :foreground ,discrete :box (:line-width -1 :color ,bg-on)))))
       ;; face to use for :tag: markers
       `(org-tag ((,class (:foreground ,prim-light))))
       ;; face used for priority cookies `[#A]`
       `(org-priority ((,class (:foreground ,tert :weight bold))))
       ;; face for special keywords such as SCHEDULED, DEADLINE and properties.
       `(org-special-keyword ((,class (:foreground ,discrete))))
       ;; face used for outline metadata :DRAWER: and :END: markers
       `(org-drawer ((,class (:foreground ,discrete))))
       ;; face for org-mode tables
       `(org-table ((,class (:foreground ,sec))))
       ;; face used for [[links][description]]
       `(org-link ((,class (:underline t :foreground ,tert))))
       ;; face used for footnotes: [fn:1]
       `(org-footnote  ((,class (:underline t :foreground ,tert))))
       ;; face for =verbatim= items
       `(org-verbatim ((,class (:foreground ,sec))))
       ;; face for ~code~ text
       `(org-code ((,class (:foreground ,sec))))
       ;; diary-like sexp date specifications like `%%(org-calendar-holiday)`
       `(org-sexp-date ((,class (:foreground ,prim-light))))
       ;; face to use for content between #+BEGIN_SRC and #+END_SRC (unless a
       ;; language syntax is specified via e.g. `#BEGIN_SRC emacs_lisp`)
       `(org-block ((,class (:background ,bg-prim :foreground ,sec :extend t))))
       ;; source code block #+BEGIN_SRC line
       `(org-block-begin-line ((,class (:background ,bg-prim :foreground ,sec :extend t))))
       ;; source code block #+END_SRC line
       `(org-block-end-line ((,class (:background ,bg-prim :foreground ,sec :extend t))))
       ;; face for #+BEGIN_VERSE blocks when `org-fontify-quote-and-verse-blocks` is set.
       `(org-verse ((,class (:slant italic))))
       ;; face for #+BEGIN_QUOTE blocks when `org-fontify-quote-and-verse-blocks` is set.
       `(org-quote ((,class (:slant italic))))
       ;; face to use for <date> occurences
       `(org-date ((,class (:underline t :foreground ,sec))))
       ;; face for highlighting date under cursor in calendar selections
       `(org-date-selected ((,class (:weight ultra-bold :background ,sec :foreground ,bg-off))))
       ;; face for Monday-Friday entries in agenda view
       `(org-agenda-date ((,class (:foreground ,sec))))
       ;; face for today in agenda view
       `(org-agenda-date-today ((,class (:foreground ,sec :weight bold :extend t :background ,bg-on))))
       ;; face for Saturday and Sunday entries in agenda view
       `(org-agenda-date-weekend ((,class (:foreground ,sec))))
       ;; face used in agenda to indicate lines switched to DONE
       `(org-agenda-done ((,class (:foreground ,discrete))))
       ;; face used in agenda for captions and dates
       `(org-agenda-structure ((,class (:inherit bold :foreground ,sec-dark))))
       ;; face used for time grid shown in agenda
       `(org-time-grid ((,class (:foreground ,sec))))
       ;; agenda face for items scheduled for a certain day
       `(org-scheduled ((,class (:foreground ,fg1))))
       ;; agenda face for items scheduled today
       `(org-scheduled-today ((,class (:foreground ,fg1))))
       ;; agenda face for items scheduled previously, and not yet done.
       `(org-scheduled-previously ((,class (:foreground ,fg1))))
       ;; face used for org-agenda deadlines
       `(org-warning ((,class (:foreground ,error))))
       ;; upcoming deadlines
       `(org-upcoming-deadline ((,class (:foreground ,warning))))
       ;; distant deadlines
       `(org-upcoming-distant-deadline ((,class (:foreground ,warning))))
       ;; face when header-line is used
       `(header-line ((,class (:background ,bg-on :foreground ,prim :weight bold))))
       ;; face to use for column view columns
       `(org-column ((,class (:background ,bg-on))))
       ;; face to use for top-row in column view
       `(org-column-title ((,class (:inherit header-line))))
       ;; face for clock display overrun tasks in mode line
       `(org-mode-line-clock-overrun ((,class (:foreground ,error))))

       ;;
       ;; diff-mode
       ;;
       ;; used to highlight file header lines in diffs
       `(diff-file-header ((,class (:foreground ,prim :weight bold))))
       `(diff-header ((,class (:foreground ,discrete))))
       ;; used to highlight function names produced by `diff -p`
       `(diff-function ((,class (:foreground ,discrete))))
       ;; used to highlight added lines
       `(diff-added ((,class (:background ,(immaterial-color "diff-added") :extend t))))
       ;; face used for added characters shown by ‘diff-refine-hunk’.
       `(diff-refine-added ((,class (:background ,(immaterial-color "diff-added-refined")))))
       ;; used to highlight indicator of added lines (+, >)
       `(diff-indicator-added ((,class (:background ,(immaterial-color "diff-added")))))
       ;; used to highlight added lines
       `(diff-removed ((,class (:background ,(immaterial-color "diff-removed")))))
       ;; face used for removed characters shown by ‘diff-refine-hunk’.
       `(diff-refine-removed ((,class (:background ,(immaterial-color "diff-removed-refined")))))
       ;; used to highlight indicator of changed lines (-, <)
       `(diff-indicator-removed ((,class (:background ,(immaterial-color "diff-removed")))))
       ;; face used to highlight changed lines
       `(diff-changed ((,class (:background ,(immaterial-color "diff-changed")))))
       ;; face used for char-based changes shown by ‘diff-refine-hunk’.
       `(diff-refine-changed ((,class (:background ,(immaterial-color "diff-changed-refined")))))
       ;; used to highlight indicator of changed lines
       `(diff-indicator-changed ((,class (:background ,(immaterial-color "diff-changed") :foreground ,(immaterial-color "diff-changed-refined")))))

       ;;
       ;; diff-hl
       ;;
       `(diff-hl-insert ((,class (:background ,(immaterial-color "diff-added") :foreground ,(immaterial-color "diff-added-refined")))))
       `(diff-hl-delete ((,class (:background ,(immaterial-color "diff-removed") :foreground ,(immaterial-color "diff-removed-refined")))))
       `(diff-hl-change ((,class (:background ,(immaterial-color "diff-changed") :foreground ,(immaterial-color "diff-changed-refined")))))

       ;;
       ;; smerge-mode
       ;;
       ;; face for conflict markers
       `(smerge-markers ((,class (:foreground ,error :weight bold))))
       ;; face for upper version in conflict
       `(smerge-upper ((,class (:background ,bg-on))))
       ;; face for lower version in conflict
       `(smerge-lower ((,class (:background ,bg-on))))
       ;; face for added characters shown by smerge-refine
       `(smerge-refined-added ((,class (:background ,(immaterial-color "diff-added-refined")))))
       ;; face for removed characters shown by smerge-refine
       `(smerge-refined-removed ((,class (:background ,(immaterial-color "diff-removed-refined")))))
       ))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'immaterial-theme)

;;; immaterial-theme.el ends here

;;; poet-theme.el --- A prose friendly theme.

;; Copyright 2018- Kunal Bhalla

;; Author: Kunal Bhalla <bhalla.kunal@gmail.com>
;; URL: https://github.com/kunalb/poet/
;; Package-Version: 20180603.1523
;; Version: 1.1

;;; Commentary:

;; Emacs has very good support for multiple fonts in a single
;; file.  Poet uses this support to make it much more convenient to
;; write prose within Emacs, with particular attention paid to
;; org-mode and markdown-mode.  Code blocks, tables, etc are
;; formatted in monospace text with the appropriate backgrounds.

;; Recommended customizations for using this theme
;;
;; - Set up the base fonts you'd like to use in Emacs before loading Poet
;;     (set-face-attribute 'default nil :family "Iosevka" :height 130)
;;     (set-face-attribute 'fixed-pitch nil :family "Iosevka")
;;     (set-face-attribute 'variable-pitch nil :family "Baskerville")
;;   On loading this theme captures the default and treats that for fixed-pitch
;;   rendering.
;;
;; - Enable variable pitch mode for editing text
;; (add-hook 'text-mode-hook
;;            (lambda ()
;;             (variable-pitch-mode 1))
;;
;; - Some other modes I like to enable/disable
;;     (olivetti-mode 1)        ;; Centers text in the buffer
;;     (flyspell-mode 1))       ;; Catch Spelling mistakes
;;     (typo-mode 1))           ;; Good for symbols like em-dash
;;     (blink-cursor-mode 0)    ;; Reduce visual noise
;;     (linum-mode 0)           ;; No line numbers for prose
;;
;; - And prettier org mode bullets:
;;     (setq org-bullets-bullet-list
;;         '("◉" "○"))
;;     (org-bullets 1)

;;; Code:

(deftheme poet
  "A variable-pitch-mode friendly theme for editing text in org and
  markdown.")

;;; Utility functions

;; Allow setting faces as (face :property value ...) directly
;; This theme is defined for the GUI only.
(defun poet--flatten (face-details)
  "Rewrite FACE-DETAILS to be usable with `custom-theme-set-faces'.

  This basically ignores Emacs's support for different types of
  terminals or backgrounds and defaults everything to the provided
  information."
  `(,(car face-details) ((t ,(cdr face-details)))))

(defun poet--create-faces (faces)
  "Add FACES to the theme definition."
    (apply
     'custom-theme-set-faces
     (cons 'poet
           (mapcar 'poet--flatten faces))))

;; TODO: Consider making this a macro instead
(defun poet--numbered-faces (prefix
                             range-start
                             range-end
                             callback)
  "Generate numbered faces: useful for defining headers, etc.

  Faces are generated as PREFIX<number> with the <number> ranging from
  RANGE-START to RANGE-END.  CALLBACK can be a plain string or a
  function that accepts the current <number> to generate the
  corresponding faces."
  (mapcar
   (lambda (iter)
     `(,(intern (concat prefix (number-to-string iter)))
       ,@(if (functionp callback)
              (funcall callback iter)
            callback)))
   (number-sequence range-start range-end)))

(defvar poet--monospace-height
  (face-attribute 'fixed-pitch :height nil 'default)
  "The base size to use: specified as a defvar to stay consistent")

(defun poet--height (multiplier)
  (truncate (* multiplier poet--monospace-height)))

;; TODO Allow choosing heading flattening height
(defun poet--header-height (index base-height max-height)
  "Use a large font only for the first heading, and then the same size"
  (if (= index 1)
      (+ (poet--height .2) base-height)
    base-height))

(let*
    (;; Theme design

     ;; Typography
     (max-heading-height (poet--height 1.8))
     (base-height (poet--height 1.23))
     (monospace-height (poet--height 1))

     ;; Colors
     (bg "#e1d9c2")
     (fg "#444444")
     (sep "#eeeeee")

     (bg-highlight "#fff8e1")

     (muted "#795548")
     (highlight "#efefef")
     (meta "#4e342e")
     (link "#303F9F")

     (header-color "#770b0b")

     (basic
      `((variable-pitch
          :family ,(face-attribute 'variable-pitch :family)
          :height ,base-height)
        (default
          :background ,bg
          :foreground ,fg)
        (italic
         :foreground "#222222"
         :slant italic)
        (highlight
         :background ,highlight)
        (region
         :background ,bg-highlight)
        (fringe
         :background ,bg)
        (button
         :inherit default
         :foreground "#616161")
        (escape-glyph
         :foreground "#673AB7")
        (link
         :underline (:color "#304FFE" :style line)
         :foreground ,link)
        (link-visited
         :inherit link
         :underline (:color "#1A237E" :style line))
        (cursor
         :background "#333333")
        (show-paren-match
         :background "#FF1744"
         :foreground "#ffffff")

        (isearch
         :foreground "#C2185B"
         :background "#ffffff")
        (isearch-fail
         :background "#f8bbd0")
        (query-replace
         :inherit isearch)

        (tooltip
         :inherit default
         :foreground "#111111"
         :background "#fff176")

        (shadow
         :foreground "#999999")

        (secondary-selection
         :background "#FFF59D")

        (trailing-whitespace
         :background "#FF8A65")

        (lazy-highlight
         :background "white")

        (next-error
         :inherit region)

        (window-divider
         :background ,sep
         :foreground ,sep)

        (vertical-border
         :background ,sep
         :foreground ,sep)))

     (evil
      `((evil-ex-substitute-replacement
         :foreground "#ffffff"
         :background "#4E342E"
         :underline nil)))

     (minibuffer
      `((minibuffer-prompt
         :inherit fixed-pitch
         :weight bold
         :foreground ,meta)))

     (mode-line
      `((mode-line
         :inherit fixed-pitch
         :foreground "#111111"
         :background ,bg
         :overline ,sep
         :box (:line-width 3
               :color ,bg))
        (header-line
         :overline nil
         :background "#e0e0e0"
         :underline ,sep
         :inherit mode-line)
        (mode-line-buffer-id
         :underline t)
        (mode-line-emphasis
         :weight bold)
        (mode-line-highlight
         :background "#ffffff")
        (mode-line-inactive
         :inherit mode-line
         :background ,bg
         :foreground "#888888"
         :box (:color ,bg
               :line-width 3))))

     (syntax
      `((error
         :inherit fixed-pitch)
        (font-lock-comment-face
         :foreground ,muted
         :inherit fixed-pitch)
        (font-lock-builtin-face
         :foreground "#795548"
         :inherit fixed-pitch)
        (font-lock-string-face
         :inherit fixed-pitch
         :foreground "#6C3082")
        (font-lock-function-name-face
         :inherit fixed-pitch
         :foreground "#388E3C")
        (font-lock-keyword-face
         :inherit fixed-pitch
         :foreground "#bf360c")
        (font-lock-comment-delimiter-face
         :inherit fixed-pitch
         :inherit font-lock-comment-face)
        (font-lock-constant-face
         :inherit fixed-pitch
         :foreground "#0288D1")
        (font-lock-doc-face
         :inherit fixed-pitch
         :inherit font-lock-string-face)
        (font-lock-preprocessor-face
         :inherit fixed-pitch
         :inherit font-lock-builtin-face)
        (font-lock-regexp-grouping-backslash
         :inherit fixed-pitch
         :inherit bold)
        (font-lock-regexp-grouping-construct
         :inherit fixed-pitch
         :inherit bold)
        (font-lock-type-face
         :foreground "#3f51b5"
         :inherit fixed-pitch)
        (font-lock-variable-name-face
         :inherit fixed-pitch
         :foreground "#455A64")
        (font-lock-warning-face
         :inherit error)))

     (org
      `(,@(poet--numbered-faces
           "org-level-" 1 8
           (lambda (index)
             (list
              ':inherit 'default
              ':foreground header-color
              ':height (poet--header-height index base-height max-heading-height))))

        (org-meta-line
         :inherit fixed-pitch
         :foreground "#8D6E63")

        (org-document-info-keyword
         :inherit fixed-pitch
         :foreground "#795548")

        (org-verbatim ; inline code
         :inherit fixed-pitch)

        (org-table
         :inherit fixed-pitch
         :background "#e0e0e0")

        (org-formula
         :inherit org-table
         :height ,monospace-height)

        (org-quote
         :inherit default
         :foreground "#4A148C"
         :background "#e0e0e0")

        (org-hide
         :inherit fixed-pitch
         :foreground ,bg)

        (org-indent
         :inherit org-hide)

        (org-date
         :inherit fixed-pitch
         :foreground "#444444"
         :underline nil)

        (org-document-title
         :inherit default
         :foreground "#B71C1C"
         :height ,max-heading-height
         :underline (:color "#aaaaaa"))

        (org-checkbox
         :inherit fixed-pitch
         :weight bold
         :foreground "#aaaaaa")

        (org-scheduled
         :foreground "#333333")

        (org-scheduled-today
         :foreground "#111111")

        (org-done
         :inherit fixed-pitch
         :foreground "#388E3C")

        (org-todo
         :inherit fixed-pitch
         :foreground "#BF360C")

        (org-tag
         :inherit fixed-pitch
         :height ,(poet--height 1)
         :foreground "#777777")

        (org-block-begin-line
         :inherit fixed-pitch
         :background "#d0d0d0")

        (org-block-end-line
         :inherit fixed-pitch
         :background "#d0d0d0")

        (org-block
         :background "#e0e0e0"
         :inherit fixed-pitch)))

     (hl-line
      `((hl-line
         :background "#efefef")))


     (linum
      `((linum-highlight-face
         :inherit default
         :foreground "#555555")
        (linum
         :inherit default
         :foreground "#aaaaaa")))

     (markdown
      `((markdown-markup-face
         :inherit fixed-pitch
         :foreground "#8D6E63")

        (markdown-inline-code-face
         :inherit fixed-pitch)

        (markdown-metadata-key-face
         :inherit fixed-pitch
         :height ,(poet--height 1)
         :foreground "#777777")

        (markdown-metadata-value-face
         :inherit fixed-pitch
         :height ,(poet--height 1)
         :foreground ,fg)

        (markdown-language-keyword-face
         :foreground "#7b1fa2")

        (markdown-list-face
         :inherit fixed-pitch
         :foreground "#000000")

        (markdown-code-face
         :inherit fixed-pitch
         :foreground ,fg
         :background "#e0e0e0")

        (markdown-pre-face
         :inherit fixed-pitch
         :color ,fg
         :background "#e0e0e0")

        (markdown-header-delimiter-face
         :inherit fixed-pitch
         :foreground "#8D6E63")

        (markdown-header-rule-face
         :inherit fixed-pitch
         :foreground "#8D6E63")

        (markdown-url-face
         :inherit fixed-pitch
         :foreground "#444444")

        ,@(poet--numbered-faces
           "markdown-header-face-" 1 8
           (lambda (index)
             (list
              ':foreground header-color
              ':inherit 'default
              ':height (max
                        (+ (poet--height .08) base-height)
                        (- max-heading-height (* (poet--height .2) index))))))))

     (imenu-list
      `(,@(poet--numbered-faces
           "imenu-list-entry-face-" 0 5
           '(:foreground "#4e342e")))))
  (poet--create-faces
   `(,@basic
     ,@minibuffer
     ,@syntax
     ,@mode-line
     ,@evil
     ,@org
     ,@linum
     ,@hl-line
     ,@imenu-list
     ,@markdown)))

(custom-theme-set-variables
 'poet
 '(line-spacing .2)
 '(fci-rule-color "#dedede"))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide-theme 'poet)
;;; poet-theme.el ends here

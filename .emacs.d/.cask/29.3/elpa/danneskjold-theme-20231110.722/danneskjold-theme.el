;; danneskjold-theme.el --- beautiful high-contrast theme

;; Copyright (c) 2016-2024 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; URL: https://github.com/rails-to-cosmos/danneskjold-theme
;; Package-Version: 20231110.0

;;; Commentary:

;;; Code:

(deftheme danneskjold
  "High-contrast minimalistic theme.")

(defun invert-color (color)
  "Invert a hex color string."
  (let* ((hex-map '(("0" . 0) ("1" . 1) ("2" . 2) ("3" . 3) ("4" . 4)
                    ("5" . 5) ("6" . 6) ("7" . 7) ("8" . 8) ("9" . 9)
                    ("a" . 10) ("b" . 11) ("c" . 12) ("d" . 13) ("e" . 14) ("f" . 15)))
         (invert-component (lambda (component)
                             (format "%02x" (logxor #xFF (string-to-number component 16))))))
    (if (string-match "^#\\([a-fA-F0-9]\\{2\\}\\)\\([a-fA-F0-9]\\{2\\}\\)\\([a-fA-F0-9]\\{2\\}\\)$" color)
        (concat "#"
                (funcall invert-component (match-string 1 color))
                (funcall invert-component (match-string 2 color))
                (funcall invert-component (match-string 3 color)))
      (error "Invalid color format"))))

(let* ((class '((class color) (min-colors 89)))
       (background "#000000")
       (foreground "#FFFFFF")

       ;; doom-molokai-colors
       ;; https://github.com/hlissner/emacs-doom-theme/blob/master/doom-molokai-theme.el
       (black          "#000000")
       (white          "#FFFFFF")
       (yellow         "#ffcc00") ;; "#E2C770" "#F9BA32"
       (yellow-c       "#281580")

       (turquoise-bright "#1abc9c")
       (turquoise-dark "#16a085")
       (green-bright "#2ecc71")
       (green-dark "#27ae60")
       (blue-bright "#3498db")
       (blue-dark "#2980b9")
       (magenta-bright "#9b59b6")
       (magenta-dark "#8e44ad")
       (grey-blue-bright "#34495e")
       (grey-blue-dark "#2c3e50")
       (yellow-bright "#f1c40f")
       (yellow-dark "#f39c12")
       (orange-bright "#e67e22")
       (orange-dark "#d35400")
       (red-bright "#e74c3c")
       (red-dark "#c0392b")
       (grey-bright2 "#ecf0f1")
       (grey-bright1 "#bdc3c7")
       (grey-dark1 "#95a5a6")
       (grey-dark2 "#7f8c8d")
       (black "#000")

       (grey           "#C0C5CF")
       (grey-.5        "#828284")
       (grey-1         "#525254")
       (grey-2         "#39393D")
       (orange         "#ffa500") ;; "#FD971F"
       (red            "#E74C3C") ;; "#F34A4A"
       (magenta        "#F92672")
       (violet         "#7b68ee")
       (blue           "#4CB5F5") ;; "#268BD2"
       (blue+2         "#727280")
       (cyan           "#66D9EF")
       (green          "#B6E63E") ;; "#B3DE81"
       (green-3        "#86B20E")
       (dark-cyan      "#8FA1B3")

       ;; danneskjold-colors
       (frost        "#D0E1F9")
       (invisible    "#2b4b6e")
       (comment      "#A4C2EB")

       (sbt-midnight   "#282c34")
       (ada-midnight   "#21252b")
       (summer-flower  "#013220")
       (diredcl        "#749AF7")
       (dvi            "DarkViolet")
       (waddles        "#FF87BA")
       (krayola        "#E38B75")
       (red-forest     "#8b0000")

       (search-bg      green)
       (search-fg      black)
       (search-rest-bg violet)
       (search-rest-fg black)
       (highlight      orange)
       (current-line   "#1F1F1F")
       (selection      "#535556")
       (builtin        orange)
       (comments       grey-1)
       (constants      green)
       (delimiters     "#c0c5ce")
       (functions      cyan)
       (keywords       magenta)
       (methods        dark-cyan)
       (operators      violet)
       (type           cyan)
       (strings        green)
       (variables      orange)

       (error-highlight red)

       (active-minibuffer "#404046")

       (vc-modified    grey-2)
       (vc-added       green-3)
       (vc-deleted     red))

  (custom-theme-set-faces 'danneskjold
                          `(default ((,class (:foreground ,foreground :background ,black))))
                          `(fringe ((,class (:foreground ,invisible))))
                          `(region ((,class (:background "#373d4f"))))
                          `(button ((,class (:foreground ,frost :underline t :weight normal))))
                          `(link ((,class (:foreground ,frost :underline t))))
                          `(menu ((,class (:foreground ,foreground :background ,ada-midnight))))
                          `(shadow ((,class (:foreground ,comment))))

                          `(secondary-selection ((,class ())))

                          `(show-paren-match ((,class (:background ,blue :foreground ,foreground))))

                          `(highlight ((,class (:background ,invisible :foreground ,black))))

                          `(font-lock-string-face ((,class (:foreground ,green))))
                          `(font-lock-doc-face ((,class (:foreground ,green))))
                          `(font-lock-builtin-face ((,class (:foreground ,blue))))
                          `(font-lock-variable-name-face ((,class (:foreground ,foreground))))
                          `(font-lock-keyword-face ((,class (:foreground ,frost))))
                          `(font-lock-comment-face ((,class (:foreground ,comment))))
                          `(font-lock-comment-delimiter-face ((,class (:foreground ,invisible))))
                          `(font-lock-function-name-face ((,class (:foreground ,yellow))))
                          `(font-lock-type-face ((,class (:foreground ,orange))))
                          `(font-lock-constant-face ((,class (:foreground ,yellow))))
                          `(font-lock-warning-face ((,class (:foreground ,yellow))))

                          `(mmm-default-submode-face ((,class (:background ,ada-midnight))))

                          `(header-line ((,class (:background ,black
                                                              :foreground ,comment
                                                              :underline ,comment
                                                              :weight normal))))

                          ;; Mode-line
                          `(mode-line ((,class (:background ,black :foreground ,comment))))
                          `(mode-line-inactive ((,class (:background ,black :foreground ,invisible))))
                          `(mode-line-buffer-id ((,class (:foreground ,foreground))))
                          `(org-mode-line-clock ((,class (:foreground ,yellow))))
                          `(org-mode-line-clock-overrun ((,class (:foreground ,red))))

                          `(compilation-error ((,class (:foreground ,red))))
                          `(compilation-line-number ((,class (:foreground ,green))))
                          `(compilation-column-number ((,class (:foreground ,blue))))
                          `(compilation-warning ((,class :foreground ,yellow)))
                          `(compilation-info ((,class (:foreground ,diredcl))))

                          ;; Linum
                          `(line-number ((,class (:foreground ,invisible))))
                          `(line-number-current-line ((,class (:foreground ,foreground))))

                          `(linum ((,class (:foreground ,invisible))))
                          `(linum-highlight-face ((,class (:foreground ,foreground))))

                          `(widget-field ((,class (:foreground ,white :background ,sbt-midnight))))
                          `(widget-button ((,class (:foreground ,yellow))))

                          ;; Highlight quoted mode-line
                          `(highlight-quoted-symbol ((,class (:foreground ,waddles))))
                          `(highlight-symbol-face ((,class (:foreground ,black :background ,diredcl))))

                          ;; Hl-line
                          `(hl-line ((,class (:background "keyboardFocusIndicatorColor"))))

                          ;; search
                          `(isearch ((,class (:foreground ,black :background ,invisible))))
                          `(lazy-highlight ((,class (:foreground ,black :background ,yellow))))

                          ;; diff
                          `(diff-added ((,class (:foreground ,green-dark))))
                          `(diff-changed ((,class (:foreground ,magenta-bright))))
                          `(diff-removed ((,class (:foreground ,yellow-dark))))
                          `(diff-header ((,class (:foreground ,magenta-dark :background nil))))
                          `(diff-file-header ((,class (:foreground ,blue-dark :background nil))))
                          `(diff-hunk-header ((,class (:foreground ,magenta-bright))))
                          `(diff-refine-removed ((,class (:inherit magit-diff-removed-highlight :foreground ,red-bright))))
                          `(diff-refine-added ((,class (:inherit magit-diff-added-highlight :foreground ,blue-bright))))

                          ;; diff-hl
                          `(diff-hl-change ((,class (:foreground ,blue-dark :background ,blue-dark))))
                          `(diff-hl-delete ((,class (:foreground ,orange-bright :background ,orange-dark))))
                          `(diff-hl-insert ((,class (:foreground ,green-dark :background ,green-dark))))

                          ;; ediff
                          `(ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
                          `(ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
                          `(ediff-odd-diff-A  ((,class (:foreground ,grey-dark1 :background nil :inverse-video t))))
                          `(ediff-odd-diff-B  ((,class (:foreground ,grey-dark1 :background nil :inverse-video t))))

                          ;; dired+
                          `(diredp-compressed-file-suffix ((,class (:foreground ,yellow-dark))))
                          `(diredp-date-time ((,class (:foreground ,yellow-bright))))
                          `(diredp-deletion ((,class (:foreground ,red-bright :weight bold :slant italic))))
                          `(diredp-deletion-file-name ((,class (:foreground ,red-bright :underline t))))
                          `(diredp-dir-heading ((,class (:foreground ,red-bright))))
                          `(diredp-dir-priv ((,class (:foreground ,magenta-bright :background nil))))
                          `(diredp-exec-priv ((,class (:foreground ,green-bright :background nil))))
                          `(diredp-executable-tag ((,class (:foreground ,green-bright :background nil))))
                          `(diredp-file-name ((,class (:foreground ,foreground))))
                          `(diredp-file-suffix ((,class (:foreground "coral"))))
                          `(diredp-flag-mark ((,class (:foreground ,red-bright :weight bold))))
                          `(diredp-flag-mark-line ((,class (:inherit highlight))))
                          `(diredp-ignored-file-name ((,class (:foreground ,grey-dark1))))
                          `(diredp-link-priv ((,class (:background nil :foreground ,orange-bright))))
                          `(diredp-mode-line-flagged ((,class (:foreground ,yellow-dark))))
                          `(diredp-mode-line-marked ((,class (:foreground ,magenta-bright))))
                          `(diredp-no-priv ((,class (:foreground ,grey-dark1 :background nil))))
                          `(diredp-number ((,class (:foreground ,yellow-dark))))
                          `(diredp-other-priv ((,class (:background nil :foreground ,yellow-dark))))
                          `(diredp-rare-priv ((,class (:foreground ,red-dark :background nil))))
                          `(diredp-read-priv ((,class (:foreground ,blue-dark :background nil))))
                          `(diredp-symlink ((,class (:foreground ,orange-bright))))
                          `(diredp-write-priv ((,class (:foreground ,magenta-bright :background nil))))
                          `(diredp-date-time ((,class (:foreground ,diredcl))))
                          `(diredp-dir-name ((,class (:foreground "DeepSkyBlue1"))))
                          `(diredp-ignored-file-name ((,class ())))
                          `(diredp-compressed-file-suffix ((,class (:foreground ,orange))))
                          `(diredp-rainbow-media-face ((,class (:foreground ,yellow))))
                          `(diredp-symlink ((,class (:foreground ,yellow))))
                          `(diredp-number ((,class (:foreground ,yellow))))

                          ;; ein
                          `(ein:cell-input-area ((,class (:background ,ada-midnight))))
                          `(ein:cell-output-area ((,class (:background ,black))))
                          `(ein:cell-input-prompt ((,class (:foreground ,yellow))))
                          `(ein:cell-output-prompt ((,class (:foreground ,orange))))

                          ;; lsp
                          `(lsp-face-highlight-read ((,class (:underline ,blue))))

                          ;; magit
                          `(magit-branch ((,class (:foreground ,green-dark))))
                          `(magit-header ((,class (:inherit nil :weight bold))))
                          `(magit-item-highlight ((,class (:inherit highlight :background nil))))
                          `(magit-log-graph ((,class (:foreground ,grey-dark2))))
                          `(magit-log-sha1 ((,class (:foreground ,yellow-bright))))
                          `(magit-log-head-label-bisect-bad ((,class (:foreground ,red-dark))))
                          `(magit-log-head-label-bisect-good ((,class (:foreground ,green-dark))))
                          `(magit-log-head-label-default ((,class (:foreground ,yellow-bright :box nil :weight bold))))
                          `(magit-log-head-label-local ((,class (:foreground ,magenta-bright :box nil :weight bold))))
                          `(magit-log-head-label-remote ((,class (:foreground ,magenta-bright :box nil :weight bold))))
                          `(magit-log-head-label-tags ((,class (:foreground ,magenta-dark :box nil :weight bold))))
                          `(magit-section-title ((,class (:foreground ,blue-dark :weight bold))))

                          ;; magit `next'
                          `(magit-section ((,class (:inherit nil))))
                          `(magit-section-highlight ((,class (:background "#0e1824"))))
                          `(magit-section-heading ((,class (:foreground ,blue-bright))))
                          `(magit-branch-local ((,class (:foreground ,turquoise-bright))))
                          `(magit-branch-remote ((,class (:foreground ,yellow-bright))))
                          `(magit-hash ((,class (:foreground ,grey-bright2))))
                          `(magit-diff-file-heading ((,class (:foreground ,yellow))))
                          `(magit-diff-file-heading-highlight ((,class (:foreground ,yellow-bright :background "#0e1824"))))
                          `(magit-diff-hunk-heading ((,class (:foreground ,magenta-bright))))
                          `(magit-diff-hunk-heading-highlight ((,class (:inherit magit-diff-hunk-heading :weight bold))))
                          `(magit-diff-context ((,class (:foreground ,grey-bright1))))
                          `(magit-diff-context-highlight ((,class (:foreground ,grey-bright2 :background "#0e1824"))))
                          `(magit-diff-added ((,class (:foreground ,green-dark))))
                          `(magit-diff-added-highlight ((,class (:foreground ,green-bright :background "#0e1824"))))
                          `(magit-diff-removed ((,class (:foreground ,red-dark))))
                          `(magit-diff-removed-highlight ((,class (:foreground ,red-bright :background "#0e1824"))))

                          ;; ledger
                          `(ledger-font-xact-highlight-face ((,class (:background "#0e1824"))))

                          ;; Monky
                          `(monky-diff-add ((,class (:foreground ,green))))
                          `(monky-diff-del ((,class (:foreground ,red))))

                          ;; Org-mode
                          `(org-tag                      ((,class (:foreground ,frost :slant italic))))
                          `(org-ellipsis                 ((,class (:inherit hs-face :underline nil))))
                          `(org-link                     ((,class (:foreground ,frost :underline ,invisible))))
                          `(org-hide                     ((,class (:foreground ,black))))
                          `(org-table                    ((,class (:foreground ,yellow))))
                          `(org-quote                    ((,class (:slant italic :foreground ,foreground))))
                          `(org-document-info            ((,class (:foreground ,foreground))))
                          `(org-document-info-keyword    ((,class (:foreground ,frost :bold t))))
                          `(org-meta-line                ((,class (:foreground ,frost))))
                          `(org-block                    ((,class (:background "#0e1824"))))
                          `(org-block-begin-line         ((,class (:foreground "selectedControlColor" :slant italic))))
                          `(org-block-end-line           ((,class (:inherit org-block-begin-line))))
                          `(org-archived                 ((,class (:foreground ,invisible))))
                          `(org-document-title           ((,class (:foreground ,foreground :bold t))))
                          `(org-level-1                  ((,class (:foreground ,foreground))))
                          `(org-level-2                  ((,class (:foreground ,foreground))))
                          `(org-level-3                  ((,class (:foreground ,foreground))))
                          `(org-level-4                  ((,class (:foreground ,foreground))))
                          `(org-level-5                  ((,class (:foreground ,foreground))))
                          `(org-level-6                  ((,class (:foreground ,foreground))))
                          `(org-level-7                  ((,class (:foreground ,foreground))))
                          `(org-level-8                  ((,class (:foreground ,foreground))))
                          `(org-code                     ((,class (:foreground ,orange))))
                          `(org-column                   ((,class (:background ,black))))
                          `(org-column-title             ((,class (:background ,black :foreground ,comment))))
                          `(org-formula                  ((,class (:foreground ,orange))))
                          `(org-latex-and-related        ((,class (:foreground "#FAF9FF"))))
                          `(org-list-dt                  ((,class (:foreground ,yellow))))
                          `(org-footnote                 ((,class (:foreground ,orange))))
                          `(org-priority                 ((,class (:foreground ,foreground))))
                          `(org-drawer                   ((,class (:foreground ,orange))))

                          `(org-date                     ((,class (:foreground "LightSteelBlue2" :underline "LightSteelBlue4"))))
                          `(org-todo                     ((,class (:foreground ,yellow))))
                          `(org-done                     ((,class (:foreground ,green))))
                          `(org-special-keyword          ((,class (:foreground "#ffcc00"))))
                          `(org-property-value          ((,class (:foreground ,foreground))))
                          `(org-checkbox-statistics-todo ((,class (:inherit org-todo))))
                          `(org-checkbox-statistics-done ((,class (:inherit org-done))))

                          `(org-headline-done ((,class (:foreground ,comment))))

                          ;; jupyter
                          `(jupyter-repl-traceback ((,class (:foreground ,orange))))

                          ;; packages
                          `(package-name                 ((,class (:inherit org-link))))

                          ;; cider
                          `(cider-stacktrace-error-class-face ((,class (:foreground ,yellow))))
                          `(cider-stacktrace-error-message-face ((,class (:foreground ,green))))

                          ;; minimap
                          `(minimap-active-region-background ((,class (:background ,sbt-midnight))))

                          ;; vc
                          `(vc-annotate-face-3F3FFF ((,class (:foreground ,blue))))
                          `(vc-annotate-face-FF3F3F ((,class (:foreground ,red))))
                          `(vc-annotate-face-FFF33F ((,class (:foreground ,yellow))))
                          `(vc-annotate-face-56FF3F ((,class (:foreground ,green))))
                          `(vc-annotate-face-3FF3FF ((,class (:foreground ,frost))))
                          `(vc-annotate-face-3FFF56 ((,class (:foreground ,violet))))

                          ;; comint
                          `(comint-highlight-input ((,class (:foreground ,foreground :bold t))))
                          `(comint-highlight-prompt ((,class (:foreground ,cyan))))

                          `(org-habit-clear-face ((,class (:background ,black :foreground ,frost :underline t))))
                          `(org-habit-clear-future-face ((,class (:background ,black :underline t :foreground ,frost))))
                          `(org-habit-ready-face ((,class (:background ,black :foreground ,green :underline t))))
                          `(org-habit-ready-future-face ((,class (:background ,black :underline t :foreground ,green))))
                          `(org-habit-alert-face ((,class (:background ,black :underline t :foreground ,yellow))))
                          `(org-habit-alert-future-face ((,class (:background ,black :underline t :foreground ,yellow))))
                          `(org-habit-overdue-face ((,class (:background ,black :underline t :foreground ,red))))
                          `(org-habit-overdue-future-face ((,class (:background ,black :underline t :foreground ,red))))

                          `(org-scheduled ((,class (:foreground ,foreground))))
                          `(org-warning ((,class (:foreground "#FFCA00"))))
                          `(org-scheduled-today ((,class (:foreground ,white))))
                          `(org-scheduled-previously ((,class (:foreground ,violet :slant italic))))
                          `(org-time-grid ((,class (:foreground ,invisible))))

                          `(org-agenda-clocking ((,class (:foreground ,diredcl :background ,ada-midnight))))

                          `(org-agenda-date ((,class (:foreground ,blue))))
                          `(org-agenda-date-today ((,class (:foreground ,white))))
                          `(org-agenda-date-weekend ((,class (:foreground ,white :bold t))))

                          `(org-agenda-structure ((,class (:foreground ,white))))
                          `(org-agenda-calendar-event ((,class (:foreground ,yellow :slant italic))))

                          `(org-agenda-done ((,class (:foreground ,green))))
                          `(org-agenda-diary ((,class (:foreground ,orange :slant italic))))
                          `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
                          `(org-agenda-current-time ((,class (:foreground ,frost))))
                          `(org-upcoming-deadline ((,class (:foreground ,orange))))
                          `(org-upcoming-distant-deadline ((,class (:foreground ,comment))))

                          ;; Hydra
                          `(hydra-face-red ((,class (:foreground ,red))))
                          `(hydra-face-blue ((,class (:foreground ,blue))))

                          ;; Hi
                          `(hi-green-b ((,class (:foreground ,green))))
                          `(hi-yellow-b ((,class (:foreground ,yellow))))
                          `(hi-yellow ((,class (:foreground ,black :background ,yellow))))
                          `(hi-red-b ((,class (:foreground ,red))))

                          ;; Bookmarks
                          `(bmkp-remote-file ((,class (:foreground ,green))))
                          `(bmkp-url ((,class (:foreground ,white))))
                          `(bmkp-local-directory ((,class (:foreground ,blue))))
                          `(bmkp-no-local ((,class (:foreground ,yellow :slant italic))))
                          `(bmkp-D-mark ((,class (:foreground ,red))))

                          ;; Wgrep
                          `(wgrep-face                  ((,class (:foreground ,yellow))))
                          `(wgrep-done-face                  ((,class (:foreground ,green))))
                          `(wgrep-file-face                  ((,class (:background ,(invert-color yellow-c) :foreground ,yellow))))
                          `(wgrep-reject-face                  ((,class (:foreground ,red))))

                          ;; Perspeen
                          `(perspeen-selected-face ((,class (:foreground ,frost))))

                          ;; Whitespace mode
                          `(whitespace-space-after-tab ((,class (:foreground ,ada-midnight))))
                          `(whitespace-space ((,class (:foreground ,ada-midnight))))
                          `(whitespace-hspace ((,class (:foreground ,ada-midnight))))
                          `(whitespace-newline ((,class (:foreground ,ada-midnight))))
                          `(whitespace-line ((,class ())))
                          `(whitespace-empty ((,class (:foreground ,ada-midnight))))
                          `(whitespace-tab ((,class (:foreground ,ada-midnight))))
                          `(whitespace-indentation ((,class (:foreground ,ada-midnight))))

                          ;; Popup
                          `(popup-face ((,class (:foreground ,foreground :background ,ada-midnight))))
                          `(popup-tip-face ((,class (:foreground ,black :background ,yellow))))
                          `(popup-menu-mouse-face ((,class (:foreground ,foreground :background ,sbt-midnight))))
                          `(popup-menu-selection-face ((,class (:foreground ,foreground :background ,sbt-midnight))))
                          `(flx-highlight-face ((,class (:underline ,red :background ,sbt-midnight :foreground ,foreground))))

                          ;; Powerline
                          `(powerline-active1 ((,class (:foreground ,yellow))))
                          `(powerline-active2 ((,class (:foreground ,comment))))
                          `(powerline-inactive1 ((,class (:foreground ,comment))))
                          `(powerline-inactive2 ((,class (:foreground ,comment))))

                          ;; Prodigy
                          `(prodigy-red-face ((,class (:foreground ,red))))
                          `(prodigy-green-face ((,class (:foreground ,green))))
                          `(prodigy-yellow-face ((,class (:foreground ,yellow))))

                          ;; Jabber
                          `(jabber-title-large ((,class (:foreground ,yellow))))
                          `(jabber-title-medium ((,class (:foreground ,yellow))))
                          `(jabber-title-small ((,class (:foreground ,yellow))))
                          `(jabber-chat-prompt-local ((,class (:foreground ,frost))))
                          `(jabber-chat-prompt-foreign ((,class (:foreground ,yellow))))
                          `(jabber-roster-user-xa ((,class (:foreground ,yellow))))
                          `(jabber-roster-user-online ((,class (:foreground ,green))))
                          `(jabber-roster-user-offline ((,class (:foreground ,comment))))
                          `(jabber-roster-user-away ((,class (:foreground ,frost))))
                          `(jabber-rare-time-face ((,class (:foreground ,sbt-midnight))))

                          ;; SQL*Plus
                          `(sqlplus-table-head-face ((,class (:foreground ,yellow))))
                          `(sqlplus-table-even-rows-face ((,class (:foreground ,white :background ,ada-midnight))))
                          `(sqlplus-table-odd-rows-face ((,class (:foreground ,white))))

                          ;; Rainbow-delimiters
                          `(rainbow-delimiters-depth-1-face ((,class (:foreground ,green))))
                          `(rainbow-delimiters-depth-2-face ((,class (:foreground ,red))))
                          `(rainbow-delimiters-depth-3-face ((,class (:foreground ,frost))))
                          `(rainbow-delimiters-depth-4-face ((,class (:foreground ,yellow))))
                          `(rainbow-delimiters-depth-5-face ((,class (:foreground ,waddles))))
                          `(rainbow-delimiters-depth-6-face ((,class (:foreground ,frost))))
                          `(rainbow-delimiters-depth-7-face ((,class (:foreground ,green))))
                          `(rainbow-delimiters-depth-8-face ((,class (:foreground ,red))))
                          `(rainbow-delimiters-depth-9-face ((,class (:foreground ,frost))))
                          `(rainbow-delimiters-depth-10-face ((,class (:foreground ,yellow))))

                          ;; Company-mode
                          `(company-tooltip ((,class (:foreground ,white :background ,ada-midnight))))
                          `(company-tooltip-selection ((,class (:foreground ,white :background ,sbt-midnight))))
                          `(company-scrollbar-fg ((,class (:background ,ada-midnight))))
                          `(company-scrollbar-bg ((,class (:background ,sbt-midnight))))
                          `(company-tooltip-common ((,class (:foreground ,yellow))))
                          `(company-preview ((,class (:background ,sbt-midnight))))
                          `(company-preview-common ((,class (:background ,sbt-midnight :foreground ,red))))
                          `(company-mouse ((,class (:background ,ada-midnight))))

                          ;; Elfeed
                          `(elfeed-search-feed-face ((,class (:foreground ,comment))))
                          `(elfeed-search-tag-face ((,class (:foreground ,green))))
                          `(elfeed-search-unread-title-face ((,class (:foreground ,white))))
                          `(elfeed-search-date-face ((,class (:foreground ,diredcl))))

                          ;; Flycheck
                          `(flycheck-warning ((,class (:underline (:color "orange" :style wave)))))
                          `(flymake-error ((,class (:underline (:color "red" :style wave)))))

                          ;; js2-mode
                          `(js2-function-param ((,class (:foreground ,yellow))))

                          ;; message
                          `(message-header-name ((,class (:foreground ,comment))))
                          `(message-header-subject ((,class (:foreground ,white))))
                          `(message-header-to ((,class (:foreground ,white))))
                          `(message-header-other ((,class (:foreground ,white))))
                          `(shr-link ((,class (:foreground ,frost :underline t))))

                          ;; erc
                          `(erc-timestamp-face ((,class (:foreground ,red))))
                          `(erc-prompt-face ((,class (:foreground ,green))))
                          `(erc-nick-default-face ((,class (:foreground ,frost))))
                          `(erc-notice-face ((,class (:foreground ,waddles))))
                          `(erc-button ((,class (:foreground ,frost))))
                          `(erc-current-nick-face ((,class (:foreground ,red))))

                          ;; eshell
                          `(eshell-prompt ((,class (:foreground ,red))))
                          `(eshell-ls-executable ((,class (:foreground ,green))))
                          `(eshell-ls-directory ((,class (:foreground ,blue))))
                          `(eshell-ls-symlink ((,class (:foreground ,waddles))))
                          `(eshell-ls-readonly ((,class (:foreground ,invisible))))
                          `(eshell-ls-missing ((,class (:foreground ,red))))
                          `(eshell-ls-product ((,class (:foreground ,comment))))
                          `(eshell-ls-special ((,class (:foreground ,comment :underline t))))

                          `(epe-remote-face ((,class (:foreground ,comment))))
                          `(epe-dir-face ((,class (:foreground ,blue))))
                          `(epe-symbol-face ((,class (:foreground ,yellow))))

                          ;; dired
                          `(dired-directory ((,class (:foreground ,blue))))
                          `(dired-git-face ((,class (:foreground ,red))))
                          `(dired-ignored ((,class (:foreground ,invisible))))
                          `(dired-filetype-omit ((,class (:foreground ,invisible))))
                          `(dired-filetype-common ((,class (:foreground ,yellow))))
                          `(dired-filetype-execute ((,class (:foreground ,green))))
                          `(dired-filetype-source ((,class (:foreground ,orange))))
                          `(dired-filetype-plain ((,class (:foreground ,comment))))
                          `(dired-filetype-link ((,class (:foreground ,blue :underline t))))
                          `(dired-flagged ((,class (:foreground ,red :underline t))))
                          `(dired-marked ((,class (:foreground ,yellow :underline t))))
                          `(dired-subtree-depth-1-face ((,class (:background ,black))))
                          `(dired-subtree-depth-2-face ((,class (:background ,black))))
                          `(dired-subtree-depth-3-face ((,class (:background ,black))))
                          `(dired-subtree-depth-4-face ((,class (:background ,black))))
                          `(dired-subtree-depth-5-face ((,class (:background ,black))))
                          `(dired-subtree-depth-6-face ((,class (:background ,black))))
                          `(dired-subtree-depth-7-face ((,class (:background ,black))))
                          `(dired-subtree-depth-8-face ((,class (:background ,black))))
                          `(dired-subtree-depth-9-face ((,class (:background ,black))))

                          ;; custom hacks
                          `(dired-collapsed-dirs ((,class (:background ,black :foreground ,frost))))

                          ;; dired-rainbow
                          `(dired-rainbow-executable-unix-face ((,class (:foreground ,green))))

                          `(diredp-dir-priv ((,class (:foreground ,blue))))
                          `(diredp-read-priv ((,class (:foreground ,red))))
                          `(diredp-write-priv ((,class (:foreground ,yellow))))
                          `(diredp-exec-priv ((,class (:foreground ,green))))
                          `(diredp-no-priv ((,class (:foreground ,white))))
                          `(diredp-rare-priv ((,class (:foreground ,waddles))))
                          `(diredp-flag-mark ((,class (:foreground ,black))))
                          `(diredp-flag-mark-line ((,class (:foreground ,black :background ,yellow))))
                          `(diredp-mode-line-marked ((,class (:foreground ,yellow))))
                          `(diredp-deletion ((,class (:foreground ,black :background ,red))))
                          `(diredp-deletion-file-name ((,class (:foreground ,black :background ,red))))
                          `(diredp-mode-line-flagged ((,class (:foreground ,red))))

                          ;; ido
                          `(minibuffer-prompt ((,class (:foreground ,diredcl))))
                          `(ido-first-match ((,class (:bold t :foreground ,white))))
                          `(ido-only-match ((,class (:bold t :foreground ,white))))
                          `(ido-subdir ((,class (:foreground ,frost))))
                          `(ido-virtual ((,class (:foreground ,comment))))
                          `(ido-vertical-match-face ((,class (:underline ,diredcl))))

                          ;; completions
                          `(ivy-current-match ((,class (:foreground "#FFCA00"))))
                          `(vertico-current ((,class (:foreground "#FFCA00"))))
                          `(consult-file ((,class (:inherit consult-buffer))))

                          `(ivy-prompt-match ((,class (:bold t :foreground ,white))))
                          `(ivy-confirm-face ((,class (:underline ,diredcl))))

                          `(ivy-virtual ((,class (:foreground ,comment))))
                          `(ivy-remote ((,class (:foreground ,white :slant italic))))
                          `(ivy-cursor ((,class (:foreground ,comment))))
                          `(ivy-action ((,class (:foreground ,yellow))))
                          `(ivy-highlight-face ((,class (:foreground ,orange))))
                          `(ivy-minibuffer-match-face-1 ((,class (:foreground ,white))))
                          `(ivy-minibuffer-match-face-2 ((,class (:underline ,red))))
                          `(ivy-minibuffer-match-face-3 ((,class (:underline ,orange))))
                          `(ivy-minibuffer-match-face-4 ((,class (:underline ,yellow))))
                          `(swiper-match-face-1 ((,class (:foreground ,white))))
                          `(swiper-match-face-2 ((,class (:underline ,red))))
                          `(swiper-match-face-3 ((,class (:underline ,orange))))
                          `(swiper-match-face-4 ((,class (:underline ,yellow))))

                          ;; vertical-border
                          `(vertical-border ((,class (:foreground "#223959"))))

                          ;; yas
                          `(yas-field-highlight-face ((,class (:background ,ada-midnight))))

                          ;; epresent
                          `(epresent-title-face ((,class (:inherit 'default :height 2.0))))
                          `(epresent-heading-face ((,class ())))
                          `(epresent-subheading-face ((,class ())))
                          `(epresent-author-face ((,class (:inherit 'default))))
                          `(epresent-bullet-face ((,class (:inherit 'default))))

                          ;; hackernews
                          `(hackernews-score-face ((,class (:foreground ,white))))
                          `(hackernews-link-face ((,class (:foreground ,green))))
                          `(hackernews-comment-count-face ((,class (:foreground ,red))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'danneskjold)
;;; danneskjold-theme.el ends here

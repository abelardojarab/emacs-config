;; danneskjold-theme.el --- beautiful high-contrast theme

;; Copyright (c) 2016 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; URL: https://github.com/rails-to-cosmos/
;; Package-Version: 20160311.458

;;; Commentary:

;;; Code:

(deftheme danneskjold
  "Amazing. Beautiful. Contrast.")

(let ((c '((class color) (min-colors 89)))
      (background     "#000000")
      (fg             "#ffffff")

      ;; doom-molokai-colors
      ;; https://github.com/hlissner/emacs-doom-theme/blob/master/doom-molokai-theme.el
      (black          "#000000")
      (black-c        "#FFFFFF")

      (white          "#FFFFFF")
      (white-c        "#000000")

      (yellow         "#FFDB45") ;; "#E2C770" "#F9BA32"
      (yellow-c       "#281580")

      (grey           "#C0C5CF")
      (grey-.5        "#828284")
      (grey-1         "#525254")
      (grey-2         "#39393D")
      (orange         "#FF9009") ;; "#FD971F"
      (red            "#E74C3C") ;; "#F34A4A"
      (magenta        "#F92672")
      (violet         "#9C91E4")
      (blue           "#4CB5F5") ;; "#268BD2"
      (blue+2         "#727280")
      (cyan           "#66D9EF")
      (green          "#B6E63E") ;; "#B3DE81"
      (green-3        "#86B20E")
      (dark-cyan      "#8FA1B3")


      ;; danneskjold-colors
      (frost        "#D0E1F9")
      (invisible    "#7B9ED2")
      (comment      "#A4C2EB")

      (sbt-midnight   "#282c34")
      (ada-midnight   "#21252b")
      (summer-flower  "#013220")
      (diredcl        "#749AF7")
      (dvi            "DarkViolet")
      (waddles        "#FF87BA")
      (krayola        "#E38B75")
      (red-forest     "#8b0000"))

  (let* ((search-bg      green)
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

    (custom-theme-set-faces
     'danneskjold
     `(default ((,c (:foreground ,white :background ,black))))
     `(fringe ((,c (:foreground ,invisible :background ,black))))
     `(region ((,c (:foreground ,black :background ,white))))
     `(button ((,c (:foreground ,frost :underline t :weight normal))))
     `(link ((,c (:foreground ,frost :underline t))))
     `(menu ((,c (:foreground ,white :background ,ada-midnight))))

     `(show-paren-match ((,c (:background ,blue :foreground ,white))))

     `(highlight ((,c (:background ,invisible :foreground ,black))))

     `(font-lock-string-face ((,c (:foreground ,green))))
     `(font-lock-doc-face ((,c (:foreground ,green))))
     `(font-lock-builtin-face ((,c (:foreground ,blue))))
     `(font-lock-variable-name-face ((,c (:foreground ,white))))
     `(font-lock-keyword-face ((,c (:foreground ,frost))))
     `(font-lock-comment-face ((,c (:foreground ,comment))))
     `(font-lock-comment-delimiter-face ((,c (:foreground ,invisible))))
     `(font-lock-function-name-face ((,c (:foreground ,yellow))))
     `(font-lock-type-face ((,c (:foreground ,orange))))
     `(font-lock-constant-face ((,c (:foreground ,yellow))))
     `(font-lock-warning-face ((,c (:underline (:color ,red :style wave)))))

     `(mmm-default-submode-face ((,c (:background ,ada-midnight))))

     `(header-line ((,c (:background ,black
                         :foreground ,comment
                         :underline ,comment
                         :weight normal))))

     ;; Mode-line
     `(mode-line ((,c (:background ,ada-midnight :foreground ,comment))))
     `(mode-line-inactive ((,c (:background ,sbt-midnight :foreground ,invisible))))
     `(mode-line-buffer-id ((,c (:foreground ,fg))))

     `(compilation-error ((,c (:foreground ,red))))
     `(compilation-line-number ((,c (:foreground ,yellow))))
     `(compilation-column-number ((,c (:foreground ,green))))
     `(compilation-warning ((,c (:underline (:color ,red :style wave)))))
     `(compilation-info ((,c (:foreground ,diredcl))))

     ;; Linum
     `(line-number ((,c (:foreground ,invisible))))
     `(line-number-current-line ((,c (:foreground ,white))))

     `(linum ((,c (:foreground ,invisible))))
     `(linum-highlight-face ((,c (:foreground ,white))))

     `(widget-field ((,c (:foreground ,white :background ,sbt-midnight))))
     `(widget-button ((,c (:foreground ,yellow))))

     ;; Highlight quoted mode-line
     `(highlight-quoted-symbol ((,c (:foreground ,waddles))))
     `(highlight-symbol-face ((,c (:foreground ,black :background ,diredcl))))

     ;; Hl-line
     `(hl-line ((,c (:background "#284D80"))))

     ;; search
     `(isearch ((,c (:foreground ,black :background ,invisible))))
     `(lazy-highlight ((,c (:foreground ,black :background ,yellow))))

     ;; Diff
     `(diff-header ((,c (:foreground ,yellow))))
     `(diff-file-header ((,c (:foreground ,yellow))))
     `(diff-indicator-removed ((,c (:foreground ,black))))
     `(diff-removed ((,c (:foreground ,red))))
     `(diff-added ((,c (:foreground ,green))))
     `(diff-indicator-added ((,c (:foreground ,black))))
     `(diff-refine-removed ((,c (:foreground ,red-forest))))
     `(diff-refine-added ((,c (:foreground ,summer-flower))))

     `(diff-context ((,c (:foreground ,comment))))

     ;; Magit
     `(git-commit-summary ((,c (:foreground ,white))))
     `(git-commit-comment-file ((,c (:foreground ,green))))
     `(git-commit-comment-heading ((,c (:foreground ,frost))))

     `(magit-diff-added ((,c (:foreground ,green))))
     `(magit-diff-added-highlight ((,c (:foreground ,green))))
     `(magit-diff-removed ((,c (:foreground ,red))))
     `(magit-diff-removed-highlight ((,c (:foreground ,red))))
     `(magit-diff-context ((,c (:background ,black :foreground ,frost))))
     `(magit-diff-context-highlight ((,c (:background ,black :foreground ,frost))))
     `(magit-section-highlight ((,c (:background ,black))))
     `(magit-section-heading ((,c (:foreground ,yellow))))
     `(magit-diff-hunk-heading ((,c (:foreground ,blue :background ,sbt-midnight))))
     `(magit-diff-hunk-heading-highlight ((,c (:foreground ,blue :background ,sbt-midnight))))
     `(magit-diff-lines-heading ((,c (:foreground ,frost :background ,sbt-midnight))))
     `(magit-blame-heading ((,c (:foreground ,blue :background ,sbt-midnight))))
     `(magit-diff-lines-heading ((,c (:foreground ,orange :background ,sbt-midnight))))
     `(magit-diff-lines-boundary ((,c (:foreground ,white :background ,sbt-midnight))))
     `(magit-diff-hunk-heading-highlight ((,c (:foreground ,white :background ,sbt-midnight))))

     ;; Monky
     `(monky-diff-add ((,c (:foreground ,green))))
     `(monky-diff-del ((,c (:foreground ,red))))

     ;; Org-mode
     `(org-tag                      ((,c (:foreground ,invisible :bold nil))))
     `(org-ellipsis                 ((,c (:inherit hs-face :underline nil))))
     `(org-link                     ((,c (:foreground ,frost :underline ,diredcl))))
     `(org-hide                     ((,c (:foreground ,black))))
     `(org-table                    ((,c (:foreground ,yellow))))
     `(org-quote                    ((,c (:slant italic :foreground ,grey :background ,current-line))))
     `(org-document-info            ((,c (:foreground ,yellow))))
     `(org-document-info-keyword    ((,c (:foreground ,comment))))
     `(org-meta-line                ((,c (:foreground ,comment))))
     `(org-block                    ((,c ())))
     `(org-block-begin-line         ((,c (:foreground ,invisible))))
     `(org-block-end-line           ((,c (:inherit org-block-begin-line))))
     `(org-archived                 ((,c (:foreground ,invisible))))
     `(org-document-title           ((,c (:foreground ,blue))))
     `(org-level-1                  ((,c (:foreground ,white))))
     `(org-level-2                  ((,c (:foreground ,white))))
     `(org-level-3                  ((,c (:foreground ,white))))
     `(org-level-4                  ((,c (:foreground ,white))))
     `(org-level-5                  ((,c (:foreground ,white))))
     `(org-level-6                  ((,c (:foreground ,white))))
     `(org-level-7                  ((,c (:foreground ,white))))
     `(org-level-8                  ((,c (:foreground ,white))))
     `(org-code                     ((,c (:foreground ,orange))))
     `(org-column                   ((,c (:background ,black))))
     `(org-column-title             ((,c (:background ,black :foreground ,comment))))
     `(org-verbatim                 ((,c (:foreground ,blue))))
     `(org-formula                  ((,c (:foreground ,orange))))
     `(org-latex-and-related        ((,c (:foreground "#FAF9FF"))))
     `(org-list-dt                  ((,c (:foreground ,yellow))))
     `(org-footnote                 ((,c (:foreground ,orange))))
     `(org-priority                 ((,c (:foreground ,red))))

     `(org-date                     ((,c (:foreground ,violet))))
     `(org-todo                     ((,c (:foreground ,yellow))))
     `(org-done                     ((,c (:foreground ,green))))
     `(org-headline-done            ((,c (:foreground ,grey-.5 :strike-through t :bold nil))))
     `(org-special-keyword          ((,c (:foreground "orange"))))
     `(org-checkbox-statistics-todo ((,c (:inherit org-todo))))
     `(org-checkbox-statistics-done ((,c (:inherit org-done))))

     ;; minimap
     `(minimap-active-region-background ((,c (:background ,sbt-midnight))))

     ;; lsp
     ;; lsp-face-highlight-textual
     ;; lsp-face-highlight-read
     ;; lsp-face-highlight-write

     ;; vc
     `(vc-annotate-face-3F3FFF ((,c (:foreground ,blue))))
     `(vc-annotate-face-FF3F3F ((,c (:foreground ,red))))
     `(vc-annotate-face-FFF33F ((,c (:foreground ,yellow))))
     `(vc-annotate-face-56FF3F ((,c (:foreground ,green))))
     `(vc-annotate-face-3FF3FF ((,c (:foreground ,frost))))
     `(vc-annotate-face-3FFF56 ((,c (:foreground ,violet))))

     ;; comint
     `(comint-highlight-input ((,c (:foreground ,white :bold t))))
     `(comint-highlight-prompt ((,c (:foreground ,invisible))))

     `(org-habit-clear-face ((,c (:background ,black :foreground ,frost :underline t))))
     `(org-habit-clear-future-face ((,c (:background ,black :underline t :foreground ,frost))))
     `(org-habit-ready-face ((,c (:background ,black :foreground ,green :underline t))))
     `(org-habit-ready-future-face ((,c (:background ,black :underline t :foreground ,green))))
     `(org-habit-alert-face ((,c (:background ,black :underline t :foreground ,yellow))))
     `(org-habit-alert-future-face ((,c (:background ,black :underline t :foreground ,yellow))))
     `(org-habit-overdue-face ((,c (:background ,black :underline t :foreground ,red))))
     `(org-habit-overdue-future-face ((,c (:background ,black :underline t :foreground ,red))))

     `(org-scheduled ((,c (:foreground ,white))))
     `(org-warning ((,c (:foreground ,orange :slant italic))))
     `(org-scheduled-today ((,c (:foreground ,white))))
     `(org-scheduled-previously ((,c (:foreground ,red))))
     `(org-time-grid ((,c (:foreground ,invisible))))

     `(org-agenda-clocking ((,c (:foreground ,diredcl :background ,ada-midnight))))

     `(org-agenda-date ((,c (:foreground ,blue))))
     `(org-agenda-date-today ((,c (:foreground ,white))))
     `(org-agenda-date-weekend ((,c (:foreground ,green))))

     `(org-agenda-structure ((,c (:foreground ,white))))
     `(org-agenda-calendar-event ((,c (:foreground ,comment :slant italic))))

     `(org-agenda-done ((,c (:foreground ,green))))
     `(org-agenda-diary ((,c (:foreground ,orange :slant italic))))
     `(org-agenda-dimmed-todo-face ((,c (:foreground ,comment))))
     `(org-agenda-current-time ((,c (:foreground ,frost))))
     `(org-upcoming-deadline ((,c (:foreground ,violet))))

     `(secondary-selection ((,c (:background ,sbt-midnight))))

     ;; Hydra
     `(hydra-face-red ((,c (:foreground ,red))))
     `(hydra-face-blue ((,c (:foreground ,blue))))

     ;; Hi
     `(hi-green-b ((,c (:foreground ,green))))
     `(hi-yellow-b ((,c (:foreground ,yellow))))
     `(hi-yellow ((,c (:foreground ,black :background ,yellow))))
     `(hi-red-b ((,c (:foreground ,red))))

     ;; Bookmarks
     `(bmkp-remote-file ((,c (:foreground ,green))))
     `(bmkp-url ((,c (:foreground ,white))))
     `(bmkp-local-directory ((,c (:foreground ,blue))))
     `(bmkp-no-local ((,c (:foreground ,yellow :slant italic))))
     `(bmkp-D-mark ((,c (:foreground ,red))))

     ;; Wgrep
     `(wgrep-face                  ((,c (:foreground ,yellow))))
     `(wgrep-done-face                  ((,c (:foreground ,green))))
     `(wgrep-file-face                  ((,c (:background ,yellow-c :foreground ,yellow))))
     `(wgrep-reject-face                  ((,c (:foreground ,red))))

     ;; Perspeen
     `(perspeen-selected-face ((,c (:foreground ,frost))))

     ;; Whitespace mode
     `(whitespace-space-after-tab ((,c (:foreground ,ada-midnight))))
     `(whitespace-space ((,c (:foreground ,ada-midnight))))
     `(whitespace-hspace ((,c (:foreground ,ada-midnight))))
     `(whitespace-newline ((,c (:foreground ,ada-midnight))))
     `(whitespace-line ((,c ())))
     `(whitespace-empty ((,c (:foreground ,ada-midnight))))
     `(whitespace-tab ((,c (:foreground ,ada-midnight))))
     `(whitespace-indentation ((,c (:foreground ,ada-midnight))))

     ;; Popup
     `(popup-face ((,c (:foreground ,white :background ,ada-midnight))))
     `(popup-menu-mouse-face ((,c (:foreground ,white :background ,sbt-midnight))))
     `(popup-menu-selection-face ((,c (:foreground ,white :background ,sbt-midnight))))
     `(flx-highlight-face ((,c (:underline ,red :background ,sbt-midnight :foreground ,white))))

     ;; Powerline
     `(powerline-active1 ((,c (:foreground ,yellow))))
     `(powerline-active2 ((,c (:foreground ,comment))))
     `(powerline-inactive1 ((,c (:foreground ,comment))))
     `(powerline-inactive2 ((,c (:foreground ,comment))))

     ;; Prodigy
     `(prodigy-red-face ((,c (:foreground ,red))))
     `(prodigy-green-face ((,c (:foreground ,green))))
     `(prodigy-yellow-face ((,c (:foreground ,yellow))))

     ;; Jabber
     `(jabber-title-large ((,c (:foreground ,yellow))))
     `(jabber-title-medium ((,c (:foreground ,yellow))))
     `(jabber-title-small ((,c (:foreground ,yellow))))
     `(jabber-chat-prompt-local ((,c (:foreground ,frost))))
     `(jabber-chat-prompt-foreign ((,c(:foreground ,yellow))))
     `(jabber-roster-user-xa ((,c (:foreground ,yellow))))
     `(jabber-roster-user-online ((,c (:foreground ,green))))
     `(jabber-roster-user-offline ((,c (:foreground ,comment))))
     `(jabber-roster-user-away ((,c (:foreground ,frost))))
     `(jabber-rare-time-face ((,c (:foreground ,sbt-midnight))))

     ;; SQL*Plus
     `(sqlplus-table-head-face ((,c (:foreground ,yellow))))
     `(sqlplus-table-even-rows-face ((,c (:foreground ,white :background ,ada-midnight))))
     `(sqlplus-table-odd-rows-face ((,c (:foreground ,white))))

     ;; Rainbow-delimiters
     `(rainbow-delimiters-depth-1-face ((,c (:foreground ,green))))
     `(rainbow-delimiters-depth-2-face ((,c (:foreground ,red))))
     `(rainbow-delimiters-depth-3-face ((,c (:foreground ,frost))))
     `(rainbow-delimiters-depth-4-face ((,c (:foreground ,yellow))))
     `(rainbow-delimiters-depth-5-face ((,c (:foreground ,waddles))))
     `(rainbow-delimiters-depth-6-face ((,c (:foreground ,frost))))
     `(rainbow-delimiters-depth-7-face ((,c (:foreground ,green))))
     `(rainbow-delimiters-depth-8-face ((,c (:foreground ,red))))
     `(rainbow-delimiters-depth-9-face ((,c (:foreground ,frost))))
     `(rainbow-delimiters-depth-10-face ((,c (:foreground ,yellow))))

     ;; Company-mode
     `(company-tooltip ((,c (:foreground ,white :background ,ada-midnight))))
     `(company-tooltip-selection ((,c (:foreground ,white :background ,sbt-midnight))))
     `(company-scrollbar-fg ((,c (:background ,ada-midnight))))
     `(company-scrollbar-bg ((,c (:background ,sbt-midnight))))
     `(company-tooltip-common ((,c (:foreground ,yellow))))
     `(company-preview ((,c (:background ,sbt-midnight))))
     `(company-preview-common ((,c (:background ,sbt-midnight :foreground ,red))))
     `(company-mouse ((,c (:background ,ada-midnight))))

     ;; Elfeed
     `(elfeed-search-feed-face ((,c (:foreground ,comment))))
     `(elfeed-search-tag-face ((,c (:foreground ,green))))
     `(elfeed-search-unread-title-face ((,c (:foreground ,white))))
     `(elfeed-search-date-face ((,c (:foreground ,diredcl))))

     ;; Flycheck
     `(flycheck-warning ((,c (:underline (:color ,red :style wave)))))

     ;; js2-mode
     `(js2-function-param ((,c (:foreground ,yellow))))

     ;; message
     `(message-header-name ((,c (:foreground ,comment))))
     `(message-header-subject ((,c (:foreground ,white))))
     `(message-header-to ((,c (:foreground ,white))))
     `(message-header-other ((,c (:foreground ,white))))
     `(shr-link ((,c (:foreground ,frost :underline t))))

     ;; erc
     `(erc-timestamp-face ((,c (:foreground ,red))))
     `(erc-prompt-face ((,c (:foreground ,green))))
     `(erc-nick-default-face ((,c (:foreground ,frost))))
     `(erc-notice-face ((,c (:foreground ,waddles))))
     `(erc-button ((,c (:foreground ,frost))))
     `(erc-current-nick-face ((,c (:foreground ,red))))

     ;; eshell
     `(eshell-prompt ((,c (:foreground ,red))))
     `(eshell-ls-executable ((,c (:foreground ,green))))
     `(eshell-ls-directory ((,c (:foreground ,blue))))
     `(eshell-ls-symlink ((,c (:foreground ,waddles))))
     `(eshell-ls-readonly ((,c (:foreground ,invisible))))
     `(eshell-ls-missing ((,c (:foreground ,red))))
     `(eshell-ls-special ((,c (:foreground ,comment :underline t))))

     `(epe-remote-face ((,c (:foreground ,comment))))
     `(epe-dir-face ((,c (:foreground ,blue))))
     `(epe-symbol-face ((,c (:foreground ,yellow))))

     ;; dired
     `(dired-directory ((,c (:foreground ,blue))))
     `(dired-git-face ((,c (:foreground ,red))))
     `(dired-ignored ((,c (:foreground ,invisible))))
     `(dired-filetype-omit ((,c (:foreground ,invisible))))
     `(dired-filetype-common ((,c (:foreground ,yellow))))
     `(dired-filetype-execute ((,c (:foreground ,green))))
     `(dired-filetype-source ((,c (:foreground ,orange))))
     `(dired-filetype-plain ((,c (:foreground ,comment))))
     `(dired-filetype-link ((,c (:foreground ,blue :underline t))))
     `(dired-flagged ((,c (:foreground ,red :underline t))))
     `(dired-marked ((,c (:foreground ,yellow :underline t))))
     `(dired-subtree-depth-1-face ((,c (:background ,black))))
     `(dired-subtree-depth-2-face ((,c (:background ,black))))
     `(dired-subtree-depth-3-face ((,c (:background ,black))))
     `(dired-subtree-depth-4-face ((,c (:background ,black))))
     `(dired-subtree-depth-5-face ((,c (:background ,black))))
     `(dired-subtree-depth-6-face ((,c (:background ,black))))
     `(dired-subtree-depth-7-face ((,c (:background ,black))))
     `(dired-subtree-depth-8-face ((,c (:background ,black))))
     `(dired-subtree-depth-9-face ((,c (:background ,black))))

     ;; dired-rainbow
     `(dired-rainbow-executable-unix-face ((,c (:foreground ,green))))

   ;;; dired+
     `(diredp-date-time ((,c (:foreground ,diredcl))))
     `(diredp-dir-heading ((,c (:foreground ,red))))
     `(diredp-dir-name ((,c (:foreground ,blue))))
     `(diredp-file-name ((,c (:foreground ,white))))
     `(diredp-file-suffix ((,c (:foreground ,invisible))))
     `(diredp-ignored-file-name ((,c (:foreground ,invisible))))
     `(diredp-compressed-file-suffix ((,c (:foreground ,orange))))
     `(diredp-rainbow-media-face ((,c (:foreground ,yellow))))
     `(diredp-symlink ((,c (:foreground ,yellow))))
     `(diredp-number ((,c (:foreground ,yellow))))

     `(diredp-dir-priv ((,c (:foreground ,blue))))
     `(diredp-read-priv ((,c (:foreground ,red))))
     `(diredp-write-priv ((,c (:foreground ,yellow))))
     `(diredp-exec-priv ((,c (:foreground ,green))))
     `(diredp-no-priv ((,c (:foreground ,white))))
     `(diredp-rare-priv ((,c (:foreground ,waddles))))
     `(diredp-flag-mark ((,c (:foreground ,black))))
     `(diredp-flag-mark-line ((,c (:foreground ,black :background ,yellow))))
     `(diredp-mode-line-marked ((,c (:foreground ,yellow))))
     `(diredp-deletion ((,c (:foreground ,black :background ,red))))
     `(diredp-deletion-file-name ((,c (:foreground ,black :background ,red))))
     `(diredp-mode-line-flagged ((,c (:foreground ,red))))

     ;; ido
     `(minibuffer-prompt ((,c (:foreground ,diredcl))))
     `(ido-first-match ((,c (:bold t :foreground ,white))))
     `(ido-only-match ((,c (:bold t :foreground ,white))))
     `(ido-subdir ((,c (:foreground ,frost))))
     `(ido-virtual ((,c (:foreground ,comment))))
     `(ido-vertical-match-face ((,c (:underline ,diredcl))))

     ;; ivy
     `(ivy-current-match ((,c (:foreground "#FFCA00"))))
     `(ivy-prompt-match ((,c (:bold t :foreground ,white))))
     `(ivy-confirm-face ((,c (:underline ,diredcl))))

     `(ivy-virtual ((,c (:foreground ,comment))))
     `(ivy-remote ((,c (:foreground ,white :slant italic))))
     `(ivy-cursor ((,c (:foreground ,comment))))
     `(ivy-action ((,c (:foreground ,yellow))))
     `(ivy-highlight-face ((,c (:foreground ,orange))))
     `(ivy-minibuffer-match-face-1 ((,c (:foreground ,white))))
     `(ivy-minibuffer-match-face-2 ((,c (:underline ,red))))
     `(ivy-minibuffer-match-face-3 ((,c (:underline ,orange))))
     `(ivy-minibuffer-match-face-4 ((,c (:underline ,yellow))))
     `(swiper-match-face-1 ((,c (:foreground ,white))))
     `(swiper-match-face-2 ((,c (:underline ,red))))
     `(swiper-match-face-3 ((,c (:underline ,orange))))
     `(swiper-match-face-4 ((,c (:underline ,yellow))))

     ;; vertical-border
     `(vertical-border ((,c (:foreground "#223959"))))

     ;; yas
     `(yas-field-highlight-face ((,c (:background ,ada-midnight))))

     ;; hackernews
     `(hackernews-score-face ((,c (:foreground ,white))))
     `(hackernews-link-face ((,c (:foreground ,green))))
     `(hackernews-comment-count-face ((,c (:foreground ,red)))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'danneskjold)
;;; danneskjold-theme.el ends here

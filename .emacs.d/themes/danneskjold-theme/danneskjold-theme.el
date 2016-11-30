;; danneskjold-theme.el --- beautiful high-contrast theme

;; Copyright (c) 2016 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; URL: https://github.com/rails-to-cosmos/
;; Package-Version: 20160311.458

;;; Commentary:

;;; Code:

(deftheme danneskjold
  "Amazing. Beautiful. Contrast.")

(let ((bg "#000000")
      (fg "#ffffff")

      ;; Shades of grey
      (black "#000000")
      (frost "#D0E1F9" )
      (comment "#8593AE")
      (anthracite "#3a3f4b")
      (slightly-brighter-than-midnight "#282c34")
      (as-dark-as-midnight "#21252b")
      (white "#FFFFFF")

      ;; Shades of yellow
      (sunrise "#FFDB45")
      (saffron "#F9BA32")
      (orange-saturated "#FF9009")

      ;; Shades of green
      (spring-flower "#B3DE81")
      (summer-flower "#013220")

      ;; Shades of blue
      (twitter "#4CB5F5")
      (diredcl "#74749A9AF7F7")
      (dvi "DarkViolet")

      ;; Shades of red
      (waddles "#FF87BA")
      (krayola "#E38B75")
      (santa "#F34A4A")
      (red-forest "#8b0000"))

  (custom-theme-set-faces
   'danneskjold
   `(default ((t (:foreground ,fg :background ,bg))))
   `(fringe ((t (:background ,bg))))
   `(region ((t (:background ,anthracite))))
   `(button ((t (:foreground ,frost :underline t :weight normal))))
   `(link ((t (:foreground ,frost :underline t))))
   `(menu ((t (:foreground ,fg :background ,as-dark-as-midnight))))

   `(show-paren-match ((t (:background ,twitter :foreground ,white))))

   `(font-lock-string-face ((t (:foreground ,spring-flower))))
   `(font-lock-doc-face ((t (:foreground ,spring-flower))))
   `(font-lock-builtin-face ((t (:foreground ,twitter))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,frost))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,anthracite))))
   `(font-lock-function-name-face ((t (:foreground ,saffron))))
   `(font-lock-type-face ((t (:foreground ,orange-saturated))))
   `(font-lock-constant-face ((t (:foreground ,sunrise))))

   `(mmm-default-submode-face ((t (:background ,as-dark-as-midnight))))

   `(header-line ((t (:background ,bg
                      :foreground ,comment
                      :underline ,comment
                      :weight normal))))

   ;; Mode-line
   `(mode-line ((t (:background ,as-dark-as-midnight
                                :foreground ,fg
                                :box ,(list
                                       :line-width 4
                                       :color as-dark-as-midnight)))))
   `(mode-line-inactive ((t (:background ,slightly-brighter-than-midnight
                                         :foreground ,fg
                                         :box ,(list
                                                :line-width 4
                                                :color slightly-brighter-than-midnight)))))
   `(mode-line-buffer-id ((t (:foreground ,fg))))

   `(font-lock-warning-face ((t (:foreground ,santa))))
   `(compilation-warning ((t (:foreground ,santa))))
   `(compilation-info ((t (:foreground ,spring-flower))))
   `(highlight ((t (:background ,as-dark-as-midnight :foreground ,frost))))

   ;; Linum
   `(linum ((t (:foreground ,anthracite))))

   `(widget-field ((t (:foreground ,fg :background ,slightly-brighter-than-midnight))))
   `(widget-button ((t (:foreground ,saffron))))

   ;; Highlight quoted mode-line
   `(highlight-quoted-symbol ((t (:foreground ,waddles))))

   ;; Hl-line and hlinum-activate
   `(linum-highlight-face ((t (:foreground ,anthracite :background ,as-dark-as-midnight :weight bold))))
   `(hl-line ((t (:background ,as-dark-as-midnight))))

   ;; Diff
   `(diff-header ((t (:foreground ,sunrise))))
   `(diff-file-header ((t (:foreground ,sunrise))))
   `(diff-indicator-removed ((t (:foreground ,bg))))
   `(diff-removed ((t (:foreground ,santa))))
   `(diff-added ((t (:foreground ,spring-flower))))
   `(diff-indicator-added ((t (:foreground ,bg))))
   `(diff-refine-removed ((t (:foreground ,red-forest))))
   `(diff-refine-added ((t (:foreground ,summer-flower))))

   `(diff-context ((t (:foreground ,comment))))

   ;; Magit
   `(magit-diff-added ((t (:foreground ,spring-flower))))
   `(magit-diff-added-highlight ((t (:foreground ,spring-flower))))
   `(magit-diff-removed ((t (:foreground ,santa))))
   `(magit-diff-removed-highlight ((t (:foreground ,santa))))
   `(magit-diff-context ((t (:background ,bg :foreground ,frost))))
   `(magit-diff-context-highlight ((t (:background ,bg :foreground ,frost))))
   `(magit-section-highlight ((t (:background ,bg))))
   `(magit-section-heading ((t (:foreground ,sunrise :inherit nil))))
   `(magit-diff-hunk-heading ((t (:foreground ,twitter :background ,slightly-brighter-than-midnight))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,twitter :background ,slightly-brighter-than-midnight))))
   `(magit-diff-lines-heading ((t (:foreground ,frost :background ,slightly-brighter-than-midnight))))
   `(magit-blame-heading ((t (:foreground ,twitter :background ,slightly-brighter-than-midnight))))

   ;; Org
   `(org-todo ((t (:foreground ,santa))))
   `(org-done ((t (:foreground ,spring-flower))))
   `(org-hide ((t (:foreground ,anthracite))))
   `(org-link ((t (:foreground ,frost :underline t))))
   `(org-date ((t (:foreground ,comment))))
   `(org-block-begin-line ((t (:foreground ,sunrise))))
   `(org-block-background ((t (:background ,bg))))
   `(org-block-end-line ((t (:foreground ,sunrise))))
   `(org-tag ((t (:foreground ,anthracite))))
   `(org-special-keyword ((t (:foreground ,sunrise))))
   `(org-priority ((t (:foreground ,comment))))
   `(org-level-1 ((t (:foreground ,twitter))))
   `(org-level-2 ((t (:foreground ,frost))))
   `(org-ellipsis ((t (:foreground ,frost))))

   ;; Prodigy
   `(prodigy-red-face ((t (:foreground ,santa))))
   `(prodigy-green-face ((t (:foreground ,spring-flower))))
   `(prodigy-yellow-face ((t (:foreground ,sunrise))))

   ;; Jabber
   `(jabber-title-large ((t (:foreground ,sunrise))))
   `(jabber-title-medium ((t (:foreground ,sunrise))))
   `(jabber-title-small ((t (:foreground ,sunrise))))
   `(jabber-chat-prompt-local ((t (:foreground ,frost))))
   `(jabber-chat-prompt-foreign ((t(:foreground ,sunrise))))
   `(jabber-roster-user-xa ((t (:foreground ,saffron))))
   `(jabber-roster-user-online ((t (:foreground ,spring-flower))))
   `(jabber-roster-user-offline ((t (:foreground ,comment))))
   `(jabber-roster-user-away ((t (:foreground ,frost))))
   `(jabber-rare-time-face ((t (:foreground ,slightly-brighter-than-midnight))))

   ;; SQL*Plus
   `(sqlplus-table-head-face ((t (:foreground ,sunrise))))
   `(sqlplus-table-even-rows-face ((t (:foreground ,fg :background ,as-dark-as-midnight))))
   `(sqlplus-table-odd-rows-face ((t (:foreground ,fg))))

   ;; Rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,spring-flower))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,santa))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,frost))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,saffron))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,waddles))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,frost))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,spring-flower))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,santa))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,frost))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,saffron))))

   ;; Company-mode
   ;; `(company-tooltip ((t (:foreground ,fg :background ,as-dark-as-midnight))))
   ;; `(company-tooltip-selection ((t (:foreground ,fg :background ,slightly-brighter-than-midnight))))
   ;; `(company-scrollbar-fg ((t (:background ,as-dark-as-midnight))))
   ;; `(company-scrollbar-bg ((t (:background ,slightly-brighter-than-midnight))))
   ;; `(company-tooltip-common ((t (:foreground ,saffron))))
   ;; `(company-preview ((t (:background ,slightly-brighter-than-midnight))))
   ;; `(company-preview-common ((t (:background ,slightly-brighter-than-midnight :foreground ,santa))))
   ;; `(company-mouse ((t (:background ,as-dark-as-midnight))))

   ;; Flycheck
   `(flycheck-warning ((t (:foreground ,santa :underline t))))

   ;; js2-mode
   `(js2-function-param ((t (:foreground ,saffron))))

   ;; erc
   `(erc-timestamp-face ((t (:foreground ,santa))))
   `(erc-prompt-face ((t (:foreground ,spring-flower))))
   `(erc-nick-default-face ((t (:foreground ,frost))))
   `(erc-notice-face ((t (:foreground ,waddles))))
   `(erc-button ((t (:foreground ,frost))))
   `(erc-current-nick-face ((t (:foreground ,santa))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,santa))))
   `(eshell-ls-executable ((t (:foreground ,spring-flower))))
   `(eshell-ls-directory ((t (:foreground ,twitter))))
   `(eshell-ls-symlink ((t (:foreground ,waddles))))
   `(eshell-ls-readonly ((t (:foreground ,anthracite))))
   `(eshell-ls-missing ((t (:foreground ,santa))))
   `(eshell-ls-special ((t (:foreground ,comment :underline t))))

   ;; dired
   `(dired-directory ((t (:foreground ,twitter))))
   `(dired-git-face ((t (:foreground ,santa))))
   `(dired-ignored ((t (:foreground ,anthracite))))
   `(dired-filetype-omit ((t (:foreground ,anthracite))))
   `(dired-filetype-common ((t (:foreground ,saffron))))
   `(dired-filetype-execute ((t (:foreground ,spring-flower))))
   `(dired-filetype-source ((t (:foreground ,orange-saturated))))
   `(dired-filetype-plain ((t (:foreground ,comment))))
   `(dired-filetype-link ((t (:foreground ,twitter :underline t))))
   `(dired-flagged ((t (:foreground ,santa :underline t))))
   `(dired-marked ((t (:foreground ,saffron :underline t))))
   `(dired-subtree-depth-1-face ((t (:background ,"#21252b"))))
   `(dired-subtree-depth-2-face ((t (:background ,"#282c34"))))
   `(dired-subtree-depth-3-face ((t (:background ,bg))))

   ;;; dired+
   `(diredp-dir-heading ((t (:foreground ,santa))))
   `(diredp-dir-name ((t (:foreground ,twitter))))
   `(diredp-file-name ((t (:foreground ,fg))))
   `(diredp-file-suffix ((t (:foreground ,frost))))
   `(diredp-ignored-file-name ((t (:foreground ,comment))))
   `(diredp-symlink ((t (:foreground ,waddles))))
   `(diredp-number ((t (:foreground ,saffron))))

   `(diredp-dir-priv ((t (:foreground ,twitter))))
   `(diredp-read-priv ((t (:foreground ,santa))))
   `(diredp-write-priv ((t (:foreground ,sunrise))))
   `(diredp-exec-priv ((t (:foreground ,spring-flower))))
   `(diredp-no-priv ((t (:foreground ,fg))))
   `(diredp-rare-priv ((t (:foreground ,waddles))))
   `(diredp-flag-mark ((t (:foreground ,bg))))
   `(diredp-flag-mark-line ((t (:foreground ,bg :background ,sunrise))))
   `(diredp-mode-line-marked ((t (:foreground ,sunrise))))
   `(diredp-deletion ((t (:foreground ,bg :background ,santa))))
   `(diredp-deletion-file-name ((t (:foreground ,bg :background ,santa))))
   `(diredp-mode-line-flagged ((t (:foreground ,santa))))

   ;; ido
   `(minibuffer-prompt ((t (:foreground ,comment))))
   `(ido-first-match ((t (:foreground ,frost))))
   `(ido-only-match ((t (:foreground ,frost))))
   `(ido-subdir ((t (:foreground ,frost))))
   `(ido-vertical-match-face ((t (:foreground ,twitter))))

   ;; vertical-border
   `(vertical-border ((t (:foreground "#282a2e"))))

   ;; hackernews
   `(hackernews-score-face ((t (:foreground ,fg))))
   `(hackernews-link-face ((t (:foreground ,spring-flower))))
   `(hackernews-comment-count-face ((t (:foreground ,santa))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'danneskjold)
;;; danneskjold-theme.el ends here

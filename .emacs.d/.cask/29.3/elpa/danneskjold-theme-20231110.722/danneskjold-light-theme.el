(deftheme danneskjold-light "Default theme with minor improvements.")

(let ((class '((class color) (min-colors 89)))
      (zoom-in-factor 1.5))
  (custom-theme-set-faces
   'danneskjold-light
   `(org-ellipsis ((t (:underline nil))))

   `(ivy-current-match ((t (:weight bold :background "#ffd600"))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground "black"))))
   ;; `(ivy-minibuffer-match-face-2 ((t (:height ,zoom-in-factor))))
   ;; `(ivy-minibuffer-match-face-3 ((t (:height ,zoom-in-factor))))
   ;; `(ivy-minibuffer-match-face-4 ((t (:height ,zoom-in-factor))))

   ;; company
   `(company-tooltip-selection ((t (:background "#ffd600"))))

   `(swiper-match-face-1 ((t (:foreground "black"))))
   ;; `(swiper-match-face-2 ((t (:height ,zoom-in-factor))))
   ;; `(swiper-match-face-3 ((t (:height ,zoom-in-factor))))
   ;; `(swiper-match-face-4 ((t (:height ,zoom-in-factor))))

   `(tooltip ((t (:foreground "black" :background "white"))))

   `(line-number ((t (:foreground "grey"))))
   `(line-number-current-line ((t (:foreground "#749AF7"))))

   `(mode-line-buffer-read-only-face ((t (:foreground "brown4"))))

   `(font-lock-comment-face ((t (:foreground "#7f8c8d"))))

   `(highlight ((t (:background "honeydew" :extend t))))

   ;; dired+
   `(diredp-compressed-file-suffix ((t (:foreground "#f39c12"))))
   `(diredp-date-time ((t (:foreground "goldenrod1"))))
   `(diredp-deletion ((t (:foreground "#e74c3c" :weight bold :slant italic))))
   `(diredp-deletion-file-name ((t (:foreground "#e74c3c" :underline t))))
   `(diredp-dir-heading ((t (:foreground "#e74c3c"))))
   `(diredp-dir-priv ((t (:foreground "#9b59b6" :background nil))))
   `(diredp-exec-priv ((t (:foreground "#2ecc71" :background nil))))
   `(diredp-executable-tag ((t (:foreground "#2ecc71" :background nil))))
   `(diredp-file-name ((t (:foreground "black"))))
   `(diredp-file-suffix ((t (:foreground "coral"))))
   `(diredp-flag-mark ((t (:foreground "#e74c3c" :weight bold))))
   `(diredp-flag-mark-line ((t (:inherit highlight))))
   `(diredp-ignored-file-name ((t (:foreground "#95a5a6"))))
   `(diredp-link-priv ((t (:background nil :foreground "#e67e22"))))
   `(diredp-mode-line-flagged ((t (:foreground "#f39c12"))))
   `(diredp-mode-line-marked ((t (:foreground "#9b59b6"))))
   `(diredp-no-priv ((t (:foreground "#95a5a6" :background nil))))
   `(diredp-number ((t (:foreground "#f39c12"))))
   `(diredp-other-priv ((t (:background nil :foreground "#f39c12"))))
   `(diredp-rare-priv ((t (:foreground "#c0392b" :background nil))))
   `(diredp-read-priv ((t (:foreground "#2980b9" :background nil))))
   `(diredp-symlink ((t (:foreground "#e67e22"))))
   `(diredp-write-priv ((t (:foreground "#9b59b6" :background nil))))
   `(diredp-date-time ((t (:foreground "#749AF7"))))
   `(diredp-dir-name ((t (:foreground "blue"))))
   `(diredp-ignored-file-name ((t ())))
   `(diredp-compressed-file-suffix ((t (:foreground "#ffa500"))))
   `(diredp-rainbow-media-face ((t (:foreground "#ffcc00"))))
   `(diredp-symlink ((t (:foreground "#ffcc00"))))
   `(diredp-number ((t (:foreground "#ffcc00"))))

   ;; hl-line
   `(hl-line ((t (:background "gray98"))))

   ;; org
   `(org-meta-line ((t (:foreground "#9b59b6"))))

   `(org-block-begin-line ((t (:foreground "black" :weight bold :background "white"))))
   `(org-block-end-line ((t (:foreground "black" :weight bold :background "white"))))
   '(org-block ((t (:extend t :background "ghost white"))))

   `(org-verbatim ((t (:background "gray96" :box (:line-width 2 :color "gray96")))))

   '(org-upcoming-deadline ((t (:foreground "purple3"))))
   `(org-agenda-current-time ((t (:foreground "black" :weight bold))))))

(provide-theme 'danneskjold-light)
;;; danneskjold-light-theme.el ends here

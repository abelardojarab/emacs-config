;;; contrib-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (semantic-default-matlab-setup) "semantic-matlab"
;;;;;;  "semantic-matlab.el" (17213 40186))
;;; Generated autoloads from semantic-matlab.el

(autoload (quote semantic-default-matlab-setup) "semantic-matlab" "\
Set up a buffer for parsing of MATLAB files." nil nil)

(add-hook (quote matlab-mode-hook) (quote semantic-default-matlab-setup))

;;;***

;;;### (autoloads (semantic-tag-folding-mode global-semantic-tag-folding-mode
;;;;;;  global-semantic-tag-folding-mode) "semantic-tag-folding"
;;;;;;  "semantic-tag-folding.el" (17213 40189))
;;; Generated autoloads from semantic-tag-folding.el

(defvar global-semantic-tag-folding-mode nil "\
*If non-nil enable global use of variable `semantic-tag-folding-mode'.
With this mode enabled, a new folding decoration mode is added.
Clicking on a + or - in the fringe will fold that tag.")

(custom-add-to-group (quote semantic) (quote global-semantic-tag-folding-mode) (quote custom-variable))

(custom-add-load (quote global-semantic-tag-folding-mode) (quote semantic-util-modes))

(autoload (quote global-semantic-tag-folding-mode) "semantic-tag-folding" "\
Toggle global use of option `semantic-tag-folding-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle." t nil)

(autoload (quote semantic-tag-folding-mode) "semantic-tag-folding" "\
Minor mode mark semantic tags for folding.
This mode will display +/- icons in the fringe.  Clicking on them
will fold the current tag.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled." t nil)

;;;***


(autoload (quote wisent-csharp-default-setup) "wisent-csharp" nil nil nil)

(add-hook (quote csharp-mode-hook) (function wisent-csharp-default-setup))

;;;***

(autoload (quote wisent-javascript-setup-parser) "wisent-javascript" "\
Setup buffer for parse." nil nil)

(add-hook (quote javascript-mode-hook) (quote wisent-javascript-setup-parser))

(add-hook (quote ecmascript-mode-hook) (quote wisent-javascript-setup-parser))

;;;***

(add-hook (quote lisp-mode-hook) (quote semantic-default-elisp-setup))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; contrib-loaddefs.el ends here

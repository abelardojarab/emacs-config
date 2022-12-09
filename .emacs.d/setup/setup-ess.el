;;; setup-ess.el ---

;; Copyright (C) 2014-2022  Abelardo Jara-Berrocal

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

(use-package ess-site
  :load-path (lambda () (expand-file-name "ESS/lisp" user-emacs-directory))
  :mode ("\\.[rR]$" . R-mode)
  :commands (R-mode
             ess-eval-function
             ess-eval-line
             ess-eval-buffer
             ess-switch-to-ESS)
  :config (progn
            (setq-default ess-dialect "R")
            (setq-default inferior-R-args " --no-restore-history --no-save ")
            (setq ess-ask-for-ess-directory nil) ;; start R on default folder
            (setq ess-local-process-name "R")

            ;; show function arguments in ESS buffers
            (use-package ess-eldoc)

            ;; also show in iESS buffers
            (add-hook 'inferior-ess-mode-hook #'ess-use-eldoc)

            ;; http://www.kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
            (add-hook 'ess-mode-hook
                      #'(lambda ()
                         (setq comint-scroll-to-bottom-on-input  t
                               comint-scroll-to-bottom-on-output t
                               comint-move-point-for-output      t)

                         (flyspell-prog-mode)
                         (run-hooks 'prog-mode-hook)))

            ;; R syntax highlight
            ;; http://permalink.gmane.org/gmane.emacs.ess.general/8419
            (setq ess-R-font-lock-keywords
                                          '((ess-R-fl-keyword:modifiers   . t)
                                            (ess-R-fl-keyword:fun-defs    . t)
                                            (ess-R-fl-keyword:keywords    . t)
                                            (ess-R-fl-keyword:assign-ops  . t)
                                            (ess-R-fl-keyword:constants   . t)
                                            (ess-fl-keyword:fun-calls     . t)
                                            (ess-fl-keyword:numbers       . t)
                                            (ess-fl-keyword:operators     . t)
                                            (ess-fl-keyword:delimiters    . t)
                                            (ess-fl-keyword:=             . t)
                                            (ess-R-fl-keyword:F&T         . t)
                                            (ess-R-fl-keyword:%op%        . t)))

            ;; Console font lock highlight
            (setq inferior-R-font-lock-keywords
                                          '((ess-S-fl-keyword:prompt      . t)
                                            (ess-R-fl-keyword:messages    . t)
                                            (ess-R-fl-keyword:modifiers   . t)
                                            (ess-R-fl-keyword:fun-defs    . t)
                                            (ess-R-fl-keyword:keywords    . t)
                                            (ess-R-fl-keyword:assign-ops  . t)
                                            (ess-R-fl-keyword:constants   . t)
                                            (ess-fl-keyword:matrix-labels . t)
                                            (ess-fl-keyword:fun-calls     . t)
                                            (ess-fl-keyword:numbers       . t)
                                            (ess-fl-keyword:operators     . t)
                                            (ess-fl-keyword:delimiters    . t)
                                            (ess-fl-keyword:=             . t)
                                            (ess-R-fl-keyword:F&T         . t)
                                            (ess-R-fl-keyword:%op%        . t)))

            ;; Enable auto-complete
            (when (featurep 'auto-complete)
              (add-hook 'R-mode-hook  (lambda ()
                                        (auto-complete-mode)
                                        (setq ess-use-auto-complete t))))

            ;; config
            (setq ess-indent-level                 2
                  ess-arg-function-offset-new-line (list ess-indent-level)
                  ess-fancy-comments               nil
                  ess-loaded-p                     t
                  ess-ask-for-ess-directory        nil
                  ess-eldoc-show-on-symbol         t)

            ;; http://emacs.readthedocs.io/en/latest/ess__emacs_speaks_statistics.html
            (setq my/ess--RProfile-string "
#### change this file name to .Rprofile and place to ~/userName so when R starts, the following command will be processed automatically

## http://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile
options(\"width\"=160)                # wide display with multiple monitors
options(\"digits.secs\"=3)            # show sub-second time stamps
options(\"repos\" = c(CRAN = \"http://www.stats.bris.ac.uk/R/\")) # hard code the UK repo for CRAN
options(\"max.print\" = 200)
## from the AER book by Zeileis and Kleiber
options(prompt=\"R> \", digits=4, show.signif.stars=FALSE)

.Libs <- function(){
    library(data.table)
    library(ggplot2)
    library(gridExtra)
##    library(sp)
##    library(geosphere)
##    library(rgeos)
##    library(sp)
##    library(dragonfly)
}

.libPaths(\"~/R_libs\")
## improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                 decreasing=FALSE, head=FALSE, n=5)
    {
        napply <- function(names, fn) sapply(names, function(x)
            fn(get(x, pos = pos)))
        names <- ls(pos = pos, pattern = pattern)
        obj.class <- napply(names, function(x) as.character(class(x))[1])
        obj.mode <- napply(names, mode)
        obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
        obj.prettysize <- napply(names, function(x) {
                                    capture.output(print(object.size(x), units = \"auto\")) })
        obj.size <- napply(names, object.size)
        obj.dim <- t(napply(names, function(x)
            as.numeric(dim(x))[1:2]))
        vec <- is.na(obj.dim)[, 1] & (obj.type != \"function\")
        obj.dim[vec, 1] <- napply(names, length)[vec]
        out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
        names(out) <- c(\"Type\", \"Size\", \"PrettySize\", \"Rows\", \"Columns\")
        if (!missing(order.by))
            out <- out[order(out[[order.by]], decreasing=decreasing), ]
        if (head)
            out <- head(out, n)
        out
    }
## shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by=\"Size\", decreasing=TRUE, head=TRUE, n=n)
}")

            (add-hook 'ess-post-run-hook
                      (lambda ()
                        (goto-char (point-max))
                        (insert my/ess--RProfile-string)
                        ;; execute the R scripts
                        (inferior-ess-send-input)
                        ;; clean up
                        (search-backward "Type 'q()' to quit R.")
                        (next-line)
                        (delete-region (point) (point-max))
                        (inferior-ess-send-input)))))

(provide 'setup-ess)

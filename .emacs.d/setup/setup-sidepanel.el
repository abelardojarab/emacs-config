;;; setup-sidepanel.el --- One key, a context-appropriate right-hand panel  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
;; Keywords: tools, convenience

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

;; `my/side-panel' splits the window and fills the right half with whatever is
;; worth looking at for the buffer you are in:
;;
;;   markdown   rendered preview, through the pandoc command already configured
;;   protobuf   structural checks -- duplicate field numbers, reserved ranges,
;;              missing syntax/package, naming, formatting drift, unused imports
;;   C / C++    symbols defined here that nothing references, plus diagnostics
;;   anything   the flycheck/flymake diagnostics for the file
;;
;; The C++ report is the expensive one: each reference query is a round trip to
;; CiderLSP, measured at roughly a second, so it runs against a bounded number
;; of symbols, reports progress, and can be interrupted with C-g without
;; leaving anything behind.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defgroup my/side-panel nil
  "Context-sensitive right-hand panel."
  :group 'convenience
  :prefix "my/side-panel-")

(defcustom my/side-panel-width 0.5
  "Fraction of the frame the panel takes."
  :type 'number
  :group 'my/side-panel)

(defcustom my/side-panel-max-symbols 40
  "Most symbols to check in the unused-symbol report.

Each check is a references round trip to the language server -- about a
second -- so an unbounded scan of a large file would look like a hang."
  :type 'integer
  :group 'my/side-panel)

(defcustom my/side-panel-slot 1
  "Side-window slot the panel occupies on the right.

Not 0: `setup-ide-layout' parks the version-control buffer at slot 0 on
the same side, and two buffers sharing a slot means whichever is
displayed second silently replaces the first."
  :type 'integer
  :group 'my/side-panel)

(defun my/side-panel--show (buffer)
  "Display BUFFER on the right, leaving point in the source window."
  (let ((win (display-buffer
              buffer
              `((display-buffer-in-side-window)
                (side . right)
                (window-width . ,my/side-panel-width)
                (slot . ,my/side-panel-slot)))))
    (when win (set-window-dedicated-p win t))
    win))

(defun my/side-panel--buffer (name)
  "A fresh panel buffer called NAME, in `special-mode'."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (special-mode))
    buf))

(defun my/side-panel--insert-heading (text)
  (insert (propertize (concat text "\n") 'face 'bold))
  (insert (propertize (make-string (min 60 (length text)) ?-) 'face 'shadow) "\n"))

;;; Diagnostics ----------------------------------------------------------------

(defun my/side-panel--diagnostics (source-buffer)
  "Lines describing flycheck/flymake diagnostics in SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (cond
     ((and (bound-and-true-p flycheck-mode) (boundp 'flycheck-current-errors)
           flycheck-current-errors)
      (mapcar (lambda (e)
                (format "  %s:%s  %s  %s"
                        (or (flycheck-error-line e) "?")
                        (or (flycheck-error-column e) "?")
                        (or (flycheck-error-level e) "")
                        (or (flycheck-error-message e) "")))
              flycheck-current-errors))
     ((bound-and-true-p flymake-mode)
      (mapcar (lambda (d)
                (format "  %s  %s"
                        (line-number-at-pos (flymake-diagnostic-beg d))
                        (flymake-diagnostic-text d)))
              (flymake-diagnostics)))
     (t nil))))

(defun my/side-panel--insert-diagnostics (source-buffer)
  (my/side-panel--insert-heading "Diagnostics")
  (let ((lines (my/side-panel--diagnostics source-buffer)))
    (if lines
        (dolist (l lines) (insert l "\n"))
      (insert "  none\n")))
  (insert "\n"))

;;; Markdown -------------------------------------------------------------------

(defcustom my/markdown-preview-dir
  (expand-file-name "emacs-markdown-preview" temporary-file-directory)
  "Where rendered markdown previews are written.

Deliberately outside any repository.  markdown-mode exports the preview
to <name>.html *next to the source*, and jj snapshots new files in the
working copy automatically -- so previewing a .md inside a workspace
silently adds an .html to the current CL, ready to be uploaded to
Critique."
  :type 'directory
  :group 'my/side-panel)

(defun my/markdown-preview-filename ()
  "Preview path for this buffer, under `my/markdown-preview-dir'.

Overrides `markdown-live-preview-get-filename'.  The name is hashed from
the full path so two files called README.md cannot collide."
  (make-directory my/markdown-preview-dir t)
  (expand-file-name
   (concat (md5 (or (buffer-file-name) (buffer-name))) ".html")
   my/markdown-preview-dir))

(with-eval-after-load 'markdown-mode
  (advice-add 'markdown-live-preview-get-filename
              :override #'my/markdown-preview-filename))

;;; Scrolling the preview with the source ---------------------------------------

(defcustom my/markdown-preview-sync t
  "Scroll the rendered preview to match the source as you move through it."
  :type 'boolean
  :group 'my/side-panel)

(defcustom my/markdown-preview-sync-idle 0.3
  "Idle seconds before the preview is scrolled to match the source."
  :type 'number
  :group 'my/side-panel)

(defvar my/markdown--sync-timer nil)
(defvar-local my/markdown--last-fraction nil)

(defun my/markdown--sync-scroll ()
  "Scroll the preview window to the same relative position as the source.

Proportional rather than anchored: mapping a markdown line to a position
in rendered HTML properly would mean injecting anchors into the export
and querying eww for them, which is a lot of machinery for a preview.
Proportion tracks well enough on prose and costs nothing."
  (when (and my/markdown-preview-sync
             (derived-mode-p 'markdown-mode 'gfm-mode)
             (bound-and-true-p markdown-live-preview-mode)
             (buffer-live-p (bound-and-true-p markdown-live-preview-buffer)))
    (let* ((total (max 1 (- (point-max) (point-min))))
           (frac (/ (float (- (point) (point-min))) total))
           (win (get-buffer-window markdown-live-preview-buffer)))
      ;; Only act on real movement, so an idle cursor does not keep
      ;; re-scrolling the preview under you.
      (when (and win (or (null my/markdown--last-fraction)
                         (> (abs (- frac my/markdown--last-fraction)) 0.01)))
        (setq my/markdown--last-fraction frac)
        (with-selected-window win
          (let ((target (+ (point-min)
                           (floor (* frac (- (point-max) (point-min)))))))
            (goto-char (max (point-min) (min target (point-max))))
            (recenter 0)))))))

(define-minor-mode my/markdown-preview-sync-mode
  "Keep the markdown preview scrolled to match the source buffer."
  :lighter " ⇵md"
  (if my/markdown-preview-sync-mode
      (unless my/markdown--sync-timer
        (setq my/markdown--sync-timer
              (run-with-idle-timer my/markdown-preview-sync-idle t
                                   #'my/markdown--sync-scroll)))
    (setq my/markdown--last-fraction nil)))

(defun my/side-panel-markdown ()
  "Render the current markdown buffer beside itself, and keep it in step."
  (if (not (fboundp 'markdown-live-preview-mode))
      (user-error "markdown-live-preview-mode is not available")
    (unless (bound-and-true-p markdown-live-preview-mode)
      (markdown-live-preview-mode 1))
    (my/markdown-preview-sync-mode 1)
    (message "markdown preview: live, scroll-synced, exported to %s"
             my/markdown-preview-dir)))

;;; Protobuf -------------------------------------------------------------------

(defconst my/proto-reserved-range '(19000 . 19999)
  "Field numbers protobuf reserves for its own use.")

(defun my/proto--checks (source-buffer)
  "Structural findings for SOURCE-BUFFER, as a list of strings."
  (with-current-buffer source-buffer
    (let (;; Case matters here and `case-fold-search' defaults to t, which
          ;; makes [a-z] match "G" -- with it left on, every naming check
          ;; silently passed: (string-match-p "\\`[a-z]..." "GoodField")
          ;; returns 0, not nil.
          (case-fold-search nil)
          (text (buffer-substring-no-properties (point-min) (point-max)))
          findings)
      (save-excursion
        ;; Whole-file declarations.
        (unless (string-match-p "^[ \t]*syntax[ \t]*=" text)
          (push "  no `syntax = ...' declaration (defaults to proto2)" findings))
        (unless (string-match-p "^[ \t]*package[ \t]+" text)
          (push "  no `package' declaration" findings))

        ;; Per-message field numbers and names.
        (goto-char (point-min))
        (let (msg numbers names)
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (cond
               ((string-match "^[ \t]*\\(message\\|enum\\)[ \t]+\\([A-Za-z0-9_]+\\)" line)
                (setq msg (match-string 2 line) numbers nil names nil)
                (unless (string-match-p "\\`[A-Z][A-Za-z0-9]*\\'" msg)
                  (push (format "  %s `%s' is not CamelCase" (match-string 1 line) msg)
                        findings)))
               ((and msg (string-match
                          "^[ \t]*\\(?:optional\\|required\\|repeated\\)?[ \t]*[A-Za-z0-9_.<>, ]+[ \t]+\\([A-Za-z0-9_]+\\)[ \t]*=[ \t]*\\([0-9]+\\)"
                          line))
                (let ((fname (match-string 1 line))
                      (num (string-to-number (match-string 2 line)))
                      (ln (line-number-at-pos)))
                  (when (memq num numbers)
                    (push (format "  line %d: %s.%s reuses field number %d"
                                  ln msg fname num) findings))
                  (when (member fname names)
                    (push (format "  line %d: %s.%s is a duplicate field name"
                                  ln msg fname) findings))
                  (when (and (>= num (car my/proto-reserved-range))
                             (<= num (cdr my/proto-reserved-range)))
                    (push (format "  line %d: %s.%s uses reserved number %d (%d-%d)"
                                  ln msg fname num
                                  (car my/proto-reserved-range)
                                  (cdr my/proto-reserved-range))
                          findings))
                  (when (or (< num 1) (> num 536870911))
                    (push (format "  line %d: %s.%s field number %d is out of range"
                                  ln msg fname num) findings))
                  (unless (string-match-p "\\`[a-z][a-z0-9_]*\\'" fname)
                    (push (format "  line %d: field `%s' is not lower_snake_case"
                                  ln fname) findings))
                  (push num numbers)
                  (push fname names)))))
            (forward-line 1))))
      (nreverse findings))))

(defun my/proto--formatting (file)
  "Formatting drift and unused imports for FILE, via protofmt."
  (let ((bin "/google/bin/releases/protofmt-cli/protofmt")
        out)
    (when (and file (file-executable-p bin))
      (with-temp-buffer
        (if (zerop (call-process bin nil (list t nil) nil file))
            (unless (string= (buffer-string)
                             (with-temp-buffer (insert-file-contents file)
                                               (buffer-string)))
              (push "  protofmt would reformat this file" out))
          (push "  protofmt failed to parse this file" out)))
      (with-temp-buffer
        (when (zerop (call-process bin nil (list t nil) nil
                                   "--remove_unused_imports" file))
          (let ((stripped (buffer-string))
                (orig (with-temp-buffer (insert-file-contents file) (buffer-string))))
            (when (and (not (string= stripped orig))
                       (< (length stripped) (length orig)))
              (push "  there are unused imports (protofmt --remove_unused_imports)" out))))))
    (nreverse out)))

(defun my/side-panel-protobuf (source-buffer)
  "Structural checks for a .proto buffer."
  (let ((buf (my/side-panel--buffer "*proto checks*"))
        (file (buffer-file-name source-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (my/side-panel--insert-heading
         (format "proto: %s" (file-name-nondirectory (or file "buffer"))))
        (let ((findings (my/proto--checks source-buffer))
              (fmt (my/proto--formatting file)))
          (if (or findings fmt)
              (progn (dolist (f findings) (insert f "\n"))
                     (dolist (f fmt) (insert f "\n")))
            (insert "  structure looks fine\n")))
        (insert "\n")
        (my/side-panel--insert-diagnostics source-buffer)))
    (my/side-panel--show buf)))

;;; C / C++ unused symbols ------------------------------------------------------

(defconst my/cc--interesting-symbol-kinds
  ;; LSP SymbolKind: 5 Class, 6 Method, 9 Constructor, 11 Interface,
  ;; 12 Function, 23 Struct.
  '(5 6 9 11 12 23))

(defvar-local my/side-panel--symbol-lines nil
  "In a report buffer: alist of (SYMBOL . LINE) for follow-along scrolling.")
(defvar-local my/side-panel--source-buffer nil
  "In a report buffer: the buffer it describes.")
(defvar my/side-panel--hunk-cache nil
  "Per-run cache: relative path -> hunk ranges with their CL label.")
(defvar my/side-panel--cl-cache nil
  "Per-run cache: change id -> \"cl/NNN  description\" or nil.")

(defun my/cc--flatten-symbols (syms)
  "Flatten LSP document symbols SYMS to ((NAME START-LINE START-CHAR END-LINE) ...).

Three things this has to get right, all of which were wrong first time:
the answer is a *hierarchy* (a namespace with its functions as
`children'), LSP resolves references by *position* rather than by name,
and the full range is needed so the panel can tell which function point
is currently inside."
  (let (out)
    (dolist (s (append syms nil) (nreverse out))
      (when (hash-table-p s)
        (let* ((name (gethash "name" s))
               (kind (gethash "kind" s))
               (children (gethash "children" s))
               (sel (or (gethash "selectionRange" s) (gethash "range" s)
                        (let ((loc (gethash "location" s)))
                          (and (hash-table-p loc) (gethash "range" loc)))))
               (full (or (gethash "range" s) sel))
               (sp (and (hash-table-p sel) (gethash "start" sel)))
               (fe (and (hash-table-p full) (gethash "end" full)))
               (fs (and (hash-table-p full) (gethash "start" full))))
          (when (and (stringp name) (hash-table-p sp)
                     (or (null kind) (memq kind my/cc--interesting-symbol-kinds)))
            (push (list name (gethash "line" sp) (gethash "character" sp)
                        (if (hash-table-p fs) (gethash "line" fs) (gethash "line" sp))
                        (if (hash-table-p fe) (gethash "line" fe) (gethash "line" sp)))
                  out))
          (when children
            (setq out (append (nreverse (my/cc--flatten-symbols children)) out))))))))

(defun my/cc--lsp-symbols ()
  "Symbols with ranges for this buffer, or nil."
  (and (bound-and-true-p lsp-mode) (fboundp 'lsp-request)
       (ignore-errors
         (my/cc--flatten-symbols
          (lsp-request "textDocument/documentSymbol"
                       (list :textDocument (lsp--text-document-identifier)))))))

(defun my/cc--refs-at (line char)
  "Reference locations at LINE/CHAR (0-based), declaration excluded."
  (ignore-errors
    (append (lsp-request "textDocument/references"
                         (list :textDocument (lsp--text-document-identifier)
                               :position (list :line line :character char)
                               :context (list :includeDeclaration :json-false)))
            nil)))

;;; Attributing a reference to the CL that touched it ---------------------------

;; Built from the stack's own diffs, not from blame.
;;
;; `jj file annotate' is the exact answer and it is not affordable here:
;; measured on one 705-line file in this workspace it takes 21 seconds, warm or
;; cold, and annotating whatever file a reference happens to land in is worse
;; still -- third_party/absl/container/internal/raw_hash_set.h was still going
;; after four minutes with Emacs blocked on it.
;;
;; `jj diff --git -r CL' is 0.5s and its @@ headers carry line ranges.  Walking
;; the stack once gives file -> [(start end CL)], which answers "which CL
;; touched this line" for every reference without touching blame at all.
;;
;; The tradeoff is honest and worth stating: hunk line numbers are in the
;; post-image of the CL that made them, so a later CL editing the same file
;; shifts them.  Attribution is therefore "which CL touched this region",
;; accurate at file level and approximate at line level -- good enough to route
;; you to the right CL, not a substitute for blame.

(defcustom my/side-panel-max-refs-per-symbol 12
  "Most references to list for one symbol."
  :type 'integer
  :group 'my/side-panel)

(defun my/cc--stack-hunks (root)
  "Hash of relative path -> ((START END LABEL) ...) from every CL in the stack."
  (or my/side-panel--hunk-cache
      (let ((map (make-hash-table :test 'equal))
            (default-directory (file-name-as-directory root)))
        (dolist (cand (ignore-errors (jj--candidates)))
          (let* ((change (cdr cand))
                 (cl (jj--cl-of change))
                 (desc (string-trim (jj--read "log" "--no-graph" "-r" change
                                              "-T" "description.first_line()")))
                 (label (cond (cl (format "cl/%-10s %s" cl desc))
                              ((string-empty-p desc)
                               (substring change 0 (min 8 (length change))))
                              (t (format "%-13s %s"
                                         (substring change 0 (min 8 (length change)))
                                         desc))))
                 (diff (jj--read "diff" "--git" "-r" change))
                 file)
            (dolist (line (split-string diff "\n"))
              (cond
               ((string-match "^diff --git a/\\(.*?\\) b/" line)
                (setq file (match-string 1 line)))
               ((and file (string-match "^@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" line))
                (let* ((st (string-to-number (match-string 1 line)))
                       (len (if (match-string 2 line)
                                (string-to-number (match-string 2 line)) 1)))
                  (push (list st (+ st (max len 1)) label) (gethash file map))))))))
        (setq my/side-panel--hunk-cache map))))

(defun my/cc--attribute (uri line root)
  "Describe the reference at URI:LINE, naming the CL that touched that region."
  (let* ((path (if (string-prefix-p "file://" uri) (substring uri 7) uri))
         (inside (string-prefix-p (file-name-as-directory root) path))
         (rel (and inside (file-relative-name path root)))
         (shown (or rel (file-name-nondirectory path)))
         (lnum (1+ line)))
    (if (not inside)
        (format "    %-34s %s:%d" "(another workspace)" shown lnum)
      (let* ((hunks (gethash rel (my/cc--stack-hunks root)))
             (hit (seq-find (lambda (h) (and (<= (nth 0 h) lnum) (< lnum (nth 1 h))))
                            (or hunks '())))
             (label (cond (hit (nth 2 hit))
                          (hunks "(file in stack, line unchanged)")
                          (t "(not touched by this stack)"))))
        (format "    %-34s %s:%d" label shown lnum)))))

;;; The report ------------------------------------------------------------------

(defun my/cc--unused-report (source-buffer)
  "Per-symbol usage for SOURCE-BUFFER, each use attributed to a CL."
  (with-current-buffer source-buffer
    (let* ((root (or (jj--root) default-directory))
           (lsp-syms (my/cc--lsp-symbols))
           (capped (seq-take (or lsp-syms '()) my/side-panel-max-symbols))
           (reporter (make-progress-reporter "Attributing symbol usage" 0 (length capped)))
           (my/side-panel--hunk-cache nil)
           (my/side-panel--cl-cache (make-hash-table :test 'equal))
           (i 0) rows)
      (dolist (entry capped)
        (setq i (1+ i))
        (progress-reporter-update reporter i)
        (pcase-let ((`(,name ,sl ,sc ,rs ,re) entry))
          (let* ((all-refs (my/cc--refs-at sl sc))
                 (refs (seq-take all-refs my/side-panel-max-refs-per-symbol))
                 (lines (mapcar
                         (lambda (r)
                           (let* ((uri (gethash "uri" r))
                                  (rng (gethash "range" r))
                                  (st (and (hash-table-p rng) (gethash "start" rng))))
                             (my/cc--attribute uri (if st (gethash "line" st) 0) root)))
                         refs)))
            (push (list :name name :count (length all-refs) :lines lines
                        :range (cons rs re))
                  rows))))
      (progress-reporter-done reporter)
      (list :root root :checked (length capped)
            :total (length (or lsp-syms '()))
            :rows (nreverse rows)))))

(defun my/side-panel-cc (source-buffer)
  "Per-CL usage report for a C/C++ buffer, in the right-hand panel."
  (let ((buf (my/side-panel--buffer "*symbol usage*")))
    (my/side-panel--show buf)
    (let ((report (my/cc--unused-report source-buffer))
          (anchors nil))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (my/side-panel--insert-heading
           (format "usage: %s"
                   (file-name-nondirectory
                    (or (buffer-file-name source-buffer) "buffer"))))
          (insert (format "  %d of %d symbol(s); each use attributed to the CL that introduced it\n"
                          (plist-get report :checked) (plist-get report :total)))
          (when (> (plist-get report :total) (plist-get report :checked))
            (insert (propertize
                     (format "  capped at %d -- raise my/side-panel-max-symbols\n"
                             my/side-panel-max-symbols)
                     'face 'shadow)))
          (insert "\n")
          (dolist (row (plist-get report :rows))
            (push (cons (plist-get row :name) (line-number-at-pos)) anchors)
            (insert (propertize
                     (format "%s  --  %s\n" (plist-get row :name)
                             (if (zerop (plist-get row :count))
                                 "NOT USED anywhere indexed"
                               (format "%d use(s)" (plist-get row :count))))
                     'face (if (zerop (plist-get row :count))
                               'warning 'font-lock-function-name-face)))
            (dolist (l (plist-get row :lines)) (insert l "\n"))
            (insert "\n"))
          (my/side-panel--insert-diagnostics source-buffer))
        (setq my/side-panel--symbol-lines (nreverse anchors)
              my/side-panel--source-buffer source-buffer)
        (goto-char (point-min)))
      ;; Start following once there is something to follow.
      (with-current-buffer source-buffer (my/side-panel-follow-mode 1)))
    buf))

;;; Follow the cursor -----------------------------------------------------------

(defcustom my/side-panel-follow-idle 0.4
  "Idle seconds before the panel scrolls to the symbol point is inside."
  :type 'number
  :group 'my/side-panel)

(defvar my/side-panel--follow-timer nil)
(defvar-local my/side-panel--last-symbol nil)

(defvar-local my/side-panel--symbol-cache nil
  "Cons of (MODIFIED-TICK . SYMBOLS) for `my/side-panel--enclosing-symbol'.")

(defun my/side-panel--symbols-cached ()
  "Symbol ranges for this buffer, re-requested only when it changes.

The follow timer runs several times a second; asking the language
server for `documentSymbol' on every tick would put a network round
trip on idle."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (and my/side-panel--symbol-cache
                 (eq (car my/side-panel--symbol-cache) tick))
      (setq my/side-panel--symbol-cache (cons tick (my/cc--lsp-symbols))))
    (cdr my/side-panel--symbol-cache)))

(defun my/side-panel--enclosing-symbol ()
  "Name of the symbol whose range contains point, or nil."
  (let ((line (1- (line-number-at-pos)))   ; LSP lines are 0-based
        (best nil) (best-span most-positive-fixnum))
    (dolist (e (or (my/side-panel--symbols-cached) '()) best)
      (pcase-let ((`(,name ,_sl ,_sc ,rs ,re) e))
        (when (and rs re (<= rs line) (<= line re))
          ;; Innermost wins, so a method beats the class containing it.
          (let ((span (- re rs)))
            (when (< span best-span)
              (setq best name best-span span))))))))

(defun my/side-panel--follow ()
  "Scroll the report to the symbol point is inside."
  (when (and my/side-panel-follow-mode
             (get-buffer "*symbol usage*")
             (get-buffer-window "*symbol usage*"))
    (let ((sym (ignore-errors (my/side-panel--enclosing-symbol))))
      (when (and sym (not (equal sym my/side-panel--last-symbol)))
        (setq my/side-panel--last-symbol sym)
        (let* ((buf (get-buffer "*symbol usage*"))
               (win (get-buffer-window buf))
               (line (cdr (assoc sym (buffer-local-value
                                      'my/side-panel--symbol-lines buf)))))
          (when line
            (with-selected-window win
              (goto-char (point-min))
              (forward-line (1- line))
              (recenter 1))))))))

(define-minor-mode my/side-panel-follow-mode
  "Scroll the usage panel to whatever function point is in."
  :lighter " ⇥panel"
  (if my/side-panel-follow-mode
      (unless my/side-panel--follow-timer
        (setq my/side-panel--follow-timer
              (run-with-idle-timer my/side-panel-follow-idle t
                                   #'my/side-panel--follow)))
    (setq my/side-panel--last-symbol nil)))

;;; Fallback -------------------------------------------------------------------

(defun my/side-panel-generic (source-buffer)
  "Just the diagnostics, for a buffer with no special handling."
  (let ((buf (my/side-panel--buffer "*diagnostics*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (my/side-panel--insert-heading
         (format "%s" (buffer-name source-buffer)))
        (my/side-panel--insert-diagnostics source-buffer)))
    (my/side-panel--show buf)))

;;; Entry point ----------------------------------------------------------------

;;;###autoload
(defun my/side-panel ()
  "Split the window and show something useful about this buffer on the right."
  (interactive)
  (let ((src (current-buffer)))
    (cond
     ((derived-mode-p 'markdown-mode 'gfm-mode) (my/side-panel-markdown))
     ((derived-mode-p 'protobuf-mode)           (my/side-panel-protobuf src))
     ((derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
      (my/side-panel-cc src))
     (t (my/side-panel-generic src)))))

;;;###autoload
(defun my/side-panel-close ()
  "Close the side panel."
  (interactive)
  (dolist (name '("*proto checks*" "*symbol usage*" "*diagnostics*"))
    (when-let ((w (get-buffer-window name))) (delete-window w)))
  (when (bound-and-true-p markdown-live-preview-mode)
    (markdown-live-preview-mode -1))
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (bound-and-true-p my/side-panel-follow-mode)
        (my/side-panel-follow-mode -1))))
  (when my/side-panel--follow-timer
    (cancel-timer my/side-panel--follow-timer)
    (setq my/side-panel--follow-timer nil))
  (when my/markdown--sync-timer
    (cancel-timer my/markdown--sync-timer)
    (setq my/markdown--sync-timer nil)))

(global-set-key (kbd "<M-f8>") #'my/side-panel)

(provide 'setup-sidepanel)
;;; setup-sidepanel.el ends here

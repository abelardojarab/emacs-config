;;; setup-google3.el --- CiderLSP, clang-format and blaze for google3  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
;; Keywords: tools, languages

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

;; The parts of a Cider-like experience that are specific to google3:
;; CiderLSP for navigation and completion, clang-format, and blaze.
;;
;;   C-c g       `my/google3-menu'   -- everything below, in one transient
;;   M-.  M-?                        -- definition / references, via CiderLSP
;;   C-c g f     clang-format buffer or region
;;   C-c g b/t/c blaze build / test / coverage for the target at point
;;
;; Why CiderLSP and not clangd: clangd needs a compilation database, and there
;; is no such thing for a monorepo you have lazily mounted.  CiderLSP answers
;; from the same index Cider and Code Search use, so definitions and references
;; are repo-wide and correct on the first keystroke, with no local indexing at
;; all.  It is registered *alongside* the existing clangd client rather than
;; replacing it: google3 paths get CiderLSP, everything else still gets clangd.
;;
;; Measured on this machine: the server answers `initialize' in 0.8s and
;; advertises definition, references, completion, hover, formatting, code
;; actions and call hierarchy.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defgroup my/google3 nil
  "google3 development support."
  :group 'tools
  :prefix "my/google3-")

(defcustom my/ciderlsp-binary "/google/bin/releases/cider/ciderlsp/ciderlsp"
  "Path to the CiderLSP binary."
  :type 'string
  :group 'my/google3)

(defcustom my/google3-roots '("/google/src/" "/usr/local/google/home/")
  "Path prefixes under which a google3 checkout may live."
  :type '(repeat string)
  :group 'my/google3)

(defun my/google3-file-p (&optional file)
  "Non-nil when FILE is inside a google3 checkout.

Matches on the resolved path, so the symlinked ~/jj_workspaces names
resolve to their /google/src target."
  (let ((f (or file buffer-file-name)))
    (and f (string-match-p "/google3/" (file-truename f)) t)))

(defun my/google3-root (&optional file)
  "The google3 directory above FILE, or nil."
  (let ((f (file-truename (or file buffer-file-name default-directory))))
    (when (string-match "\\`\\(.*/google3\\)/" f)
      (match-string 1 f))))

(defun my/google3-relative (&optional file)
  "FILE relative to its google3 root."
  (let ((root (my/google3-root file))
        (f (file-truename (or file buffer-file-name))))
    (and root (file-relative-name f root))))

;;; CiderLSP -------------------------------------------------------------------

;; Registered with lsp-mode, which this config already uses, rather than
;; switching everything to eglot.  Both work -- CiderLSP's own --tooltag flag
;; names emacs-eglot and emacs-lsp-mode -- but swapping the LSP client would
;; also swap the completion, UI and keybinding integration that is already
;; working here, which is a much larger change than adding one more server.

(with-eval-after-load 'lsp-mode
  (when (file-executable-p my/ciderlsp-binary)

    ;; File watchers must stay off.  lsp-mode would otherwise register
    ;; recursive watches over the project root, and in a CitC workspace that
    ;; root is a lazily fetched mount of all of google3 -- the watch alone
    ;; would pull gigabytes and never finish.
    (setq lsp-enable-file-watchers nil)

    (defun my/ciderlsp-activate-p (filename &optional _mode)
      "Use CiderLSP only inside google3."
      (and filename (string-match-p "/google3/" (file-truename filename))))

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection
                       (lambda ()
                         (list my/ciderlsp-binary "-tooltag=emacs-lsp-mode")))
      :activation-fn #'my/ciderlsp-activate-p
      ;; Above clangd, so google3 C++ goes to the indexed server and
      ;; everything outside google3 keeps the clangd client untouched.
      :priority 10
      ;; The -ts- modes matter as much as the classic ones: google3 C++ is
      ;; remapped to c++-ts-mode below, and a client that does not list it
      ;; simply never attaches.
      :major-modes '(c-mode c++-mode c-ts-mode c++-ts-mode objc-mode
                     python-mode python-ts-mode go-mode go-ts-mode
                     java-mode java-ts-mode kotlin-mode
                     js-mode js2-mode typescript-mode typescript-ts-mode
                     protobuf-mode sh-mode bash-ts-mode
                     bazel-mode google3-build-mode)
      :server-id 'ciderlsp
      :multi-root t))))

;;; Speed ----------------------------------------------------------------------

;; The defaults are tuned for a local project; this is a network-backed server
;; over a lazily mounted monorepo, so the knobs that matter are the ones that
;; stop Emacs doing local work it cannot afford.
(with-eval-after-load 'lsp-mode
  (setq lsp-log-io nil                      ; logging every payload is not free
        lsp-enable-file-watchers nil        ; see above -- non-negotiable here
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil          ; let cc-mode/google-c-style do it
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-snippet t
        lsp-idle-delay 0.3
        lsp-completion-provider :capf       ; company-capf, no extra bridge
        lsp-keep-workspace-alive nil
        lsp-signature-render-documentation nil
        lsp-headerline-breadcrumb-enable nil))

(with-eval-after-load 'lsp-ui
  ;; Sideline is the single most expensive part of lsp-ui: it re-renders on
  ;; every cursor move.  Doc-on-hover is cheap by comparison.
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-peek-enable t))


;;; Tree-sitter everywhere it exists -------------------------------------------

;; cc-mode's `c-after-change' reparses on every keystroke.  Measured on a
;; 705-line google3 .cc file, inserting 300 characters:
;;
;;     c++-mode      7627 us per character
;;     c++-ts-mode     13 us per character
;;
;; That is not a tuning difference; it is the difference between typing being
;; free and costing 7.6ms a keystroke.  The same argument applies to the other
;; languages, so where a grammar exists the tree-sitter mode is preferred.
;;
;; Done with `major-mode-remap-alist', which is the supported mechanism and
;; applies before the mode hooks run -- unlike switching modes afterwards from
;; a hook, which re-runs everything and fights whatever set the mode first.
;;
;; Two things this does NOT cover:
;;   * proto -- there is no tree-sitter grammar for it on this machine, so
;;     .proto keeps protobuf-mode (see below); nothing is lost, it was never
;;     a cc-mode derivative.
;;   * Anything you have hung off `c++-mode-hook' and friends stops firing for
;;     remapped buffers, because the mode is genuinely different.  The
;;     indentation offsets are carried over below; if some other c++-mode hook
;;     matters to you, add it to the -ts- hook too, or set
;;     `my/prefer-tree-sitter' to nil.

(defcustom my/treesit-grammar-dir
  (expand-file-name "tree-sitter"
                    (expand-file-name (or (bound-and-true-p my/emacs-cache-dir)
                                          "~/.emacs.cache")))
  "Where locally built tree-sitter grammars go.

Machine-local on purpose.  Grammars are compiled shared objects -- the
ones in use here are `ELF 64-bit LSB shared object, x86-64' -- so they
are not portable between an x86 cloudtop and an ARM laptop, or between
Linux and macOS.  Emacs would install them to
<user-emacs-directory>/tree-sitter, which for this config is inside the
emacs-config git repo, so the default would commit x86-64 binaries into
a configuration shared across architectures.  ~/.emacs.cache is not
versioned, so each machine builds its own."
  :type 'directory
  :group 'my/google3)

(when (boundp 'treesit-extra-load-path)
  (add-to-list 'treesit-extra-load-path my/treesit-grammar-dir))

(defconst my/treesit-grammar-sources
  '((c          . ("https://github.com/tree-sitter/tree-sitter-c"))
    (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
    (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
    (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
    (java       . ("https://github.com/tree-sitter/tree-sitter-java"))
    (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
    (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
    (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
    (bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
    (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
    (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")))
  "Grammar sources, for building on a machine that has none.")

;;;###autoload
(defun my/treesit-grammar-status ()
  "Report which grammars are available here, and from where."
  (interactive)
  (unless (and (fboundp 'treesit-available-p) (treesit-available-p))
    (user-error "This Emacs has no tree-sitter support"))
  (let (have missing)
    (dolist (e my/treesit-grammar-sources)
      (if (treesit-language-available-p (car e))
          (push (car e) have)
        (push (car e) missing)))
    (message "tree-sitter: %d available (%s); %d missing (%s); local dir %s"
             (length have) (mapconcat #'symbol-name (nreverse have) " ")
             (length missing) (or (mapconcat #'symbol-name (nreverse missing) " ") "-")
             my/treesit-grammar-dir)))

;;;###autoload
(defun my/treesit-install-grammars (&optional all)
  "Build the missing tree-sitter grammars for THIS machine.

Compiles from source into `my/treesit-grammar-dir', so the result
matches the local architecture.  With ALL, rebuild everything rather
than only what is missing.  Needs git and a C compiler."
  (interactive "P")
  (unless (fboundp 'treesit-install-language-grammar)
    (user-error "This Emacs cannot install tree-sitter grammars"))
  (dolist (tool '("git" "cc"))
    (unless (executable-find tool)
      (user-error "%s is needed to build grammars" tool)))
  (make-directory my/treesit-grammar-dir t)
  (let ((treesit-language-source-alist my/treesit-grammar-sources)
        (built 0))
    (dolist (e my/treesit-grammar-sources)
      (when (or all (not (treesit-language-available-p (car e))))
        (message "tree-sitter: building %s..." (car e))
        (condition-case err
            (progn (treesit-install-language-grammar (car e) my/treesit-grammar-dir)
                   (setq built (1+ built)))
          (error (message "tree-sitter: %s failed: %s"
                          (car e) (error-message-string err))))))
    (my/apply-tree-sitter-remaps)
    (message "tree-sitter: built %d grammar(s) into %s" built my/treesit-grammar-dir)))

(defcustom my/prefer-tree-sitter t
  "Prefer tree-sitter major modes wherever a grammar is installed."
  :type 'boolean
  :group 'my/google3)

(defconst my/tree-sitter-remaps
  '((cpp        . ((c++-mode . c++-ts-mode)))
    (c          . ((c-mode . c-ts-mode)))
    (python     . ((python-mode . python-ts-mode)))
    (typescript . ((typescript-mode . typescript-ts-mode)))
    (tsx        . ((tsx-mode . tsx-ts-mode)))
    (javascript . ((js-mode . js-ts-mode)
                   (js2-mode . js-ts-mode)
                   (javascript-mode . js-ts-mode)))
    (json       . ((js-json-mode . json-ts-mode)))
    (yaml       . ((yaml-mode . yaml-ts-mode)))
    (bash       . ((sh-mode . bash-ts-mode)))
    (go         . ((go-mode . go-ts-mode)))
    (java       . ((java-mode . java-ts-mode)))
    (rust       . ((rust-mode . rust-ts-mode))))
  "Grammar -> ((classic-mode . ts-mode) ...) preferences.")

(defun my/apply-tree-sitter-remaps ()
  "Remap to tree-sitter modes for every grammar actually installed."
  (when (and my/prefer-tree-sitter
             (fboundp 'treesit-available-p) (treesit-available-p)
             (boundp 'major-mode-remap-alist))
    (let (applied)
      (dolist (entry my/tree-sitter-remaps)
        (when (treesit-language-available-p (car entry))
          (dolist (pair (cdr entry))
            (when (fboundp (cdr pair))
              (setf (alist-get (car pair) major-mode-remap-alist) (cdr pair))
              (push (cdr pair) applied)))))
      applied)))

(with-eval-after-load 'treesit (my/apply-tree-sitter-remaps))
(when (require 'treesit nil t) (my/apply-tree-sitter-remaps))

(with-eval-after-load 'c-ts-mode
  ;; Google style is two spaces.  The tree-sitter modes have their own offset
  ;; and do not read `c-basic-offset'.
  (setq c-ts-mode-indent-offset 2
        c-ts-mode-indent-style 'gnu))

;;; CiderLSP and the rest, in whatever mode the buffer ended up in -------------

(defun my/google3-start-lsp ()
  "Start CiderLSP in a google3 buffer.

The existing `my/lsp-if-c-source' hook is attached to `c-mode' and
`c++-mode'.  Remapping to the tree-sitter modes therefore silently took
LSP away with it -- fast typing, no navigation.  This puts it back, for
every mode the remap can produce, google3 only."
  (when (and (my/google3-file-p)
             (file-executable-p my/ciderlsp-binary)
             (fboundp 'lsp-deferred))
    (lsp-deferred)))

(dolist (h '(c-ts-mode-hook c++-ts-mode-hook python-ts-mode-hook
             go-ts-mode-hook java-ts-mode-hook typescript-ts-mode-hook
             tsx-ts-mode-hook js-ts-mode-hook bash-ts-mode-hook
             protobuf-mode-hook))
  (add-hook h #'my/google3-start-lsp))

;;; protobuf --------------------------------------------------------------------

;; There is no tree-sitter proto grammar here, so .proto uses the
;; protobuf-mode.el in elisp/.  It derives from `prog-mode', which is what
;; makes the rest of the setup apply for free: `diff-hl-flydiff' is not
;; involved, `jj--setup-buffer-vc-ui' keys off the file being in a jj repo
;; rather than off the major mode, so a .proto buffer gets the same fringe
;; marks against @- and the same inline jj blame as everything else.

(defun my/protobuf-setup ()
  "Round out protobuf buffers: fringe marks, blame, checking."
  (setq-local comment-start "// "
              comment-end ""
              tab-width 2
              indent-tabs-mode nil)
  (when (fboundp 'flycheck-mode) (flycheck-mode 1))
  ;; The jj UI is mode-agnostic, but call it explicitly: protobuf-mode may be
  ;; entered by `M-x' on an already-open buffer, after find-file-hook has run.
  (when (fboundp 'jj--setup-buffer-vc-ui) (jj--setup-buffer-vc-ui)))

(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode)
  :commands protobuf-mode
  :hook (protobuf-mode . my/protobuf-setup))

;;; clang-format ---------------------------------------------------------------

(defcustom my/clang-format-binary (or (executable-find "clang-format")
                                      "/usr/bin/clang-format")
  "clang-format binary."
  :type 'string
  :group 'my/google3)

;;;###autoload
(defun my/clang-format ()
  "Format the region, or the whole buffer when nothing is selected.

Uses clang-format directly rather than the LSP formatter: it honours the
nearest .clang-format, it works on a buffer the server has not opened
yet, and it is instant."
  (interactive)
  (unless (file-executable-p my/clang-format-binary)
    (user-error "clang-format not found at %s" my/clang-format-binary))
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end   (if (use-region-p) (region-end) (point-max)))
        (line (line-number-at-pos))
        (col (current-column)))
    (call-process-region start end my/clang-format-binary t t nil
                         "-assume-filename" (or buffer-file-name "x.cc"))
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column col)
    (message "clang-format: %s" (if (use-region-p) "region" "buffer"))))

(defcustom my/clang-format-on-save nil
  "Format google3 C/C++ buffers with clang-format on save."
  :type 'boolean
  :group 'my/google3)

(defun my/clang-format-maybe-on-save ()
  (when (and my/clang-format-on-save
             (my/google3-file-p)
             (derived-mode-p 'c-mode 'c++-mode))
    (my/clang-format)))

(add-hook 'before-save-hook #'my/clang-format-maybe-on-save)

;;;###autoload
(defun my/clang-tidy ()
  "Run clang-tidy on this file into a compilation buffer."
  (interactive)
  (unless buffer-file-name (user-error "Not visiting a file"))
  (let ((default-directory (or (my/google3-root) default-directory)))
    (compilation-start
     (format "clang-tidy %s" (shell-quote-argument (my/google3-relative)))
     nil (lambda (&rest _) "*clang-tidy*"))))

;;; blaze ----------------------------------------------------------------------

(defcustom my/blaze-binary "blaze"
  "Blaze binary for interactive builds.

Deliberately plain `blaze\\=', not the agent wrapper: this output goes to
a compilation buffer a human reads and clicks through, which is exactly
what the wrapper strips out."
  :type 'string
  :group 'my/google3)

(defun my/blaze--package ()
  "The blaze package path for the current buffer, e.g. //foo/bar."
  (let* ((rel (my/google3-relative))
         (dir (and rel (file-name-directory rel))))
    (if dir (concat "//" (directory-file-name dir)) "//...")))

(defun my/blaze--read-target (verb)
  (read-string (format "blaze %s: " verb) (concat (my/blaze--package) ":all")))

(defun my/blaze--run (verb target &rest extra)
  (let ((default-directory (or (my/google3-root)
                               (user-error "Not inside google3"))))
    (compilation-start
     (string-join (append (list my/blaze-binary verb) extra
                          (list (shell-quote-argument target)))
                  " ")
     nil (lambda (&rest _) (format "*blaze %s*" verb)))))

;;;###autoload
(defun my/blaze-build (target)
  "blaze build TARGET."
  (interactive (list (my/blaze--read-target "build")))
  (my/blaze--run "build" target))

;;;###autoload
(defun my/blaze-test (target)
  "blaze test TARGET."
  (interactive (list (my/blaze--read-target "test")))
  (my/blaze--run "test" target "--test_output=errors"))

;;;###autoload
(defun my/blaze-coverage (target)
  "blaze coverage TARGET, then offer to show the LCOV report."
  (interactive (list (my/blaze--read-target "coverage")))
  (my/blaze--run "coverage" target "--combined_report=lcov")
  (message "Coverage running; M-x my/blaze-coverage-show when it finishes"))

;;;###autoload
(defun my/blaze-coverage-show ()
  "Open the combined LCOV report blaze coverage produced."
  (interactive)
  (let* ((root (or (my/google3-root) (user-error "Not inside google3")))
         (lcov (expand-file-name "../blaze-out/_coverage/_coverage_report.dat" root)))
    (if (file-readable-p lcov)
        (find-file lcov)
      (user-error "No coverage report at %s -- has `blaze coverage' finished?" lcov))))

;;; Which xref backend answers M-. and M-? ------------------------------------

;; Measured: in a google3 C++ buffer the backend that actually ran was
;; `dumb-jump' -- not lsp, not gtags -- because lsp attaches lazily and
;; dumb-jump sits first in `xref-backend-functions'.  dumb-jump answers by
;; regex-searching the project, and in a CitC workspace the project is all of
;; google3 on a lazily fetched mount.  gxref is no better there: `global' would
;; want a GTAGS database over the same tree.
;;
;; So inside google3 the only correct answer is CiderLSP, which already has the
;; index -- 21 references in 1.1s, measured.  Outside google3, nothing changes
;; and gtags/dumb-jump keep working as before.

(defun my/google3-xref-backend ()
  "Prefer CiderLSP inside google3; refuse the tree-walking backends there."
  (when (my/google3-file-p)
    (cond
     ((and (fboundp 'lsp-workspaces) (lsp-workspaces)) 'xref-lsp)
     (t
      (message "google3: CiderLSP is not attached yet (M-x lsp) -- not letting dumb-jump/gtags walk the CitC tree")
      'my/google3-refuse))))

(cl-defmethod xref-backend-identifier-at-point ((_b (eql my/google3-refuse))) nil)
(cl-defmethod xref-backend-definitions ((_b (eql my/google3-refuse)) _id) nil)
(cl-defmethod xref-backend-references ((_b (eql my/google3-refuse)) _id) nil)
(cl-defmethod xref-backend-identifier-completion-table ((_b (eql my/google3-refuse))) nil)

(defun my/google3-install-xref-guard ()
  "Put the google3 backend ahead of the others, buffer-locally."
  (when (my/google3-file-p)
    (add-hook 'xref-backend-functions #'my/google3-xref-backend -100 t)))

(add-hook 'find-file-hook #'my/google3-install-xref-guard 96)

;;; gtags / GNU Global, for everything that is not google3 ---------------------

(defun my/gtags-xref-backend ()
  "Prefer gxref when this project actually has a GTAGS database.

Without this, `dumb-jump' wins simply by being first in
`xref-backend-functions', and answers by regex-searching the tree even
when an index is sitting right there.  Only claims the buffer when a
GTAGS file exists above it, so projects without one keep the old
behaviour."
  (when (and (not (my/google3-file-p))
             buffer-file-name
             (fboundp 'gxref-xref-backend)
             (locate-dominating-file default-directory "GTAGS"))
    'gxref))

(defun my/gtags-install-xref ()
  "Prefer gxref, and let `global' see the project-local database."
  (when (and (not (my/google3-file-p))
             buffer-file-name
             (locate-dominating-file default-directory "GTAGS"))
    (add-hook 'xref-backend-functions #'my/gtags-xref-backend -90 t)
    ;; GTAGSROOT is exported globally to ~/.gtags for the shared database.
    ;; Left set, `global' looks there and reports "GTAGS not found" while
    ;; standing in a project that has one, so drop it for this buffer.
    (setq-local process-environment
                (cons "GTAGSLABEL=native"
                      (seq-remove (lambda (v)
                                    (or (string-prefix-p "GTAGSROOT=" v)
                                        (string-prefix-p "GTAGSDBPATH=" v)))
                                  process-environment)))))

(add-hook 'find-file-hook #'my/gtags-install-xref 97)

;; GNU Global 6.6.14 is installed and `gxref' is already registered as an xref
;; backend, so M-. / M-? use it once a database exists.  What was missing is a
;; way to create one -- and a refusal to create one over a CitC mount, which is
;; the same mistake in a different shape.

(defcustom my/gtags-labels "native"
  "GTAGSLABEL to use.

`native\=' is the only label measured to produce *references* as well as
definitions: `ctags\=' gives definitions but no references at all (so
find-references looks broken), and `pygments\=' produced a corrupted
database.  See the note in ~/.bashrc_local."
  :type 'string
  :group 'my/google3)

(defun my/gtags-root ()
  "Project root for a GTAGS database, or nil."
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      (and (fboundp 'vc-root-dir) (vc-root-dir))
      default-directory))

;;;###autoload
(defun my/gtags-update (&optional root)
  "Build or update the GTAGS database for ROOT.

Refuses inside google3: `gtags' would walk a lazily fetched mount of the
whole monorepo.  Use CiderLSP there -- it is already indexed."
  (interactive)
  (let ((root (or root (my/gtags-root))))
    (unless (executable-find "global")
      (user-error "GNU Global is not installed"))
    (when (string-match-p "/google3/\\|\\`/google/src/" (file-truename root))
      (user-error "Refusing to run gtags inside google3 -- CiderLSP already indexes it"))
    (let* ((default-directory (file-name-as-directory root))
           ;; GTAGSROOT is exported globally to ~/.gtags for the shared
           ;; database; left set, `global' looks there and reports
           ;; "GTAGS not found" while standing in a project that has one.
           (process-environment
            (cons (format "GTAGSLABEL=%s" my/gtags-labels)
                  (seq-remove (lambda (v) (or (string-prefix-p "GTAGSROOT=" v)
                                              (string-prefix-p "GTAGSDBPATH=" v)))
                              process-environment))))
      (compilation-start
       (if (file-exists-p (expand-file-name "GTAGS" root))
           "global -u"          ; incremental
         "gtags --statistics")  ; first build
       nil (lambda (&rest _) "*gtags*")))))

;;;###autoload
(defun my/find-references ()
  "Find references with whichever backend is right for this buffer.

Results render through `xref-show-xrefs-function', which this config
already points at helm, so the hit list is a helm buffer."
  (interactive)
  (call-interactively #'xref-find-references))

;;;###autoload
(defun my/symbol-usage (&optional identifier)
  "Answer \"is this used?\" for the symbol at point.

Counts references and separates them from the definition, so a function
with only its own definition reports as unused rather than as one hit.
Uses whichever backend is correct here -- CiderLSP inside google3, gtags
or whatever xref resolves to outside -- and says which one answered, so
a surprising count can be attributed rather than guessed at."
  (interactive)
  (let* ((backend (xref-find-backend))
         (id (or identifier
                 (xref-backend-identifier-at-point backend)
                 (user-error "No symbol at point")))
         (refs (or (xref-backend-references backend id) '()))
         (defs (or (ignore-errors (xref-backend-definitions backend id)) '()))
         (def-locs (mapcar (lambda (x)
                             (let ((l (xref-item-location x)))
                               (cons (xref-location-group l)
                                     (xref-location-line l))))
                           defs))
         (uses (seq-remove (lambda (x)
                             (let ((l (xref-item-location x)))
                               (member (cons (xref-location-group l)
                                             (xref-location-line l))
                                       def-locs)))
                           refs))
         (files (delete-dups (mapcar (lambda (x)
                                       (xref-location-group (xref-item-location x)))
                                     uses))))
    (cond
     ((null refs)
      (message "%s: no references found (backend: %s)" id backend))
     ((null uses)
      (message "%s: APPEARS UNUSED -- %d hit(s), all at its definition (backend: %s)"
               id (length refs) backend))
     (t
      (message "%s: %d use(s) in %d file(s), %d definition(s) (backend: %s)"
               id (length uses) (length files) (length defs) backend)))
    ;; Show the list too; it lands in helm via `xref-show-xrefs-function'.
    (when refs
      (xref-show-xrefs (lambda () refs) nil))
    (list :uses (length uses) :files (length files) :backend backend)))

;;;###autoload
(defun my/workspace-symbol ()
  "Search symbols across the whole workspace, via helm or ivy."
  (interactive)
  (cond
   ((and (fboundp 'helm-lsp-workspace-symbol) (bound-and-true-p lsp-mode))
    (call-interactively #'helm-lsp-workspace-symbol))
   ((and (fboundp 'lsp-ivy-workspace-symbol) (bound-and-true-p lsp-mode))
    (call-interactively #'lsp-ivy-workspace-symbol))
   ((fboundp 'xref-find-apropos) (call-interactively #'xref-find-apropos))
   (t (user-error "No workspace symbol search available here"))))

;;; Everything as a searchable list, for helm / ivy / vertico -----------------

;; The transients stay, but the primary way in is meant to be typing part of
;; the name rather than a chord: one `completing-read', which helm-mode, ivy
;; and vertico each take over natively, plus real helm sources for people who
;; want actions on the candidate.

(defconst my/google3-command-table
  '(("refs: is this symbol used?"            . my/symbol-usage)
    ("refs: find references"                 . my/find-references)
    ("refs: find definition"                 . xref-find-definitions)
    ("refs: workspace symbol search"         . my/workspace-symbol)
    ("refs: symbols in this file"            . imenu)
    ("refs: rename symbol"                   . my/lsp-rename)
    ("refs: back (pop marker)"               . xref-go-back)
    ("format: clang-format buffer/region"    . my/clang-format)
    ("lint: clang-tidy this file"            . my/clang-tidy)
    ("tags: build/update GTAGS"              . my/gtags-update)
    ("blaze: build"                          . my/blaze-build)
    ("blaze: test"                           . my/blaze-test)
    ("blaze: coverage"                       . my/blaze-coverage)
    ("blaze: show coverage report"           . my/blaze-coverage-show)
    ("lsp: start CiderLSP here"              . lsp)
    ("lsp: restart workspace"                . lsp-workspace-restart)
    ("lsp: describe thing at point"          . lsp-describe-thing-at-point))
  "(DESCRIPTION . COMMAND) for the google3 menu.")

;;;###autoload
(defun my/google3-commands ()
  "Pick a google3 command by name."
  (interactive)
  (let* ((choice (completing-read "google3: "
                                  (mapcar #'car my/google3-command-table) nil t))
         (cmd (cdr (assoc choice my/google3-command-table))))
    (when cmd (call-interactively cmd))))

;;;###autoload
(defalias 'counsel-google3 #'my/google3-commands)

(with-eval-after-load 'helm
  (defvar my/helm-google3-source
    (helm-build-sync-source "google3"
      :candidates (lambda () my/google3-command-table)
      :action (list (cons "Run" #'call-interactively)))
    "Helm source over `my/google3-command-table'.")

  (defun helm-google3 ()
    "Pick a google3 command with helm."
    (interactive)
    (helm :sources 'my/helm-google3-source :buffer "*helm google3*")))

;;; One entry point for all of it -----------------------------------------------

(defun my/dev--table ()
  "Everything, flattened and prefixed so it is searchable by category."
  (append
   my/google3-command-table
   (when (boundp 'jj-command-table)
     (mapcar (lambda (c) (cons (concat "jj " (car c)) (cdr c))) jj-command-table))))

;;;###autoload
(defun my/dev-menu ()
  "One searchable menu over everything: refs, blaze, format, jj, CLs.

Bound to a single Alt chord and a function key rather than a Ctrl
prefix; `M-x my/dev-menu' works too.  Under helm this opens as several
sources in one buffer so the CL stack and the command list are both
there; under ivy or vertico it is one flat, prefixed list."
  (interactive)
  (if (and (fboundp 'helm) (boundp 'my/helm-google3-source))
      (helm :sources (append (list 'my/helm-google3-source)
                             (when (boundp 'jj-helm-command-source)
                               (list 'jj-helm-command-source))
                             (when (boundp 'jj-helm-stack-source)
                               (list 'jj-helm-stack-source))
                             (when (boundp 'jj-helm-files-source)
                               (list 'jj-helm-files-source))
                             (when (boundp 'jj-helm-workspace-source)
                               (list 'jj-helm-workspace-source)))
            :buffer "*helm dev*")
    (let* ((table (my/dev--table))
           (choice (completing-read "dev: " (mapcar #'car table) nil t))
           (cmd (cdr (assoc choice table))))
      (when cmd (call-interactively cmd)))))

;; Alt and a function key, no Ctrl prefix.  Both were verified unbound.
(global-set-key (kbd "M-o") #'my/dev-menu)
(global-set-key (kbd "<S-f8>") #'my/dev-menu)

(defun my/lsp-rename ()
  "Rename the symbol at point via LSP.

A thin wrapper because transient validates its suffixes when the menu
opens: naming `lsp-rename' directly made the whole google3 menu fail
with \"Suffix lsp-rename is not defined or autoloaded as a command\" on
a machine where lsp-mode had not been loaded."
  (interactive)
  (if (and (require 'lsp-mode nil t) (fboundp 'lsp-rename))
      (call-interactively #'lsp-rename)
    (user-error "lsp-mode is not available here")))

;;; Menu -----------------------------------------------------------------------

(with-eval-after-load 'transient
  (transient-define-prefix my/google3-menu ()
    "google3."
    [["Navigate"
      ("d" "definition"        xref-find-definitions)
      ("r" "references (helm)" my/find-references)
      ("u" "is it used?"       my/symbol-usage)
      ("w" "workspace symbol"  my/workspace-symbol)
      ("s" "symbol in file"    imenu)
      ("R" "rename"            my/lsp-rename)]
     ["Tags"
      ("g" "gtags: build/update" my/gtags-update)]
     ["Format / lint"
      ("f" "clang-format"   my/clang-format)
      ("T" "clang-tidy"     my/clang-tidy)]
     ["Blaze"
      ("b" "build"          my/blaze-build)
      ("t" "test"           my/blaze-test)
      ("c" "coverage"       my/blaze-coverage)
      ("C" "show coverage"  my/blaze-coverage-show)]]))

;;;###autoload
(defun my/google3-dispatch ()
  "Open the google3 menu."
  (interactive)
  (require 'transient nil t)
  (if (fboundp 'my/google3-menu)
      (call-interactively #'my/google3-menu)
    (call-interactively #'my/clang-format)))

(global-set-key (kbd "C-c g") #'my/google3-dispatch)

(provide 'setup-google3)
;;; setup-google3.el ends here

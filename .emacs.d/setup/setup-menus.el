;;; setup-menus.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
;; Keywords: abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'setup-menu-macros)
(require 'setup-menu-styles)

;;; Reconfigure Text Mode Menu

(easy-menu-remove-item text-mode-menu nil "Center Line")
(easy-menu-remove-item text-mode-menu nil "Center Region")
(easy-menu-remove-item text-mode-menu nil "Center Paragraph")
(easy-menu-remove-item text-mode-menu nil "Paragraph Indent")
(easy-menu-remove-item text-mode-menu nil "---")

(easy-menu-add-item text-mode-menu nil my/transform-text-menu "Auto Fill")
(easy-menu-add-item text-mode-menu nil my/emphasize-menu "Auto Fill")

;;; Reconfigure Edit Menu
;;
;; NOTE: the Edit-menu customizations (Transpose / Move Text / Delete Space /
;; Flush / Keep Lines) used to live here and ran at load time.  ergoemacs
;; rebuilds the whole Edit menu at `after-init' (it does `define-key-after ...
;; [menu-bar edit] (cons "Edit" ...)'), which wiped them out.  They are now
;; installed from `my/install-extra-menus' below, which runs *after* the
;; ergoemacs menu bar is built, so they survive.

;;; Reconfigure Tools Menu

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Agenda - All TODOs"
                     (lambda () (interactive)(org-agenda nil "n"))
                     :help "Show Org agenda with all TODO tasks."]
                    "Shell Commands")

(keymap-set-after (lookup-key global-map [menu-bar tools])
				  "<separator-org>"
				  '(menu-item "--")
				  'Agenda\ -\ All\ TODOs)

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Find File…"
                     helm-find-files
                     :help "Fuzzy find file."]
                    "Shell Commands")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Find in Files (rgrep)…"
                     rgrep
                     :help "Recursively grep for REGEXP in FILES in directory \
tree rooted at DIR."]
                    "Shell Commands")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["IELM"
                     ielm
                     :help "Interactively evaluate Emacs Lisp expressions."]
                    "Language Server Support (Eglot)")

(keymap-set-after (lookup-key global-map [menu-bar tools])
				  "<separator-shell>"
				  '(menu-item "--")
				  'Search\ Org\ Notes…)

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Magit Status"
                     magit-status
                     :visible (vc-responsible-backend default-directory t)
                     :help "Show the status of the current Git repository \
in a buffer"]
                    "Version Control")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Count Words"
                     count-words
                     :help "Count words in buffer or region if active."]
                    "Calendar")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Eshell"
                     eshell
                     :help "Create an interactive Eshell buffer."]
                    "Calendar")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Python Shell"
                     run-python
                     :help "Run an inferior Python process."]
                    "Calendar")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["RE-Builder"
                     re-builder
                     :help "Construct a regexp interactively."]
                    "Calendar")

(keymap-set-after (lookup-key global-map [menu-bar tools])
				  "<separator-re>"
				  '(menu-item "--")
				  'RE-Builder)

(easy-menu-add-item global-map '(menu-bar tools)
                    ["World Clock"
                     world-clock
                     :help "Display a world clock buffer with times in \
various time zones."]
                    "Programmable Calculator")

;;; Menu entries for tools that previously had no menu entry.
;;
;; These cover the major interactive subsystems configured in the various
;; setup-* files (projectile, lsp, flycheck, dumb-jump, avy, treemacs, bm,
;; gptel, vterm, docker, ediff, undo-tree, …) which were reachable only via
;; key bindings.  Each item is placed in its most natural existing ergoemacs
;; menu (Edit / Search / View) where one fits; the remaining code-intelligence
;; and project commands get two dedicated top-level menus (Project, Code) so
;; the Tools menu does not become cluttered; genuine external tools go in the
;; stock Tools menu next to their relatives.
;;
;; EVERYTHING here is installed from `my/install-extra-menus', run on
;; `after-init-hook' (appended, so last), i.e. AFTER ergoemacs rebuilds the
;; File/Edit/Search/View/Help menus -- otherwise insertions into those menus
;; are clobbered by that rebuild.
;;
;; Commands are all autoloaded by their packages, so selecting an item pulls
;; in the feature on demand.  Items that only make sense when a minor mode is
;; active (LSP) or a package is actually installed (vterm) carry an `:enable'
;; guard so they grey out instead of erroring.

(easy-menu-define my/project-menu nil
  "Project-scoped commands."
  '("Project"
    ["Switch Project…"        projectile-switch-project
     :help "Switch to a known project and run a command in it."]
    ["Find File in Project…"  projectile-find-file
     :help "Jump to a file in the current project."]
    ["Search in Project (rg)…" projectile-ripgrep
     :help "Ripgrep for a regexp across the current project."]
    ["Switch Project Buffer…" projectile-switch-to-buffer
     :help "Switch to a buffer belonging to the current project."]
    "---"
    ["Recent Files…"          recentf-open-files
     :help "Open a file from the recently-visited list."]))

(easy-menu-define my/code-menu nil
  "Code-intelligence, diagnostics and AI commands."
  '("Code"
    ["Go to Definition"       xref-find-definitions
     :help "Find the definition of the identifier at point."]
    ["Find References"        xref-find-references
     :help "Find references to the identifier at point."]
    ["Jump (dumb-jump)"       dumb-jump-go
     :help "Jump to definition using dumb-jump heuristics."]
    ["Jump Back"              dumb-jump-back
     :help "Return to where the last dumb-jump started."]
    "---"
    ["Rename Symbol…"         lsp-rename
     :enable (bound-and-true-p lsp-mode)
     :help "Rename the symbol at point via the language server."]
    ["Format Buffer"          lsp-format-buffer
     :enable (bound-and-true-p lsp-mode)
     :help "Format the whole buffer via the language server."]
    ["Code Action…"           lsp-execute-code-action
     :enable (bound-and-true-p lsp-mode)
     :help "Run a code action offered by the language server."]
    ["Describe at Point"      lsp-describe-thing-at-point
     :enable (bound-and-true-p lsp-mode)
     :help "Show documentation for the symbol at point."]
    "---"
    ["List Errors (flycheck)" flycheck-list-errors
     :help "Show all flycheck diagnostics in a buffer."]
    ["Next Error"             flycheck-next-error
     :help "Go to the next flycheck error."]
    ["Previous Error"         flycheck-previous-error
     :help "Go to the previous flycheck error."]
    ["Check Buffer"           flycheck-buffer
     :help "Run a syntax check on the current buffer."]
    "---"
    ["AI Chat (gptel)"        gptel
     :help "Open or switch to a gptel AI chat buffer."]
    ["Send to AI (gptel)"     gptel-send
     :help "Send the region or buffer to the AI model."]
    ["AI Rewrite (gptel)"     gptel-rewrite
     :help "Rewrite/refactor the region with the AI model."]
    "---"
    ["Complete (company)"     company-complete
     :help "Trigger company completion at point."]
    ["Insert Snippet…"        yas-insert-snippet
     :help "Insert a YASnippet template."]))

(defun my/install-extra-menus ()
  "Install all extra menu entries after the ergoemacs menu bar exists.
Run from `after-init-hook' (appended) so it executes after the
ergoemacs menu bar has been built, avoiding clobbering."

  ;; ---- Edit menu: text transforms (rescued from clobber) + undo history ----
  (easy-menu-add-item global-map '(menu-bar edit) my/transpose-menu)
  (easy-menu-add-item global-map '(menu-bar edit) my/move-text-menu)
  (easy-menu-add-item global-map '(menu-bar edit) my/delete-space-menu)
  (easy-menu-add-item global-map '(menu-bar edit)
                      ["Flush Lines…"
                       flush-lines
                       :help "Delete lines containing matches for REGEXP."
                       :visible (not buffer-read-only)])
  (easy-menu-add-item global-map '(menu-bar edit)
                      ["Keep Lines…"
                       keep-lines
                       :help "Delete all lines except those matching REGEXP."
                       :visible (not buffer-read-only)])
  (easy-menu-add-item global-map '(menu-bar edit)
                      ["Undo-Tree Visualize"
                       undo-tree-visualize
                       :help "Show and browse the undo history as a tree."]
                      "Cut")

  ;; ---- Search menu: extra search backends + visual bookmarks (bm) ----
  (easy-menu-add-item global-map '(menu-bar search)
                      ["Search Files (deadgrep)…"
                       deadgrep
                       :help "Ripgrep search with an interactive results buffer."])
  (easy-menu-add-item global-map '(menu-bar search)
                      ["Jump to Char (avy)…"
                       avy-goto-char-timer
                       :help "Jump to a visible character chosen with avy."])
  (easy-menu-add-item global-map '(menu-bar search)
                      ["Toggle Bookmark (bm)"
                       bm-toggle
                       :help "Toggle a visual bookmark on the current line."])
  (easy-menu-add-item global-map '(menu-bar search)
                      ["Next Bookmark"
                       bm-next
                       :help "Jump to the next visual bookmark."])
  (easy-menu-add-item global-map '(menu-bar search)
                      ["Previous Bookmark"
                       bm-previous
                       :help "Jump to the previous visual bookmark."])

  ;; ---- View menu: file trees + imenu side panel (next to Speedbar) ----
  (easy-menu-add-item global-map '(menu-bar view)
                      ["File Tree (Treemacs)"
                       treemacs
                       :help "Toggle the Treemacs project file tree."])
  (easy-menu-add-item global-map '(menu-bar view)
                      ["File Tree (Neotree)"
                       neotree-toggle
                       :help "Toggle the Neotree file tree."])
  (easy-menu-add-item global-map '(menu-bar view)
                      ["Side Dired"
                       dired-sidebar-toggle-sidebar
                       :help "Toggle a dired sidebar for the current directory."])
  (easy-menu-add-item global-map '(menu-bar view)
                      ["Imenu List"
                       imenu-list
                       :help "Show a side buffer with the buffer's imenu index."])

  ;; ---- New top-level Project and Code menus, placed just before Help ----
  (easy-menu-add-item global-map '(menu-bar) my/project-menu "Help")
  (easy-menu-add-item global-map '(menu-bar) my/code-menu "Help")

  ;; ---- Tools menu: genuine external tools, next to Eshell / Magit ----
  (easy-menu-add-item global-map '(menu-bar tools)
                      ["Terminal (vterm)"
                       vterm
                       :enable (fboundp 'vterm)
                       :help "Open a fully-featured terminal emulator."]
                      "Calendar")
  (easy-menu-add-item global-map '(menu-bar tools)
                      ["Ediff Files…"
                       ediff-files
                       :help "Run a visual diff between two files."]
                      "Calendar")
  (easy-menu-add-item global-map '(menu-bar tools)
                      ["Ediff (DWIM)"
                       my/ediff-dwim
                       :help "Diff regions/buffers/files, guessing what you mean."]
                      "Calendar")
  (easy-menu-add-item global-map '(menu-bar tools)
                      ["Docker"
                       docker-containers
                       :help "Manage Docker containers, images and volumes."]
                      "Calendar"))

;; Run after the ergoemacs menu bar is installed (appended so it is last on
;; the hook), or immediately when reloaded after startup.
(if after-init-time
    (my/install-extra-menus)
  (add-hook 'after-init-hook #'my/install-extra-menus t))

(provide 'setup-menus)
;;; setup-menus.el ends here

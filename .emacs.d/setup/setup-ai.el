;;; setup-ai.el --- AI commit messages and code questions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
;; Keywords: convenience, tools, vc

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

;; Two things, both off one backend:
;;
;;   C-c C-a in a magit commit buffer or in `*jj describe*'
;;             -> write the commit message from the diff
;;   M-x my/ai-explain-region / my/ide-ai-review-region
;;             -> ask about the code in front of you
;;
;; Backend, picked automatically by `my/ai--backend':
;;
;; * gptel, when it has a usable key.  gptel here reads it from auth-source
;;   for api.anthropic.com, so "installed" and "usable" are different
;;   questions -- `my/ai--gptel-usable-p' checks for the key rather than the
;;   package, otherwise every request would fail at the network layer with a
;;   message that does not mention the real problem.
;; * the CloudCode CLI otherwise, driven through `cloudcode run'.
;;
;; Both are asynchronous.  A commit message takes a few seconds and Emacs
;; stays live throughout; the buffer is filled in when the answer lands, so
;; you can keep typing your own message and it will simply be replaced.
;;
;; CLI output is not a clean channel -- `cloudcode run' prints a banner and
;; ANSI colour before the reply -- so the prompt asks for the answer between
;; two markers and `my/ai--extract' pulls out what is between them.  Parsing
;; by "skip the first N lines" broke the moment the banner changed.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'auth-source)

(defgroup my/ai nil
  "AI assistance for commits and code."
  :group 'tools
  :prefix "my/ai-")

(defcustom my/ai-backend nil
  "Which backend to use, or nil to pick the best available."
  :type '(choice (const :tag "Automatic" nil)
                 (const :tag "gptel"     gptel)
                 (const :tag "CloudCode CLI" cloudcode))
  :group 'my/ai)

(defcustom my/ai-cloudcode-executable
  (or (executable-find "cloudcode")
      (let ((p (expand-file-name "~/.cloudcode/bin/cloudcode")))
        (and (file-executable-p p) p))
      (executable-find "opencode"))
  "Path to the CloudCode (or opencode) CLI."
  :type '(choice (const :tag "None" nil) file)
  :group 'my/ai)

(defcustom my/ai-timeout 120
  "Seconds to wait for a reply before giving up."
  :type 'integer
  :group 'my/ai)

(defcustom my/ai-diff-limit 60000
  "Most diff characters to send.

A whole-repository commit runs to megabytes, which is slow, expensive and
worse at the job than the first part of the diff plus the file list."
  :type 'integer
  :group 'my/ai)

(defcustom my/ai-commit-instructions
  "You are writing a git/jj commit message for an experienced engineer.

Rules:
- Exactly two parts: a subject line, then a blank line, then ONE body line.
- Subject: imperative mood, no trailing period, at most 72 characters.
  Prefix with the touched area and a colon when one dominates, e.g.
  \"ide-layout: \" or \"c++: \".
- Body: one sentence saying WHY the change exists or what it fixes, not a
  restatement of the diff. No bullet points.
- Plain US English. No markdown, no code fences, no AI attribution,
  no \"Co-authored-by\", no emoji."
  "System instructions used when writing a commit message."
  :type 'string
  :group 'my/ai)

(declare-function gptel-request "gptel-request" (&optional prompt &rest keys))
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function jj--root      "setup-jj" (&optional dir))
(declare-function jj--read      "setup-jj" (&rest args))
(defvar gptel-backend)


;;; Backend --------------------------------------------------------------------

(defun my/ai--gptel-usable-p ()
  "Whether gptel is present AND has a key it can actually use.

gptel is configured here to read the Anthropic key from auth-source.  A
gptel that loads but has no key fails at request time with a network
error that says nothing about the missing credential, so treat \"no
key\" as \"no gptel\" and fall through to the CLI."
  (and (or (featurep 'gptel) (locate-library "gptel"))
       (require 'gptel nil t)
       (or (getenv "ANTHROPIC_API_KEY")
           (ignore-errors
             (car (auth-source-search :host "api.anthropic.com" :max 1))))
       t))

(defun my/ai--backend ()
  "The backend to use, or nil if nothing is available."
  (or my/ai-backend
      (cond ((my/ai--gptel-usable-p) 'gptel)
            ((and my/ai-cloudcode-executable
                  (file-executable-p my/ai-cloudcode-executable))
             'cloudcode)
            (t nil))))

(defconst my/ai--begin "<<<ANSWER"
  "Opening marker the model is asked to wrap its answer in.")

(defconst my/ai--end "ANSWER>>>"
  "Closing marker the model is asked to wrap its answer in.")

(defun my/ai--strip-ansi (text)
  "Remove ANSI escape sequences from TEXT."
  (replace-regexp-in-string "\033\\[[0-9;?]*[a-zA-Z]" "" text))

(defun my/ai--extract (raw)
  "Pull the answer out of RAW, which may carry a CLI banner and colour."
  (let* ((clean (my/ai--strip-ansi raw))
         (start (string-search my/ai--begin clean)))
    (string-trim
     (if start
         (let* ((from (+ start (length my/ai--begin)))
                (stop (string-search my/ai--end clean from)))
           (substring clean from (or stop (length clean))))
       ;; No markers: drop the CLI's "> agent · model" banner line and hope.
       (replace-regexp-in-string "\\`\\(?:[ \t]*\n\\)*>[^\n]*\n" "" clean)))))

(defun my/ai--cloudcode (prompt callback)
  "Send PROMPT through the CloudCode CLI, calling CALLBACK with the answer."
  (let* ((buf (generate-new-buffer " *ai-cli*"))
         (proc (make-process
                :name "my-ai"
                :buffer buf
                :noquery t
                :connection-type 'pipe
                :command (list my/ai-cloudcode-executable
                               "run" "--title=" prompt)
                :sentinel
                (lambda (proc _event)
                  (unless (process-live-p proc)
                    (let ((raw (with-current-buffer (process-buffer proc)
                                 (buffer-string))))
                      (kill-buffer (process-buffer proc))
                      (if (zerop (process-exit-status proc))
                          (funcall callback (my/ai--extract raw))
                        (message "AI: cloudcode exited %d"
                                 (process-exit-status proc)))))))))
    ;; Close stdin at once.  `cloudcode run' takes its prompt on the command
    ;; line but still reads stdin, so with the pipe left open it waits for
    ;; input that never comes and the request hangs until the timeout below
    ;; kills it -- which looks exactly like a slow model.
    (process-send-eof proc)
    ;; Do not leave a wedged CLI running forever holding a buffer.
    (run-with-timer my/ai-timeout nil
                    (lambda ()
                      (when (process-live-p proc)
                        (message "AI: timed out after %ds" my/ai-timeout)
                        (delete-process proc))))
    proc))

(defun my/ai--gptel (prompt system callback)
  "Send PROMPT with SYSTEM through gptel, calling CALLBACK with the answer."
  (gptel-request prompt
    :system system
    :callback (lambda (response info)
                (if (stringp response)
                    (funcall callback (my/ai--extract response))
                  (message "AI: gptel request failed (%s)"
                           (plist-get info :status))))))

(defun my/ai-request (prompt system callback)
  "Ask the available backend PROMPT under SYSTEM; call CALLBACK with the text."
  (pcase (my/ai--backend)
    ('gptel     (my/ai--gptel prompt system callback))
    ('cloudcode (my/ai--cloudcode
                 ;; The CLI has no system-prompt argument, so fold the
                 ;; instructions in and ask for delimiters we can find again.
                 (format "%s\n\nWrap your entire answer between %s and %s.\n\n%s"
                         system my/ai--begin my/ai--end prompt)
                 callback))
    (_ (user-error
        "No AI backend: gptel has no key and no cloudcode CLI was found"))))


;;; Commit messages ------------------------------------------------------------

(defun my/ai--shell (&rest args)
  "Run ARGS and return stdout, or nil when the command fails."
  (with-temp-buffer
    (when (zerop (apply #'call-process (car args) nil (list t nil) nil (cdr args)))
      (buffer-string))))

(defun my/ai-commit--context ()
  "Return (LABEL . DIFF) describing what is about to be committed.

Looks at jj first: in a jj workspace there is usually a .git directory
too, and asking git about it produces a diff of the wrong thing."
  (let ((default-directory (or (and (fboundp 'jj--root) (jj--root))
                               default-directory)))
    (cond
     ((and (fboundp 'jj--root) (jj--root))
      (cons "jj working copy"
            (or (my/ai--shell "jj" "--ignore-working-copy" "diff" "--git")
                "")))
     (t
      (let ((staged (my/ai--shell "git" "diff" "--cached")))
        (if (and staged (not (string-empty-p (string-trim staged))))
            (cons "staged changes" staged)
          (cons "unstaged changes"
                (or (my/ai--shell "git" "diff") ""))))))))

(defun my/ai-commit--prompt ()
  "The prompt describing the pending change, or nil when there is none."
  (pcase-let ((`(,label . ,diff) (my/ai-commit--context)))
    (when (string-empty-p (string-trim (or diff "")))
      (user-error "Nothing to describe: no %s" label))
    (let* ((files (or (my/ai--shell "sh" "-c"
                                    "git diff --cached --name-only 2>/dev/null || true")
                      ""))
           (trimmed (if (> (length diff) my/ai-diff-limit)
                        (concat (substring diff 0 my/ai-diff-limit)
                                "\n\n[diff truncated]\n")
                      diff)))
      (format "Write the commit message for these %s.\n\nFiles:\n%s\n\nDiff:\n%s"
              label (if (string-empty-p files) "(see diff)" files) trimmed))))

(defun my/ai-commit--replace (message)
  "Put MESSAGE at the top of the current commit buffer.

In a magit commit buffer everything from point-min to the first comment
line is the message; in `*jj describe*' the whole buffer is."
  (save-excursion
    (goto-char (point-min))
    (let ((end (if (derived-mode-p 'git-commit-mode)
                   (or (save-excursion
                         (when (re-search-forward "^#" nil t)
                           (line-beginning-position)))
                       (point-max))
                 (point-max))))
      (delete-region (point-min) end)
      (goto-char (point-min))
      (insert (string-trim message) "\n")
      (when (derived-mode-p 'git-commit-mode) (insert "\n")))))

;;;###autoload
(defun my/ai-commit-message ()
  "Write this commit's message from its diff, using the available AI backend."
  (interactive)
  (let ((prompt (my/ai-commit--prompt))
        (target (current-buffer)))
    (message "AI: writing a commit message via %s..." (my/ai--backend))
    (my/ai-request
     prompt my/ai-commit-instructions
     (lambda (answer)
       (if (not (buffer-live-p target))
           (message "AI: commit buffer is gone; message was:\n%s" answer)
         (with-current-buffer target
           (my/ai-commit--replace answer))
         (message "AI: commit message written"))))))


;;; Asking about code ----------------------------------------------------------

(defconst my/ai--code-instructions
  "You are a careful senior engineer reviewing a colleague's code.
Be concrete and brief. Prefer naming the specific line or construct over
general advice. Plain US English, no markdown headings, no emoji."
  "System instructions for the code questions below.")

(defun my/ai--region-or-defun ()
  "Return (DESCRIPTION . TEXT) for the region, or the surrounding defun."
  (if (use-region-p)
      (cons (format "%s lines %d-%d"
                    (or (buffer-file-name) (buffer-name))
                    (line-number-at-pos (region-beginning))
                    (line-number-at-pos (region-end)))
            (buffer-substring-no-properties (region-beginning) (region-end)))
    (save-excursion
      (let ((beg (progn (beginning-of-defun) (point)))
            (end (progn (end-of-defun) (point))))
        (cons (format "%s (enclosing definition)"
                      (or (buffer-file-name) (buffer-name)))
              (buffer-substring-no-properties beg end))))))

(defun my/ai--show (title text)
  "Show TEXT under TITLE in the AI output buffer."
  (let ((buf (get-buffer-create "*ai*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (concat title "\n") 'face 'bold))
        (insert (propertize (make-string (min 72 (length title)) ?-) 'face 'shadow)
                "\n\n")
        (insert text "\n"))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buf)))

(defun my/ai--ask-about-code (question title)
  "Ask QUESTION about the region or defun and show the reply under TITLE."
  (pcase-let ((`(,what . ,code) (my/ai--region-or-defun)))
    (when (string-empty-p (string-trim code))
      (user-error "Nothing selected and no enclosing definition"))
    (message "AI: asking %s..." (my/ai--backend))
    (my/ai-request
     (format "%s\n\nThis is %s, in %s:\n\n%s"
             question what (symbol-name major-mode) code)
     my/ai--code-instructions
     (lambda (answer) (my/ai--show title answer)))))

;;;###autoload
(defun my/ai-explain-region ()
  "Explain the region, or the definition around point."
  (interactive)
  (my/ai--ask-about-code
   "Explain what this does, and call out anything surprising about it."
   "AI: explanation"))

;;;###autoload
(defun my/ai-review-region ()
  "Review the region, or the definition around point, for problems."
  (interactive)
  (my/ai--ask-about-code
   (concat "Review this for real defects: bugs, wrong edge cases, "
           "performance traps, misuse of the language or its libraries. "
           "If you find nothing serious, say so rather than inventing nits.")
   "AI: review"))

;;;###autoload
(defun my/ai-suggest-improvement ()
  "Suggest how to improve the region, or the definition around point."
  (interactive)
  (my/ai--ask-about-code
   (concat "Suggest the two or three highest-value improvements, most "
           "important first. Show the changed code for each.")
   "AI: suggestions"))


;;; Wiring ---------------------------------------------------------------------

;; magit's commit buffer.
(with-eval-after-load 'git-commit
  (when (boundp 'git-commit-mode-map)
    (define-key git-commit-mode-map (kbd "C-c C-a") #'my/ai-commit-message)))

;; `jj-describe' builds its buffer with `local-set-key' rather than a real
;; mode map, so there is no keymap to extend -- bind into the buffer once it
;; exists instead.
(with-eval-after-load 'setup-jj
  (advice-add 'jj-describe :after
              (lambda (&rest _)
                (let ((buf (get-buffer "*jj describe*")))
                  (when buf
                    (with-current-buffer buf
                      (local-set-key (kbd "C-c C-a") #'my/ai-commit-message)
                      (setq-local header-line-format
                                  (concat (or header-line-format "")
                                          "  C-c C-a AI message"))))))
              '((name . my/ai-bind-in-jj-describe))))

(with-eval-after-load 'setup-jj
  (when (boundp 'jj-command-table)
    (dolist (entry '(("ai: write this commit message"   . my/ai-commit-message)
                     ("ai: explain region or defun"     . my/ai-explain-region)
                     ("ai: review region or defun"      . my/ai-review-region)
                     ("ai: suggest improvements"        . my/ai-suggest-improvement)))
      (unless (rassq (cdr entry) jj-command-table)
        (setq jj-command-table (append jj-command-table (list entry)))))))

;; C-c i, not C-c a: `setup-cursor' already binds C-c a to
;; `mc/mark-all-like-this'.
(defvar my/ai-map (make-sparse-keymap) "Keymap for the AI commands.")
(define-key my/ai-map (kbd "c") #'my/ai-commit-message)
(define-key my/ai-map (kbd "e") #'my/ai-explain-region)
(define-key my/ai-map (kbd "r") #'my/ai-review-region)
(define-key my/ai-map (kbd "s") #'my/ai-suggest-improvement)
(global-set-key (kbd "C-c i") my/ai-map)

(provide 'setup-ai)
;;; setup-ai.el ends here

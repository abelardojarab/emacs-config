;;; setup-cmake.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxcj9063.pdx.intel.com>
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

;; rtags
;; sudo apt-get install libclang-dev / brew install llvm --with-clang
;; git clone --recursive https://github.com/Andersbakken/rtags.git &
;; (LIBCLANG_LLVM_CONFIG_EXECUTABLE=path_to_llvm-config CC=gcc CXX=g++ cmake ../rtags -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=where_to_install_rtags)
;; cmake --build ./ --target install
(use-package rtags
  :defer 2
  :if (executable-find "rdm")
  :bind (:map c++-mode-map
              ("C-c I" . rtags-print-symbol-info)
              ("C-c S" . rtags-find-symbol-at-point))
  :diminish rtags-cmake-mode
  :load-path (lambda () (expand-file-name "rtags/src/" user-emacs-directory))
  :config (progn
            (setq rtags-use-helm t)
            (setq rtags-autostart-diagnostics t)

            ;; Start rdm as a subprocess, with output in a buffer
            (defun my/rtags-start-rdm-maybe ()
              "Start rdm if not already running. Return t if started and nil
otherwise."
              (unless (my/rtags-rdm-running-p)
                (my/rtags-start-rdm-impl nil)
                t))

            (defun my/rtags-rdm-running-p ()
              "Predicate testing if rdm is running"
              (let ((process (get-process "rdm")))
                (or
                 ;; Rdm runs in a process started from Emacs
                 (and (processp process)
                      (not (eq (process-status process) 'exit))
                      (not (eq (process-status process) 'signal)))
                 ;; User has started rdm outside of Emacs
                 ;; Note: sadly this does not work on macOS
                 (let ((uuid (user-uid)))
                   (dolist (pid (reverse (list-system-processes)))
                     (let* ((attrs (process-attributes pid))
                            (pname (cdr (assoc 'comm attrs)))
                            (puid  (cdr (assoc 'euid attrs))))
                       (when (and (eq puid uuid)
                                  (string= pname "rdm"))
                         (return t))))))))

            (defun my/rtags-start-rdm-impl (&optional open-buffer)
              "Start rdm in a subprocess. Open the rdm log buffer if
open-buffer is true."
              (let ((buffer (get-buffer-create "*RTags rdm*")))
                (when open-buffer
                  (switch-to-buffer buffer))
                (with-current-buffer buffer
                  (rtags-rdm-mode)
                  (read-only-mode))
                (let ((process (if my/rtags-rdm-args
                                   (start-process "rdm" buffer "rdm" my/rtags-rdm-args)
                                 (start-process "rdm" buffer "rdm"))))
                  (message "Started rdm - PID %d" (process-id process)))))

            (defun rtags-start ()
              "Start the rdm deamon in a subprocess and display output in a
buffer. Also start the RTag diagostics mode."
              (interactive)
              (setq rtags-autostart-diagnostics t)
              (my/rtags-start-rdm-impl t))

            (defun rtags-stop ()
              "Stop both RTags diagnostics and rdm, if they are running."
              (interactive)
              ;; Stop RTags Diagnostics and kill its buffer without prompt
              (when (and rtags-diagnostics-process
                         (not (eq (process-status rtags-diagnostics-process) 'exit)))
                (kill-process rtags-diagnostics-process))
              (when (get-buffer "*RTags Diagnostics*")
                (let ((kill-buffer-query-functions nil))
                  (kill-buffer "*RTags Diagnostics*")))
              ;; Stop rdm and kill its buffer without prompt
              (rtags-quit-rdm)
              (when (get-buffer "*RTags rdm*")
                (let ((kill-buffer-query-functions nil))
                  (kill-buffer "*RTags rdm*"))))

            (defun rtags-show-rdm-buffer ()
              "Show/hide the rdm log buffer"
              (interactive)
              (let* ((buffer-name "*RTags rdm*")
                     (buffer (get-buffer buffer-name))
                     (window (and buffer (get-buffer-window buffer))))
                (cond (window
                       (bury-buffer buffer)
                       (delete-window window))
                      (buffer
                       (display-buffer buffer))
                      (t
                       (message "rtags rdm is not running (use M-x rtags-start)")))))

;;; Mode for rdm log output
;;; See http://ergoemacs.org/emacs/elisp_syntax_coloring.html

            (defsubst rtags-rdm-record-search-forward (&optional regexp bound)
              "Search forward from point for a log line matching REGEXP.
Set point to the end of the occurrence found, and return point.
An optional second argument BOUND bounds the search: the match
found must not extend after that position. This function also
sets `match-data' to the entire match."
              (let ((org-pos (point)))
                (block while-loop
                  ;; While there are more matches for REGEXP
                  (while (re-search-forward regexp bound t)
                    (if (re-search-backward "^" org-pos t)
                        (let ((begin-pos (point)))
                          ;; If we found a matching log line, set match data and return
                          (if (re-search-forward "$" bound t)
                              (progn
                                (set-match-data (list begin-pos (point)))
                                (return-from while-loop (point)))
                            (return-from while-loop))))))))

            (defun rtags-rdm-match-record-error (bound)
              "Search forward from point to BOUND for error."
              (rtags-rdm-record-search-forward "\\(error:\\)" bound))

            (defun rtags-rdm-match-record-warning (bound)
              "Search forward from point to BOUND for warning."
              (rtags-rdm-record-search-forward "\\(warning:\\)" bound))

            (defun rtags-rdm-match-record-note (bound)
              "Search forward from point to BOUND for note."
              (rtags-rdm-record-search-forward "\\(note:\\)" bound))

            (defun rtags-rdm-match-record-done (bound)
              "Search forward from point to BOUND for Jobs."
              (rtags-rdm-record-search-forward "\\(Jobs\\)" bound))

            (defconst rtags-rdm-mode-keywords
              (list '(rtags-rdm-match-record-error 0 'compilation-error)
                    '(rtags-rdm-match-record-warning 0 'compilation-warning)
                    '(rtags-rdm-match-record-note 0 'compilation-info)
                    '(rtags-rdm-match-record-done 0 'underline))
              "Describes how to syntax highlight keywords in rtags-rdm-mode.")

            (defconst rtags-rdm-mode-syntax-table
              ;; Defines a "comment" as anything that starts with a square bracket, e.g.
              ;; [100%] /path/to/file.cpp in 437ms. (1259 syms, etc) (dirty)
              (let ((synTable (make-syntax-table)))
                (modify-syntax-entry ?\[ "< b" synTable)
                (modify-syntax-entry ?\n "> b" synTable)
                synTable))

            (define-derived-mode rtags-rdm-mode fundamental-mode
              "rdm-log"
              "Mode for viewing rdm logs"
              :syntax-table rtags-rdm-mode-syntax-table
              ;; Syntax highlighting:
              (setq font-lock-defaults '(rtags-rdm-mode-keywords t t)))

            ;; Using the diagnostics buffer
            (defun rtags-show-diagnostics-buffer ()
              "Show/hide the diagnostics buffer in a dedicated
window (similar to `rtags-diagnostics' but without reparsing)."
              (interactive)
              (if (rtags-has-diagnostics)
                  (let* ((buffer-name "*RTags Diagnostics*")
                         (buffer (get-buffer buffer-name))
                         (window (get-buffer-window buffer)))
                    (cond (window
                           (bury-buffer buffer)
                           (delete-window window))
                          (buffer
                           (display-buffer buffer-name)
                           (other-window 1)
                           (goto-char (point-min))
                           (fit-window-to-buffer (get-buffer-window (current-buffer)) 10 2)
                           (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
                           (other-window -1))))
                (message "rtags diagnostics is not running.")))

            (define-key c-mode-base-map [(control c)(r)(d)] 'rtags-show-diagnostics-buffer)

            ;; Used in powerline:
            (defun rtags-diagnostics-has-errors ()
              "Return t or nil depending if RTags diagnostics displays errors"
              (let ((diag-buff (get-buffer "*RTags Diagnostics*")))
                (if (and diag-buff
                         rtags-diagnostics-process
                         (not (eq (process-status rtags-diagnostics-process) 'exit))
                         (not (eq (process-status rtags-diagnostics-process) 'signal)))
                    (> (buffer-size diag-buff) 0)
                  nil)))

            ;; AC source for #include
            ;; The following function fixes a bug in achead:documentation-for-candidate
            (defun my-documentation-for-candidate (candidate)
              "Generate documentation for a candidate `candidate'. For now,
just returns the path and content of the header file which
`candidate' specifies."
              (let ((path
                     (assoc-default candidate achead:ac-latest-results-alist 'string=)))
                (ignore-errors
                  (with-temp-buffer
                    (insert path)
                    (unless (file-directory-p path)
                      (insert "\n--------------------------\n")
                      (insert-file-contents path nil 0 200)) ;; first 200 content bytes
                    (buffer-string)))))

            ;; Ensure rdm is running
            (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
            (add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)

            ;; You may also set these variables:
            ;; - `my/rtags-rdm-args': list of strings containing the arguments to
            ;;   rdm, empty by default. For example:
            ;;   '("--isystem" "/opt/bb/lib64/clang/3.6.2/include -DBAS_NOBBENV")
            ;; - `my/rtags-cmake-build-dir': Path to your build dir, absolute or
            ;;   relative to the root of a project. Default "cmake.bld/<arch>" where
            ;;   <arch> is replaced by the uname of your OS.

            (defvar my/rtags-rdm-args () "List of strings containing the arguments to rdm")
            (defvar my/rtags-cmake-build-dir "cmake_build_dir/" "Path to your build dir, absolute or relative to the root of a project")
            (setq my/rtags-cmake-build-dir "cmake_build_dir/")

            ;; When you open a C++ file, it will detect if your project is CMake-enabled
            ;; by looking for CMakeLists.txt files up to the root of your repo. If it
            ;; finds such a file, it starts rdm if it is not running, and indexes your
            ;; CMake-generated compilation database (e.g. "rd -J <build-dir>").
            ;;
            ;; TODO: it should work as well when creating a new file, by running "cmake"
            ;; on the project to update the complation database. This can be done using an
            ;; after-save-hook I think.

            ;; Note: for now we actually don't need to keep the build-dir of any known
            ;; project but we should need it later to automatically index new files created
            ;; from Emacs.
            (defvar my/rtags-cmake-mode-known-projects ()
              "Alist of known projects, growing as the user opens files. Each
  pair is (project-dir . build-dir). Note that the build-dir
  contains the compilation database.")

            (define-minor-mode rtags-cmake-mode
              "Minor mode for using RTags with zero-configuration, for CMake
enabled projects."
              :lighter "Cm"
              :global  nil ;; buffer local
              (when rtags-cmake-mode
                ;; This is a workaround for C/C++ mode hooks being called twice:
                ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16759
                (unless (local-variable-p 'my/rtags-cmake-mode-buffer-enabled)
                  (setq-local my/rtags-cmake-mode-buffer-enabled t)
                  ;; Check if this is a CMake project
                  (let ((project-dir (my/rtags-cmake-find-buffer-project-root)))
                    (when project-dir
                      ;; Start rdm if needed, and index the project if this is the first
                      ;; time we're seeing a file from the project
                      (let ((buff (current-buffer)))
                        (when (my/rtags-start-rdm-maybe)
                          ;; For some weird reason the buffer is in rdm log mode!
                          (switch-to-buffer buff)
                          (with-current-buffer buff
                            (c++-mode))))
                      (my/rtags-cmake-index-project-maybe project-dir))))))

            (defun my/enable-rtags-cmake-mode ()
              "Enable rtags-cmake-mode for C/C++ buffers."
              (add-hook 'c-mode-hook   #'rtags-cmake-mode)
              (add-hook 'c++-mode-hook #'rtags-cmake-mode))

            ;; Find the (CMake) project root directory
            (defun my/rtags-cmake-find-buffer-project-root ()
              "Check if the current buffer's project is a CMake project, and
  if so return the root directory of the project. Otherwise
  return nil: this is a new buffer not saved or this is not a
  CMake project. Note that this function stops the search as soon
  as it finds a .git subdirectory."
              (let ((filename (buffer-file-name)))
                (when (and filename (file-exists-p filename))
                  (let ((dir (file-name-directory filename)))
                    (my/rtags-cmake-find-project-root-impl dir nil)))))

            (defun my/rtags-cmake-find-project-root-impl (dir &optional is-cmake)
              "Return the path of the git repo containing the specified dir,
if this is a CMake repo, otherwise return nil. The extra argument
is-cmake is for recursion."
              (unless is-cmake
                (setq is-cmake (file-exists-p (concat dir "CMakeLists.txt"))))
              (cond ((string= dir "/")
                     ;; stop: we've reached the root directory
                     nil)
                    ((file-exists-p (concat dir ".git"))
                     ;; stop: we've reached the root of the repo
                     (if is-cmake dir nil))
                    (t
                     ;; try the parent directory
                     (my/rtags-cmake-find-project-root-impl (my/parent-directory dir)
                                                            is-cmake))))

            ;; Index the project
            (defun my/rtags-cmake-index-project-maybe (project-dir)
              "Index the project at project-dir unless we have done that before."
              (unless (assoc project-dir my/rtags-cmake-mode-known-projects)
                ;; Find the build dir containing the compilation DB
                (let ((build-dir (my/rtags-cmake-get-build-dir project-dir)))
                  (if (not (file-exists-p (concat (file-name-as-directory build-dir)
                                                  "compile_commands.json")))
                      (message "rtags can't find compilation DB in: %s" build-dir)
                    ;; Tell rdm to index the compilation DB and add this project to the
                    ;; list of projects that have been indexed already
                    (my/rtags-cmake-index-project-impl build-dir)
                    (setq my/rtags-cmake-mode-known-projects
                          (cons (cons project-dir build-dir)
                                my/rtags-cmake-mode-known-projects))))))

            (defun my/rtags-cmake-index-project-impl (comp-db-dir)
              "Invoke rc -J <comp-db-dir>"
              (message "Rtags indexing: %s" comp-db-dir)
              (with-current-buffer (rtags-get-buffer)
                (rtags-call-rc "-J" comp-db-dir)))

            ;; Find the compilation database (e.g. the build directory)
            (defun my/rtags-cmake-get-build-dir (project-dir)
              "Return the absolute path of the build directory for the
project located in the specified project-dir. If a file .rtags
exist at project-dir, it is expected to contain a record 'build
<dir>'. Otherwise the pref `expordium-rtags-cmake-build-dir' is
assumed to contain the relative path of any project's build
directory."
              (let* ((rtags-file (concat project-dir ".rtags"))
                     (build-dir
                      (if (file-exists-p rtags-file)
                          (or (my/rtags-cmake-get-key-from-file rtags-file "build") "")
                        (my/rtags-cmake-get-expanded-build-dir))))
                (if (string-prefix-p "/" build-dir)
                    build-dir ;; already an absolute path
                  (concat project-dir build-dir))))

            (defun my/rtags-cmake-get-key-from-file (file key)
              "Read the specified file and return the value associated with
the specified key, or nil if no such key."
              (let* ((records (my/read-file-lines file))
                     (matching-records (remove-if-not #'(lambda (record)
                                                          (string-prefix-p key record))
                                                      records)))
                (when matching-records
                  (second (split-string (car matching-records) " ")))))

            (defun my/rtags-cmake-get-expanded-build-dir ()
              "Return the value of the pref `my/rtags-cmake-build-dir'
  with any occurence of <arch> replaced by the uname of the local
  OS."
              (let ((uname (replace-regexp-in-string "\n$" "" (shell-command-to-string "uname"))))
                (replace-regexp-in-string "<arch>" uname my/rtags-cmake-build-dir)))

            ;; Enable hooks
            (my/enable-rtags-cmake-mode)

            ;; This module provides a single command, `rtags-create-compilation-database',
            ;; which is an easy way to generate a CLang compilation database
            ;; (`compile_commands.json') for non-CMake projects.
            ;;
            ;; The first step is to create a file `compile_includes' in the project root
            ;; dir, which specifies how to compile your project and in particular where
            ;; are all the source files and all the include files. For example:
            ;;
            ;;   # Where are the source files (there could be multiple directories).
            ;;   # We will scan recursively any subdirectories that do not match any
            ;;   # 'exclude' regex.
            ;;   src .
            ;;
            ;;   # What to put in -I directives (in addition to the source files above).
            ;;   # We will scan recursively any subdirectories that do not match any
            ;;   # 'exclude' regex.
            ;;   include /Users/phil/Code/cpp/include/bsl
            ;;   include /Users/phil/Code/cpp/include/bdl
            ;;
            ;;   # Optional: patterns to exclude in -I directives and for looking for
            ;;   # sources:
            ;;   exclude /test$
            ;;   exclude /doc$
            ;;   exclude /group$
            ;;   exclude /package$
            ;;
            ;;   # Optional: if any file name pattern must be excluded from the "src" files,
            ;;   # use the "excludesrc" directive. For example this will exclude all test
            ;;   # drivers:
            ;;   excludesrc \.t\.cpp$
            ;;
            ;; In addition, the creation of a compilation database uses these variables:
            ;;
            ;; - `rtags-compile-includes-base-dir': set this to your workspace path
            ;;   if you want to use relative paths in `compile_includes' (by default any
            ;;   relative path in this file is relative to the project root dir).
            ;; - `rtags-clang-command-prefix': default is "/usr/bin/clang++ -Irelative"
            ;;   (Note that rtags ignores the clang++ command because it uses libclang).
            ;; - `rtags-clang-command-suffix': default is "-c -o".
            ;;
            ;; Once you have created the `compile_includes' file, run the command
            ;; M-x `rtags-create-compilation-database'. It will:
            ;;
            ;; - Prompt for the project root dir
            ;; - Scan all source dirs and include dirs
            ;; - Create `compilation_database.json' (it overwrites without asking)
            ;; - Ask if you want to reload it (if rdm is running).

            ;; Override these variables in your .emacs as needed:
            (defvar rtags-clang-command-prefix
              "/usr/bin/clang++ "
              "Compilation command prefix to use for creating compilation
  databases. Override this variable for your local environment.")

            (defvar rtags-clang-command-suffix
              " -c -o "
              "Compilation command suffix to use for creating compilation
  databases. Override this variable for you local environment.")

            (defvar rtags-compile-includes-base-dir
              nil
              "If non-nil, base directory to use for all relative paths in
  `compile_include'. Use nil for absolute paths.")

            ;; Creating a compilation DB
            (defun rtags-load-compile-includes-file-content (compile-includes-file)
              "Read and parse the specified compile-includes file, and return
a list of five sublists:
- The list of `src' directives,
- The list of `include' directives,
- The list of `exclude' directives,
- The list of `excludesrc' directives,
- The list of `macro' directives."
              (let ((line-number      1)
                    (value            nil)
                    (src-list         ())
                    (include-list     ())
                    (exclude-list     ())
                    (exclude-src-list ())
                    (macro-list       ()))
                (dolist (record (my/read-file-lines compile-includes-file))
                  (incf line-number)
                  (setq value (second (split-string record " ")))
                  (cond ((or (eq "" record)
                             (string-prefix-p "#" record))
                         ;; Comment or empty string; skip it
                         nil)
                        ((string-prefix-p "src" record)
                         (when value
                           (setq src-list (cons value src-list))))
                        ((string-prefix-p "include" record)
                         (when value
                           (setq include-list (cons value include-list))))
                        ((string-prefix-p "excludesrc" record)
                         (when value
                           (setq exclude-src-list (cons value exclude-src-list))))
                        ((string-prefix-p "exclude" record)
                         (when value
                           (setq exclude-list (cons value exclude-list))))
                        ((string-prefix-p "macro" record)
                         (when value
                           (setq macro-list (cons value macro-list))))
                        (t
                         (error "Syntax error line %d: %s" line-number record))))
                (list src-list include-list exclude-list exclude-src-list macro-list)))

            (defun rtags-is-excluded-p (path excluded-regexs)
              "Return non-nil if the specified path matches any regex in
the list of excluded regexs"
              (catch 'return
                (dolist (excluded excluded-regexs)
                  (when (string-match excluded path)
                    (throw 'return t)))
                (throw 'return nil)))

            (defun rtags-directory-contains-sources-p (path)
              "Return non-nil if the specified path contains any C/C++ source
  or header file"
              (directory-files path nil ".*\\.\\(c\\|cpp\\|h\\|hpp\\)$" nil))

            (defun rtags-scan-subdirectories (dir excluded-regexs)
              "Return a list of subdirectories under the specified root dir,
excluding any that match any regex in the specified excluded
regex list."
              (let ((result ()))
                (dolist (subdir (cons dir (my/directory-tree dir)))
                  (when (and (rtags-directory-contains-sources-p subdir)
                             (not (rtags-is-excluded-p subdir excluded-regexs)))
                    (setq result (cons subdir result))))
                result))

            (defun rtags-load-compile-includes-file (dir)
              "Loads the `compile_includes' file from the specified directory
and returns its content as a property list, or nil if the file
could not be loaded. The property list looks like this:
'(:src-dirs (...)
  :include-dirs (...)
  :exclude-src (...)
  :macros (...))"
              (let ((compile-includes-file (concat (file-name-as-directory dir)
                                                   "compile_includes")))
                (cond ((file-exists-p compile-includes-file)
                       ;; Parse the file and return 3 lists: src, include, exclude
                       (let ((directives (rtags-load-compile-includes-file-content
                                          compile-includes-file)))
                         (let ((src-dirs    (first directives))
                               (incl-dirs   (second directives))
                               (excl-regexs (third directives))
                               (excl-src    (fourth directives))
                               (macros      (fifth directives))
                               (result      ()))
                           ;; Scan src to get all subdirs that do not match the excludes
                           (let (dirs)
                             (dolist (path src-dirs)
                               (unless (file-name-absolute-p path)
                                 (setq path (expand-file-name path
                                                              (or rtags-compile-includes-base-dir
                                                                  dir))))
                               (message "rtags is scanning source dir: %s ..." path)
                               (setq dirs (nconc dirs (rtags-scan-subdirectories path excl-regexs))))
                             (setq result (list :src-dirs dirs)))
                           ;; Same with includes
                           (let (dirs)
                             (dolist (path incl-dirs)
                               (setq path (expand-file-name path rtags-compile-includes-base-dir))
                               (message "rtags is scanning include dir: %s ..." path)
                               (setq dirs (nconc dirs (rtags-scan-subdirectories path excl-regexs))))
                             (setq result (nconc result (list :include-dirs dirs))))
                           ;; Add exclude-src and macros into the result
                           (setq result (nconc result (list :exclude-src excl-src
                                                            :macros macros)))
                           ;; Done
                           (message "Project has %d source dirs and %d include dirs"
                                    (length (plist-get result :src-dirs))
                                    (length (plist-get result :include-dirs)))
                           result)))
                      (t
                       (message "No compilation_includes file")
                       nil))))

            (defun rtags-create-compilation-command (plist)
              "Returns a string containing the clang compilation command to
use for the compilation database, using the content of PLIST."
              (let ((command rtags-clang-command-prefix))
                ;; -D options:
                (dolist (m (plist-get plist :macros))
                  (setq command (concat command " -D" m)))
                ;; -I options
                (dolist (path (plist-get plist :src-dirs))
                  (setq command (concat command " -I" path)))
                (dolist (path (plist-get plist :include-dirs))
                  (setq command (concat command " -I" path)))
                (concat command rtags-clang-command-suffix)))

            (defun rtags-prompt-compilation-database-dir ()
              "Prompts the user for the directory where to generate the
compilation database. If we're in a projectile project, propose
the project root first, and prompt for a dir if the user
declines. Returns the directory string."
              (let ((project-root (and (featurep 'projectile)
                                       (projectile-project-root))))
                (if (and project-root
                         (y-or-n-p (format "Create at project root (%s)?" project-root)))
                    project-root
                  (read-directory-name "Project root: "))))

            (defun rtags-create-compilation-database (dir)
              "Regenerates `compile_commands.json' from `compile_includes' in
the specified directory."
              (interactive (list (rtags-prompt-compilation-database-dir)))
              (let ((plist (rtags-load-compile-includes-file dir)))
                (when plist
                  (let ((dbfilename (concat (file-name-as-directory dir)
                                            "compile_commands.json"))
                        (compile-command (rtags-create-compilation-command plist))
                        (exclude-files (plist-get plist :exclude-src))
                        (num-files 0))
                    (with-temp-buffer
                      (insert "[")
                      (newline)
                      ;; Note: dynamic binding of variable default-directory
                      (dolist (default-directory (plist-get plist :src-dirs))
                        (message "rtags is processing directory: %s ..." default-directory)
                        (let ((files (mapcan #'file-expand-wildcards
                                             my/rtags-source-file-extensions))
                              ;; rdm does not like directories starting with "~/"
                              (dirname (if (string-prefix-p "~/" default-directory)
                                           (substitute-in-file-name
                                            (concat "$HOME/" (substring default-directory 2)))
                                         default-directory)))
                          (dolist (file files)
                            (unless (rtags-is-excluded-p file exclude-files)
                              (incf num-files)
                              (insert "  { \"directory\": \"" dirname "\",")
                              (newline)
                              (insert "    \"command\":   \""
                                      compile-command
                                      (file-name-sans-extension file) ".o "
                                      file "\",")
                              (newline)
                              (insert "    \"file\":      \"" file "\" },")
                              (newline)))))
                      (insert "];")
                      (newline)
                      (write-region (buffer-string) nil dbfilename))
                    (when (yes-or-no-p
                           (format "Wrote compile_commands.json (%d files). Reload it?" num-files))
                      ;; FIXME: rtags-call-rc does not work if you don't specify a current buffer?
                      ;; That seems broken.
                      (rtags-call-rc :path t :output nil :unsaved (current-buffer) "-J" dir)
                      (message "Reloaded (check rdm's logs)"))))))

            ;; Mode for compile_includes files

            (defconst rtags-compile-includes-mode-keywords
              ;; Words and associated face.
              `(( "\\(^src\\|^include\\|^excludesrc\\|^exclude\\|^macro\\)"
                  . font-lock-keyword-face)))

            (defconst rtags-compile-includes-mode-syntax-table
              ;; Defines a "comment" as anything that starts with hash tag
              (let ((synTable (make-syntax-table)))
                (modify-syntax-entry ?\# "< b" synTable)
                (modify-syntax-entry ?\n "> b" synTable)
                synTable))

            (define-derived-mode rtags-compile-includes-mode fundamental-mode
              "compile-includes"
              "Mode for editing compile_includes files"
              :syntax-table rtags-compile-includes-mode-syntax-table
              ;; Syntax highlighting:
              (setq font-lock-defaults '((rtags-compile-includes-mode-keywords))))

            (add-to-list 'auto-mode-alist
                         '("compile_includes" . rtags-compile-includes-mode))))

;; cmake syntax highlighting
(use-package cmake-mode
  :defer t
  :commands cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :load-path (lambda () (expand-file-name "cmake-mode/" user-emacs-directory)))

;; cmake-based IDE
(use-package cmake-ide
  :defer 2
  :if (executable-find "cmake")
  :commands (use-cmake-ide cmake-ide--locate-cmakelists)
  :after (rtags flycheck)
  :load-path (lambda () (expand-file-name "cmake-ide/" user-emacs-directory))
  :init (if (executable-find "cmake")
            (add-hook 'c-mode-common-hook #'use-cmake-ide))
  :config (progn
            (defun use-cmake-ide ()
              (cmake-ide-setup)
              (if (cmake-ide--locate-cmakelists)
                  (setq cmake-ide-build-dir (concat (cmake-ide--locate-cmakelists) "cmake_build_dir/"))))))

;; minor-mode integrating the CMake build process
(use-package cmake-project
  :after cmake-ide
  :if (executable-find "cmake")
  :load-path (lambda () (expand-file-name "cmake-project/" user-emacs-directory))
  :diminish cmake-project-mode
  :config (progn
            (if (cmake-ide--locate-cmakelists)
                (setq-default cmake-project-build-directory (concat
                                                             (cmake-project-find-root-directory)
                                                             "cmake_build_dir/"))
              (setq cmake-project-default-build-dir-name "cmake_build_dir/"))

            (defun cmake-project-hook ()
              (when (cmake-ide--locate-cmakelists)
                (if (not (file-exists-p cmake-project-default-build-dir-name))
                    (make-directory cmake-project-default-build-dir-name) t)
                (cmake-project-mode)))

            (add-hook 'c-common-mode-hook 'cmake-project-hook)))

;; EDE project managment, slows down Emacs
(global-ede-mode 1)
(ede-enable-generic-projects)
(setq ede-project-directories t)

;; Default EDE directory
(setq-default ede-project-placeholder-cache-file "~/.emacs.cache/ede-projects.el")

;; Redefine ede add projects to avoid errors
(defun ede-add-project-to-global-list (proj)
  "Add the project PROJ to the master list of projects.
On success, return the added project."
  (ignore-errors
    (when (not proj)
      (error "No project created to add to master list"))
    (when (not (eieio-object-p proj))
      (error "Attempt to add non-object to master project list"))
    (when (not (obj-of-class-p proj ede-project-placeholder))
      (error "Attempt to add a non-project to the ede projects list"))
    (add-to-list 'ede-projects proj))
  proj)

;; Enable the wrapper for compilation database projects
(use-package ede-compdb
  :load-path (lambda () (expand-file-name "ede-compdb/" user-emacs-directory)))

;; Extensions to Emacs Development Environment for use with CMake-based projects
(use-package ede-cmake
  :load-path (lambda () (expand-file-name "ede-cmake/" user-emacs-directory)))

(provide 'setup-cmake)
;;; setup-cmake.el ends here

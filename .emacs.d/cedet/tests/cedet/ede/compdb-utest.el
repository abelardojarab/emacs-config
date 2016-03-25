;; ede/compdb-test.el --- ede/compdb unit tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014 Alastair Rankine

;; Author: Alastair Rankine <alastair@girtby.net>
;; Keywords: development ninja build cedet ede

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the documentation at https://github.com/randomphrase/ede-compdb for
;; quickstart and usage information

;;; Code:

(require 'ert)
(require 'semantic)
(require 'cl-lib)
(require 'rx)
(require 'inversion)
(require 'ede/compdb)

;; Need to have EDE and semantic automatically enabled for new buffers
;(semantic-mode t)
(global-ede-mode t)

;; We test with .ipp files - map to c++ mode so they can be parsed correctly
;; TODO: remove this dependency
(add-to-list 'auto-mode-alist '("\\.ipp$" . c++-mode))

;;; Test helpers

(defun compdb-utest ()
  (ert-run-tests-batch "ede-compdb-.*"))

(defconst cmake-generate-compdb-args
  '("-DCMAKE_EXPORT_COMPILE_COMMANDS=1")
  "Arguments to cmake to generate compilation database.")

(defconst cmake-generate-ninja-args
  '("-G" "Ninja")
  "Arguments to cmake to generate ninja build files.")

(defconst cmake-version-rx
  (rx "cmake version "
      (group
       (+ (char digit))
       "."
       (+ (char digit))
       (zero-or-one
        "."
        (+ (char digit)))
       )))

(defvar ede-compdb-test-srcdir
  (file-name-as-directory
   (expand-file-name "src/compdb/utest" (when load-file-name (file-name-directory load-file-name)))))

(defvar ede-compdb-test-cmake-path
  (executable-find "cmake")
  "Set to the path for the cmake tool, or nil if not available")

(defun invoke-cmake (srcdir builddir &rest args)
  "Invoke cmake on SRCDIR to build into BUILDDIR with ARGS."
  (with-current-buffer (get-buffer-create "*ede-compdb-test*")
    (erase-buffer)
    (insert (format "-*- default-directory: %s -*-\n" builddir))
    (let* ((default-directory builddir)
           (ret (apply 'call-process `("cmake" nil t t ,@args ,srcdir))))
      (when (> 0 ret)
        (error "Error running CMake: error %d" ret)))
    ))

(defun cmake-version ()
  "Returns the version of CMake installed."
  (let ((l (and ede-compdb-test-cmake-path
                (car (process-lines ede-compdb-test-cmake-path "--version")))))
    (when (and l (string-match cmake-version-rx l))
      (inversion-decode-version (match-string 1 l))
      )))

(defun sleep-until-compilation-done ()
  "Watches the *compilation* buffer and blocks until its process is complete."
  (let* ((comp-buf (get-buffer "*compilation*"))
         (comp-proc (get-buffer-process comp-buf)))
    (while comp-proc
      (sleep-for 1)
      (setq comp-proc (get-buffer-process comp-buf)))))

(defmacro with-temp-directory (dir &rest body)
  "Create DIR as a temporary directory and invoke BODY.  The temporary directory will be deleted on exit/error."
  (declare (indent 1))
  `(let ((,dir (file-name-as-directory (make-temp-file "build-" t))))
     (unwind-protect
         ,@body
       (progn
         (delete-directory ,dir t)
         (ede-flush-deleted-projects)))))
(def-edebug-spec with-temp-directory (sexp body))

(defmacro with-temp-file-buffer (buf file &rest body)
  "Open a buffer with symbol BUF from FILE and ensure it is killed after BODY is evaluated."
  (declare (indent 2))
  `(let ((,buf (find-file-noselect ,file)))
     (unwind-protect
         (with-current-buffer ,buf
           ,@body
           )
       (kill-buffer ,buf))))
(def-edebug-spec with-temp-file-buffer (sexp form body))

(defmacro with-cmake-build-directory (dir &rest body)
  ;; TODO: indent?
  (let (cmake-args)
    ;; TODO: remove duplicate code here **
    (when (eq :generate-compdb (car body))
      (setq cmake-args (append cmake-args cmake-generate-compdb-args))
      (setq body (cdr body)))
    (when (eq :generate-ninja (car body))
      (setq cmake-args (append cmake-args cmake-generate-ninja-args))
      (setq body (cdr body)))
    `(with-temp-directory ,dir
      (should (file-exists-p ,dir))
      (invoke-cmake ede-compdb-test-srcdir ,dir ,@cmake-args)
      ,@body
     )))
(def-edebug-spec with-cmake-build-directory (sexp body))

(defmacro with-insource-build (dir &rest body)
  "Sets up a source tree in a temporary directory DIR for an in-source build"
  ;; TODO: indent?
  (let (cmake-args)
    ;; TODO: remove duplicate code here **
    (when (eq :generate-compdb (car body))
      (setq cmake-args (append cmake-args cmake-generate-compdb-args))
      (setq body (cdr body)))
    (when (eq :generate-ninja (car body))
      (setq cmake-args (append cmake-args cmake-generate-ninja-args))
      (setq body (cdr body)))
    `(with-temp-directory
      ,dir
      (copy-directory ede-compdb-test-srcdir ,dir)
      (setq ,dir (file-name-as-directory (concat ,dir ,(file-name-nondirectory
                                                        (directory-file-name ede-compdb-test-srcdir)))))
      (invoke-cmake "." ,dir ,@cmake-args)
      ,@body
      )))
(def-edebug-spec with-insource-build (sexp body))

(defmacro with-temp-ede-project (proj def &rest body)
  "Bind PROJ to DEF and add it to the global list temporarily."
  (declare (indent 2))
  `(let ((,proj (ede-add-project-to-global-list ,def)))
     (unwind-protect
         (progn ,@body)
       (ede-delete-project-from-global-list ,proj))))
(def-edebug-spec with-temp-ede-project (place sexp body))

;;; ede-compdb-entry tests

(ert-deftest ede-compdb-parse-command-line ()
  "Tests parsing of command lines"
  (cl-letf*
      ((cmdline "g++ -Dfoo -Dbar=baz -Uqux -isystem =/opt/quxx/include -I/opt/local/include -include bar.hpp -imacros config.h -isystem/opt/foo/include -iquote includes --sysroot=/sysroot main.cpp")
       (e (compdb-entry "foo.cpp" :directory "." :command-line cmdline))

       ;; mock out ede-compdb-compiler-include-path
       ((symbol-function 'ede-compdb-compiler-include-path) (lambda (comp dir) '("/opt/g++/include"))))

    (should (equal cmdline (get-command-line e)))

    (should (equal '(("foo") ("bar" . "baz")) (oref e defines)))
    (should (equal '("qux") (oref e undefines)))

    (should (equal '("foo" "bar=baz") (get-defines e)))

    (should (equal
             `("." "/opt/local/include/" "/sysroot/opt/quxx/include/" "/opt/foo/include/")
             (get-system-include-path e t)))

    (should (equal
             `("." ,(expand-file-name "includes/") "/opt/local/include/" "/sysroot/opt/quxx/include/" "/opt/foo/include/")
             (get-user-include-path e t)))

    (should (equal (mapcar #'expand-file-name '("config.h" "bar.hpp")) (get-includes e)))
    ))

(ert-deftest ede-compdb-parse-compiler-includes ()
  "Tests discovery of compiler include paths"
  (with-temp-buffer
    ;; This is a real example from Apple Clang 5.0
    (insert "clang -cc1 version 5.0 based upon LLVM 3.3svn default target x86_64-apple-darwin13.1.0
ignoring nonexistent directory \"/usr/include/c++/v1\"
ignoring nonexistent directory \"/usr/local/include\"
#include \"...\" search starts here:
#include <...> search starts here:
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/c++/v1
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/5.0/include
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
 /usr/include
 /System/Library/Frameworks (framework directory)
 /Library/Frameworks (framework directory)
End of search list.
")
    (goto-char (point-min))

    (should (equal (ede-compdb-parse-compiler-includes)
                   '("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/c++/v1"
                     "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/5.0/include"
                     "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
                     "/usr/include")))
    ))

;;; ede-compdb tests

(ert-deftest ede-compdb-empty-build-dir ()
  "Tests that we can still open files when the build directory can't be located, or is empty"
  (with-temp-directory tmpdir
    (with-temp-ede-project
        proj (ede-compdb-project "TESTPROJ"
                                 :compdb-file (expand-file-name "compile_commands.json" tmpdir)
                                 :file (expand-file-name "CMakeLists.txt" ede-compdb-test-srcdir))

      (should (eq proj (ede-directory-get-open-project ede-compdb-test-srcdir)))

      (with-temp-file-buffer buf (expand-file-name "main.cpp" ede-compdb-test-srcdir)
        ;; Should have set up the current project and target
        (should (eq proj (ede-current-project)))
        (should (not (oref ede-object compilation)))

        ;; Should have been parsed
        (should (semantic-active-p))
        ))))

(ert-deftest ede-compdb-open-file-parsed ()
  "Tests the parsing of source files in a project. We ensure it correctly locates all include files, amongst other things."
  :expected-result (if ede-compdb-test-cmake-path :passed :failed)
  (with-cmake-build-directory
   builddir :generate-compdb
   (let* ((testdir ede-compdb-test-srcdir)
          (maincpp (expand-file-name "main.cpp" testdir))
          (worldcpp (expand-file-name "world/world.cpp" testdir))
          (utilityhpp (expand-file-name "utility/utility.hpp" testdir))
          (testbufs))

     (with-temp-ede-project
      proj (ede-compdb-project "TESTPROJ"
                               :compdb-file (expand-file-name "compile_commands.json" builddir)
                               :file (expand-file-name "CMakeLists.txt" testdir))

      ;; Basic sanity checks on the project itself
      (should (eq proj (ede-directory-get-open-project testdir)))
      (should (gethash (file-truename maincpp) (oref proj compdb)))

      ;; Now we'll open source files and check that they are parsed correctly
      (unwind-protect
          (progn
            (let ((buf (find-file-noselect maincpp)))
              (setq testbufs (cons buf testbufs))
              (with-current-buffer buf
                ;; Should have set up the current project and target
                (should ede-object)
                (should (eq proj (ede-current-project)))
                (should (oref ede-object compilation))

                ;; Include path should include certain dirs:
                (let ((P (ede-system-include-path ede-object)))
                  (should (member (file-name-as-directory (expand-file-name "world" testdir)) P))
                  (should (or (member builddir P) (member (file-truename builddir) P)))
                  )

                ;; Should have been parsed
                (should (semantic-active-p))
                (should (not semantic-parser-warnings))

                (let* ((tags (semantic-fetch-tags))
                       (includes (semantic-find-tags-included tags))
                       (funcs (semantic-find-tags-by-class 'function tags)))
                  ;; All includes should be parsed
                  (should includes)
                  (dolist (inc includes)
                    (should (semantic-dependency-tag-file inc)))

                  ;; These function names are defined using macros, so shouldn't be visible unless we
                  ;; have parsed the preprocessor map correctly
                  ;; (should (semantic-find-tags-by-name "HelloFoo" funcs))
                  ;; (should (semantic-find-tags-by-name "HelloBar" funcs))
                  ;; (should (semantic-find-tags-by-name "HelloBaz" funcs))
                  )
                ))

            ;; All of the world.[chi]pp files should also be parsed
            (dolist (EXT '(".cpp" ".hpp" ".ipp"))
              (let* ((filepath (expand-file-name (concat "world/world" EXT) testdir))
                     (buf (find-file-noselect filepath)))
                (setq testbufs (cons buf testbufs))
                (with-current-buffer buf
                  (should (eq proj (ede-current-project)))
                  (should (semantic-active-p))
                  (should (not semantic-parser-warnings))

                  ;; Compilation should be pointed to world.cpp
                  (should (eq (oref ede-object compilation)
                              (gethash (file-truename worldcpp) (oref proj compdb))))
                  )))

            ;; Try a header with no matching source file
            (let ((buf (find-file-noselect utilityhpp)))
              (setq testbufs (cons buf testbufs))
              (with-current-buffer buf
                (should (eq proj (ede-current-project)))
                (should (semantic-active-p))
                (should (not semantic-parser-warnings))

                ;; Compilation should be pointed to main.cpp
                (should (eq (oref ede-object compilation)
                            (gethash (file-truename maincpp) (oref proj compdb))))
                ))

            ;; Try a generated source file
            (let ((buf (find-file-noselect (expand-file-name "build_type.cpp" builddir))))
              (setq testbufs (cons buf testbufs))
              (with-current-buffer buf
                ;; FIXME: Should have set up the current project and target with compilation
                ;; (should (eq proj (ede-current-project)))
                ;; (should (oref ede-object compilation))

                ;; FIXME: should have been parsed
                ;; (should (semantic-active-p))
                ;; (should (not semantic-parser-warnings))
                ))

            ;; Force a rescan with all these buffers open, just to make sure it works
            (with-current-buffer (car testbufs)
              (let (hookrun)

                ;; Add a local hook so it will go away when we close the buffer
                (add-hook 'ede-compdb-project-rescan-hook (lambda () (setq hookrun t)) nil t)

                (ede-rescan-toplevel)

                ;; Check that the rescan hook is run
                (should hookrun)))

            ;; All compilation entries should have been updated
            ;; FIXME: need ede-object for all buffers, including above
            (with-current-buffer (get-file-buffer maincpp)
              ;; Check that compilation entry has been updated
              (should (eq (oref ede-object compilation)
                          (gethash (file-truename buffer-file-name) (oref proj compdb)))))

            ;; Close all buffers and check we can still rescan
            (while testbufs
              (kill-buffer (pop testbufs)))
            (project-rescan proj)

            )
        (while testbufs
          (kill-buffer (pop testbufs)))
        )))))

(ert-deftest ede-compdb-compiler-include-path-cache ()
  "Tests that the compiler include paths are detected."
  :expected-result (if ede-compdb-test-cmake-path :passed :failed)
  (with-cmake-build-directory
   builddir :generate-compdb
   (cl-letf
       ((ede-compdb-compiler-cache nil))

     (with-temp-ede-project
      proj (ede-compdb-project "TESTPROJ"
                               :compdb-file (expand-file-name "compile_commands.json" builddir)
                               :file (expand-file-name "CMakeLists.txt" ede-compdb-test-srcdir))

      (should (listp ede-compdb-compiler-cache))
      ;; Check that compiler includes are present in the project includes
      (when (car ede-compdb-compiler-cache)
        (let* ((maincpp (expand-file-name "main.cpp" testdir))
               (buf (find-file-noselect maincpp)))
          (unwind-protect
              (let* ((target (ede-find-target proj buf))
                     (entry (oref target compilation))
                     (compiler (oref entry compiler)))
                (should (memq (cdr (assoc compiler ede-compdb-compiler-cache)) (oref entry include-path))))
            (kill-buffer buf))))
      ))))

(ert-deftest ede-compdb-multiple-configuration-directories ()
  "Tests that we can track multiple configuration directories. We create two projects, Debug and Release, and check that they can both build"
  :expected-result (if ede-compdb-test-cmake-path :passed :failed)
  (with-temp-directory
   builddir
   (let ((dbgdir (file-name-as-directory (concat builddir "debug")))
         (reldir (file-name-as-directory (concat builddir "release")))
         (custdir (file-name-as-directory (concat builddir "custom"))))
     (make-directory dbgdir)
     (make-directory reldir)
     (make-directory custdir)

     (with-temp-buffer
       (apply 'invoke-cmake `(,ede-compdb-test-srcdir ,dbgdir "-DCMAKE_BUILD_TYPE=Debug" ,@cmake-generate-compdb-args))
       (apply 'invoke-cmake `(,ede-compdb-test-srcdir ,reldir "-DCMAKE_BUILD_TYPE=Release" ,@cmake-generate-compdb-args))
       (apply 'invoke-cmake `(,ede-compdb-test-srcdir ,custdir "-DCMAKE_BUILD_TYPE=Release" ,@cmake-generate-compdb-args))

       (let ((proj (ede-compdb-project
                    "TESTPROJ"
                    :compdb-file "compile_commands.json"
                    :file (expand-file-name "CMakeLists.txt" ede-compdb-test-srcdir)
                    :configuration-directories (list dbgdir reldir))))

         (should (file-exists-p dbgdir))
         (project-compile-project proj)
         (sleep-until-compilation-done)
         (should (file-executable-p (concat dbgdir "hello")))

         (oset proj configuration-default "release")
         (should (file-directory-p reldir))
         (project-compile-project proj)
         (sleep-until-compilation-done)
         (should (file-executable-p (concat reldir "hello")))

         ;; Set the current configuration directory to custdir
         (ede-compdb-set-configuration-directory custdir proj)
         (project-compile-project proj)
         (sleep-until-compilation-done)
         (should (file-executable-p (concat custdir "hello")))

         )))))

(ert-deftest ede-compdb-autoload-project ()
  "Tests that we can autoload a project depending on the presence of a compilation database file"
  :expected-result (if ede-compdb-test-cmake-path :passed :failed)
  (with-insource-build
   dir :generate-compdb
   (dolist (f '("main.cpp" "world/world.cpp"))
     (with-temp-file-buffer buf (expand-file-name f dir)
       ;; Should have set up the current project and target
       (should (ede-current-project))
       (should ede-object)
       (should (oref ede-object compilation)))
     )))

(ert-deftest ede-compdb-flymake-init-test ()
  "Tests that `ede-compdb-flymake-init' works correctly."
  :expected-result (if ede-compdb-test-cmake-path :passed :failed)
  (with-cmake-build-directory
   builddir :generate-compdb
   (require 'flymake)
   (cl-letf*
       (((symbol-function 'flymake-init-create-temp-buffer-copy) (lambda (f) "dummyfile.cpp"))
        (maincpp (expand-file-name "main.cpp" ede-compdb-test-srcdir)))

     (with-temp-ede-project
      proj (ede-compdb-project "TESTPROJ"
                               :compdb-file (expand-file-name "compile_commands.json" builddir)
                               :file (expand-file-name "CMakeLists.txt" ede-compdb-test-srcdir))

      (with-temp-file-buffer buf maincpp

        ;; Should have set up the current project and target
        (should (eq proj (ede-current-project)))
        (should (oref ede-object compilation))

        (let ((ret (ede-compdb-flymake-init)))
          ;; Init function needs to return a list of (compiler, args, dir)
          (should (listp ret))
          (should (= 3 (length ret)))

          ;; Args should be processed correctly
          (should (cl-search '("-o" "/dev/null") (nth 1 ret) :test 'equal))
          (should (not (cl-find "-MT" (nth 1 ret) :test 'equal)))
          (should (not (cl-find maincpp (nth 1 ret) :test 'equal)))
          (should (not (cl-find "-c" (nth 1 ret) :test 'equal)))

          ;; Directory should be the build dir
          (should (file-equal-p builddir (nth 2 ret)))
          ))))))

(ert-deftest ede-compdb-compile-tests ()
  "Tests that we build the correct command lines for compiling."
  :expected-result (if ede-compdb-test-cmake-path :passed :failed)
  (with-cmake-build-directory
   builddir :generate-compdb
   (cl-letf*
       ((compile-cmd nil)
        (compile-dir nil)
        ((symbol-function 'compile) (lambda (c)
                                      (setq compile-cmd c)
                                      (setq compile-dir default-directory)))
        (maincpp (expand-file-name "main.cpp" ede-compdb-test-srcdir)))

     (with-temp-ede-project
      proj (ede-compdb-project "TESTPROJ"
                               :compdb-file (expand-file-name "compile_commands.json" builddir)
                               :build-exe "make" :compile-args `("-j3")
                               :file (expand-file-name "CMakeLists.txt" ede-compdb-test-srcdir))

      (with-temp-file-buffer buf maincpp
        ;; Compile entire project
        (ede-compile-project)
        (should (file-equal-p compile-dir builddir))
        (should (string= compile-cmd "make -j3"))

        ;; Compile target from compdb
        (ede-compile-target)
        (should (file-equal-p compile-dir builddir))
        (should (string-match (regexp-quote maincpp) compile-cmd))
        (should (string-match (regexp-quote "main.cpp.o") compile-cmd))

        ;; Compile phony target
        (project-compile-target proj "foo")
        (should (file-equal-p compile-dir builddir))
        (should (string= compile-cmd "make -j3 foo"))

        )))))


;;; ede-ninja-project tests

(ert-deftest ede-compdb-ninja-expand-vars ()
  "Tests resolving Ninja variable expansions."
  (should (equal "abc/def"
                 (ede-ninja-expand-vars "$A/${D}"
                                        '(("A" . "abc") ("D" . "def")))))
  )

(ert-deftest ede-compdb-ninja-scan-build-rules ()
  "Tests parsing of Ninja build file to determine build rules."
  (cl-letf*
      ((files '(("build.ninja" .
                 "VAR1=rules.ninja
VAR2=${VAR1}
include $VAR2
rule CXX_2")
                ("rules.ninja" .
                 "rule C_1
rule CXX_1")))
       ((symbol-function 'insert-file-contents)
        (lambda (f) (insert (cdr (assoc f files))) (goto-char (point-min)))))
    (should (equal '("CXX_1" "CXX_2")
                   (sort (ede-ninja-scan-build-rules "build.ninja" "^CXX") #'string-lessp)))
    ))

(ert-deftest ede-compdb-ninja-autoload-project ()
  "Tests autoloading of ninja projects when rules.ninja files are discovered"
  :expected-result (if (and ede-compdb-test-cmake-path
                            ede-compdb-ninja-exe-path)
                       :passed :failed)
  (with-insource-build
   dir :generate-ninja
   (dolist (f '("main.cpp" "world/world.cpp"))
     (with-temp-file-buffer buf (expand-file-name f dir)
       ;; Should have set up the current project and target
       (should (ede-current-project))
       (should ede-object)
       (should (oref ede-object compilation)))
     )))

(ert-deftest ede-compdb-ninja-phony-targets ()
  "Tests that when we are using the ede-ninja-project type, the targets list is populated with phony targets"
  :expected-result (if (and ede-compdb-test-cmake-path ede-compdb-ninja-exe-path) :passed :failed)
  (with-cmake-build-directory
   builddir :generate-ninja
   (with-temp-ede-project
       proj (ede-ninja-project "TESTPROJ"
                               :compdb-file (expand-file-name "build.ninja" builddir)
                               :file (expand-file-name "CMakeLists.txt" ede-compdb-test-srcdir))

     (project-compile-project proj)
     (sleep-until-compilation-done)
     (should (file-executable-p (expand-file-name "hello" builddir)))

     (project-compile-target proj "clean")
     (sleep-until-compilation-done)
     (should (not (file-executable-p (expand-file-name "hello" builddir))))
     )))

(provide 'cedet/ede/compdb-utest)
;;; ede-compdb-test.el ends here

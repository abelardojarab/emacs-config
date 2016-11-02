;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "auto" "auto.el" (0 0 0 0))
;;; Generated autoloads from auto.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "auto" '("ede-")))

;;;***

;;;### (autoloads nil "autoconf-edit" "autoconf-edit.el" (0 0 0 0))
;;; Generated autoloads from autoconf-edit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "autoconf-edit" '("autoconf-")))

;;;***

;;;### (autoloads nil "compdb" "compdb.el" (0 0 0 0))
;;; Generated autoloads from compdb.el

(autoload 'ede-compdb-load-project "compdb" "\
Create an instance of option `ede-compdb-project' for DIR.

\(fn DIR)" nil nil)

(autoload 'ede-ninja-load-project "compdb" "\
Create an instance of option `ede-ninja-project' for DIR.

\(fn DIR)" nil nil)

(ede-add-project-autoload (ede-project-autoload "compdb" :name "Compilation DB" :file 'ede/compdb :proj-file "compile_commands.json" :load-type 'ede-compdb-load-project :class-sym 'ede-compdb-project))

(ede-add-project-autoload (ede-project-autoload "ninja" :name "Ninja" :file 'ede/compdb :proj-file "build.ninja" :load-type 'ede-ninja-load-project :class-sym 'ede-ninja-project))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "compdb" '("ff-other-file-list" "ede-" "parse-command-line" "project-" "initialize-instance" "insert-compdb" "get-" "current-co" "compdb-entry" "other-file-list" "set-configuration-directory")))

;;;***

;;;### (autoloads nil "detect" "detect.el" (0 0 0 0))
;;; Generated autoloads from detect.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "detect" '("ede-")))

;;;***

;;;### (autoloads nil "ede/android" "android.el" (0 0 0 0))
;;; Generated autoloads from android.el

(autoload 'ede-android-load "ede/android" "\
Return an Android Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project.

\(fn DIR &optional ROOTPROJ)" nil nil)

(ede-add-project-autoload (ede-project-autoload "android" :name "ANDROID ROOT" :file 'ede/android :proj-file "AndroidManifest.xml" :load-type 'ede-android-load :class-sym 'ede-android-project :new-p t :safe-p t))

(eieio-defclass-autoload 'ede-android-project '(ede-project) "ede/android" "Project for Android applications.")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/android" '("ede-" "project-" "initialize-instance")))

;;;***

;;;### (autoloads nil "ede/ant" "ant.el" (0 0 0 0))
;;; Generated autoloads from ant.el

(defconst ede-ant-project-file-name "build.xml" "\
name of project file for Ant projects")

(autoload 'ede-ant-load "ede/ant" "\
Return a Leiningen Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project.

\(fn DIR &optional ROOTPROJ)" nil nil)

(eieio-defclass-autoload 'ede-ant-project '(ede-jvm-base-project) "ede/ant" "EDE Ant project class.")

(ede-add-project-autoload (ede-project-autoload "ant" :name "Ant" :file 'ede/ant :proj-file (if (featurep 'ede/ant) ede-ant-project-file-name "build.xml") :load-type 'ede-ant-load :class-sym 'ede-ant-project :new-p nil :safe-p t) 'generic)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/ant" '("ede-" "project-" "initialize-instance")))

;;;***

;;;### (autoloads nil "ede/arduino" "arduino.el" (0 0 0 0))
;;; Generated autoloads from arduino.el

(autoload 'ede-arduino-root "ede/arduino" "\
Get the root project directory for DIR.
The only arduino sketches allowed are those configured by the arduino IDE
in their sketch directory.

If BASEFILE is non-nil, then convert root to the project basename also.

\(fn &optional DIR BASEFILE)" nil nil)

(autoload 'ede-arduino-file "ede/arduino" "\
Get a file representing the root of this arduino project.
It is a file ending in .pde or .ino that has the same basename as
the directory it is in.  Optional argument DIR is the directory
to check.

\(fn &optional DIR)" nil nil)

(autoload 'ede-arduino-load "ede/arduino" "\
Return an Arduino project object if there is one.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, sinc there is only one project for a directory tree.

\(fn DIR &optional ROOTPROJ)" nil nil)

(ede-add-project-autoload (ede-project-autoload "arduino" :name "ARDUINO SKETCHBOOK" :file 'ede/arduino :root-only nil :proj-root-dirmatch (ede-project-autoload-dirmatch "arduino" :fromconfig (lambda nil (if (boundp 'ede-arduino-preferences-file) ede-arduino-preferences-file "~/.arduino/preferences.txt")) :configregex "^sketchbook.path=\\([^\n]+\\)$" :configregexidx 1) :proj-file 'ede-arduino-file :load-type 'ede-arduino-load :class-sym 'ede-arduino-project :safe-p t :new-p t) 'unique)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/arduino" '("eieio-done-customizing" "ede-" "project-" "cedet-arduino-serial-monitor")))

;;;***

;;;### (autoloads nil "ede/base" "base.el" (0 0 0 0))
;;; Generated autoloads from base.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/base" '("ede-")))

;;;***

;;;### (autoloads nil "ede/config" "config.el" (0 0 0 0))
;;; Generated autoloads from config.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/config" '("eieio-done-customizing" "ede-" "project-")))

;;;***

;;;### (autoloads nil "ede/cpp-root" "cpp-root.el" (0 0 0 0))
;;; Generated autoloads from cpp-root.el

(eieio-defclass-autoload 'ede-cpp-root-project '(ede-project eieio-instance-tracker) "ede/cpp-root" "EDE cpp-root project class.\nEach directory needs a project file to control it.")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/cpp-root" '("ede-" "project-" "initialize-instance")))

;;;***

;;;### (autoloads nil "ede/custom" "custom.el" (0 0 0 0))
;;; Generated autoloads from custom.el

(autoload 'ede-customize-project "ede/custom" "\
Edit fields of the current project through EIEIO & Custom.

\(fn)" t nil)

(defalias 'customize-project 'ede-customize-project)

(autoload 'ede-customize-current-target "ede/custom" "\
Edit fields of the current target through EIEIO & Custom.

\(fn)" t nil)

(defalias 'customize-target 'ede-customize-current-target)

(autoload 'ede-project-sort-targets "ede/custom" "\
Create a custom-like buffer for sorting targets of current project.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/custom" '("eieio-" "ede-")))

;;;***

;;;### (autoloads nil "ede/dired" "dired.el" (0 0 0 0))
;;; Generated autoloads from dired.el

(autoload 'ede-dired-minor-mode "ede/dired" "\
A minor mode that should only be activated in DIRED buffers.
If ARG is nil or a positive number, force on, if
negative, force off.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/dired" '("ede-dired-")))

;;;***

;;;### (autoloads nil "ede/emacs" "emacs.el" (0 0 0 0))
;;; Generated autoloads from emacs.el

(ede-add-project-autoload (ede-project-autoload "emacs" :name "EMACS ROOT" :file 'ede/emacs :proj-file "src/emacs.c" :load-type 'ede-emacs-load :class-sym 'ede-emacs-project :new-p nil :safe-p t) 'unique)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/emacs" '("project-" "ede-" "initialize-instance")))

;;;***

;;;### (autoloads nil "ede/files" "files.el" (0 0 0 0))
;;; Generated autoloads from files.el

(autoload 'ede-find-file "ede/files" "\
Find FILE in project.  FILE can be specified without a directory.
There is no completion at the prompt.  FILE is searched for within
the current EDE project.

\(fn FILE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/files" '("ede-")))

;;;***

;;;### (autoloads nil "ede/generic" "generic.el" (0 0 0 0))
;;; Generated autoloads from generic.el

(autoload 'ede-enable-generic-projects "ede/generic" "\
Enable generic project loaders.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/generic" '("ede-" "initialize-instance")))

;;;***

;;;### (autoloads nil "ede/java-root" "java-root.el" (0 0 0 0))
;;; Generated autoloads from java-root.el

(eieio-defclass-autoload 'ede-java-root-project '(ede-jvm-base-project eieio-instance-tracker) "ede/java-root" "EDE java-root project class.\nEach directory needs a project file to control it.")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/java-root" '("ede-" "project-rescan" "initialize-instance")))

;;;***

;;;### (autoloads nil "ede/jvm-base" "jvm-base.el" (0 0 0 0))
;;; Generated autoloads from jvm-base.el

(eieio-defclass-autoload 'ede-jvm-base-project '(ede-project) "ede/jvm-base" "Base project class for JVM-base projects.")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/jvm-base" '("ede-" "project-compile-target" "initialize-instance")))

;;;***

;;;### (autoloads nil "ede/lein2" "lein2.el" (0 0 0 0))
;;; Generated autoloads from lein2.el

(defconst ede-lein2-project-file-name "project.clj" "\
name of project file for Lein2 projects")

(autoload 'ede-lein2-load "ede/lein2" "\
Return a Leiningen Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project.

\(fn DIR &optional ROOTPROJ)" nil nil)

(eieio-defclass-autoload 'ede-lein2-project '(ede-jvm-base-project) "ede/lein2" "EDE Leiningen2 project class.")

(ede-add-project-autoload (ede-project-autoload "lein2" :name "Lein2" :file 'ede/lein2 :proj-file (if (featurep 'ede/lein2) ede-lein2-project-file-name "project.clj") :load-type 'ede-lein2-load :class-sym 'ede-lein2-project :new-p nil :safe-p t) 'generic)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/lein2" '("project-" "ede-" "lein2-outfile-name" "initialize-instance")))

;;;***

;;;### (autoloads nil "ede/linux" "linux.el" (0 0 0 0))
;;; Generated autoloads from linux.el

(autoload 'ede-linux-load "ede/linux" "\
Return an Linux Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project.

\(fn DIR &optional ROOTPROJ)" nil nil)

(ede-add-project-autoload (ede-project-autoload "linux" :name "LINUX ROOT" :file 'ede/linux :proj-file "scripts/ver_linux" :load-type 'ede-linux-load :class-sym 'ede-linux-project :new-p nil :safe-p t) 'unique)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/linux" '("project-" "ede-" "initialize-instance")))

;;;***

;;;### (autoloads nil "ede/locate" "locate.el" (0 0 0 0))
;;; Generated autoloads from locate.el

(autoload 'ede-enable-locate-on-project "ede/locate" "\
Enable an EDE locate feature on PROJECT.
Attempt to guess which project locate style to use
based on `ede-locate-setup-options'.

\(fn &optional PROJECT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/locate" '("ede-locate-" "initialize-instance")))

;;;***

;;;### (autoloads nil "ede/m3" "m3.el" (0 0 0 0))
;;; Generated autoloads from m3.el

(autoload 'ede-m3-install "ede/m3" "\


\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/m3" '("ede-m3-ede-items")))

;;;***

;;;### (autoloads nil "ede/make" "make.el" (0 0 0 0))
;;; Generated autoloads from make.el

(autoload 'ede-make-check-version "ede/make" "\
Check the version of GNU Make installed.
The check passes if the MAKE version is no high enough, or if it
is not GNU make.
If NOERROR is non-nil, return t for success, nil for failure.
If NOERROR is nil, then throw an error on failure.  Return t otherwise.

\(fn &optional NOERROR)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/make" '("ede-make-")))

;;;***

;;;### (autoloads nil "ede/maven2" "maven2.el" (0 0 0 0))
;;; Generated autoloads from maven2.el

(autoload 'ede-maven2-load "ede/maven2" "\
Return a Maven Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project.

\(fn DIR &optional ROOTPROJ)" nil nil)

(eieio-defclass-autoload 'ede-maven2-project '(ede-jvm-base-project) "ede/maven2" "Project Type for Maven2 based Java projects.")

(ede-add-project-autoload (ede-project-autoload "maven2" :name "MAVEN2" :file 'ede/maven2 :proj-file (if (featurep 'ede/lein2) ede-maven2-project-file-name "pom.xml") :load-type 'ede-maven2-load :class-sym 'ede-maven2-project :new-p nil :safe-p t) 'generic)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/maven2" '("project-" "ede-" "maven2-outfile-name" "initialize-instance")))

;;;***

;;;### (autoloads nil "ede/shell" "shell.el" (0 0 0 0))
;;; Generated autoloads from shell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/shell" '("ede-shell-")))

;;;***

;;;### (autoloads nil "ede/speedbar" "speedbar.el" (0 0 0 0))
;;; Generated autoloads from speedbar.el

(autoload 'ede-speedbar-file-setup "ede/speedbar" "\
Setup some keybindings in the Speedbar File display.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/speedbar" '("eieio-speedbar-" "ede-")))

;;;***

;;;### (autoloads nil "ede/system" "system.el" (0 0 0 0))
;;; Generated autoloads from system.el

(autoload 'ede-web-browse-home "ede/system" "\
Browse the home page of the current project.

\(fn)" t nil)

(autoload 'ede-edit-web-page "ede/system" "\
Edit the web site for this project.

\(fn)" t nil)

(autoload 'ede-upload-distribution "ede/system" "\
Upload the current distribution to the correct location.
Use /user@ftp.site.com: file names for FTP sites.
Download tramp, and use /r:machine: for names on remote sites w/out FTP access.

\(fn)" t nil)

(autoload 'ede-upload-html-documentation "ede/system" "\
Upload the current distributions documentation as HTML.
Use /user@ftp.site.com: file names for FTP sites.
Download tramp, and use /r:machine: for names on remote sites w/out FTP access.

\(fn)" t nil)

(autoload 'ede-vc-project-directory "ede/system" "\
Run `vc-dir' on the current project.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ede/util" "util.el" (0 0 0 0))
;;; Generated autoloads from util.el

(autoload 'ede-update-version "ede/util" "\
Update the current projects main version number.
Argument NEWVERSION is the version number to use in the current project.

\(fn NEWVERSION)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ede/util" '("ede-" "project-update-version")))

;;;***

;;;### (autoloads nil "makefile-edit" "makefile-edit.el" (0 0 0 0))
;;; Generated autoloads from makefile-edit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "makefile-edit" '("makefile-")))

;;;***

;;;### (autoloads nil "pconf" "pconf.el" (0 0 0 0))
;;; Generated autoloads from pconf.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pconf" '("ede-p")))

;;;***

;;;### (autoloads nil "pmake" "pmake.el" (0 0 0 0))
;;; Generated autoloads from pmake.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pmake" '("ede-p")))

;;;***

;;;### (autoloads nil "proj" "proj.el" (0 0 0 0))
;;; Generated autoloads from proj.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proj" '("project-" "eieio-done-customizing" "ede-")))

;;;***

;;;### (autoloads nil "proj-archive" "proj-archive.el" (0 0 0 0))
;;; Generated autoloads from proj-archive.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proj-archive" '("ede-")))

;;;***

;;;### (autoloads nil "proj-aux" "proj-aux.el" (0 0 0 0))
;;; Generated autoloads from proj-aux.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proj-aux" '("ede-")))

;;;***

;;;### (autoloads nil "proj-comp" "proj-comp.el" (0 0 0 0))
;;; Generated autoloads from proj-comp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proj-comp" '("ede-" "proj-comp-insert-variable-once" "initialize-instance")))

;;;***

;;;### (autoloads nil "proj-elisp" "proj-elisp.el" (0 0 0 0))
;;; Generated autoloads from proj-elisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proj-elisp" '("ede-" "project-compile-target")))

;;;***

;;;### (autoloads nil "proj-info" "proj-info.el" (0 0 0 0))
;;; Generated autoloads from proj-info.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proj-info" '("ede-" "object-write")))

;;;***

;;;### (autoloads nil "proj-misc" "proj-misc.el" (0 0 0 0))
;;; Generated autoloads from proj-misc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proj-misc" '("ede-")))

;;;***

;;;### (autoloads nil "proj-obj" "proj-obj.el" (0 0 0 0))
;;; Generated autoloads from proj-obj.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proj-obj" '("ede-")))

;;;***

;;;### (autoloads nil "proj-prog" "proj-prog.el" (0 0 0 0))
;;; Generated autoloads from proj-prog.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proj-prog" '("project-" "ede-proj-")))

;;;***

;;;### (autoloads nil "proj-scheme" "proj-scheme.el" (0 0 0 0))
;;; Generated autoloads from proj-scheme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proj-scheme" '("ede-proj-t")))

;;;***

;;;### (autoloads nil "proj-shared" "proj-shared.el" (0 0 0 0))
;;; Generated autoloads from proj-shared.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proj-shared" '("ede-")))

;;;***

;;;### (autoloads nil "project-am" "project-am.el" (0 0 0 0))
;;; Generated autoloads from project-am.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "project-am" '("ede-" "project-")))

;;;***

;;;### (autoloads nil "source" "source.el" (0 0 0 0))
;;; Generated autoloads from source.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "source" '("ede-" "initialize-instance")))

;;;***

;;;### (autoloads nil "srecode" "srecode.el" (0 0 0 0))
;;; Generated autoloads from srecode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode" '("ede-srecode-")))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here

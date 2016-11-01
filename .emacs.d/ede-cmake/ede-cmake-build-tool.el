;; EDE CMake Build Tool -- Support for build tool-specific behaviour in ede-cmake

;; Author: Alastair Rankine <alastair@girtby.net>

(require 'cmake-target-cache)

(defclass ede-cmake-build-tool ()
  ((generator-string
    :type string
    :initarg :generator-string
    :documentation "Which CMake generator to use for build files")
   (additional-parameters
    :initarg :additional-parameters
    :initform ""
    :type string
    :documentation "Additional parameters to build tool")
   (target-cache
    :type cmake-target-cache)
   )
  )

(defclass cmake-makelike-build-tool (ede-cmake-build-tool)
  ;; Base class for tools that work like make 
  () )

(defclass cmake-make-build-tool (cmake-makelike-build-tool)
  ;; GNU Make and friends
  ((generator-string
    :type string
    :initarg :generator-string
    :initform "Unix Makefiles")
   )
  )

(defclass cmake-ninja-build-tool (cmake-makelike-build-tool)
  ;; "Ninja is a small build system with a focus on speed"
  ;; http://martine.github.com/ninja/
  ((generator-string
    :type string
    :initarg :generator-string
    :initform "Ninja")
   (target-cache
    :initform (cmake-ninja-target-cache "target-cache")
    )
   )
  )

(defclass cmake-visual-studio-build-tool (ede-cmake-build-tool)
  ()
  )

(defmethod get-target-directory ((this cmake-makelike-build-tool) project target)
  (let ((builddir (cmake-build-directory project))
        (path (oref target path)))
    (concat (file-name-as-directory builddir) (file-name-as-directory path))))

(defmethod target-build-dir ((this cmake-makelike-build-tool) target)
  "Returns the path to the build directory for target"
  (concat (file-name-as-directory (cmake-build-directory (ede-target-parent target)))
          (file-name-as-directory (oref target path))))

(defmethod target-binary ((this cmake-makelike-build-tool) target)
  "Returns the path to the binary for target"
  (concat (target-build-dir this target) (oref target name)))

(defmethod debug-target ((this cmake-makelike-build-tool) target)
  "Invokes gdb to debug TARGET in BUILD-DIR"
  (let* ((exe (target-binary this target))
         (cmd (concat "gdb -i=mi " exe)))
    (gdb cmd)))

(defmethod get-target-names ((this cmake-makelike-build-tool) builddir)
  "Gets a list of possible targets for the build directory"
  (get-targets (oref this target-cache) builddir))


(defmethod invoke-make ((this cmake-make-build-tool) dir targetname)
  "Invokes make in DIR for TARGETNAME"
  (let* ((args (if (slot-boundp this 'additional-parameters) (oref this additional-parameters) ""))
         (cmd (format "(cd %s ; make %s %s )" dir args targetname)))
    (compile cmd)))

(defmethod compile-target-file ((this cmake-make-build-tool) target file)
  "Compiles FILE in BUILD-DIR in TARGET"
  (let* ((dir (target-build-dir this target))
         (doto (concat(file-name-sans-extension (file-name-nondirectory file)) ".o")))
    (invoke-make this dir doto)
    ))

(provide 'ede-cmake-build-tool)

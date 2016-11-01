;; EDE Cmake Target Cache -- maintains a list of valid targets populated by the build tool

(defclass cmake-target-cache ()
  ((target-file
    :type string
    :initarg :target-file
    :documentation "A file in the build directory which is updated when the list of targets change")
   (target-file-timestamp
    :initform nil
    :protection :protected
    :documentation "The last mod time for the target file")
   (target-file-size
    :initform nil
    :protection :protected
    :documentation "The last measured size of the target file")
   (targets
    :protection :protected
    :documentation "The cached targets")
   )
  )

(defclass cmake-ninja-target-cache (cmake-target-cache)
  ((target-file
    :type string
    :initarg :target-file
    :initform "rules.ninja")
   )
  )

(defmethod cache-file-path ((this cmake-target-cache) builddir)
  (concat (expand-file-name (file-name-as-directory builddir)) (oref this target-file)))

(defmethod get-targets ((this cmake-target-cache) builddir)
  (reload-if-dirty this builddir)
  (oref this targets))

(defmethod reload-if-dirty ((this cmake-target-cache) builddir)
  (let* ((target-file-path (cache-file-path this builddir))
         (stats (file-attributes target-file-path))
         (size (nth 7 stats))
         (mod (nth 5 stats)))
    
    ;; Logic stolen from ede/arduino.el
    (when (or (not (oref this target-file-timestamp))
              (/= (or (oref this target-file-size) 0) size)
              (not (equal (oref this target-file-timestamp) mod)))
      
      (reload-cache this builddir)
      (oset this target-file-timestamp mod)
      (oset this target-file-size size)
      )
    ))
         
(defvar cmake-ninja-target-regexp "^\\(.+\\): \\(phony\\|CLEAN\\)$")

(defmethod reload-cache ((this cmake-ninja-target-cache) builddir)
  (let ((default-directory (expand-file-name (file-name-as-directory builddir)))
        (tlist '()))
    ;;(with-temp-buffer
    (with-current-buffer (get-buffer-create "*ninja-targets*")
      (erase-buffer)
      (call-process "ninja" nil t t "-t" "targets")
      (goto-char 0)
      (while (re-search-forward cmake-ninja-target-regexp nil t)
        (add-to-list 'tlist (match-string 1)))
      (oset this targets tlist)
      )))

(provide 'cmake-target-cache)

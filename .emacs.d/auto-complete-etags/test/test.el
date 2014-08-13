(require 'auto-complete-etags)
(require 'el-expectations)

(eval-when-compile
  (require 'cl))

;; ac-etags-get-item-location-in-tags
(expectations
  (desc "Single location")
  (expect `((,(expand-file-name "test.c") 1))
    (ac-etags-get-item-location-in-tags "simple_func" (expand-file-name "c.TAGS")))

  (desc "Multiple locatioins")
  (expect `((,(expand-file-name "test.cc") 8) (,(expand-file-name "test.cc") 9))
    (ac-etags-get-item-location-in-tags "overloaded_func" (expand-file-name "cc.TAGS")))

  (desc "No entry")
  (expect nil
    (ac-etags-get-item-location-in-tags "none" (expand-file-name "cc.TAGS"))))

;; Testing function for ac-etags-search-for-documentation
(defun test-ac-etags-search-for-documentation (mode item &optional tagfile)
  (let ((ret nil) (major-mode mode) (org-name tags-file-name) (org-list tags-table-list)
        (tagfile (and tagfile (expand-file-name tagfile))))
    (and tagfile (setq tags-table-list `(,tagfile)))
    (setq ret (ac-etags-search-for-documentation item))
    (setq tags-file-name org-name)
    (setq tags-table-list org-list)
    ret))

;; Tests for ac-etags-search-for-documentation
(expectations
  (desc "Test for signature of a func-decl on single line")
  (expect "void simple_func(void)"
    (test-ac-etags-search-for-documentation 'c-mode "simple_func" "c.TAGS"))

  (desc "Test for signature of a func-decl with multiple args")
  (expect "void simple_func2(int a, int b)"
    (test-ac-etags-search-for-documentation 'c-mode "simple_func2" "c.TAGS"))

  (desc "Test for signature on multiple lines")
  (expect "int multiple_line_func(void)"
    (test-ac-etags-search-for-documentation 'c-mode "multiple_line_func" "c.TAGS"))

  (desc "Test for signature of multiple args on multiple lines")
  (expect "const char* multiple_line_va_arg_func(int a, int b, ...)"
    (test-ac-etags-search-for-documentation 'c-mode "multiple_line_va_arg_func" "c.TAGS"))

  (desc "Test for a function with macro")
  (expect "MACRO1 MACRO2 const char * macro_func(int a)"
    (test-ac-etags-search-for-documentation 'c-mode "macro_func" "c.TAGS"))

  (desc "No documentation found")
  (expect "No documentation found."
    (test-ac-etags-search-for-documentation 'c-mode "foo" "c.TAGS"))

  ;; For now, we ignore old-style functions.
  ;; (desc "Test for old-style function declaration")
  ;; (expect "void old_style_func(a, b) int a; int b;"
  ;;   (test-ac-etags-search-for-documentation 'c-mode "old_style_func" "c.TAGS"))

  (desc "Test for a multiple-line funtion following comment.")
  (expect "void comment_func(int a, int b)"
    (test-ac-etags-search-for-documentation 'c-mode "comment_func" "c.TAGS"))

  (desc "Test for a function follwing comment.")
  (expect "const char * comment_func2(void)"
    (test-ac-etags-search-for-documentation 'c-mode "comment_func2" "c.TAGS"))

  (desc "Test for a crammed function.")
  (expect "void crammed_func(void)"
    (test-ac-etags-search-for-documentation 'c-mode "crammed_func" "c.TAGS"))
  )

;; Test when TAGS has changed.
(expectations
  (desc "Completing from c.TAGS")
  (expect "void simple_func(void)"
    (visit-tags-table (expand-file-name "c.TAGS") t)
    (test-ac-etags-search-for-documentation 'c-mode "simple_func"))

  (desc "Completing from c.another.TAGS")
  (expect "static const char *g(void)"
    (visit-tags-table (expand-file-name "c.another.TAGS") t)
    (test-ac-etags-search-for-documentation 'c-mode "g"))

  ;; Switching again
  (desc "Completing from c.TAGS")
  (expect "const char* multiple_line_va_arg_func(int a, int b, ...)"
    (visit-tags-table (expand-file-name "c.TAGS") t)
    (test-ac-etags-search-for-documentation 'c-mode "multiple_line_va_arg_func"))
  )

;; Tests for when TAGS file has been updated.
(expectations
  (desc "Test for the old tags file")
  (expect "No documentation found."
    (progn
      (call-process "/usr/local/bin/ctags" nil nil nil "-e" "-f" "./c.TAGS" "test.c")
      (test-ac-etags-search-for-documentation 'c-mode "updated_func" "c.TAGS")))
  (expect "void simple_func(void)"
    (test-ac-etags-search-for-documentation 'c-mode "simple_func" "c.TAGS"))
  ;; Next, create tags file of updated source.
  (desc "After having been updated TAGS file")
  (expect "void simple_func(void)"
    (test-ac-etags-search-for-documentation 'c-mode "simple_func" "c.TAGS"))
  (expect "int updated_func(void)"
    (progn
      (sit-for 1)
      (call-process "/usr/local/bin/ctags" nil nil nil "-e" "-f" "./c.TAGS" "test.updated.c")
      (test-ac-etags-search-for-documentation 'c-mode "updated_func" "c.TAGS"))))

;; Test for completion in the mode that is not the same as the source file.
(expectations
  (desc "Completing from .h file in emacs-lisp-mode.")
  ;; Currently, this expect fails.
  (expect nil
    (visit-tags-table (expand-file-name "c.TAGS") t)
    (ac-etags-search-for-documentation "simple_func"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c++-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(expectations
  (desc "Normal function")
  (expect "void normal_func()"
    (test-ac-etags-search-for-documentation 'c++-mode "normal_func" "cc.TAGS"))

  (desc "Getter")
  (expect "int get() const"
    (test-ac-etags-search-for-documentation 'c++-mode "get" "cc.TAGS"))

  (desc "Setter")
  (expect "void set(int i)"
    (test-ac-etags-search-for-documentation 'c++-mode "set" "cc.TAGS"))

  (desc "Overloaded functions")
  (expect "void overloaded_func(int i)\nvoid overloaded_func(double d)"
    (test-ac-etags-search-for-documentation 'c++-mode "overloaded_func" "cc.TAGS"))

  (desc "Names with classname qualifiers")
  (expect "void normal_func()"
    (test-ac-etags-search-for-documentation 'c++-mode "TestClass::normal_func" "cc.TAGS"))
  (expect "int get() const"
    (test-ac-etags-search-for-documentation 'c++-mode "TestClass::get" "cc.TAGS"))
  (expect "void set(int i)"
    (test-ac-etags-search-for-documentation 'c++-mode "TestClass::set" "cc.TAGS"))
  (expect "void overloaded_func(int i)\nvoid overloaded_func(double d)"
    (test-ac-etags-search-for-documentation 'c++-mode "TestClass::overloaded_func" "cc.TAGS"))
  )

;; Tests using Qt headers
(expectations
  (desc "replace in qbytearray.h and so on")
  (expect "inline QByteArray &QByteArray::replace(char before, const char *c)\ninline QByteArray &QByteArray::replace(const QByteArray &before, const char *c)\ninline QByteArray &QByteArray::replace(const char *before, const char *after)\ninline typename QHash<Key, T>::iterator replace(const Key &key, const T &value)\ninline void QList<T>::replace(int i, const T &t)\ninline QT3_SUPPORT iterator replace(const Key &aKey, const T &aValue)\ninline typename QMap<Key, T>::iterator replace(const Key &key, const T &value)\ninline QT3_SUPPORT QString &replace(QChar c, const QString &after, bool cs)\ninline QT3_SUPPORT QString &replace(const QString &before, const QString &after, bool cs)\ninline QT3_SUPPORT QString &replace(char c, const QString &after, bool cs)\ninline QT3_SUPPORT QString &replace(char c, const QString &after, Qt::CaseSensitivity cs)\ninline QByteArray &QByteArray::replace(char c, const QString &after)\ninline QByteArray &QByteArray::replace(const QString &before, const char *after)\ninline QByteArray &QByteArray::replace(const QString &before, const QByteArray &after)\ninline void QVector<T>::replace(int i, const T &t)"
    (test-ac-etags-search-for-documentation 'c++-mode "replace" "qt.TAGS")))

;; test for ac-etags-is-target-mode-p
(expectations
  (desc "Current mode: c-mode, Filename: foo.c")
  (expect t
    (ac-etags-is-target-mode-p "foo.c" 'c-mode))

  (desc "Current mode: c-mode, Filename: foo.h")
  (expect t
    (ac-etags-is-target-mode-p "foo.h" 'c-mode))

  (desc "Current mode: c++-mode, Filename: bar.cc")
  (expect t
    (ac-etags-is-target-mode-p "bar.cc" 'c++-mode))

  (desc "Current mode: c++-mode, Filename: bar.hh")
  (expect t
    (ac-etags-is-target-mode-p "bar.hh" 'c++-mode))

  (desc "Current mdoe: c-mode, Filename: foo.cc")
  (expect nil
    (ac-etags-is-target-mode-p "foo.cc" 'c-mode))

  (desc "Current mode: c++-mode, Filename: foo.c")
  ;; Should be t?
  (expect nil
    (ac-etags-is-target-mode-p "foo.c" 'c++-mode))

  (desc "Current mode: c++-mode, Filename: foo.h")
  (expect t
    (ac-etags-is-target-mode-p "foo.h" 'c++-mode))

  (desc "Current mode: lisp-mode, Filename: foo.c")
  (expect nil
    (ac-etags-is-target-mode-p "foo.c" 'lisp-mode))
  )

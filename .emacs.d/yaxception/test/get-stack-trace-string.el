(require 'yaxception)
(require 'el-expectations)

(expectations
  (desc "get-stack-trace-string simple error")
  (expect "Exception is 'wrong-type-argument'. Wrong type argument: sequencep, yaxception-active-p
  at replace-regexp-in-string(\" \" \"\" yaxception-active-p)
  in yaxception:try"
    (let* ((ret ""))
      (yaxception:$
        (yaxception:try
          (replace-regexp-in-string " " "" 'yaxception-active-p))
        (yaxception:catch 'wrong-type-argument e
          (setq ret (yaxception:get-stack-trace-string e))))
      ret)))

(expectations
  (desc "get-stack-trace-string not exist function")
  (expect "Exception is 'void-function'. Symbol's function definition is void: yaxception-zzz

  in yaxception:try"
    (let* ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception-zzz))
        (yaxception:catch 'void-function e
          (setq ret (yaxception:get-stack-trace-string e))))
      ret)))

(expectations
  (desc "get-stack-trace-string call functions")
  (expect "Exception is 'wrong-type-argument'. Wrong type argument: sequencep, yaxception-active-p
  at replace-regexp-in-string(\" \" \"\" yaxception-active-p)
  at yaxception-ccc()
  at yaxception-bbb()
  at yaxception-aaa()
  in yaxception:try"
    (defun yaxception-aaa () (yaxception-bbb))
    (defun yaxception-bbb () (yaxception-ccc))
    (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-active-p))
    (defun yaxception-xxx () "xxx")
    (defun yaxception-yyy () "yyy")
    (let* ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception-xxx)
          (yaxception-yyy)
          (yaxception-aaa))
        (yaxception:catch 'wrong-type-argument e
          (setq ret (yaxception:get-stack-trace-string e))))
      ret)))

(expectations
  (desc "get-stack-trace-string call functions has arguments")
  (expect "Exception is 'wrong-type-argument'. Wrong type argument: sequencep, yaxception-active-p
  at replace-regexp-in-string(\" \" \"\" yaxception-active-p)
  at yaxception-ccc()
  at yaxception-bbb(\"DEF\")
  at yaxception-aaa(\"ABC\" \"DEF\" (\"GHI\" \"JKL\"))
  in yaxception:try"
    (defun yaxception-aaa (aa bb &optional cc) (yaxception-bbb bb))
    (defun yaxception-bbb (bb) (yaxception-ccc))
    (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-active-p))
    (defun yaxception-xxx (zzz) "xxx")
    (defun yaxception-yyy () "yyy")
    (let* ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception-xxx "hoge")
          (yaxception-yyy)
          (yaxception-aaa "ABC" "DEF" '("GHI" "JKL")))
        (yaxception:catch 'wrong-type-argument e
          (setq ret (yaxception:get-stack-trace-string e))))
      ret)))

(expectations
  (desc "get-stack-trace-string catch as parent symbol")
  (expect "Exception is 'wrong-type-argument'. Wrong type argument: sequencep, yaxception-active-p
  at replace-regexp-in-string(\" \" \"\" yaxception-active-p)
  at yaxception-ccc()
  at yaxception-bbb(\"DEF\")
  at yaxception-aaa(\"ABC\" \"DEF\" (\"GHI\" \"JKL\"))
  in yaxception:try"
    (defun yaxception-aaa (aa bb &optional cc) (yaxception-bbb bb))
    (defun yaxception-bbb (bb) (yaxception-ccc))
    (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-active-p))
    (defun yaxception-xxx (zzz) "xxx")
    (defun yaxception-yyy () "yyy")
    (let* ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception-xxx "hoge")
          (yaxception-yyy)
          (yaxception-aaa "ABC" "DEF" '("GHI" "JKL")))
        (yaxception:catch 'error e
          (setq ret (yaxception:get-stack-trace-string e))))
      ret)))

(expectations
  (desc "get-stack-trace-string nest syntax")
  (expect "Exception is 'wrong-type-argument'. Wrong type argument: sequencep, yaxception-active-p
  at replace-regexp-in-string(\" \" \"\" yaxception-active-p)
  at yaxception-ccc()
  at yaxception-bbb(\"DEF\")
  at yaxception-aaa(\"ABC\" \"DEF\" (\"GHI\" \"JKL\"))
  in yaxception:try"
    (defun yaxception-aaa (aa bb &optional cc) (yaxception-bbb bb))
    (defun yaxception-bbb (bb) (yaxception-ccc))
    (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-active-p))
    (defun yaxception-xxx (zzz) "xxx")
    (defun yaxception-yyy () "yyy")
    (let* ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception-xxx "hoge")
          (yaxception:$
            (yaxception:try
              (yaxception-yyy)
              (yaxception-aaa "ABC" "DEF" '("GHI" "JKL")))
            (yaxception:catch 'error e
              (setq ret (yaxception:get-stack-trace-string e)))))
        (yaxception:catch 'error e
          (setq ret "Catch at parent")))
      ret)))

(expectations
  (desc "get-stack-trace-string error in catch")
  (expect "Exception is 'wrong-type-argument'. Wrong type argument: sequencep, yaxception-active-p
  at replace-regexp-in-string(\" \" \"\" yaxception-active-p)
  at yaxception-ccc()
  at yaxception-bbb(\"WVU\")
  at yaxception-aaa(\"ZYX\" \"WVU\")
  in yaxception:catch"
    (defun yaxception-aaa (aa bb &optional cc) (yaxception-bbb bb))
    (defun yaxception-bbb (bb) (yaxception-ccc))
    (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-active-p))
    (defun yaxception-xxx (zzz) "xxx")
    (defun yaxception-yyy () "yyy")
    (let* ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception-xxx "hoge")
          (yaxception:$
            (yaxception:try
              (yaxception-yyy)
              (yaxception-aaa "ABC" "DEF" '("GHI" "JKL")))
            (yaxception:catch 'error e
              (yaxception-aaa "ZYX" "WVU"))))
        (yaxception:catch 'error e
          (setq ret (yaxception:get-stack-trace-string e))))
      ret)))

(expectations
  (desc "get-stack-trace-string error in finally")
  (expect "Exception is 'wrong-type-argument'. Wrong type argument: sequencep, yaxception-active-p
  at replace-regexp-in-string(\" \" \"\" yaxception-active-p)
  at yaxception-ccc()
  at yaxception-bbb(\"WVU\")
  at yaxception-aaa(\"ZYX\" \"WVU\")
  in yaxception:finally"
    (defun yaxception-aaa (aa bb &optional cc) (yaxception-bbb bb))
    (defun yaxception-bbb (bb) (yaxception-ccc))
    (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-active-p))
    (defun yaxception-xxx (zzz) "xxx")
    (defun yaxception-yyy () "yyy")
    (let* ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception-xxx "hoge")
          (yaxception:$
            (yaxception:try
              (yaxception-yyy)
              (yaxception-aaa "ABC" "DEF" '("GHI" "JKL")))
            (yaxception:catch 'error e
              )
            (yaxception:finally
              (yaxception-aaa "ZYX" "WVU"))))
        (yaxception:catch 'error e
          (setq ret (yaxception:get-stack-trace-string e))))
      ret)))

(expectations
  (desc "get-stack-trace-string error in finally 2")
  (expect "Exception is 'wrong-type-argument'. Wrong type argument: sequencep, yaxception-active-p
  at replace-regexp-in-string(\" \" \"\" yaxception-active-p)
  at yaxception-ccc()
  at yaxception-bbb(\"WVU\")
  at yaxception-aaa(\"ZYX\" \"WVU\")
  in yaxception:finally"
    (defun yaxception-aaa (aa bb &optional cc) (yaxception-bbb bb))
    (defun yaxception-bbb (bb) (yaxception-ccc))
    (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-active-p))
    (defun yaxception-xxx (zzz) "xxx")
    (defun yaxception-yyy () "yyy")
    (let* ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception-xxx "hoge")
          (yaxception:$
            (yaxception:try
              (yaxception-yyy))
            (yaxception:finally
              (yaxception-aaa "ZYX" "WVU"))))
        (yaxception:catch 'error e
          (setq ret (yaxception:get-stack-trace-string e))))
      ret)))

(expectations
  (desc "get-stack-trace-string error by throw")
  (expect "Exception is 'wrong-type-argument'. Wrong type argument: sequencep, yaxception-active-p
  at signal(wrong-type-argument (sequencep yaxception-active-p))
  at yaxception:throw(e)
  in yaxception:catch"
    (defun yaxception-aaa (aa bb &optional cc) (yaxception-bbb bb))
    (defun yaxception-bbb (bb) (yaxception-ccc))
    (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-active-p))
    (defun yaxception-xxx (zzz) "xxx")
    (defun yaxception-yyy () "yyy")
    (let* ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception-xxx "hoge")
          (yaxception:$
            (yaxception:try
              (yaxception-yyy)
              (yaxception-aaa "ABC" "DEF" '("GHI" "JKL")))
            (yaxception:catch 'error e
              (yaxception:throw e))))
        (yaxception:catch 'error e
          (setq ret (yaxception:get-stack-trace-string e))))
      ret)))

(expectations
  (desc "get-stack-trace-string error by custom throw")
  (expect "Exception is 'yaxception-test-error'. This is YAX 'hoge' test.
  at signal(yaxception-test-error nil)
  at yaxception:throw((quote yaxception-test-error) :yax-a \"hoge\" :yax-b \"fuga\")
  in yaxception:catch"
    (defun yaxception-aaa (aa bb &optional cc) (yaxception-bbb bb))
    (defun yaxception-bbb (bb) (yaxception-ccc))
    (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-active-p))
    (defun yaxception-xxx (zzz) "xxx")
    (defun yaxception-yyy () "yyy")
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX '%s' test." 'yax-a)
    (let* ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception-xxx "hoge")
          (yaxception:$
            (yaxception:try
              (yaxception-yyy)
              (yaxception-aaa "ABC" "DEF" '("GHI" "JKL")))
            (yaxception:catch 'error e
              (yaxception:throw 'yaxception-test-error :yax-a "hoge" :yax-b "fuga"))))
        (yaxception:catch 'error e
          (setq ret (yaxception:get-stack-trace-string e))))
      ret)))

(expectations
  (desc "get-stack-trace-string error from not catch sexp")
  (expect "Exception is 'wrong-type-argument'. Wrong type argument: sequencep, yaxception-active-p
  at signal(wrong-type-argument (sequencep yaxception-active-p))
  in yaxception:try"
    (defun yaxception-aaa (aa bb &optional cc) (yaxception-bbb bb))
    (defun yaxception-bbb (bb) (yaxception-ccc))
    (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-active-p))
    (defun yaxception-xxx (zzz) "xxx")
    (defun yaxception-yyy () "yyy")
    (let* ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception-xxx "hoge")
          (yaxception:$
            (yaxception:try
              (yaxception-yyy)
              (yaxception-aaa "ABC" "DEF" '("GHI" "JKL")))))
        (yaxception:catch 'error e
          (setq ret (yaxception:get-stack-trace-string e))))
      ret)))


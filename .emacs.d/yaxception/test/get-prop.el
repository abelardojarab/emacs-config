(require 'yaxception)
(require 'el-expectations)

(expectations
  (desc "get-prop defined prop")
  (expect "hoge"
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX '%s' test." 'yax-a)
    (let ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception:throw 'yaxception-test-error :yax-a "hoge" :yax-b "fuga"))
        (yaxception:catch 'error e
          (setq ret (yaxception:get-prop e 'yax-a))))
      ret)))

(expectations
  (desc "get-prop undefined prop")
  (expect "fuga"
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX '%s' test." 'yax-a)
    (let ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception:throw 'yaxception-test-error :yax-a "hoge" :yax-b "fuga"))
        (yaxception:catch 'error e
          (setq ret (yaxception:get-prop e 'yax-b))))
      ret)))

(expectations
  (desc "get-prop not exist")
  (expect nil
    (let ((ret ""))
      (yaxception:$
        (yaxception:try
          (replace-regexp-in-string " " "" 'yaxception-active-p))
        (yaxception:catch 'error e
          (setq ret (yaxception:get-prop e 'something))))
      ret)))


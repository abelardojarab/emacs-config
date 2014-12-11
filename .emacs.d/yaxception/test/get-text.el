(require 'yaxception)
(require 'el-expectations)

(expectations
  (desc "get-text builtin error")
  (expect "Wrong type argument: sequencep, yaxception-active-p"
    (let ((ret ""))
      (yaxception:$
        (yaxception:try
          (replace-regexp-in-string " " "" 'yaxception-active-p))
        (yaxception:catch 'error e
          (setq ret (yaxception:get-text e))))
      ret)))

(expectations
  (desc "get-text custom error")
  (expect "This is YAX 'hoge' test."
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX '%s' test." 'yax-a)
    (let ((ret ""))
      (yaxception:$
        (yaxception:try
          (yaxception:throw 'yaxception-test-error :yax-a "hoge" :yax-b "fuga"))
        (yaxception:catch 'error e
          (setq ret (yaxception:get-text e))))
      ret)))

(expectations
  (desc "get-text typo")
  (expect ""
    (let ((ret "hoge"))
      (yaxception:$
        (yaxception:try
          (replace-regexp-in-string " " "" 'yaxception-active-p))
        (yaxception:catch 'error e
          (setq ret (yaxception:get-text ret))))
      ret)))

(expectations
  (desc "get-text message when error")
  (expect "[yaxception:get-text] Wrong type argument: listp, \"\""
    (let ((ret ""))
      (ad-with-auto-activation-disabled
       (flet ((message (format-string &rest args)
                       (setq ret (apply 'format format-string args))))
         (yaxception:$
           (yaxception:try
             (replace-regexp-in-string " " "" 'yaxception-active-p))
           (yaxception:catch 'error e
             (yaxception:get-text ret)))))
      ret)))


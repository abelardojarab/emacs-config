(require 'yaxception)
(require 'el-expectations)

(expectations
  (desc "catch-single not catch defined")
  (expect 11
    (let ((i 0))
      (yaxception:$
        (yaxception:try
          (setq i 10)
          (incf i)
          i)))))

(expectations
  (desc "catch-single not catch")
  (expect 11
    (let ((i 0))
      (yaxception:$
        (yaxception:try
          (setq i 10)
          (incf i)
          i)
        (yaxception:catch 'error e
          (incf i 2))))))

(expectations
  (desc "catch-single catch")
  (expect 2
    (let ((i 0))
      (yaxception:$
        (yaxception:try
          (delete-file "not found")
          (setq i 10))
        (yaxception:catch 'error e
          (incf i 2)))
      i)))

(expectations
  (desc "catch-single catch match error")
  (expect 5
    (let ((i 0))
      (yaxception:$
        (yaxception:try
          (delete-file "not found")
          (setq i 10))
        (yaxception:catch 'file-error e
          (incf i 5))
        (yaxception:catch 'error e
          (incf i 2)))
      i)))

(expectations
  (desc "catch-single catch custom error")
  (expect 5
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX '%s' test." 'yax-a)
    (let ((i 0))
      (yaxception:$
        (yaxception:try
          (yaxception:throw 'yaxception-test-error)
          (setq i 10))
        (yaxception:catch 'yaxception-test-error e
          (incf i 5))
        (yaxception:catch 'error e
          (incf i 2)))
      i)))

(expectations
  (desc "catch-single catch parent error")
  (expect 5
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX '%s' test." 'yax-a)
    (let ((i 0))
      (yaxception:$
        (yaxception:try
          (yaxception:throw 'yaxception-test-error)
          (setq i 10))
        (yaxception:catch 'file-error e
          (incf i 5))
        (yaxception:catch 'error e
          (incf i 2)))
      i)))

(expectations
  (desc "catch-single of yaxception:$~")
  (expect 5
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX '%s' test." 'yax-a)
    (let ((i 0))
      (yaxception:$~
        (yaxception:try
          (yaxception:throw 'yaxception-test-error)
          (setq i 10))
        (yaxception:catch 'file-error e
          (incf i 5))
        (yaxception:catch 'error e
          (incf i 2)))
      i)))


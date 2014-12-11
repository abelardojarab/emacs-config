(require 'yaxception)
(require 'el-expectations)

(expectations
  (desc "catch-nest not catch defined")
  (expect 16
    (let ((i 0))
      (yaxception:$
        (yaxception:try
          (setq i 10)
          (yaxception:$
            (yaxception:try
              (incf i 5)))
          (incf i)
          i)))))

(expectations
  (desc "catch-nest not catch")
  (expect 16
    (let ((i 0))
      (yaxception:$
        (yaxception:try
          (setq i 10)
          (yaxception:$
            (yaxception:try
              (incf i 5))
            (yaxception:catch 'error e
              (incf i 100)))
          (incf i)
          i)
        (yaxception:catch 'error e
          (incf i 2))))))

(expectations
  (desc "catch-nest catch")
  (expect 15
    (let ((i 0))
      (yaxception:$
        (yaxception:try
          (yaxception:$
            (yaxception:try
              (delete-file "not found")
              (incf i 100))
            (yaxception:catch 'error e
              (incf i 10)))
          (incf i 5))
        (yaxception:catch 'error e
          (incf i 2)))
      i)))


(require 'yaxception)
(require 'el-expectations)

(expectations
  (desc "errscope catch")
  (expect 0
    (let ((ret1 "")
          (ret2 "")
          (i 0))
      (yaxception:$
        (yaxception:try
          (yaxception:$
            (yaxception:try
              (delete-file "not found"))
            (yaxception:catch 'file-error e
              (setq ret1 (error-message-string e))
              (replace-regexp-in-string "0" "" i))
            (yaxception:catch 'error e
              (setq ret1 (error-message-string e))
              (replace-regexp-in-string "0" "" i))))
        (yaxception:catch 'wrong-type-argument e
          (setq ret2 (error-message-string e)))
        (yaxception:catch 'error e
          (setq ret2 (error-message-string e))))
      (and (string-match "\\`Removing old name: " ret1)
           (string-match "\\`Wrong type argument: " ret2)))))


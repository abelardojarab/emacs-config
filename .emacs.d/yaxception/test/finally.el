(require 'yaxception)
(require 'el-expectations)

(expectations
  (desc "finally do")
  (expect "CaughtFinished"
    (let ((msg ""))
      (yaxception:$
        (yaxception:try
          (delete-file "not found")
          (setq msg (concat msg "OK")))
        (yaxception:catch 'error e
          (setq msg (concat msg "Caught")))
        (yaxception:finally
          (setq msg (concat msg "Finished"))))
      msg)))

(expectations
  (desc "finally do nest")
  (expect "ThrowingFinishedCaughtComplete"
    (let ((msg ""))
      (yaxception:$
        (yaxception:try
          (yaxception:$
            (yaxception:try
              (delete-file "not found")
              (setq msg (concat msg "OK")))
            (yaxception:catch 'error e
              (setq msg (concat msg "Throwing"))
              (yaxception:throw e))
            (yaxception:finally
              (setq msg (concat msg "Finished")))))
        (yaxception:catch 'error e
          (setq msg (concat msg "Caught")))
        (yaxception:finally
          (setq msg (concat msg "Complete"))))
      msg)))


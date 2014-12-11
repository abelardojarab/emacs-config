(require 'yaxception)
(require 'el-expectations)


(expectations
  (desc "err-symbol-p message is not err symbol")
  (expect nil
    (yaxception-err-symbol-p 'message)))

(expectations
  (desc "err-symbol-p file-error is err symbol")
  (expect t
    (yaxception-err-symbol-p 'file-error)))

(expectations
  (desc "err-symbol-p symbol by deferror is err symbol")
  (expect t
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX test.")
    (yaxception-err-symbol-p 'yaxception-test-error)))


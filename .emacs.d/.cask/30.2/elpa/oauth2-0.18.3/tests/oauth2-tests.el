;;; oauth2-tests.el --- oauth2.el tests -*- lexical-binding: t -*-

(require 'oauth2)
(require 'ert)

(ert-deftest oauth2--build-url-param-str-test ()
  (should (string=
           (oauth2--build-url-param-str "simple" "plain"
                                        "empty" nil
                                        "empty2" ""
                                        "email" "a@example.com")
           "simple=plain&email=a%40example.com"))
  (should (string=
           (oauth2--build-url-param-str "url" "http://localhost"
                                        "random" "12+3_4_=5=/6/")
           "url=http%3A%2F%2Flocalhost&random=12%2B3_4_%3D5%3D%2F6%2F"))
  (should-error (oauth2--build-url-param-str "novalue")
                :type 'error))

(ert-deftest oauth2--build-url-test ()
  (should (string=
           (oauth2--build-url "http://127.0.0.1"
                              "request=auth&login_hint=manphiz%40outlook.com")
           "http://127.0.0.1?request=auth&login_hint=manphiz%40outlook.com"))
  (should (string=
           (oauth2--build-url "https://localhost"
                              "simple" "plain"
                              "empty" nil
                              "complex" "1+2@3#4_5/6"
                              "empty2" "")
           "https://localhost?simple=plain&complex=1%2B2%403%234_5%2F6")))

(ert-deftest oauth2--generate-code-verifier-length-test ()
  ;; base64 encoding on a string of 90 results in 120.
  (should (=
           (length (oauth2--generate-code-verifier 90))
           120)))

(ert-deftest oauth2--get-challenge-from-verifier-test ()
  ;; Using pre-generated code-verifier values from mutt_oauth2.py for testing.
  (let ((test-cases
         '((:verifier
            "nDe_cq5hGQC6-_OUhE4Y3jVdrPmRVvzSRuNci4efeXeHBiGSqAmVbzMioNMwD1fQn96IL2mChFBzhv2kI02kHNTU1tHI2T9tWn5_Lp9rqy3fGR90WYxYXGKz"
            :challenge "hqvORBgWMedJHg2HnNs7DcRjEnVuk7gGQi9iBcp7PRs")
           (:verifier
            "WItNqcP9W_HFOZV__P5FgYKlbkTOBolU0jWMMIiTTh6rcG3TyoRtV4Ozx7nIJhowhjAjt41gmHwuKgxGhtv1k_5XDj52udYwHdSgqUrmkvhaqYgLADAp7rrf"
            :challenge "lB2AKQFg6caqfa3u0cnxXihnU69vvGG1cUPRi8_cvpE")))
        (expected-challenge-length 43))
    (dolist (test-case test-cases)
      (let* ((verifier (plist-get test-case :verifier))
             (challenge (oauth2--get-challenge-from-verifier verifier))
             (expected-challenge (plist-get test-case :challenge)))
        (should (string= challenge expected-challenge))
        (should (= (length challenge) expected-challenge-length))))))

;;; oauth2-tests.el ends here.

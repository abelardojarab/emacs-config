
;;; requires and setup

(when load-file-name
  (setq pcache-directory (expand-file-name "test_output/" (file-name-directory load-file-name)))
  (setq package-enable-at-startup nil)
  (setq package-load-list '((pcache t)
                            (list-utils t)
                            (persistent-soft t)))
  (when (fboundp 'package-initialize)
    (package-initialize)))

(require 'list-utils)
(require 'persistent-soft)
(require 'font-utils)


;;; font-utils--repair-split-list

(ert-deftest font-utils--repair-split-list-01 nil
  (let* ((value "this:that:the:other")
         (split-val (split-string value ":")))
    (should
     (equal split-val
            (font-utils--repair-split-list split-val ":")))))

(ert-deftest font-utils--repair-split-list-02 nil
  (let* ((value "this:that:the\\:other")
         (split-val (split-string value ":")))
    (should
     (equal '("this" "that" "the\\:other")
            (font-utils--repair-split-list split-val ":")))))

(ert-deftest font-utils--repair-split-list-03 nil
  (let* ((value "this:that:the:other:")
         (split-val (split-string value ":")))
    (should
     (equal split-val
            (font-utils--repair-split-list split-val ":")))))

(ert-deftest font-utils--repair-split-list-04 nil
  (let* ((value "this:that:the:other\\:")
         (split-val (split-string value ":")))
    (should
     (equal '("this" "that" "the" "other\\:")
            (font-utils--repair-split-list split-val ":")))))

(ert-deftest font-utils--repair-split-list-05 nil
  (should
   (equal '("family" "demi\\-bold")
          (font-utils--repair-split-list
           (split-string (replace-regexp-in-string
                          "\\-\\(semi\\|demi\\|half\\|double\\|ultra\\|extra\\)-" "-\\1\\\\-" "family-demi-bold") "-") "-"))))


;;; font-utils-parse-name

(ert-deftest font-utils-parse-name-01 nil
  (should
   (equal '("Courier" nil)
          (font-utils-parse-name "Courier"))))


(ert-deftest font-utils-parse-name-02 nil
  (should
   (equal '("Courier\\:colon" nil)
          (font-utils-parse-name "Courier\\:colon"))))


(ert-deftest font-utils-parse-name-03 nil
  (should
   (equal '("Courier" ("size=12"))
          (font-utils-parse-name "Courier-12"))))


(ert-deftest font-utils-parse-name-04 nil
  (should
   (equal '("Courier" ("foundry=apple" "size=12" "width=condensed"))
          (font-utils-parse-name "Courier-12:width=condensed:foundry=apple"))))


(ert-deftest font-utils-parse-name-05 nil
  (should
   (equal '("Courier" ("foundry=apple" "size=12" "width=condensed"))
          (font-utils-parse-name "Courier-12::foundry=apple:width=condensed:"))))


;;; font-utils-normalize-name

(ert-deftest font-utils-normalize-name-01 nil
  (should
   (equal "Courier:foundry=apple:size=12:width=condensed"
          (font-utils-normalize-name "Courier-12::width=condensed:foundry=apple:"))))


;;; font-utils-is-qualified-variant

(ert-deftest font-utils-is-qualified-variant-01 nil
  (should-not
   (font-utils-is-qualified-variant "Dai Banna SIL Book" "Dai Banna SIL Book")))

(ert-deftest font-utils-is-qualified-variant-02 nil
  (should
   (font-utils-is-qualified-variant "Dai Banna SIL Book" "Dai Banna SIL Book:style=Regular")))

(ert-deftest font-utils-is-qualified-variant-03 nil
  (should
   (font-utils-is-qualified-variant "Dai Banna SIL Book:style=Regular" "Dai Banna SIL Book")))

(ert-deftest font-utils-is-qualified-variant-04 nil
  (should-not
   (font-utils-is-qualified-variant "Dai Banna SIL Book:style=Regular" "Dai Banna SIL Book:style=Regular")))

(ert-deftest font-utils-is-qualified-variant-05 nil
  (should-not
   (font-utils-is-qualified-variant "Dai Banna SIL Book:style=Regular:width=condensed" "Dai Banna SIL Book:style=Regular:width=condensed")))

(ert-deftest font-utils-is-qualified-variant-06 nil
  (should-not
   (font-utils-is-qualified-variant "Dai Banna SIL Book:style=Regular:width=condensed" "Dai Banna SIL Book:width=condensed:style=Regular")))


;;; font-utils-name-from-xlfd

(ert-deftest font-utils-name-from-xlfd-01 nil
  (should (equal "Monaco"
                 (font-utils-name-from-xlfd
                  "-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"))))

(ert-deftest font-utils-name-from-xlfd-02 nil
  (should (equal "Handwriting_-_Dakota"
                 (font-utils-name-from-xlfd
                  "-apple-Handwriting_-_Dakota-medium-normal-normal-*-12-*-*-*-p-0-iso10646-1"))))

(ert-deftest font-utils-name-from-xlfd-03 nil
  (should (equal "Monaco"
                 (font-utils-name-from-xlfd
                  "-apple-Monaco-bold-normal-normal-*-12-*-*-*-m-0-iso10646-1"))))

(ert-deftest font-utils-name-from-xlfd-04 nil
  (should (equal "Monaco"
                 (font-utils-name-from-xlfd
                  "-apple-Monaco-demi-bold-normal-normal-*-12-*-*-*-m-0-iso10646-1"))))


;;; font-utils-create-fuzzy-matches

(ert-deftest font-utils-create-fuzzy-matches-01 nil
  (should (equal '("Monaco")
                 (font-utils-create-fuzzy-matches "Monaco"))))

(ert-deftest font-utils-create-fuzzy-matches-02 nil
  (should (equal '("Monaco-12" "Monaco")
                 (font-utils-create-fuzzy-matches "Monaco-12"))))

(ert-deftest font-utils-create-fuzzy-matches-03 nil
  (should (equal '("Monaco-12")
                 (font-utils-create-fuzzy-matches "Monaco-12" 'keep-size))))

(ert-deftest font-utils-create-fuzzy-matches-04 nil
  (should (>= 7
              (length
               (font-utils-create-fuzzy-matches "DejaVu Sans Mono")))))


;;; font-utils-lenient-name-equal

(ert-deftest font-utils-lenient-name-equal-01 nil
  (should
   (font-utils-lenient-name-equal "DejaVu Sans Mono" "dejavusansmono")))

(ert-deftest font-utils-lenient-name-equal-02 nil
  (should
   (font-utils-lenient-name-equal "DejaVu Sans Mono" "DE-JA_VU_SANS-MONO")))

(ert-deftest font-utils-lenient-name-equal-03 nil
  (should-not
   (font-utils-lenient-name-equal "DejaVu Sans Mono" "DE-JA_VU_SANS-MON0")))


;;; font-utils-load-names

(ert-deftest font-utils-load-names-01 nil
  :tags '(:interactive)
  (should (hash-table-p
           (progn
             (font-utils-load-names 'progress 'regenerate)
             font-utils-all-names))))


;;; font-utils-list-names

(ert-deftest font-utils-list-names-01 nil
  :tags '(:interactive)
  (should (< 20
             (length (font-utils-list-names)))))

(ert-deftest font-utils-list-names-02 nil
  :tags '(:interactive)
  (should (member "Courier"
                  (font-utils-list-names))))

(ert-deftest font-utils-list-names-03 nil
  :tags '(:interactive)
  (should (member "Arial"
                  (font-utils-list-names))))


;;; font-utils-first-existing-font

(ert-deftest font-utils-first-existing-font-01 nil
  :tags '(:interactive)
  (should
   (font-utils-first-existing-font '("Courier" "Arial" "Monaco" "Consolas" "DejaVu Sans"))))


;;; font-utils-read-name

(ert-deftest font-utils-read-name-01 nil
  :tags '(:interactive)
  (should (equal "Courier"
                 (let ((cursor-in-echo-area t))
                   (read-char "Press a key, then enter \"Courier\" at the next prompt (with completions).")
                   (font-utils-read-name)))))

(ert-deftest font-utils-read-name-02 nil
  :tags '(:interactive)
  (should (equal "Courier"
                 (let ((cursor-in-echo-area t))
                   (read-char "Press a key, then enter \"Courier\" at the next prompt (with ido completions).")
                   (font-utils-read-name 'ido)))))


;;; font-utils-exists-p

(ert-deftest font-utils-exists-p-01 nil
  :tags '(:interactive)
  (should
   (font-utils-exists-p "Courier")))

(ert-deftest font-utils-exists-p-02 nil
  :tags '(:interactive)
  (should-not
   (font-utils-exists-p "__nonexistent-font__")))



;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;

;;; font-utils-test.el ends here

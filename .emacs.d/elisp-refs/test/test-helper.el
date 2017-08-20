;;; test-helper.el --- Helper for tests              -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Wilfred Hughes

;; Author:  <me@wilfred.me.uk>

;;; Code:

(require 'ert)
(require 'f)

(let ((elisp-refs-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path elisp-refs-dir))

(require 'undercover)
(undercover "elisp-refs.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))

;;; test-helper.el ends here

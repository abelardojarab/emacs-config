[![Build Status](https://travis-ci.org/aki2o/yaxception.svg?branch=master)](https://travis-ci.org/aki2o/yaxception)

What's this?
============

This is a extension of Emacs provides framework about exception like Java for Elisp.


Feature
=======

### Grammar like Java's try/catch/finally/throw

You can write like the following about exception.

```lisp
(yaxception:$
  (yaxception:try
    (delete-file "not found"))
  (yaxception:catch 'file-error e
    (message "Error happened by handling file : %s" (yaxception:get-text e)))
  (yaxception:catch 'error e
    (message "Error happened by something : %s" (yaxception:get-text e))
    (yaxception:throw e))
  (yaxception:finally
    (message "finished doing")))
```

### Customized Error

For customizing error, you can write like the following.

```lisp
;; define hoge-error as child of file-error
(yaxception:deferror 'hoge-error
                     'file-error
                     "Errored by hoge file - path:[%s] size:[%s]" 'path 'size)

;; raise customized error
(yaxception:throw 'hoge-error
                  :path "c:/hoge.txt"
                  :size (nth 7 (file-attributes "c:/hoge.txt")))
```

### Stacktrace like Java

If want to get stacktrace, the way is only getting the buffer named "\*Backtrace\*" as far as I know.  
But it has some bad points. List them at the following ...

* It need `(setq debug-on-error t)`.
* The buffer has trouble for seeing its stacktrace because all of function/special-form shown.
* Can't get string of stacktrace without popup the buffer.

So, it's enabled that you get the string of stacktrace easy for seeing.  
For example, if you write the following.

```lisp
(defun aaa (aa bb &optional cc) (bbb bb))
(defun bbb (bb) (ccc))
(defun ccc () (replace-regexp-in-string " " "" 'hogege))
(defun xxx (zzz) "xxx")
(defun yyy () "yyy")

(yaxception:$
  (yaxception:try
    (xxx "hoge")
    (yyy)
    (aaa "ABC" "DEF" '("GHI" "JKL")))
  (yaxception:catch 'error e
    (message "%s" (yaxception:get-stack-trace-string e))))
```

Then, output the following.

    Exception is 'wrong-type-argument'. Wrong type argument: sequencep, hogege
      at replace-regexp-in-string(\" \" \"\" hogege)
      at ccc()
      at bbb(\"DEF\")
      at aaa(\"ABC\" \"DEF\" (\"GHI\" \"JKL\"))
      in yaxception:try

List function only.  
I want to list macro, but I don't know the way that discriminate macro from builtin special-form.


Install
=======

### If use package.el

2013/07/19 It's available by using melpa.  

### If use el-get.el

2013/07/26 It's available. But, master branch only.  

### If use auto-install.el

```lisp
(auto-install-from-url "https://raw.github.com/aki2o/yaxception/master/yaxception.el")
```

### Manually

Download yaxception.el and put on your load-path.


Usage
=====

### Initially

Write the following in the elisp.

```lisp
(require 'yaxception)
```

### Start handling exception

Use `yaxception:$` for starting handling exception.  
`yaxception:$` receive the following arguments.

1. A sexp of `yaxception:try`.
2. The sexp of `yaxception:catch` or `yaxception:finally`. This is optional.

`yaxception:$` has the following specification.

* If error is happened while run `yaxception:try`, run `yaxception:catch` that match it first.
About matching, see "Match exception" section below.
* If not use `yaxception:catch` matched the happened error, raise its error.
* Use `yaxception:finally` only once.
* If has `yaxception:finally`, execute it at last without relation to if error was happened.
* Return the value that last run sexp other than `yaxception:finally` return.

### Catch exception

Use `yaxception:catch` for each error which you want to catch.  

```lisp
(yaxception:catch

    ;; The error which you want to catch
    'error
    
    ;; The variable which you want to use as the error
    err
    
    ;; If catch the error, run and return the last sexp returned
    (message "Error happened by something : %s" (yaxception:get-text err))
    (yaxception:throw err))
```

### Match exception

Error symbol has `error-conditions`. e.g. `file-error` has `'(file-error error)`.  
If happen `file-error`,  
run `(yaxception:catch 'file-error ...)` or `(yaxception:catch 'error ...)` first.

### Throw exception

You can use `yaxception:throw` to signal error anywhere.  
`yaxception:throw` has the following way for use.

```lisp
;; Throw the error object directly
(yaxception:throw err)

;; Signal the error of the symbol
(yaxception:throw 'file-error)

;; If signal, you can give the keyword arguments.
;; Their value is replaced with the special part of the error message.
;; You can get them by yaxception:get-prop.
(yaxception:throw 'hoge-error
                  :path "c:/hoge.txt"
                  :size (nth 7 (file-attributes "c:/hoge.txt")))
```

### Customize exception

You can use `yaxception:deferror` to define the customized error.  

```lisp
(yaxception:deferror

 ;; The error symbol. It's OK that it's defined not yet.
 'hoge-error
 
 ;; THe parent of the error. If nil, this value is 'error.
 'file-error
 
 ;; The message of the error. This value is passed to format.
 "Errored by hoge file - path:[%s] size:[%s]"
 
 ;; The keyword symbol replaced with the special part (e.g. %s) of the above message.
 ;; See "Throw exception" section above.
 'path 'size)
```

### Other

For getting the message of the error, you can use `yaxception:get-text`.  
For getting the property of the error, you can use `yaxception:get-prop`.  
For getting the stacktrace of the error, you can use `yaxception:get-stack-trace-string`.


Tested On
=========

* Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK


**Enjoy!!!**


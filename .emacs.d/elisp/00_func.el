;; -*- encoding: utf-8-unix; -*-
;; * arg-parse
(defun arg-parse (lst)
  (if (atom (car lst))
      lst
    (car lst)))

;; * findp
(defun findp (fn lst &optional default)
  "find-if"
  (while
      (and
       lst
       (null
        (if (funcall fn (car lst))
            (setq default (car lst))
          nil)))
    (setq lst (cdr lst)))
  default)

;; * list-to-alist
(defun to-alist (&rest lst)
  "(to-alist '(1 2 3 4 5 6)) => ((1 . 2) (3 . 4) (5 . 6))"
  (let ((l (if (listp (car lst)) (car lst) lst))
        (alist (lambda(x)
                 (if x
                     (cons
                      (cons (nth 0 x)(nth 1 x))
                      (funcall alist (nthcdr 2 x)))))))
    (funcall alist l)))

(defun group (source &optional n)
  (let ((n (or n 2))
        (rec (lambda (source acc)
               (let ((rest (nthcdr n source)))
                 (if (consp rest)
                     (funcall rec rest (cons (butlast source (- (length source) n)) acc))
                   (nreverse (cons source acc)))))))
    (if source (funcall rec source nil) nil)))

;; * make hash-table
(defun mkhtb (&rest rest)
  (let* ((lst (if (eq (logand (length rest) 1) 1)
                  `[,@rest nil]
                `[,@rest]))
         (cnt (/ (length lst) 2))
         (size (+ cnt 2 (/ cnt 5)))
         (h (make-hash-table :test 'equal :size size)))
    (while (> cnt 0)
      (puthash
       (aref lst (- (* cnt 2) 2)) (aref lst (- (* cnt 2) 1)) h)
      (setq cnt (1- cnt)))
    h))
(defmacro mkht (&rest rest)
  `(apply 'mkhtb '(,@rest)))

;; * concat symbol
(defun concat-symbol (&rest lst)
  (intern (apply 'concat (mapcar (lambda(x)(if (symbolp x) (symbol-name x) x)) lst))))

;; * define-key-s
(defun define-key-s (keymap key-defs &optional group)
  "(define-key-s 0 '(\"key\" def \"key\" def ...))
\(define-key-s 0 '(\"a\" \"b\" \"c\" ...) 'self-insert-command)
If keymap is 0, run as global-set-key
If keymap is 1, run as local-set-key
If keymap is xxx-mode-map, run as define-key xxx-mode-map
See also `def-key-s'."
  (let ((map (cond
              ((eq keymap 0) (current-global-map))
              ((eq keymap 1) (current-local-map))
              (t keymap)))
        (defs (if (null group)
                  (to-alist key-defs)
                (mapcar (lambda (k) (cons k group)) key-defs))))
    (mapc
     ;; [remap COMMAND] remaps any key binding for COMMAND
     (lambda (d)
       (let ((k (car d)))
         (define-key
           map
           (if (stringp k) (eval `(kbd ,k)) k)
           (cdr d))))
     ;; (lambda (d) (define-key map (eval `(kbd ,(car d))) (cdr d)))
     defs)))

(defmacro def-k-s (km &rest kd)
  "(def-key-s map \"key\" def \"key\" def ...)
See also `define-key-s'."
  ;; (list 'define-key-s km `',kd))
  `(define-key-s ,km ',kd))

(defun def-key-s (keymap &rest key-defs)
  "(def-key-s map \"key\" 'def \"key\" 'def ...)
See also `define-key-s'."
  (define-key-s keymap key-defs))

;; * adjust-color
(defun adjust-color (color percentage)
  (let ((p (* 65535 (/ percentage 100.0))))
    (apply
     (lambda(r g b)
       (format "#%02x%02x%02x" r g b))
     (mapcar
      (lambda(x)
        (let ((v (+ x p)))
          (/ (cond
              ((> v 65535) 65535)
              ((< v 0) 0)
              (t v))
             257.0)))
      (color-values color)))))

;; * add-exec-path
(defun add-environment (env elt)
  (let* ((sep (if (eq system-type 'windows-nt)
                  ";" ":"))
         (ev (split-string (or (getenv env) "") sep))
         nev)
    (unless (member elt ev)
      (mapc
       (lambda(x)(if (equal x "") nil
              (setq nev (append nev (list x)))))
       (cons elt ev))
      (setenv env (mapconcat 'identity nev sep)))))

(defun add-exec-path (path)
  (interactive "Dexec-path: ")
  (add-environment "PATH" path)
  (push path exec-path))
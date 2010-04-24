;;; imakado.el - imakado's usefull macros

;; Author: IMAKADO <ken.imakado@gmail.com>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Prefix: ik:

;;; Commentary:

;;; Todo:

(eval-when-compile
  (require 'cl)
  (require 'rx)
  ) ;; eval-when-compile

;;;; Version
(defvar imakado-version 0.01)
(defun imakado-require-version (v)
  (unless (>= imakado-version v)
    (error "\
You need to upgrade imakado.el %s to %s.
Loaded imakado.el is %s.
Your version is %s."
           imakado-version
           v
           (progn (require 'find-func)
                  (and (fboundp 'find-library-name)
                       (find-library-name "imakado")))
           imakado-version))
  t)

;;;; with-gensyms
(eval-and-compile
  (defvar ik:gensym-prefix "--imakado--")
  (defvar ik:*gensym-counter* 0)
  (defun ik:gensym (&optional prefix)
    "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"G\"."
    (let ((pfix (if (stringp prefix) prefix ik:gensym-prefix))
          (num (if (integerp prefix) prefix
                 (prog1 ik:*gensym-counter*
                   (setq ik:*gensym-counter* (1+ ik:*gensym-counter*))))))
      (make-symbol (format "%s%d" pfix num))))
  (defun* ik:group (source n &key (error-check nil))
    (assert (not (zerop n)))
    (when error-check
      (assert (= (mod (length source) n) 0)))
    (let ((copied-source (copy-sequence source))
          (res))
      (while copied-source
        (push (let* ((ret nil)
                     (source-len (length copied-source)))
                (dotimes (var (min source-len n) (nreverse ret))
                  (push (pop copied-source) ret)))
              res))
      (nreverse res)))
  (defun ik:allquote (args)
    (loop for arg in args
          collect `(quote ,arg)))
  (defun* ik:in-aux-test (v choises &key (test 'eq))
    (loop for c in choises
          collect `(,test ,v ,c)))
  (defun ik:flatten (list)
    "Flatten any lists within ARGS, so that there are no sublists."
    (loop for item in list
          if (listp item)
          nconc (ik:flatten item)
          else
          collect item))
  ;; copied from cl
  (defun ik:subseq (seq start &optional end)
    "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
    (if (stringp seq) (substring seq start end)
      (let (len)
        (and end (< end 0) (setq end (+ end (setq len (length seq)))))
        (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
        (cond ((listp seq)
               (if (> start 0) (setq seq (nthcdr start seq)))
               (if end
                   (let ((res nil))
                     (while (>= (setq end (1- end)) start)
                       (push (pop seq) res))
                     (nreverse res))
                 (copy-sequence seq)))
              (t
               (or end (setq end (or len (length seq))))
               (let ((res (make-vector (max (- end start) 0) nil))
                     (i 0))
                 (while (< start end)
                   (aset res i (aref seq start))
                   (setq i (1+ i) start (1+ start)))
                 res))))))
  (defun* ik:remove-if (pred seq &optional (key 'identity))
    (loop for elem in seq
          unless (funcall pred (funcall key elem))
          collect elem))
  (defun* ik:remove-if-not (pred seq &optional (key 'identity))
    (loop for elem in seq
          when (funcall pred (funcall key elem))
          collect elem))
  ) ;; eval-and-compile

(defmacro with-gensyms (syms &rest body)
  (declare (indent 1)
           (debug ((&rest symbolp)
                   body)))
  (let ((clauses (loop for sym in syms
                      collect `( ,sym  ',(ik:gensym ik:gensym-prefix) ))))
    `(let ( ,@clauses )
       ,@body)))

(defmacro with-lexical-bindings (syms &rest body)
  (declare (indent 1)
           (debug ((&rest symbolp)
                   body)))
  (let ((clauses (loop for sym in syms
                      collect `( ,sym  ,sym ))))
    `(lexical-let ( ,@clauses )
       ,@body)))

;;;; dirconcat
(defmacro dirconcat (d1 str)
  (declare (debug (form form)))
  (with-gensyms (d1-tmp str-temp)
    `(let* ((,d1-tmp ,d1)
            (,d1-tmp (if (file-directory-p ,d1-tmp)
                         (file-name-as-directory ,d1-tmp)
                       (error "not directory: %s" ,d1-tmp)))
            (,str-temp ,str))
       (concat ,d1-tmp ,str))))

;;;; remif
(defsubst ik:remif-aux (pred seq key cond)
  (with-gensyms (g-pred g-seq g-key g-elem)
    `(let ((,g-pred ,pred)
           (,g-seq ,seq)
           (,g-key ,key)
           (,g-elem nil))
       (loop for ,g-elem in ,g-seq
             ,cond (funcall ,g-pred (funcall ,g-key ,g-elem))
             collect ,g-elem))))
(defmacro* remif (pred seq &key (key (quote 'identity)))
  (declare (debug (form form &rest [":key" function-form])))
  (ik:remif-aux pred seq key 'unless))

(defmacro* !remif (pred seq &key (key (quote 'identity)))
  (declare (debug (form form &rest [":key" function-form])))
  (ik:remif-aux pred seq key 'when))


;;;; Special
;; almost copied from anything.el
(defmacro aif (test-form then-form &rest else-forms)
  (declare (indent 2)
           (debug (form form &rest form)))
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro awhen (test-form &rest body)
  (declare (indent 1)
           (debug (form body)))
  `(aif ,test-form
       (progn ,@body)))

(defmacro awhile (expr &rest body)
  (declare (indent 1)
           (debug (form body)))
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (declare (debug (&rest form)))
  (cond
   ((null (car args)) t)
   ((null (cdr args)) (car args))
   (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (declare (indent 0)
           (debug cond))
  (cond
   ((null clauses) nil)
   (t
    (with-gensyms (test)
      (let ((clause (car clauses)))
        `(let ((,test ,(car clause)))
           (if ,test
               (let ((it ,test))
                 ,@(cdr clause))
             (acond ,@(cdr clauses)))))))))

(defmacro alambda (params &rest body)
  (declare (indent 1)
           (debug lambda))
  `(labels ((caller ,params ,@body))
     'caller))

;;;; cond-let
(defsubst ik:cond-let-aux-vars (clauses)
  (let ((vars (delete-dups
               (loop for cla in clauses
                     append (mapcar 'car (cdr cla))))))
    (loop for var in vars
          collect (cons var (ik:gensym ik:gensym-prefix)))))
(defsubst ik:cond-let-aux-binds (vars cla)
  (loop for bindform in (cdr cla)
        for (bind-var . bind-body) = bindform
        when (consp bindform)
        collect `( ,(assoc-default bind-var vars) . ,bind-body)))
(defsubst ik:cond-let-aux-clause (vars cla body-fn)
  `( ,(car cla)  (let ,(mapcar 'cdr vars)
                   (let ,(ik:cond-let-aux-binds vars cla)
                     (,body-fn ,@(mapcar 'cdr vars))))))

(defmacro cond-let (clauses &rest body)
  (declare (indent 1)
           (debug ((&rest (form &rest (symbolp body))) body)))
  (with-gensyms (body-fn)
    (let ((vars (ik:cond-let-aux-vars clauses)))
      `(labels ((,body-fn ,(mapcar 'car vars)
                          ,@body))
         (cond
          ,@(loop for cla in clauses
                  collect (ik:cond-let-aux-clause vars cla body-fn)))))))

;;;; fn
(eval-when-compile
  (defvar ik:fn-aux-anaph-arg-map
    (loop for n from 1 to 20
          for sym = (intern (format "_%s" n))
          collect `( ,sym . ,n))))
(defsubst ik:fn-aux-anaph-arg-syms (flatten fn-args-arg fn-args-rest)
  (and (not (equal fn-args-arg '(_)))
       (let ((anaph-arg-syms (mapcar 'car (eval-when-compile
                                            ik:fn-aux-anaph-arg-map))))
         (ik:remove-if-not (lambda (atom)
                             (memq atom anaph-arg-syms))
                           flatten))))
(defsubst ik:fn-aux-appear_? (flatten fn-args-arg fn-args-rest)
  (and (not (equal fn-args-arg '(_)))
       ;; (fn (_ b) (list b))
       (or (and (null fn-args-rest)
                (memq '_ (ik:flatten fn-args-arg)))
           (and fn-args-rest
                (not (remove '_ fn-args-arg))))
       (memq  '_ flatten)))
(defsubst ik:fn-aux (fn-args)
  (let ((flatten (ik:flatten fn-args))
        (fn-args-arg (car fn-args))
        (fn-args-rest (cdr fn-args)))
    (acond
      ((ik:fn-aux-anaph-arg-syms flatten fn-args-arg fn-args-rest)
       (assert (not (memq '_ flatten))
               nil
               "cant use \"_\" and \"(_1 _2 ...)\" at the same time!!")
       (let* ((arg-count (apply 'max
                                (loop for sym in it
                                      collect (assoc-default sym (eval-when-compile
                                                                   ik:fn-aux-anaph-arg-map)))))
              (lambda-args (mapcar 'car (ik:subseq (eval-when-compile ik:fn-aux-anaph-arg-map) 0 arg-count))))
         `(lambda ,lambda-args
            (let ((_0 (list ,@lambda-args)))
              ,@fn-args))))
      ((ik:fn-aux-appear_? flatten fn-args-arg fn-args-rest)
       `(lambda (_) ,@fn-args))
      (t
       `(lambda ,@fn-args)))))
(defmacro fn (&rest args)
  (declare (indent defun)
           (debug (&or [&define lambda-list
                                [&optional stringp]
                                [&optional ("interactive" interactive)]
                                def-body]
                       body)))
  `,(ik:fn-aux args))


;;;; Macro aliases
(defmacro define-macro-alias (short long)
  `(defmacro ,short (&rest args)
     (declare ,(awhen (and (fboundp long)
                           (get long 'lisp-indent-function))
                 `(indent ,it))
              ,(awhen (and (fboundp long)
                           (get long 'edebug-form-spec))
                 `(debug ,it)))
     ,(documentation long)
     `(,',long ,@args)))

(defmacro define-macro-aliases (&rest args)
  `(progn
     ,@(loop for (short long) in (ik:group args 2 :error-check t)
             collect `(define-macro-alias ,short ,long))))

;;;; define abbrevs
(define-macro-aliases
  dbind destructuring-bind
  mvbind multiple-value-bind
  mvsetq multiple-value-setq
  )

;;; destructuring-bind's edebug-spec has broken.
;;; so fix it.
(def-edebug-spec dbind
  (loop-var form body))


;;;; List

(defmacro cars (seq)
  (declare (debug (form)))
  `(mapcar 'car ,seq))

(defmacro cdrs (seq)
  (declare (debug (form)))
  `(mapcar 'cdr ,seq))

(defmacro cadrs (seq)
  (declare (debug (form)))
  `(mapcar 'cadr ,seq))

(defmacro assoc-cdrs (keys alist &optional test default)
  (declare (debug (form form)))
  `(mapcar (fn (assoc-default _ ,alist ,test ,default)) ,keys))

(defmacro nths (n seq)
  (declare (debug (form form)))
  `(mapcar (fn (nth ,n _)) ,seq))

(defmacro in (obj  &rest choises)
  (declare (indent 1)
           (debug (form &rest form)))
  (with-gensyms (insym)
    `(let ((,insym ,obj))
       (or ,@(ik:in-aux-test insym choises)))))

(defmacro inq (obj &rest args)
  (declare (indent 1)
           (debug (form &rest symbolp)))
  `(in ,obj ,@(ik:allquote args)))

(defmacro in= (obj &rest choises)
  (declare (indent 1)
           (debug in))
  (with-gensyms (inobj)
    `(let ((,inobj ,obj))
       (or ,@(ik:in-aux-test inobj choises
                             :test 'equal)))))

(defmacro* join+ (seq &optional (separator "\n"))
  (declare (debug (form &optional form)))
  `(mapconcat 'identity ,seq ,separator))


;;; Case
(defsubst* ik:case-cond-clause-aux (v cla &optional (test 'in))
  (dbind (key . body) cla
    (cond
     ((consp key) `((,test ,v ,@key) ,@body))
     ((inq key t otherwise) `(t ,@body))
     (t (error "bad clause")))))
(defmacro xcase (expr &rest clauses)
  (declare (indent 1)
           (debug case))
  (with-gensyms (v)
    `(let ((,v ,expr))
       (cond
        ,@(loop for cla in clauses
                collect (ik:case-cond-clause-aux v cla))))))

;;;; xcase=
(defsubst* ik:xcase-clause-aux-test (v keys body &key test)
  `((or ,@(loop for key in keys
                collect `(,test ,key ,v)))
    ,@body))
(defsubst* ik:xcase-clause-aux (v cla &key (test 'equal))
  (dbind (key . body) cla
    (cond
     ((consp key) `,@(ik:xcase-clause-aux-test v key body
                                               :test test))
     ((inq key t otherwise) `(t ,@body))
     (t (error "bad clause")))))
(defmacro xcase= (expr &rest clauses)
  (declare (indent 1)
           (debug (form &rest ((&rest form) body))))
  (with-gensyms (g-expr)
    `(let ((,g-expr ,expr))
       (cond
        ,@(loop for cla in clauses
                collect (ik:xcase-clause-aux g-expr cla))))))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ xcase= +++++")
      (expect '(OPEN "aaa" CLOSE)
        (let ((tokens '("[" "aaa" "]"))
              (tag1 "[")
              (tag2 "]"))
          (loop for token in tokens
                collect
                (xcase= token
                  ((tag1) 'OPEN)
                  ((tag2) 'CLOSE)
                  (otherwise
                   token)))))

      (desc "case sensitive.")
      (expect '(token-a token-b otherwise otherwise)
        (let ((tokens '("a" "b" "A" "B"))
              (case-fold-search t))
          (loop for token in tokens
                collect
                (xcase= token
                  (("a") 'token-a)
                  (("b") 'token-b)
                  (otherwise 'otherwise)))))

      (desc "key must be consed")
      (expect (error)
        (xcase= "a"
          ("a" 'a)))

      (desc "this is ok")
      (expect 'a
        (xcase= "a"
          (("a") 'a)))

      (desc "multi key")
      (expect '(OPEN "aaa" CLOSE OPEN "bbb" CLOSE)
        (let ((tokens '("[" "aaa" "]" "(" "bbb" ")"))
              (open "(")
              (openb "[")
              (close ")")
              (closeb "]"))
          (loop for token in tokens
                collect
                (xcase= token
                  ((open openb) 'OPEN)
                  ((close closeb) 'CLOSE)
                  (otherwise
                   token)))))

      (desc "nil")
      (expect '("nil" "otherwise" "nil" "otherwise")
        (let ((tokens '(nil a nil b)))
          (loop for token in tokens
                collect
                (xcase= token
                  ((nil) "nil")
                  (otherwise
                   "otherwise")))))
      )))


;;;; Struct
;; copied from slime.el
(defmacro* with-struct ((conc-name &rest slots) struct &body body)
  "Like with-slots but works only for structs.
\(fn (CONC-NAME &rest SLOTS) STRUCT &body BODY)"
  (declare (indent 2)
           (debug ((symbolp &rest symbolp) form body)))
  (flet ((reader (slot) (intern (concat (symbol-name conc-name)
                                        (symbol-name slot)))))
    (let ((struct-var (ik:gensym "with-struct")))
      `(let ((,struct-var ,struct))
         (symbol-macrolet
             ,(mapcar (lambda (slot)
                        (etypecase slot
                          (symbol `(,slot (,(reader slot) ,struct-var)))
                          (cons `(,(first slot) (,(reader (second slot))
                                                 ,struct-var)))))
                      slots)
           . ,body)))))

(defmacro* define-with-struct-macro (name conc-name)
  `(defmacro* ,name ( slots struct &rest body)
     (declare (indent 2)
              (debug ((&rest symbolp) form body)))
     `(with-struct (,',conc-name ,@slots) ,struct ,@body)))

(defsubst ik:with-struct-all-slots-get-getters-slot? (sym)
  (let ((plist (symbol-plist sym)))
    (and (memq 'cl-compiler-macro plist)
         (memq 'setf-method plist))))
(defsubst ik:with-struct-all-slots-get-all-slots (prefix)
  (loop for s in (all-completions prefix obarray)
        for sym = (intern s)
        when (ik:with-struct-all-slots-get-getters-slot? sym)
        collect (let ((slotname (replace-regexp-in-string (concat "^" prefix)
                                                          ""
                                                          s)))
                  (intern slotname))))
(defmacro define-with-all-slots-struct (name conc-name)
  `(defmacro ,name (struct &rest body)
     (declare (indent 1)
              (debug (form body)))
     (let ((ik:slots ',(ik:with-struct-all-slots-get-all-slots (symbol-name conc-name))))
       `(with-struct (,',conc-name ,@ik:slots) ,struct ,@body))))

;;;; Progress Reporter
(defmacro dolist-with-progress-reporter (spec message min-change min-time &rest body)
  (declare (indent 4)
           (debug ((symbolp form &optional form) form form form body)))
  (with-gensyms (seq seq-length reporter loop-temp)
    `(let* ((,seq ,(nth 1 spec))
            (,seq-length (length ,seq))
            (,reporter (make-progress-reporter ,message 0 (length ,seq) nil ,min-change ,min-time)))
       (loop for ,loop-temp from 0 to (1- ,seq-length)
             do (let ((,(nth 0 spec) (nth ,loop-temp ,seq)))
                  ,@body
                  (progress-reporter-update ,reporter
                                            ,loop-temp))
             finally return (prog1 ,(nth 2 spec)
                              (progress-reporter-done ,reporter))))))

;;;; Regexp
;; idea from rails-lib.el
(defmacro with-anaphoric-match-utilities (string-used-match &rest body)
  (declare (indent 1)
           (debug (form body)))
  (with-gensyms (str)
    `(lexical-let ((,str ,string-used-match))
       (symbol-macrolet ,(loop for i to 9 collect
                               (let ((sym (intern (concat "$" (number-to-string i)))))
                                 `(,sym (match-string ,i ,str))))
         (flet (($ (i) (match-string i ,str))
                ($sub (replacement &optional (i 0) &key fixedcase literal-string) ;args
                      (replace-match replacement fixedcase literal-string ,str i))) ;body
           (symbol-macrolet ( ;;before
                             ,(awhen str
                                `($b (substring ,it 0 (match-beginning 0))))
                             ;;match
                             ($m (match-string 0 ,str))
                             ;;after
                             ,(awhen str
                                `($a (substring ,it (match-end 0) (length ,str))))
                             )
             ,@body))))))

(defmacro =~ (regexp string &rest body)
  (declare (indent 2)
           (debug (form form body)))
  "regexp matching similar to the =~ operator found in other languages."
  (with-gensyms (str)
    `(let ((,str ,string))
       (when (string-match ,regexp ,str)
         (with-anaphoric-match-utilities ,str
           ,@(if body body '(t)))))))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "test initialize")
      (expect (true)
        (and (require 'imakado)
             (imakado-require-version 0.01)))

      (desc "+++++ =~ +++++")
      (desc "If match, return last expr")
      (expect 'b
        (=~ ".*" "re" 'a 'b))
      (expect nil
        (=~ ".*" "re" 'a nil))
      (expect t
        (=~ ".*" "re"))
      (desc "If fail, return nil")
      (expect nil
        (=~ "never match regexp" "string"))
      (expect nil
        (=~ "never match regexp" "string" 'body))

      (desc "with-anaphoric-match-utilities")
      (desc "$1 $2 ...")
      (expect '("Dog" "Alice")
        (let ((re (concat
                   "\\(" "\\w+" "\\)"   ;1
                   "[ \t]+"
                   "looking"
                   ".*"
                   "\\(" "Alice" "\\)"  ;2
                   "\\."
                   )))
          (=~ re "---- Dog looking at tiny Alice. ----"
            (list $1 $2))))
      (desc "($ 1) ($ 2) ...")
      (expect '("Dog" "Alice")
        (let ((re (concat
                   "\\(" "\\w+" "\\)"   ;1
                   "[ \t]+"
                   "looking"
                   ".*"
                   "\\(" "Alice" "\\)"  ;2
                   "\\."
                   )))
          (=~ re "---- Dog looking at tiny Alice. ----"
            (list ($ 1) ($ 2)))))
      (desc "$b, $m, $a")
      (expect '("---- " "Dog looking at tiny Alice." " ----")
        (let ((re (concat
                   "\\(" "\\w+" "\\)"   ;1
                   "[ \t]+"
                   "looking"
                   ".*"
                   "\\(" "Alice" "\\)"  ;2
                   "\\."
                   )))
          (=~ re "---- Dog looking at tiny Alice. ----"
            (list $b
                  $m
                  $a))))
      (desc "$sub") 
      (expect "---- gone ----"
        (let ((re (concat
                   "\\(" "\\w+" "\\)"   ;1
                   "[ \t]+"
                   "looking"
                   ".*"
                   "\\(" "Alice" "\\)"  ;2
                   "\\."
                   )))
          (=~ re "---- Dog looking at tiny Alice. ----"
            ($sub "gone"))))
      (desc "\\&")
      (expect "---- Dog looking at tiny Alice. ----"
        (let ((re (concat
                   "\\(" "\\w+" "\\)"   ;1
                   "[ \t]+"
                   "looking"
                   ".*"
                   "\\(" "Alice" "\\)"  ;2
                   "\\."
                   )))
          (=~ re "---- Dog looking at tiny Alice. ----"
            ($sub "\\&"))))
      (desc "\\N")
      (expect "---- Dog Alice ----"
        (let ((re (concat
                   "\\(" "\\w+" "\\)"   ;1
                   "[ \t]+"
                   "looking"
                   ".*"
                   "\\(" "Alice" "\\)"  ;2
                   "\\."
                   )))
          (=~ re "---- Dog looking at tiny Alice. ----"
            ($sub "\\1 \\2"))))
      (desc ":literal")
      (expect "---- \\1 \\2 ----"
        (let ((re (concat
                   "\\(" "\\w+" "\\)"   ;1
                   "[ \t]+"
                   "looking"
                   ".*"
                   "\\(" "Alice" "\\)"  ;2
                   "\\."
                   )))
          (=~ re "---- Dog looking at tiny Alice. ----"
            ($sub "\\1 \\2" 0 :literal-string t))))
      (expect (error)
        (let ((re (concat
                   "\\(" "\\w+" "\\)"   ;1
                   "[ \t]+"
                   "looking"
                   ".*"
                   "\\(" "Alice" "\\)"  ;2
                   "\\."
                   )))
          (=~ re "---- Dog looking at tiny Alice. ----"
            ;; missing subexp arg.
            ($sub "\\1 \\2" :literal-string t))))

      (desc "($sub subexp)")
      (expect t
        (let ((re (concat
                   "\\(" "\\w+" "\\)"   ;1
                   "[ \t]+"
                   "looking"
                   ".*"
                   "\\(" "Alice" "\\)"  ;2
                   "\\."
                   )))
          (let ((newstr (=~ re "---- Dog looking at tiny Alice. ----"
                          ($sub "elise" 2)))
                (case-fold-search nil)) ; case-sensitive
            (=~ "Elise" newstr))))
      (desc "($sub subexp :fixedcase t)")
      (expect nil
        (let ((re (concat
                   "\\(" "\\w+" "\\)"   ;1
                   "[ \t]+"
                   "looking"
                   ".*"
                   "\\(" "Alice" "\\)"  ;2
                   "\\."
                   )))
          (let ((newstr (=~ re "---- Dog looking at tiny Alice. ----"
                          ($sub "elise" 2
                                :fixedcase t)))
                (case-fold-search nil)) ; case-sensitive
            (=~ "Elise" newstr))))
      )))

;;; case-regexp
(defsubst ik:case-regexp-aux (expr-sym clauses)
  (loop for (regexp-or-sym . body) in clauses
        for body = (or body '(nil))
        collect
        (cond
         ((inq regexp-or-sym t otherwise)
          `(t ,@body))
         (t
          `((=~ ,regexp-or-sym ,expr-sym)
            (with-anaphoric-match-utilities ,expr-sym
              ,@body))))))
(defmacro case-regexp (expr &rest clauses)
  (declare (indent 1)
           (debug (form &rest (form body))))
  (with-gensyms (expr-tmp)
    `(let ((,expr-tmp ,expr))
       (etypecase ,expr-tmp
         (string (cond ,@(ik:case-regexp-aux expr-tmp clauses)))))))

;;;; match-with-temp-buffer
(defmacro match-with-temp-buffer (spec &rest body)
  (declare (indent 1)
           (debug ((form form &optional form) body)))
  (let* ((ret-form (nth 2 spec)))
    (with-gensyms (re str g-loop-res)
      `(dbind (,re ,str ) (list ,(nth 0 spec) ,(nth 1 spec))
         (with-temp-buffer
             (insert ,str)
             (loop with ,g-loop-res
                   initially (goto-char (point-min))
                   while (re-search-forward ,re nil t)
                   ,@(if ret-form
                        `( do (with-anaphoric-match-utilities nil
                                ,@body))
                       `(do (push (with-anaphoric-match-utilities nil
                                  ,@body)
                                ,g-loop-res)))
                   finally return ,(or ret-form `,g-loop-res)))))))

;;;; With-

(defmacro with-temp-buffer-file (file &rest body)
  (declare (indent 1)
           (debug (form body)))
  `(with-temp-buffer
     (insert-file-contents ,file)
     (goto-char (point-min))
     ,@body))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ prepare +++++")
      (expect (true)
        (require 'imakado))
      (desc "+++++ with-temp-buffer-file +++++")
      (expect ";;; imakado.el - imakado's usefull macros"
        (require 'find-func)
        (with-temp-buffer-file (find-library-name "imakado")
          (buffer-substring-no-properties (point) (point-at-eol))))
      (expect (error)
        (let ((nonexistent-filename (loop for n from 1 to 9999
                                          for nonexistent-filename = (format "/aa-%s" n)
                                          unless (file-readable-p nonexistent-filename)
                                          return nonexistent-filename)))
          (with-temp-buffer-file nonexistent-filename
            'no-error))))))

(defmacro match-with-temp-buffer-file (spec &rest body)
  (declare (indent 1)
           (debug ((form form &optional form) body)))
  (with-gensyms (g-file-contents-str)
    `(let ((,g-file-contents-str ,(with-temp-buffer-file (nth 1 spec)
                                    (buffer-substring-no-properties (point-min) (point-max)))))
       (match-with-temp-buffer (,(nth 0 spec) ,g-file-contents-str ,(nth 2 spec))
         ,@body))))


;;;; Buffer
(defmacro save-excursion-force (&rest body)
  (declare (indent 0)
           (debug (body)))
  (with-gensyms (saved-point saved-current-buffer)
    `(let ((,saved-point (point))
           (,saved-current-buffer (current-buffer)))
       (prog1 (progn ,@body)
         (ignore-errors
           (and (eq ,saved-current-buffer (current-buffer))
                (goto-char ,saved-point)))))))

(defmacro with-point-buffer (str &rest body)
  (declare (indent 1)
           (debug (form body)))
  `(with-temp-buffer
     (insert ,str)
     (goto-char (point-min))
     (when (re-search-forward (rx "`!!'") nil t)
       (replace-match ""))
     (progn
       ,@body)))

;;;; Keymap
(defmacro* define-define-keymap (name keymap &key (doc nil))
  `(etypecase ,keymap
     (keymap
      (defmacro ,name (key-str-or-vector command)
        ,doc
        (typecase key-str-or-vector
          (string `(define-key ,',keymap (kbd ,key-str-or-vector) ,command))
          (t `(define-key ,',keymap ,key-str-or-vector ,command)))))))

;;;; defcacheable
(defmacro* defcacheable (name args &rest body)
  (declare (indent 2)
           (debug defun*))
  (flet ((ik:defcacheable-doc&body (body)
          (typecase (car-safe body)
            (string (values (car body) (cdr body)))
            (otherwise (values nil body)))))
    (destructuring-bind (doc body) (ik:defcacheable-doc&body body)
      (let* ((cache-var-sym (intern (format "%s-cache" name)))
             (clear-cache-fn-sym (intern (format "%s-clear-cache" name))))
        `(progn
           (defvar ,cache-var-sym nil)
           (setf ,cache-var-sym nil)
           (defun* ,name ,args
             ,doc
             (or ,cache-var-sym
                 (setf ,cache-var-sym
                       (progn ,@body))))
           (defun* ,clear-cache-fn-sym ,args
             (interactive)
             (setf ,cache-var-sym
                   nil)))))))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ defcacheable +++++")
      (desc "defined ok")
      (expect t
        (let ((call-count 0))
          (makunbound 'ik:--test-defcacheable-fn-cache)
          (fmakunbound 'ik:--test-defcacheable-fn-clear-cache)
          (fmakunbound 'ik:--test-defcacheable-fn)
          (defcacheable ik:--test-defcacheable-fn ()
            "docstr"
            (prog1 "cache me if you can!"
              (incf call-count)))
          (and (boundp 'ik:--test-defcacheable-fn-cache)
               (fboundp 'ik:--test-defcacheable-fn)
               (fboundp 'ik:--test-defcacheable-fn-clear-cache))))
      (desc "call just once")
      (expect '("cache me if you can!" 1)
        (let ((call-count 0))
          (makunbound 'ik:--test-defcacheable-fn-cache)
          (fmakunbound 'ik:--test-defcacheable-fn-clear-cache)
          (fmakunbound 'ik:--test-defcacheable-fn)
          (defcacheable ik:--test-defcacheable-fn ()
            "docstr"
            (prog1 "cache me if you can!"
              (incf call-count)))
          (ik:--test-defcacheable-fn)
          (let ((res (ik:--test-defcacheable-fn)))
            (list res call-count))))
      (desc "clear cache")
      (expect 2
        (let ((call-count 0))
          (makunbound 'ik:--test-defcacheable-fn-cache)
          (fmakunbound 'ik:--test-defcacheable-fn-clear-cache)
          (fmakunbound 'ik:--test-defcacheable-fn)
          (defcacheable ik:--test-defcacheable-fn ()
            "docstr"
            (prog1 "cache me if you can!"
              (incf call-count)))
          (ik:--test-defcacheable-fn)
          (ik:--test-defcacheable-fn-clear-cache)
          (ik:--test-defcacheable-fn)
          call-count))
      (desc "cleanup")
      (expect t
        (progn (makunbound 'ik:--test-defcacheable-fn-cache)
               (fmakunbound 'ik:--test-defcacheable-fn-clear-cache)
               (fmakunbound 'ik:--test-defcacheable-fn)
               t))
      )))


;;;; do-temp-file
(defmacro* do-temp-file ((temp-file-var &optional file-contents-string temp-file-prefix) &rest body)
  (declare (indent 1)
           (debug ((symbolp form &optional form) body)))
  (with-gensyms (g-file-contents-string)
    `(let ((,g-file-contents-string ,file-contents-string)
           (,temp-file-var (make-temp-file (format "%s" (or ,temp-file-prefix "do-temp-file.")))))
       (unwind-protect
           (progn
             (with-temp-file ,temp-file-var
               (when ,g-file-contents-string
                 (insert ,g-file-contents-string)))
             ,@body)
         (when (file-exists-p ,temp-file-var)
           (delete-file ,temp-file-var))))))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ do-temp-file +++++")
      (expect (regexp "do-temp-file\\.")
        (do-temp-file (tempfile "contents")
          tempfile))
      (expect (regexp "prefix-")
        (do-temp-file (tempfile "contents" "prefix-")
          tempfile))
      (expect "contents"
        (do-temp-file (tempfile "contents" "prefix-")
          (with-temp-buffer-file tempfile
            (buffer-substring-no-properties (point-min) (point-max)))))
      (desc "omit arg")
      (expect t
        (do-temp-file (tempfile)
          (file-exists-p tempfile)))
      (desc "cleanup")
      (expect nil
        (let ((gtempfile nil))
          (do-temp-file (tempfile "contents" "prefix-")
            (setq gtempfile tempfile))
          (file-exists-p gtempfile)))
      )))


;;;; for-each-single-property-change
(defmacro for-each-single-property-change (property fn)
  "fn is called with two args (current-point next-change-point)"
  (declare (debug (form form)))
  (with-gensyms (g-fn g-property g-next-change g-current-point)
    `(let ((,g-fn ,fn)
           (,g-property ,property)
           (,g-next-change nil)
           (,g-current-point nil))
       (save-excursion
         (loop initially (goto-char (point-min))
               for ,g-next-change = (or (next-single-property-change (point) ,g-property (current-buffer))
                                          (point-max))
               for ,g-current-point = (point)
               until (eobp)
               do (goto-char ,g-next-change)
               do (funcall ,g-fn ,g-current-point ,g-next-change))))))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ for-each-single-property-change +++++")
      (expect (true)
        (with-temp-buffer-file (find-library-name "imakado")
          (emacs-lisp-mode)
          (font-lock-mode t)
          (save-excursion (font-lock-fontify-region (point-min) (point-max)))
          (let ((ret nil)
                (func-name-collector (lambda (p next-change)
                                       (when (eq (get-text-property p 'face) 'font-lock-function-name-face)
                                         (push (buffer-substring-no-properties p next-change) ret)))))
            (for-each-single-property-change 'face func-name-collector)
            (remif (fn (=~ "^ik:test-" _)) ret))))
      )))

;;;; Eieio utils
(defsubst ik:define-defmethod-modifies&rest (args)
  (case (car-safe args)
    ((:before :primary :after :static)
     (values (car args) (cdr args)))
    (t (values nil args))))
(defsubst ik:define-defmethod-add-self-to-args (class rest)
  (dbind (args . body) rest
    (cons (cons `(self ,class) args)
          body)))
(defmacro define-defmethod (name class)
  (declare (debug (symbolp symbolp)))
  `(defmacro* ,name (method &rest body)
     (declare (indent 2)
              (debug (&define
                      [&or name
                           ("setf" :name setf name)]
                      [&optional symbolp]
                      list
                      [&optional stringp]
                      def-body)))
     (dbind (modifier rest) (ik:define-defmethod-modifies&rest
                             body)
       (let* ((rest (ik:define-defmethod-add-self-to-args ',class rest))
              (args (if modifier
                        (cons modifier rest)
                      rest)))
         (cons 'defmethod
               (cons method args))))))

;;;; defclass+
(defmacro defclass+ (&rest args)
  (let* ((name (car args))
         (defmethod-macro-sym (intern (format "%s-defmethod" name))))
    `(prog1
       (defclass ,@args)
       (define-defmethod ,defmethod-macro-sym ,name))))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ defclass+ +++++")
      (expect "aa"
        (flet ((tests/class-b ()))
          (defclass+ tests/class-b ()
            ((slot-a :initarg :slot-a
                     :initform nil))
            "class a")
          (tests/class-b-defmethod tests/--test-method ()
                                   (with-slots (slot-a) self
                                     (setf slot-a "aa"))
                                   (with-slots (slot-a) self
                                     slot-a))
          (let ((ins (make-instance 'tests/class-b)))
            (tests/--test-method ins))))
      )))

;;;; require-methods
(defsubst* ik:require-methods-aux (class methods)
  (loop for method in methods
        collect `(defmethod ,method ((self ,class) &rest ignore)
                   (error "this function should be implemented in subclasses."))))
(defmacro require-methods (class methods)
  (declare (indent 1)
           (debug (symbolp list)))
  `(progn ,@(ik:require-methods-aux class methods)))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ require-methods +++++")
      (expect (error)
        (progn
          (defclass+ ik:--test-require-methods ()
            ()
            "class")
          (require-methods ik:--test-require-methods
            (ik:--test-require-methods-interface-fn))
          (let ((ins (make-instance 'ik:--test-require-methods)))
            (ik:--test-require-methods-interface-fn ins))))
      (desc "call with arg")
      (expect (error)
        (progn
          (defclass+ ik:--test-require-methods ()
            ()
            "class")
          (require-methods ik:--test-require-methods
            (ik:--test-require-methods-interface-fn))
          (let ((ins (make-instance 'ik:--test-require-methods)))
            (ik:--test-require-methods-interface-fn ins 'a 'b 'c))))
      )))


(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ define-defmethod +++++")
      (expect (true)
        (flet ((tests/class-a ())
               (message (&rest args) (print (apply 'format args))))
          (defclass tests/class-a ()
            ((slot-a :initarg :slot-a
                     :initform nil))
            "class a")
          (define-defmethod def-tests/class-a-method tests/class-a)
          (def-tests/class-a-method ik:--test-say-ya ()
            (message "%s" self))
          (def-tests/class-a-method ik:--test-say-ya :before ()
            (message "%s" ":before ik:--test-say-ya"))
          (let ((res (with-output-to-string
                       (ik:--test-say-ya (make-instance 'tests/class-a)))))
            (and (string-match ":before ik:--test-say-ya" res)
                 (string-match "[object tests/class-a tests/class-a nil]" res)))))

      (desc "+++++ ik:define-defmethod-modifies&rest +++++")
      (expect '(:before (((cla-a tests/class-a)) (message "%s" ":before say-ya")))
        (dbind (modifier body) (ik:define-defmethod-modifies&rest
                                '(:before
                                  ((cla-a tests/class-a))
                                  (message "%s" ":before say-ya")))
          (list modifier body))
        )
      (expect '(nil (((cla-a tests/class-a)) (message "%s" ":before say-ya")))
        (dbind (modifier body) (ik:define-defmethod-modifies&rest
                                '(((cla-a tests/class-a))
                                  (message "%s" ":before say-ya")))
          (list modifier body)))

      (desc "+++++ ik:define-defmethod-add-self-to-args +++++")
      (expect '(((self tests/class-a) arg1 arg2) (message "%s" ":before say-ya"))
        (ik:define-defmethod-add-self-to-args
         'tests/class-a
         '((arg1 arg2)
           (message "%s" ":before say-ya"))))
      )))


;;;; Simple template
(eval-when-compile
  (defvar ik:simple-template-expression_mark "=")
  (defvar ik:simple-template-comment_mark "#")

  (defvar ik:simple-template-tag-start "[%")
  (defvar ik:simple-template-tag-end "%]")
  (defvar ik:simple-template-tag-expression-start
    (concat ik:simple-template-tag-start
            ik:simple-template-expression_mark))
  (defvar ik:simple-template-tag-comment-start
    (concat ik:simple-template-tag-start
            ik:simple-template-comment_mark))
  ) ;; eval-when-compile
(eval-when-compile
  (defvar ik:simple-template-token-separator-re
    (rx (or (eval (eval-when-compile ik:simple-template-tag-expression-start))
            (eval (eval-when-compile ik:simple-template-tag-comment-start))
            (eval (eval-when-compile ik:simple-template-tag-start))
            (eval (eval-when-compile ik:simple-template-tag-end))))))

(defsubst ik:simple-template-parse-buffer-get-token-and-advance ()
  (let ((p (point)))
    (cond
     ((eobp) nil)
     ((looking-at (eval-when-compile ik:simple-template-token-separator-re))
      (prog1 (match-string-no-properties 0)
        (goto-char (match-end 0))))
     ((re-search-forward (eval-when-compile ik:simple-template-token-separator-re) nil t)
      (goto-char (match-beginning 0))
      (buffer-substring-no-properties p (point)))
     (t
      (prog1 (buffer-substring-no-properties p (point-max))
        (goto-char (point-max)))))))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "ik:simple-template-parse-buffer-get-token-and-advance")
      (expect '("The " "[%" "  " "%]" " " "[%#" " perfect " "%]" " " "[%=" " insider " "%]" ".")
        (with-point-buffer "The [%  %] [%# perfect %] [%= insider %]."
          (goto-char (point-min))
          (loop for token = (ik:simple-template-parse-buffer-get-token-and-advance)
                while token
                collect token)))
      (desc "newline")
      (expect '("The " "[%" "  " "%]" " " "[%#" " \nperfect " "%]" " " "[%=" " outsider " "%]" ".")
        (with-point-buffer "The [%  %] [%# \nperfect %] [%= outsider %]."
          (goto-char (point-min))
          (loop for token = (ik:simple-template-parse-buffer-get-token-and-advance)
                while token
                collect token)))
      (expect nil
        (with-point-buffer ""
          (goto-char (point-min))
          (loop for token = (ik:simple-template-parse-buffer-get-token-and-advance)
                while token
                collect token)))
      (expect '("newline\n")
        (with-point-buffer "newline\n"
          (goto-char (point-min))
          (loop for token = (ik:simple-template-parse-buffer-get-token-and-advance)
                while token
                collect token)))
      (expect '("[%" " (if flag " "%]" "\n" "[%" " ) " "%]")
        (let ((o "you")
              (flag t)
              (tmpl "\
\[% (if flag %]
\[% ) %]"))
          (with-point-buffer tmpl
            (goto-char (point-min))
            (loop for token = (ik:simple-template-parse-buffer-get-token-and-advance)
                  while token
                  collect token))))
      )))

(defsubst ik:simple-template-parse-buffer ()
  (let ((state 'text)
        (multiline_expression nil)
        (tree nil)
        (tag-end (eval-when-compile ik:simple-template-tag-end))
        (tag-start (eval-when-compile ik:simple-template-tag-start))
        (tag-comment-start (eval-when-compile ik:simple-template-tag-comment-start))
        (expression-start (eval-when-compile ik:simple-template-tag-expression-start)))
    (loop initially (goto-char (point-min))
          with tokens
          for token = (ik:simple-template-parse-buffer-get-token-and-advance)
          while token
          do (cond
              ((string= tag-end token)
               (setq state 'text
                     multiline_expression nil))
              ((string= tag-start token)
               (setq state 'code))
              ((string= tag-comment-start token)
               (setq state 'comment))
              ((string= expression-start token)
               (setq state 'expr))
              (t
               (unless (eq state 'comment)
                 (when multiline_expression
                   (setq state 'code))
                 (when (eq state 'expr)
                   (setq multiline_expression t))
                 (push (list state token) tokens))))
          finally return (nreverse tokens))))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "ik:simple-template-parse-buffer")
      (expect nil
        (with-point-buffer "[%# \nperfect %]"
          (ik:simple-template-parse-buffer)))
      (expect '((text "The ") (code " aa ") (text " \n ") (text " ") (expr " insider ") (text "."))
        (with-point-buffer "The [% aa %] \n [%# \nperfect %] [%= insider %]."
          (ik:simple-template-parse-buffer)))
      )))

(defsubst* ik:simple-template-compile-aux (type value &key (insert-fn 'insert))
  (ecase type
    (text (prin1 `(,insert-fn ,value) (current-buffer)))
    (code (insert value))
    (expr (insert "(insert " value ")"))))
(defsubst* ik:simple-template-compile (tree)
  (with-temp-buffer
    (insert "(progn ")
    (loop for (type value) in tree
          do (ik:simple-template-compile-aux type value))
    (insert ")")
    (goto-char (point-min))
    (condition-case err
        (read (current-buffer))
      (error
       (error "ERROR occur during compiling template.\ntree: %S\ncompiled source: %S"
              tree (buffer-substring-no-properties (point-min) (point-max)))))))
(defsubst* ik:simple-template-buffer ()
  ;; this temp var name should be prefixed.
  ;; please see test -> (desc "Note, variable `ik:--form' is seen inside tmplate.")
  (let ((ik:--form (ik:simple-template-compile
                    (ik:simple-template-parse-buffer))))
    (with-temp-buffer
      (condition-case err
          (progn (eval ik:--form)
                 (buffer-string))
        (error
         (error "ERROR during eval.\ncompiled source: %S"
                ik:--form))))))

(defmacro simple-template (file-or-list-of-file)
  (declare (debug (form)))
  (with-gensyms (g-file-or-list-of-file)
    `(let* ((,g-file-or-list-of-file ,file-or-list-of-file))
       (assert (not (null ,g-file-or-list-of-file)))
       (with-temp-buffer
         (etypecase ,g-file-or-list-of-file
           (list (dolist (file ,g-file-or-list-of-file)
                   (insert-file-contents file)
                   (goto-char (point-max))))
           (string (insert-file-contents ,g-file-or-list-of-file)))
         (ik:simple-template-buffer)))))

(defmacro simple-template-string (str-or-list-of-string)
  (declare (debug (form)))
  (with-gensyms (g-str-or-list-of-string)
    `(let* ((,g-str-or-list-of-string ,str-or-list-of-string))
       (assert (not (null ,g-str-or-list-of-string)))
       (with-temp-buffer
         (etypecase ,g-str-or-list-of-string
           (list (insert (mapconcat 'identity ,g-str-or-list-of-string "")))
           (string (insert ,g-str-or-list-of-string)))
         (ik:simple-template-buffer)))))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ simple-template +++++")
      (desc "basic string")
      (expect "\nflag is non-nil\n"
        (let ((o "you")
              (flag t)
              (tmpl "\
\[% (cond (flag %]
\[% (insert \"flag is non-nil\") %]
\[%)%][%# end FLAG  %][% (t %][%# ELSE %]
\[%# *commant*  %]
\[% (insert \"flag is nil\") %]
\[% )) %][%# end COND %]"))
          (simple-template-string tmpl)))

      (desc "Note, variable `ik:--form' is seen inside tmplate.")
      (expect (error)
        (let ((ik:--form "lexical"))
          (do-temp-file (tfile  "loving [%= ik:--form %].")
            (simple-template tfile))))

      (desc "this gonna well")
      (expect "i miss lexical scope."
        (let ((other-var-name "lexical"))
          (do-temp-file (tfile  "i miss [%= other-var-name %] scope.")
            (simple-template tfile))))

      (desc "last newline")
      (expect "newline\n"
        (do-temp-file (tfile  "newline\n")
          (simple-template tfile)))

      (desc "basic file")
      (expect "loving you."
        (let ((o  "you"))
          (do-temp-file (tfile  "loving [%= o %].")
            (simple-template tfile))))
      (desc "allow newline inside tag")
      (expect "loving you."
        (let ((o "you"))
          (do-temp-file (tfile  "loving [%= \n  o \n %].")
            (simple-template tfile))))

      (desc "just string")
      (expect "loving you."
        (do-temp-file (tfile  "loving you.")
          (simple-template tfile)))

      (desc "[%# ... %] COMMENT")
      (expect "loving"
        (do-temp-file (tfile  "loving[%# COMMENT %]")
          (simple-template tfile)))

      (desc "[%# ... %] newline inside COMMENT ")
      (expect "loving"
        (do-temp-file (tfile  "loving[%# \n COMMENT \n  %]")
          (simple-template tfile)))

      (desc "call with nil")
      (expect (error)
        (do-temp-file (tfile  "loving[%# \n COMMENT \n  %]")
          (simple-template nil)))

      (desc "call with empty string")
      (expect (error)
        (simple-template ""))

      (desc "multiple file")
      (expect "1\n2"
        (let ((tmpl1 "1")
              (tmpl2 "2"))
          (do-temp-file (tfile1  "[%= tmpl1 %]\n")
            (do-temp-file (tfile2 "[%= tmpl2 %]")
              (simple-template (list tfile1 tfile2))))))

      (desc "+++++ simple-template-string +++++")
      (desc "call with nil")
      (expect (error)
        (simple-template-string nil))

      (desc "call with empty string")
      (expect ""
        (simple-template-string ""))
      )))

;;;; try-these
(defmacro try-these (&rest forms)
  (declare (debug (&rest form)))
  `(or ,@(loop for form in forms
               collect `(ignore-errors ,form))))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ try-these +++++")
      (expect "ok"
        (try-these (error "aa")
                   (progn "ok")
                   (error "bb")))
      (expect "ok"
        (flet ((ik:--try-these-test () (return nil)))
          (try-these (ik:--try-these-test)
                     (progn "ok")
                     (error "bb"))))
      (expect nil
        (let ((called nil))
          (flet ((ik:--try-these-test () (setq called t)))
            (try-these (error "aa")
                       (progn "ok")
                       (ik:--try-these-test))
            called)))
      (expect t
        (let ((called nil))
          (flet ((ik:--try-these-test () (setq called t)))
            (try-these (error "aa")
                       (progn (ik:--try-these-test)
                              "ok")
                       "bb")
            called)))
      )))

;;;; Font lock support
(defgroup imakado nil
  "imakado.el"
  :prefix "ik:"
  :group 'convenience)
(defvar ik:enable-font-lock nil)
(defface ik:keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for macros are defined in imakado.el"
  :group 'imakado)

(defvar ik:keyword-face 'ik:keyword-face
  "Face name to use for function names.")

(defun ik:font-lock-keyword-matcher1 (limit)
  (when ik:enable-font-lock
    (let ((re (rx "("
                  (group (or "ik:font-lock-matcher"
                             "do-temp-file"
                             "for-each-single-property-change" "defcacheable"
                             "define-define-keymap" "with-point-buffer"
                             "save-excursion-force" "match-with-temp-buffer-file"
                             "with-temp-buffer-file" "match-with-temp-buffer"
                             "ik:case-regexp-aux" "case-regexp"
                             "=~" "with-anaphoric-match-utilities"
                             "dolist-with-progress-reporter"
                             "define-with-all-slots-struct"
                             "ik:with-struct-all-slots-get-all-slots"
                             "ik:with-struct-all-slots-get-getters-slot?"
                             "define-with-struct-macro"
                             "with-struct"
                             "ik:xcase-clause-aux-test"
                             "ik:xcase-clause-aux" "xcase="
                             "ik:case-cond-clause-aux" "xcase"
                             "join+" "in=" "inq" "in" "nths" "assoc-cdrs"
                             "cadrs" "cdrs" "cars" "define-macro-aliases"
                             "define-macro-alias" "ik:fn-aux-appear_?"
                             "ik:fn-aux-anaph-arg-syms" "ik:fn-aux" "fn"
                             "ik:cond-let-aux-binds" "ik:cond-let-aux-clause"
                             "ik:cond-let-aux-vars" "cond-let" "alambda" "acond"
                             "aand" "awhile" "awhen" "aif" "!remif" "remif"
                             "dirconcat" "with-lexical-bindings" "with-gensyms"
                             "ik:remove-if-not" "ik:remove-if"
                             "ik:subseq" "ik:flatten" "ik:in-aux-test"
                             "ik:allquote" "ik:group" "ik:gensym"
                             "simple-template" "simple-template-string"
                             "require-methods"
                             ))
                  symbol-end)))
      (when (re-search-forward re limit t)
        (set-match-data (match-data))
        t))))

(defun ik:font-lock-keyword-matcher2 (limit)
  (when ik:enable-font-lock
    (let ((re (rx "("
                  (group (or "defcacheable"
                             "define-define-keymap" 
                             "define-with-all-slots-struct"
                             "define-with-struct-macro"
                             "define-macro-aliases"
                             "define-macro-alias"
                             "defclass+"
                             "define-defmethod"
                             ))
                  (+ space)
                  (group (+ (or (syntax symbol) (syntax word)))))))
      (when (re-search-forward re limit t)
        (set-match-data (match-data))
        t))))


(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '((ik:font-lock-keyword-matcher1
      (1 ik:keyword-face append))
     (ik:font-lock-keyword-matcher2
      (1 ik:keyword-face append)
      (2 font-lock-function-name-face append)))))

;;;; Tests
;; need el-expectations.el(written by rubikitch) to run.
;; http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "awhen")
      (expect "ya"
        (awhen (or nil "ya")
          it))
      (desc "awhile")
      (expect '(do you rock me?)
        (let ((l '(do you rock me?))
              (ret nil))
          (awhile (pop l)
            (push it ret))
          (nreverse ret)))
      (desc "aand")
      (expect 3
        (aand (1+ 0)
              (1+ it)
              (1+ it)))
      (desc "acond")
      (expect "cla1"
        (let ((ret nil))
          (acond
            (ret (push "ret is non-nil" ret))
            ((progn "cla1") (push it ret))
            (t (push "t" ret)))
          (car ret)))
      (desc "alambda")
      (expect 120
        (let ((factor (alambda (lst)
                        (cond
                         ((null lst) 1)
                         (t (* (car lst) (caller (cdr lst))))))))
          (funcall factor '(1 2 3 4 5))))

      (desc "with-struct")
      (expect 'st-a
        (defstruct (ik:test-struct
                    (:constructor ik:make-test-struct (a b)))
          a b)
        (let ((st (ik:make-test-struct 'st-a 'st-b)))
          (with-struct (ik:test-struct- a b) st
            a)))
      (desc "with-struct setf")
      (expect 'changed
        (defstruct (ik:test-struct
                    (:constructor ik:make-test-struct (a b)))
          a b)
        (let ((st (ik:make-test-struct 'st-a 'st-b)))
          (with-struct (ik:test-struct- a b) st
            (setf a 'changed))
          (with-struct (ik:test-struct- a) st
            a)))

      (desc "define-with-all-slots-struct")
      (expect '(st-a st-b)
        (progn
          (defstruct (ik:test-struct
                      (:constructor ik:make-test-struct (a b)))
            a b)
          (define-with-all-slots-struct ik:with-all-slots-test-struct ik:test-struct-)
          (let ((st (ik:make-test-struct 'st-a 'st-b)))
            (ik:with-all-slots-test-struct st
              (list a b)))))
      (desc "define-with-all-slots-struct setf")
      (expect '(aa bb)
        (progn
          (defstruct (ik:test-struct
                      (:constructor ik:make-test-struct (a b)))
            a b)
          (define-with-all-slots-struct ik:with-all-slots-test-struct ik:test-struct-)
          (let ((st (ik:make-test-struct 'st-a 'st-b)))
            (ik:with-all-slots-test-struct st
              (setq a 'aa
                    b 'bb)
              (list a b)))))

      (desc "with-lexical-bindings")
      (expect '(var-a var-b changed)
        (let ((funcs (let ((a 'var-a)
                           (b 'var-b)
                           (c 'var-c))
                       (with-lexical-bindings (a b)
                         (list (lambda () a)
                               (lambda () b)
                               (lambda () c))))))
          (destructuring-bind (a b c) '(changed changed changed)
            (mapcar 'funcall funcs))))

      (desc "define-with-struct-macro")
      (expect 'st-a
        (defstruct (ik:test-struct
                    (:constructor ik:make-test-struct (a b)))
          a b)
        (define-with-struct-macro ik:with-test-struct ik:test-struct-)
        (let ((st (ik:make-test-struct 'st-a 'st-b)))
          (ik:with-test-struct (a) st
            a)))

      (desc "ik:case-regexp-aux")
      (expect '(((=~ "aa" expr) (with-anaphoric-match-utilities expr "match aa")) ((=~ "bb" expr) (with-anaphoric-match-utilities expr "match bb")))
        (ik:case-regexp-aux 'expr '(("aa" "match aa") ("bb" "match bb"))))

      (expect '((t nil))
        (ik:case-regexp-aux 'expr '( (t) )))

      (desc "case-regexp")
      (expect "match huga"
        (case-regexp (concat "hu" "ga")
          ("^huga" (progn (message "%s" "match!!")
                          "match huga"))
          (t "otherwise")))
      (expect nil
        (case-regexp (concat "hu" "ga")
          ("huga" nil)
          (t "otherwise")))
      (expect nil
        (case-regexp (concat "hu" "ga")
          ("never match"
           (progn (message "%s" "match!!")
                  "match"))
          (t nil)))
      (expect nil
        (case-regexp (concat "hu" "ga")
          ("never match" (progn (message "%s" "match!!")
                                "match"))
          (t )))
      (expect nil
        (case-regexp (concat "hu" "ga")
          ("never match" (progn (message "%s" "match!!")
                                "match"))
          (otherwise nil)))
      (expect "huga"
        (case-regexp (concat "hu" "ga")
          ("^huga" $0)
          (t "otherwise")))

      (expect  "match huga"
        (case-regexp (concat "hu" "ga")
          ((rx "huga") "match huga")
          (t "otherwise")))

      (desc "define-define-keymap")
      (expect 'ik:test-command
        (let ((ik:test-keymap (make-sparse-keymap)))
          (flet ((ik:test-command (&rest args) (interactive) (apply 'identity args)))
            (define-define-keymap ik:define-key-test-keymap ik:test-keymap)
            (ik:define-key-test-keymap "<return>" 'ik:test-command)
            (lookup-key ik:test-keymap (kbd "<return>")))))

      (expect 'ik:test-command
        (let ((ik:test-keymap (make-sparse-keymap)))
          (flet ((ik:test-command (&rest args) (interactive) (apply 'identity args)))
            (define-define-keymap ik:define-key-test-keymap ik:test-keymap)
            (ik:define-key-test-keymap [f1] 'ik:test-command)
            (lookup-key ik:test-keymap [f1]))))

      (expect 'ik:test-command
        (let ((ik:test-keymap (make-sparse-keymap)))
          (flet ((ik:test-command (&rest args) (interactive) (apply 'identity args)))
            (define-define-keymap ik:define-key-test-keymap ik:test-keymap)
            (ik:define-key-test-keymap (progn nil "C-c a") (progn "ignore" 'ik:test-command))
            (lookup-key ik:test-keymap (progn nil "C-c a")))))

      (expect 'ik:test-command
        (let ((ik:test-keymap (make-sparse-keymap)))
          (flet ((ik:test-command (&rest args) (interactive) (apply 'identity args)))
            (define-define-keymap ik:define-key-test-keymap ik:test-keymap :doc "define ik:test-keymap key")
            (ik:define-key-test-keymap "<return>" 'ik:test-command)
            (lookup-key ik:test-keymap (kbd "<return>")))))

      (desc "dolist-with-progress-reporter")
      (expect (regexp "[[:digit:]][[:digit:]]%")
        (flet ((message (&rest args) (print (apply 'format args))))
          (with-output-to-string
            (dolist-with-progress-reporter (v (number-sequence 0 5)) "msg: " nil 0.1
              (sit-for 0.1)))))
      (expect '(1 2 3)
        (let (ret)
          (dolist-with-progress-reporter (n '(1 2 3) (nreverse ret)) "" nil nil
            (push n ret))))
      (desc "cond-let")
      (expect "cd(d c nil)"
        (with-output-to-string
          (cond-let (((= 1 2) (x (princ 'a)) (y (princ 'b)))
                     ((= 1 1) (y (princ 'c)) (x (princ 'd)))
                     (t       (x (princ 'e)) (z (princ 'f))))
            (princ (list x y z)))))
      (expect 'aa
        (cond-let ((t (a 'aa)))
          a))

      (desc "in")
      (expect '(t nil)
        (let (var)
          (flet ((op () 'two))
            (list (in (op) 'one 'two (prog1 'three (setq var 'called)))
                  var))))

      (desc "ik:group")
      (expect '((1 2) (3 4))
        (ik:group '(1 2 3 4) 2))
      (expect '((1 2) (3))
        (ik:group '(1 2 3) 2))
      (expect (error)
        (ik:group '(1 2 3) 2 :error-check t))
      (expect '((1))
        (ik:group '(1) 2))
      (expect (error)
        (ik:group '(1) 2 :error-check t))


      (desc "define-macro-alias")
      (expect 'alias
        (defmacro ik:test-macro (arg)
          (with-gensyms (a)
            `(let ((,a ,arg))
               ,a)))
        (define-macro-alias ik:test-macro-alias ik:test-macro)
        (ik:test-macro-alias 'alias))

      (desc "in")
      (expect '(t nil)
        (let (called)
          (let ((op 'otherwise))
            (list
             (in op 't 'otherwise (prog1 'ya (setq called t)))
             called))))
      (desc "inq")
      (expect t
        (let ((op 'otherwise))
          (inq op t otherwise ya)))

      (desc "xcase")
      (expect '(first t nil)
        (let (called called2)
          (list
           (xcase 'key
             (((prog1 'key (setq called t))) 'first)
             ((prog1 'ya (setq called2 t)) 'second))
           called called2)))

      (desc "in")
      (expect t
        (in (prog1 'apple "apple") "banana" "apple" "pine" 'apple))
      (expect nil
        (in (progn 'ignore "apple") "banana" "apple" "pine"))
      (desc "in=")
      (expect t
        (in= (progn 'ignore "apple") "banana" "apple" "pine"))
      (expect t
        (in= (progn 'ignore 'apple) "banana" "apple" "pine" (prog1 'apple 'ignore)))

      (desc "join+")
      (expect "oh my god"
        (join+ (list "oh" "my" "god") (prog1 " " 'ignore)))


      (desc "ik:flatten")
      (expect '(a b c d e)
        (ik:flatten '(a (b (c d)) e)))

      (desc "ik:subseq")
      (expect '(3 4 5 6 7)
        (ik:subseq (number-sequence 0 10) 3 8))

      (desc "ik:remove-if")
      (expect '(0 2 4 6 8 10)
        (ik:remove-if 'oddp (number-sequence 0 10)))
      (desc "ik:remove-if-not")
      (expect '(1 3 5 7 9)
        (ik:remove-if-not 'oddp (number-sequence 0 10)))

      (desc "fn")
      (expect '(3 2 1)
        (sort (list 1 2 3)
              (fn (_ b) (> _ b))))
      (expect '(3 2 1)
        (sort (list 1 2 3)
              (fn (> _1 _2))))
      (expect '(0 1 2 3)
        (ik:remove-if (fn (_) (> _ 3)) (number-sequence 0 10)))
      (expect '(0 1 2 3)
        (ik:remove-if (fn (> _ 3)) (number-sequence 0 10)))
      (expect '(1 3)
        (apply (fn (list _1 _3)) '(1 2 3)))

      (expect t
        (funcall (fn (= 1 (length _)))
                 (list 'a)))
      (desc "fn error")
      (expect "cant use \"_\" and \"(_1 _2 ...)\" at the same time!!"
        (condition-case err
            (fn (list _ _1 _2))
          (error "%s" (error-message-string err))))

      (desc "fn cant use both _<n> and _")
      (expect (error)
        (sort (list 1 2 3)
              (fn (< _1 _))))

      (desc "fn _0 is bound to arglist")
      (expect '(a b (a b))
        (apply (fn (list _1
                         _2
                         _0))
               '(a b)))

      (desc "cars")
      (expect '("a" "c")
        (cars '( ("a" . "b") ("c" . "d"))))
      (desc "cdrs")
      (expect '("b" "d")
        (cdrs '( ("a" . "b") ("c" . "d"))))

      (desc "match-with-temp-buffer")
      (expect '("string" "string" "string")
        (match-with-temp-buffer ((rx "string") "string string string" )
          $m))
      (expect "string string string"
        (match-with-temp-buffer ((rx "ing") "string string string" (buffer-string))
          $m))
      (expect "  "
        (match-with-temp-buffer ((rx "string") "string string string" (buffer-string))
          ($sub "")))

      (desc "remif")
      (expect '("a" "b")
        (remif (fn (=~ (rx bol ".") _))
               (list "a" "b" ".hoge")))
      (expect '("a" "b")
        (remif (fn (=~ (rx bol ".") _))
               (list "a" "b" ".hoge")
               :key 'identity))
      (expect '("a" "b")
        (remif (fn (=~ (rx bol ".") _))
               (list "a" "b" ".hoge")
               :key (fn (a) a)))

      (desc "!remif")
      (expect '((1 . "huga"))
        (!remif (fn (=~ "huga" _))
                '( (1 . "huga")
                   (2 . "hoge")
                   (3 . "piyo") )
                :key 'cdr))
      (expect '(".hoge")
        (!remif (fn (=~ (rx bol ".") _))
                (list "a" "b" ".hoge")))
      (expect '(".hoge")
        (!remif (fn (=~ (rx bol ".") _))
                (list "a" "b" ".hoge")
                :key 'identity))
      (expect '(".hoge")
        (!remif (fn (=~ (rx bol ".") _))
                (list "a" "b" ".hoge")
                :key (fn (a) a)))

      (desc "cadr")
      (expect '("2" "4")
        (cadrs '( ("1" "2") ("3"  "4"))))

      (desc "with-point-buffer")
      (expect '("a" "b")
        (with-point-buffer "aaa`!!'bbb"
          (list
           (char-to-string (preceding-char))
           (char-to-string (following-char)))))

      (desc "assoc-cdrs")
      (expect '("kval" "vval")
        (assoc-cdrs '("k" "v")
                    '(("k" . "kval")
                      ("v" . "vval"))))
      (expect '("kval" "vval")
        (assoc-cdrs '( k v )
                    '((k . "kval")
                      (v . "vval"))))
      (expect '("kval" "vval" "def")
        (assoc-cdrs '( k v default)
                    '((k . "kval")
                      default
                      (v . "vval"))
                    'eq
                    "def"))
      (desc "nths")
      (expect '(2 2 2)
        (nths 2
              (list (number-sequence 0 10)
                    (number-sequence 0 10)
                    (number-sequence 0 10))))
      )))

(provide 'imakado)
;;; imakado.el ends here

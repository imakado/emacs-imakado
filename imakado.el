;;; imakado.el - imakado's usefull macros and functions

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

;; Prefix: i/

;;; Commentary:

;;; Todo:

(eval-when-compile
  (require 'cl)
  (require 'rx)) ;; eval-when-compile

;;;; Version
(defvar imakado-version 0.11)
(defun imakado-require-version (v)
  (unless (>= imakado-version v)
    (error "\
You need to upgrade imakado.el %s to %s.
You have imakado.el %s at %s."
           imakado-version
           v
           imakado-version
           (progn (require 'find-func)
                  (and (fboundp 'find-library-name)
                       (find-library-name "imakado")))))
  t)
(defalias 'imakado-el-require-version->= 'imakado-require-version)


;;;; i/with-gensyms
(eval-and-compile
  (defvar i/gensym-prefix "--imakado--")
  (defvar i/*gensym-counter* 0)
  (defun i/gensym (&optional prefix)
    "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"G\"."
    (let ((pfix (if (stringp prefix) prefix i/gensym-prefix))
          (num (if (integerp prefix) prefix
                 (prog1 i/*gensym-counter*
                   (setq i/*gensym-counter* (1+ i/*gensym-counter*))))))
      (make-symbol (format "%s%d" pfix num))))
  ;; slices
  (defun* i/group (source n &key (error-check nil))
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
  (defun i/allquote (args)
    (loop for arg in args
          collect `(quote ,arg)))
  (defun* i/in-aux-test (v choises &key (test 'eq))
    (loop for c in choises
          collect `(,test ,v ,c)))
  (defun i/flatten (list)
    "Flatten any lists within ARGS, so that there are no sublists."
    (loop for item in list
          if (listp item)
          nconc (i/flatten item)
          else
          collect item))
  ;; copied from cl
  (defun i/subseq (seq start &optional end)
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
  (defun* i/remove-if (pred seq &key (key 'identity))
    (loop for elem in seq
          unless (funcall pred (funcall key elem))
          collect elem))
  (defun* i/remove-if-not (pred seq &key (key 'identity))
    (loop for elem in seq
          when (funcall pred (funcall key elem))
          collect elem))
  (defsubst i/acdr (key alist)
    (cdr (assq key alist)))
  ) ;; eval-and-compile

(defmacro i/with-gensyms (syms &rest body)
  (declare (indent 1)
           (debug ((&rest symbolp)
                   body)))
  (let ((clauses (loop for sym in syms
                      collect `( ,sym  ',(i/gensym i/gensym-prefix) ))))
    `(let ( ,@clauses )
       ,@body)))

(defmacro i/with-lexical-bindings (syms &rest body)
  (declare (indent 1)
           (debug ((&rest symbolp)
                   body)))
  (let ((clauses (loop for sym in syms
                      collect `( ,sym  ,sym ))))
    `(lexical-let ( ,@clauses )
       ,@body)))

;;;; i/dirconcat
(defmacro i/dirconcat (d1 str)
  (declare (debug (form form)))
  (i/with-gensyms (d1-tmp str-temp)
    `(let* ((,d1-tmp ,d1)
            (,d1-tmp (if (file-directory-p ,d1-tmp)
                         (file-name-as-directory ,d1-tmp)
                       (error "not directory: %s" ,d1-tmp)))
            (,str-temp ,str))
       (concat ,d1-tmp ,str))))

(defmacro* i/in-directory (directory &rest body)
  (declare (debug (form body))
           (indent 1))
  (let ((before-directory (i/gensym)))
    `(let ((,before-directory default-directory)
           (default-directory ,directory))
       (cd ,directory)
       ,@body
       (cd ,before-directory))))

;;;; i/remif
(defsubst i/remif-aux (pred seq key cond)
  (i/with-gensyms (g-pred g-seq g-key g-elem)
    `(let ((,g-pred ,pred)
           (,g-seq ,seq)
           (,g-key ,key)
           (,g-elem nil))
       (loop for ,g-elem in ,g-seq
             ,cond (funcall ,g-pred (funcall ,g-key ,g-elem))
             collect ,g-elem))))
(defmacro* i/remif (pred seq &key (key (quote 'identity)))
  (declare (debug (form form &rest [":key" function-form])))
  (i/remif-aux pred seq key 'unless))

(defmacro* i/!remif (pred seq &key (key (quote 'identity)))
  (declare (debug (form form &rest [":key" function-form])))
  (i/remif-aux pred seq key 'when))


;;;; Special
;; almost copied from anything.el
(defmacro i/aif (test-form then-form &rest else-forms)
  (declare (indent 2)
           (debug (form form &rest form)))
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro i/awhen (test-form &rest body)
  (declare (indent 1)
           (debug (form body)))
  `(i/aif ,test-form
       (progn ,@body)))

(defmacro i/awhile (expr &rest body)
  (declare (indent 1)
           (debug (form body)))
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro i/aand (&rest args)
  (declare (debug (&rest form)))
  (cond
   ((null (car args)) t)
   ((null (cdr args)) (car args))
   (t `(i/aif ,(car args) (i/aand ,@(cdr args))))))

(defmacro i/acond (&rest clauses)
  (declare (indent 0)
           (debug cond))
  (cond
   ((null clauses) nil)
   (t
    (i/with-gensyms (test)
      (let ((clause (car clauses)))
        `(let ((,test ,(car clause)))
           (if ,test
               (let ((it ,test))
                 ,@(cdr clause))
             (i/acond ,@(cdr clauses)))))))))

(defmacro i/alambda (params &rest body)
  (declare (indent 1)
           (debug lambda))
  `(labels ((caller ,params ,@body))
     'caller))

;;;; i/cond-let
(defsubst i/cond-let-aux-vars (clauses)
  (let ((vars (delete-dups
               (loop for cla in clauses
                     append (mapcar 'car (cdr cla))))))
    (loop for var in vars
          collect (cons var (i/gensym i/gensym-prefix)))))
(defsubst i/cond-let-aux-binds (vars cla)
  (loop for bindform in (cdr cla)
        for (bind-var . bind-body) = bindform
        when (consp bindform)
        collect `( ,(assoc-default bind-var vars) . ,bind-body)))
(defsubst i/cond-let-aux-clause (vars cla body-fn)
  `( ,(car cla)  (let ,(mapcar 'cdr vars)
                   (let ,(i/cond-let-aux-binds vars cla)
                     (,body-fn ,@(mapcar 'cdr vars))))))

(defmacro i/cond-let (clauses &rest body)
  (declare (indent 1)
           (debug ((&rest (form &rest (symbolp body))) body)))
  (i/with-gensyms (body-fn)
    (let ((vars (i/cond-let-aux-vars clauses)))
      `(labels ((,body-fn ,(mapcar 'car vars)
                          ,@body))
         (cond
          ,@(loop for cla in clauses
                  collect (i/cond-let-aux-clause vars cla body-fn)))))))

;;;; i/when-let
;; the code is taken from slime.el
(defmacro* i/when-let ((var value) &rest body)
  "Evaluate VALUE, if the result is non-nil bind it to VAR and eval BODY."
  (declare (indent 1)
           (debug ((symbolp form) body)))
  `(let ((,var ,value))
     (when ,var ,@body)))

;;;; i/fn
(eval-when-compile
  (defvar i/fn-aux-anaph-arg-map
    (loop for n from 1 to 20
          for sym = (intern (format "_%s" n))
          collect `( ,sym . ,n))))
(defsubst i/fn-aux-anaph-arg-syms (flatten fn-args-arg fn-args-rest)
  (and (not (equal fn-args-arg '(_)))
       (let ((anaph-arg-syms (mapcar 'car (eval-when-compile
                                            i/fn-aux-anaph-arg-map))))
         (i/remove-if-not (lambda (atom)
                             (memq atom anaph-arg-syms))
                           flatten))))
(defsubst i/fn-aux-appear_? (flatten fn-args-arg fn-args-rest)
  (and (not (equal fn-args-arg '(_)))
       ;; (i/fn (_ b) (list b))
       (or (and (null fn-args-rest)
                (memq '_ (i/flatten fn-args-arg)))
           (and fn-args-rest
                (not (remove '_ fn-args-arg))))
       (memq  '_ flatten)))
(defsubst i/fn-aux (fn-args)
  (let ((flatten (i/flatten fn-args))
        (fn-args-arg (car fn-args))
        (fn-args-rest (cdr fn-args)))
    (i/acond
      ((i/fn-aux-anaph-arg-syms flatten fn-args-arg fn-args-rest)
       (assert (not (memq '_ flatten))
               nil
               "cant use \"_\" and \"(_1 _2 ...)\" at the same time!!")
       (let* ((arg-count (apply 'max
                                (loop for sym in it
                                      collect (assoc-default sym (eval-when-compile
                                                                   i/fn-aux-anaph-arg-map)))))
              (lambda-args (mapcar 'car (i/subseq (eval-when-compile i/fn-aux-anaph-arg-map) 0 arg-count))))
         `(lambda ,lambda-args
            (let ((_0 (list ,@lambda-args)))
              ,@fn-args))))
      ((i/fn-aux-appear_? flatten fn-args-arg fn-args-rest)
       `(lambda (_) ,@fn-args))
      (t
       `(lambda ,@fn-args)))))
(defmacro i/fn (&rest args)
  (declare (indent defun)
           (debug (&or [&define lambda-list
                                [&optional stringp]
                                [&optional ("interactive" interactive)]
                                def-body]
                       body)))
  `,(i/fn-aux args))


;;;; Macro aliases
(eval-and-compile
(defmacro i/define-macro-alias (short long)
  `(defmacro ,short (&rest args)
     (declare ,(i/awhen (and (fboundp long)
                           (get long 'lisp-indent-function))
                 `(indent ,it))
              ,(i/awhen (and (fboundp long)
                           (get long 'edebug-form-spec))
                 `(debug ,it)))
     ,(documentation long)
     `(,',long ,@args)))

(defmacro i/define-macro-aliases (&rest args)
  `(progn
     ,@(loop for (short long) in (i/group args 2 :error-check t)
             collect `(i/define-macro-alias ,short ,long))))
)

;;;; define abbrevs
(i/define-macro-aliases
  dbind destructuring-bind
  mvbind multiple-value-bind
  mvsetq multiple-value-setq
  )
(i/define-macro-aliases
  i/dbind destructuring-bind
  i/mvbind multiple-value-bind
  i/mvsetq multiple-value-setq
  )

;;; destructuring-bind's edebug-spec has broken.
;;; so fix it.
(def-edebug-spec dbind
  (loop-var form body))


;;;; List

(defmacro i/cars (seq)
  (declare (debug (form)))
  `(mapcar 'car ,seq))

(defmacro i/cdrs (seq)
  (declare (debug (form)))
  `(mapcar 'cdr ,seq))

(defmacro i/cadrs (seq)
  (declare (debug (form)))
  `(mapcar 'cadr ,seq))

(defmacro i/assoc-cdrs (keys alist &optional test default)
  (declare (debug (form form)))
  `(mapcar (i/fn (assoc-default _ ,alist ,test ,default)) ,keys))

(defmacro i/nths (n seq)
  (declare (debug (form form)))
  `(mapcar (i/fn (nth ,n _)) ,seq))

(defmacro i/in (obj  &rest choises)
  (declare (indent 1)
           (debug (form &rest form)))
  (i/with-gensyms (insym)
    `(let ((,insym ,obj))
       (or ,@(i/in-aux-test insym choises)))))

(defmacro i/inq (obj &rest args)
  (declare (indent 1)
           (debug (form &rest symbolp)))
  `(i/in ,obj ,@(i/allquote args)))

(defmacro i/in= (obj &rest choises)
  (declare (indent 1)
           (debug i/in))
  (i/with-gensyms (inobj)
    `(let ((,inobj ,obj))
       (or ,@(i/in-aux-test inobj choises
                             :test 'equal)))))

(defmacro* i/join+ (seq &optional (separator "\n"))
  (declare (debug (form &optional form)))
  `(mapconcat 'identity ,seq ,separator))


;;; Case
(defsubst* i/case-cond-clause-aux (v cla &optional (test 'i/in))
  (dbind (key . body) cla
    (cond
     ((consp key) `((,test ,v ,@key) ,@body))
     ((i/inq key t otherwise) `(t ,@body))
     (t (error "bad clause")))))
(defmacro i/xcase (expr &rest clauses)
  (declare (indent 1)
           (debug case))
  (i/with-gensyms (v)
    `(let ((,v ,expr))
       (cond
        ,@(loop for cla in clauses
                collect (i/case-cond-clause-aux v cla))))))

;;;; i/xcase=
(defsubst* i/xcase-clause-aux-test (v keys body &key test)
  `((or ,@(loop for key in keys
                collect `(,test ,key ,v)))
    ,@body))
(defsubst* i/xcase-clause-aux (v cla &key (test 'equal))
  (dbind (key . body) cla
    (cond
     ((consp key) `,@(i/xcase-clause-aux-test v key body
                                               :test test))
     ((i/inq key t otherwise) `(t ,@body))
     (t (error "bad clause")))))
(defmacro i/xcase= (expr &rest clauses)
  (declare (indent 1)
           (debug (form &rest ((&rest form) body))))
  (i/with-gensyms (g-expr)
    `(let ((,g-expr ,expr))
       (cond
        ,@(loop for cla in clauses
                collect (i/xcase-clause-aux g-expr cla))))))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/xcase= +++++")
      (expect '(OPEN "aaa" CLOSE)
        (let ((tokens '("[" "aaa" "]"))
              (tag1 "[")
              (tag2 "]"))
          (loop for token in tokens
                collect
                (i/xcase= token
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
                (i/xcase= token
                  (("a") 'token-a)
                  (("b") 'token-b)
                  (otherwise 'otherwise)))))

      (desc "key must be consed")
      (expect (error)
        (i/xcase= "a"
          ("a" 'a)))

      (desc "this is ok")
      (expect 'a
        (i/xcase= "a"
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
                (i/xcase= token
                  ((open openb) 'OPEN)
                  ((close closeb) 'CLOSE)
                  (otherwise
                   token)))))

      (desc "nil")
      (expect '("nil" "otherwise" "nil" "otherwise")
        (let ((tokens '(nil a nil b)))
          (loop for token in tokens
                collect
                (i/xcase= token
                  ((nil) "nil")
                  (otherwise
                   "otherwise")))))
      )))


;;;; Struct
;; copied from slime.el
(defmacro* i/with-struct ((conc-name &rest slots) struct &body body)
  "Like with-slots but works only for structs.
\(i/fn (CONC-NAME &rest SLOTS) STRUCT &body BODY)"
  (declare (indent 2)
           (debug ((symbolp &rest symbolp) form body)))
  (flet ((reader (slot) (intern (concat (symbol-name conc-name)
                                        (symbol-name slot)))))
    (let ((struct-var (i/gensym "i/with-struct")))
      `(let ((,struct-var ,struct))
         (symbol-macrolet
             ,(mapcar (lambda (slot)
                        (etypecase slot
                          (symbol `(,slot (,(reader slot) ,struct-var)))
                          (cons `(,(first slot) (,(reader (second slot))
                                                 ,struct-var)))))
                      slots)
           . ,body)))))

(defmacro* i/define-with-struct-macro (name conc-name)
  `(defmacro* ,name ( slots struct &rest body)
     (declare (indent 2)
              (debug ((&rest symbolp) form body)))
     `(i/with-struct (,',conc-name ,@slots) ,struct ,@body)))

(defsubst i/with-struct-all-slots-get-getters-slot? (sym)
  (let ((plist (symbol-plist sym)))
    (and (memq 'cl-compiler-macro plist)
         (memq 'setf-method plist))))
(defsubst i/with-struct-all-slots-get-all-slots (prefix)
  (loop for s in (all-completions prefix obarray)
        for sym = (intern s)
        when (i/with-struct-all-slots-get-getters-slot? sym)
        collect (let ((slotname (replace-regexp-in-string (concat "^" prefix)
                                                          ""
                                                          s)))
                  (intern slotname))))
(defmacro i/define-with-all-slots-struct (name conc-name)
  `(defmacro ,name (struct &rest body)
     (declare (indent 1)
              (debug (form body)))
     (let ((i/slots ',(i/with-struct-all-slots-get-all-slots (symbol-name conc-name))))
       `(i/with-struct (,',conc-name ,@i/slots) ,struct ,@body))))

;;;; Progress Reporter
(defmacro i/dolist-with-progress-reporter (spec message min-change min-time &rest body)
  (declare (indent 4)
           (debug ((symbolp form &optional form) form form form body)))
  (i/with-gensyms (seq seq-length reporter loop-temp)
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
(defmacro i/with-anaphoric-match-utilities (string-used-match &rest body)
  (declare (indent 1)
           (debug (form body)))
  (i/with-gensyms (str)
    `(lexical-let ((,str ,string-used-match))
       (symbol-macrolet (
                         ,@(loop for i to 9 append
                                 (let ((sym (intern (concat "$" (number-to-string i))))
                                       (sym-match-beg (intern (concat "$MB-" (number-to-string i))))
                                       (sym-match-end (intern (concat "$ME-" (number-to-string i)))))
                                   `((,sym (match-string ,i ,str))
                                     (,sym-match-beg (match-beginning ,i))
                                     (,sym-match-end (match-end ,i)))))
                         )
         (flet (($ (i) (match-string i ,str))
                ($sub (replacement &optional (i 0) &key fixedcase literal-string) ;args
                      (replace-match replacement fixedcase literal-string ,str i)) ;body
                ($gsub (replacement &optional (i 0) &key fixedcase literal-string start)
                       (with-no-warnings
                         ;; see `i/=~'
                         (assert (boundp 'i/--regexp-used-by-=~))
                         (replace-regexp-in-string i/--regexp-used-by-=~
                                                   replacement
                                                   ,str
                                                   fixedcase
                                                   literal-string
                                                   i
                                                   start))))
           (symbol-macrolet ( ;;before
                             ,(i/awhen str
                                `($b (substring ,it 0 (match-beginning 0))))
                             ;;match
                             ($m (match-string 0 ,str))
                             ($M (match-string-no-properties 0 ,str))
                             ;;after
                             ,(i/awhen str
                                `($a (substring ,it (match-end 0) (length ,str))))
                             )
             ,@body))))))

(defmacro i/=~ (regexp string &rest body)
  (declare (indent 2)
           (debug (form form body)))
  "regexp matching similar to the i/=~ operator found in other languages."
  (i/with-gensyms (str)
    `(let ((,str ,string)
           (i/--regexp-used-by-=~ ,regexp))
       (when (string-match ,regexp ,str)
         (i/with-anaphoric-match-utilities ,str
           ,@(if body body '(t)))))))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "test initialize")
      (expect (true)
        (and (require 'imakado)
             (imakado-require-version 0.01)))

      (expect "huga"
        (i/with-point-buffer "\
hoge-huga-foo
"
          (when (re-search-forward (rx "-"(group (+ not-newline)) "-") nil t)
            (i/with-anaphoric-match-utilities nil
              (buffer-substring $MB-1
                                $ME-1)))))
          

      (desc "+++++ i/=~ +++++")
      (desc "If match, return last expr")
      (expect 'b
        (i/=~ ".*" "re" 'a 'b))
      (expect nil
        (i/=~ ".*" "re" 'a nil))
      (expect t
        (i/=~ ".*" "re"))
      (desc "If fail, return nil")
      (expect nil
        (i/=~ "never match regexp" "string"))
      (expect nil
        (i/=~ "never match regexp" "string" 'body))

      (desc "i/with-anaphoric-match-utilities")
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
          (i/=~ re "---- Dog looking at tiny Alice. ----"
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
          (i/=~ re "---- Dog looking at tiny Alice. ----"
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
          (i/=~ re "---- Dog looking at tiny Alice. ----"
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
          (i/=~ re "---- Dog looking at tiny Alice. ----"
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
          (i/=~ re "---- Dog looking at tiny Alice. ----"
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
          (i/=~ re "---- Dog looking at tiny Alice. ----"
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
          (i/=~ re "---- Dog looking at tiny Alice. ----"
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
          (i/=~ re "---- Dog looking at tiny Alice. ----"
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
          (let ((newstr (i/=~ re "---- Dog looking at tiny Alice. ----"
                          ($sub "elise" 2)))
                (case-fold-search nil)) ; case-sensitive
            (i/=~ "Elise" newstr))))
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
          (let ((newstr (i/=~ re "---- Dog looking at tiny Alice. ----"
                          ($sub "elise" 2
                                :fixedcase t)))
                (case-fold-search nil)) ; case-sensitive
            (i/=~ "Elise" newstr))))
      (desc "($gsub subexp)")
      (expect t
        (let ((newstr (i/=~ "/" "a/b/c"
                        ($gsub "::")))
              (case-fold-search nil)) ; case-sensitive
          (i/=~ "a::b::c" newstr)))
      )))

;;; i/case-regexp
(defsubst i/case-regexp-aux (expr-sym clauses)
  (loop for (regexp-or-sym . body) in clauses
        for body = (or body '(nil))
        collect
        (cond
         ((i/inq regexp-or-sym t otherwise)
          `(t ,@body))
         (t
          `((i/=~ ,regexp-or-sym ,expr-sym)
            (i/with-anaphoric-match-utilities ,expr-sym
              ,@body))))))
(defmacro i/case-regexp (expr &rest clauses)
  (declare (indent 1)
           (debug (form &rest (form body))))
  (i/with-gensyms (expr-tmp)
    `(let ((,expr-tmp ,expr))
       (etypecase ,expr-tmp
         (string (cond ,@(i/case-regexp-aux expr-tmp clauses)))))))

;;;; i/match-with-temp-buffer
(defmacro i/match-with-temp-buffer (spec &rest body)
  (declare (indent 1)
           (debug ((form form &optional form) body)))
  (let* ((ret-form (nth 2 spec)))
    (i/with-gensyms (re str g-loop-res)
      `(dbind (,re ,str ) (list ,(nth 0 spec) ,(nth 1 spec))
         (with-temp-buffer
             (insert ,str)
             (loop with ,g-loop-res
                   initially (goto-char (point-min))
                   while (re-search-forward ,re nil t)
                   ,@(if ret-form
                        `( do (i/with-anaphoric-match-utilities nil
                                ,@body))
                       `(do (push (i/with-anaphoric-match-utilities nil
                                  ,@body)
                                ,g-loop-res)))
                   finally return ,(or ret-form `,g-loop-res)))))))

;;;; With-

(defmacro i/with-temp-buffer-file (file &rest body)
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
      (desc "+++++ i/with-temp-buffer-file +++++")
      (expect ";;; imakado.el - imakado's usefull macros"
        (require 'find-func)
        (i/with-temp-buffer-file (find-library-name "imakado")
          (buffer-substring-no-properties (point) (point-at-eol))))
      (expect (error)
        (let ((nonexistent-filename (loop for n from 1 to 9999
                                          for nonexistent-filename = (format "/aa-%s" n)
                                          unless (file-readable-p nonexistent-filename)
                                          return nonexistent-filename)))
          (i/with-temp-buffer-file nonexistent-filename
            'no-error))))))

(defmacro i/match-with-temp-buffer-file (spec &rest body)
  (declare (indent 1)
           (debug ((form form &optional form) body)))
  (i/with-gensyms (g-file-contents-str)
    `(let ((,g-file-contents-str ,(i/with-temp-buffer-file (nth 1 spec)
                                    (buffer-substring-no-properties (point-min) (point-max)))))
       (i/match-with-temp-buffer (,(nth 0 spec) ,g-file-contents-str ,(nth 2 spec))
         ,@body))))


;;;; Buffer
(defmacro i/save-excursion-force (&rest body)
  (declare (indent 0)
           (debug (body)))
  (i/with-gensyms (saved-point saved-current-buffer)
    `(let ((,saved-point (point))
           (,saved-current-buffer (current-buffer)))
       (prog1 (progn ,@body)
         (ignore-errors
           (and (eq ,saved-current-buffer (current-buffer))
                (goto-char ,saved-point)))))))

(defsubst i/goto-pointmark-and-delete ()
  (when (re-search-forward (rx "`!!'") nil t)
    (replace-match "")))
(defmacro i/with-point-buffer (str &rest body)
  (declare (indent 1)
           (debug (form body)))
  `(with-temp-buffer
     (insert ,str)
     (goto-char (point-min))
     (i/goto-pointmark-and-delete)
     (progn
       ,@body)))

;;;; Keymap
(defmacro* i/define-define-keymap (name keymap &key (doc nil))
  `(etypecase ,keymap
     (keymap
      (defmacro ,name (key-str-or-vector command)
        ,doc
        (typecase key-str-or-vector
          (string `(define-key ,',keymap (kbd ,key-str-or-vector) ,command))
          (t `(define-key ,',keymap ,key-str-or-vector ,command)))))))

;;;; i/defcacheable
(defmacro* i/defcacheable (name args &rest body)
  (declare (indent 2)
           (debug defun*))
  (flet ((i/defcacheable-doc&body (body)
          (typecase (car-safe body)
            (string (values (car body) (cdr body)))
            (otherwise (values nil body)))))
    (destructuring-bind (doc body) (i/defcacheable-doc&body body)
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
      (desc "+++++ i/defcacheable +++++")
      (desc "defined ok")
      (expect t
        (let ((call-count 0))
          (makunbound 'i/--test-defcacheable-fn-cache)
          (fmakunbound 'i/--test-defcacheable-fn-clear-cache)
          (fmakunbound 'i/--test-defcacheable-fn)
          (i/defcacheable i/--test-defcacheable-fn ()
            "docstr"
            (prog1 "cache me if you can!"
              (incf call-count)))
          (and (boundp 'i/--test-defcacheable-fn-cache)
               (fboundp 'i/--test-defcacheable-fn)
               (fboundp 'i/--test-defcacheable-fn-clear-cache))))
      (desc "call just once")
      (expect '("cache me if you can!" 1)
        (let ((call-count 0))
          (makunbound 'i/--test-defcacheable-fn-cache)
          (fmakunbound 'i/--test-defcacheable-fn-clear-cache)
          (fmakunbound 'i/--test-defcacheable-fn)
          (i/defcacheable i/--test-defcacheable-fn ()
            "docstr"
            (prog1 "cache me if you can!"
              (incf call-count)))
          (i/--test-defcacheable-fn)
          (let ((res (i/--test-defcacheable-fn)))
            (list res call-count))))
      (desc "clear cache")
      (expect 2
        (let ((call-count 0))
          (makunbound 'i/--test-defcacheable-fn-cache)
          (fmakunbound 'i/--test-defcacheable-fn-clear-cache)
          (fmakunbound 'i/--test-defcacheable-fn)
          (i/defcacheable i/--test-defcacheable-fn ()
            "docstr"
            (prog1 "cache me if you can!"
              (incf call-count)))
          (i/--test-defcacheable-fn)
          (i/--test-defcacheable-fn-clear-cache)
          (i/--test-defcacheable-fn)
          call-count))
      (desc "cleanup")
      (expect t
        (progn (makunbound 'i/--test-defcacheable-fn-cache)
               (fmakunbound 'i/--test-defcacheable-fn-clear-cache)
               (fmakunbound 'i/--test-defcacheable-fn)
               t))
      )))

(defmacro* i/memoize
    (fn-sym
     &key
     (save-directory nil)
     (encoding nil)
     (hash-size 60))
  (let* ((fn-sym (eval fn-sym))
         (advice-sym (intern (format "memoize-%s" fn-sym)))
         (get-fn-sym (intern (format "%s-get-from-cache" fn-sym)))
         (save-fn-sym (intern (format "%s-save" fn-sym)))
         (load-fn-sym (intern (format "%s-load" fn-sym)))
         (hash-fn-sym (intern (format "%s-hash" fn-sym)))
         (saved-alist-sym (intern (format "%s-alist" fn-sym)))
         (save-file-name (replace-regexp-in-string "/" "-" (format "%s-alist.el" fn-sym))))
    `(progn
       (defvar ,saved-alist-sym nil)
       (defun ,load-fn-sym ()
         (when (and ,save-directory
                    (i/aand (i/dirconcat ,save-directory ,save-file-name)
                          (file-readable-p it)))
           (ignore-errors
             (load (file-name-sans-extension
                    (i/dirconcat ,save-directory ,save-file-name)))
             ,saved-alist-sym)))
       (i/defcacheable ,hash-fn-sym ()
         (let ((hsh (make-hash-table :test 'equal :size ,hash-size)))
           (cond
            (,save-directory
             (i/aand (,load-fn-sym)
                   (loop for (k . v) in it
                         do (puthash k v hsh)))
             hsh)
            (t hsh))))
       (defun ,save-fn-sym ()
         (ignore-errors
           (when (boundp ',(intern (format "%s-cache" fn-sym)))
             (let ((hash->alist
                    (lambda (hash)
                      (check-type hash hash-table)
                      (loop for k being the hash-keys of hash
                            for v = (gethash k hash)
                            collect (cons k v)))))
               (with-temp-buffer
                 (insert ";; -*- mode: emacs-lisp -*-\n" )
                 (prin1 `(setq ,',saved-alist-sym ',(funcall hash->alist
                                                             (,hash-fn-sym)))
                        (current-buffer))
                 (insert "\n")
                 (let ((coding-system-for-write ,encoding)
                       (save-file (i/dirconcat ,save-directory ,save-file-name)))
                   (write-region (point-min)
                                 (point-max)
                                 save-file
                                 nil
                                 )))))))
       (defun ,get-fn-sym (key)
         (gethash key (,hash-fn-sym)))
       (defadvice ,fn-sym (around ,(intern (format "memoize-%s" (symbol-name fn-sym)))
                                  (key)
                                  activate)
         (i/acond
           ((,get-fn-sym key)
            (setq ad-return-value it))
           (t
            (let ((res ad-do-it))
              (puthash key res (,hash-fn-sym))))))
       ,(when save-directory
          `(add-hook 'kill-emacs-hook
                     ',save-fn-sym))
       )))
;; (i/memoize 'fnn)
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/memoize +++++")
      (desc "prepare")
      (expect t
        (lexical-let ((alist '(("apple" . "fruit")
                               ("cat" . "animal")))
                      (call-count 0))
          (fmakunbound 'i/--test-memoize-fn)
          (defun* i/--test-memoize-fn (key)
            (prog1 (assoc-default key alist)
              (incf call-count)))
          t)
        )
      (desc "basic")
      (expect t
        (i/memoize 'i/--test-memoize-fn
                 :save-directory (i/dirconcat (file-name-directory
                                             (find-library-name "imakado"))
                                            "tmp")
                 :encoding 'japanese-shift-jis-dos
                 :hash-size 20000)
        (i/--test-memoize-fn "apple")
        (i/--test-memoize-fn "cat")
        (i/--test-memoize-fn-hash)
        (i/--test-memoize-fn-hash-clear-cache)
        (i/--test-memoize-fn-save)
        (i/--test-memoize-fn-load)
        )
      )))


;;;; i/do-temp-file
(defmacro* i/do-temp-file
    ((temp-file-var
      &optional
      file-contents-string
      temp-file-prefix
      (clean-up-p t)
      (temp-file-suffix nil))
     &rest body)
  (declare (indent 1)
           (debug ((symbolp form &optional form symbolp form) body)))
  (i/with-gensyms (g-file-contents-string)
    `(let ((,g-file-contents-string ,file-contents-string)
           (,temp-file-var (make-temp-file (format "%s" (or ,temp-file-prefix "do-temp-file."))
                                           nil
                                           ,temp-file-suffix)))
       (unwind-protect
           (progn
             (with-temp-file ,temp-file-var
               (when ,g-file-contents-string
                 (insert ,g-file-contents-string)))
             ,@body)
         (and ,clean-up-p
              (file-exists-p ,temp-file-var)
              (delete-file ,temp-file-var))))))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/do-temp-file +++++")
      (expect (regexp "i/do-temp-file\\.")
        (i/do-temp-file (tempfile "contents")
          tempfile))
      (expect (regexp "prefix-")
        (i/do-temp-file (tempfile "contents" "prefix-")
          tempfile))
      (expect "contents"
        (i/do-temp-file (tempfile "contents" "prefix-")
          (i/with-temp-buffer-file tempfile
            (buffer-substring-no-properties (point-min) (point-max)))))
      (desc "omit arg")
      (expect t
        (i/do-temp-file (tempfile)
          (file-exists-p tempfile)))
      (desc "cleanup")
      (expect nil
        (let ((gtempfile nil))
          (i/do-temp-file (tempfile "contents" "prefix-")
            (setq gtempfile tempfile))
          (file-exists-p gtempfile)))
      )))


;;;; i/for-each-single-property-change
(defmacro i/for-each-single-property-change (property i/fn)
  "i/fn is called with two args (current-point next-change-point)"
  (declare (debug (form form)))
  (i/with-gensyms (g-fn g-property g-next-change g-current-point)
    `(let ((,g-fn ,i/fn)
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
      (desc "+++++ i/for-each-single-property-change +++++")
      (expect (true)
        (i/with-temp-buffer-file (find-library-name "imakado")
          (emacs-lisp-mode)
          (font-lock-mode t)
          (save-excursion (font-lock-fontify-region (point-min) (point-max)))
          (let ((ret nil)
                (func-name-collector (lambda (p next-change)
                                       (when (eq (get-text-property p 'face) 'font-lock-function-name-face)
                                         (push (buffer-substring-no-properties p next-change) ret)))))
            (i/for-each-single-property-change 'face func-name-collector)
            (i/remif (i/fn (i/=~ "^ik:test-" _)) ret))))
      )))

;;;; Eieio utils
(defsubst i/define-defmethod-modifies&rest (args)
  (case (car-safe args)
    ((:before :primary :after :static)
     (values (car args) (cdr args)))
    (t (values nil args))))
(defsubst i/define-defmethod-add-self-to-args (class rest)
  (dbind (args . body) rest
    (cons (cons `(self ,class) args)
          body)))
(defmacro i/define-defmethod (name class)
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
     (dbind (modifier rest) (i/define-defmethod-modifies&rest
                             body)
       (let* ((rest (i/define-defmethod-add-self-to-args ',class rest))
              (args (if modifier
                        (cons modifier rest)
                      rest)))
         (cons 'defmethod
               (cons method args))))))

;;;; i/defclass+
(defmacro i/defclass+ (&rest args)
  (let* ((name (car args))
         (defmethod-macro-sym (intern (format "%s-defmethod" name))))
    `(prog1
       (defclass ,@args)
       (i/define-defmethod ,defmethod-macro-sym ,name))))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/defclass+ +++++")
      (expect "aa"
        (flet ((tests/class-b ()))
          (i/defclass+ tests/class-b ()
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

;;;; i/require-methods
(defsubst* i/require-methods-aux (class methods)
  (loop for method in methods
        collect `(defmethod ,method ((self ,class) &rest ignore)
                   (error "this function should be implemented in subclasses."))))
(defmacro i/require-methods (class methods)
  (declare (indent 1)
           (debug (symbolp list)))
  `(progn ,@(i/require-methods-aux class methods)))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/require-methods +++++")
      (expect (error)
        (progn
          (i/defclass+ i/--test-require-methods ()
            ()
            "class")
          (i/require-methods i/--test-require-methods
            (i/--test-require-methods-interface-fn))
          (let ((ins (make-instance 'i/--test-require-methods)))
            (i/--test-require-methods-interface-fn ins))))
      (desc "call with arg")
      (expect (error)
        (progn
          (i/defclass+ i/--test-require-methods ()
            ()
            "class")
          (i/require-methods i/--test-require-methods
            (i/--test-require-methods-interface-fn))
          (let ((ins (make-instance 'i/--test-require-methods)))
            (i/--test-require-methods-interface-fn ins 'a 'b 'c))))
      )))


;;;; i/with-orefs
(defmacro i/with-orefs (clauses obj &rest body)
  (declare (indent 2)
           (debug ((symbolp &rest symbolp) form body)))
  (let ((clas
         (loop for cla in clauses
               collect
               (etypecase cla
                 (cons `( ,(car cla) ,`(oref ,obj ,(second cla))))
                 (symbol `( ,cla ,`(oref ,obj ,cla)))))))
    `(let ( ,@clas )
       ,@body)))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/with-orefs +++++")
      (expect '(1 2)
        (progn
          (i/defclass+ i/--test-with-orefs ()
            ((slo1 :initarg :slo1
                   :type integer)
             (slo2 :initarg :slo2
                   :type integer))
            "class")
          (let ((ins (make-instance 'i/--test-with-orefs
                                    :slo1 1
                                    :slo2 2)))
            (i/with-orefs (slo1 (s2 slo2)) ins
                        (list slo1 s2))))
        )
      )))

;;;; i/require-slots
(defmacro i/require-slots (class slots)
  (declare (indent 1)
           (debug (symbolp list)))
  `(defmethod initialize-instance :AFTER ((self ,class) &rest ignore)
     (progn
       ,@(loop for slo in slots
               collect
               `(assert (slot-boundp self ',slo))))))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/require-slots +++++")
      (expect (error)
        (progn
          (i/defclass+ i/--test-require-slots ()
            ((slo1 :initarg :slo1
                   :type integer)
             (slo2 :initarg :slo2
                   :type integer))
            "class")
          (i/require-slots i/--test-require-slots
                         (slo2))
          (make-instance 'i/--test-require-slots
                         :slo1 1))
        )
      (expect t
        (progn
          (i/defclass+ i/--test-require-slots ()
            ((slo1 :initarg :slo1
                   :type integer)
             (slo2 :initarg :slo2
                   :type integer))
            "class")
          (i/require-slots i/--test-require-slots
                         (slo2))
          (i/--test-require-slots-child-p
           (make-instance 'i/--test-require-slots
                          :slo1 1
                          :slo2 2)))
        )
      )))




(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/define-defmethod +++++")
      (expect (true)
        (flet ((tests/class-a ())
               (message (&rest args) (print (apply 'format args))))
          (defclass tests/class-a ()
            ((slot-a :initarg :slot-a
                     :initform nil))
            "class a")
          (i/define-defmethod def-tests/class-a-method tests/class-a)
          (def-tests/class-a-method i/--test-say-ya ()
            (message "%s" self))
          (def-tests/class-a-method i/--test-say-ya :before ()
            (message "%s" ":before i/--test-say-ya"))
          (let ((res (with-output-to-string
                       (i/--test-say-ya (make-instance 'tests/class-a)))))
            (and (string-match ":before i/--test-say-ya" res)
                 (string-match "[object tests/class-a tests/class-a nil]" res)))))

      (desc "+++++ i/define-defmethod-modifies&rest +++++")
      (expect '(:before (((cla-a tests/class-a)) (message "%s" ":before say-ya")))
        (dbind (modifier body) (i/define-defmethod-modifies&rest
                                '(:before
                                  ((cla-a tests/class-a))
                                  (message "%s" ":before say-ya")))
          (list modifier body))
        )
      (expect '(nil (((cla-a tests/class-a)) (message "%s" ":before say-ya")))
        (dbind (modifier body) (i/define-defmethod-modifies&rest
                                '(((cla-a tests/class-a))
                                  (message "%s" ":before say-ya")))
          (list modifier body)))

      (desc "+++++ i/define-defmethod-add-self-to-args +++++")
      (expect '(((self tests/class-a) arg1 arg2) (message "%s" ":before say-ya"))
        (i/define-defmethod-add-self-to-args
         'tests/class-a
         '((arg1 arg2)
           (message "%s" ":before say-ya"))))
      )))


;;;; Simple template
(eval-when-compile
  (defvar i/simple-template-expression_mark "=")
  (defvar i/simple-template-comment_mark "#")

  (defvar i/simple-template-tag-start "[%")
  (defvar i/simple-template-tag-end "%]")
  (defvar i/simple-template-tag-expression-start
    (concat i/simple-template-tag-start
            i/simple-template-expression_mark))
  (defvar i/simple-template-tag-comment-start
    (concat i/simple-template-tag-start
            i/simple-template-comment_mark))
  ) ;; eval-when-compile
(eval-when-compile
  (defvar i/simple-template-token-separator-re
    (rx (or (eval (eval-when-compile i/simple-template-tag-expression-start))
            (eval (eval-when-compile i/simple-template-tag-comment-start))
            (eval (eval-when-compile i/simple-template-tag-start))
            (eval (eval-when-compile i/simple-template-tag-end))))))

(defsubst i/simple-template-parse-buffer-get-token-and-advance ()
  (let ((p (point)))
    (cond
     ((eobp) nil)
     ((looking-at (eval-when-compile i/simple-template-token-separator-re))
      (prog1 (match-string-no-properties 0)
        (goto-char (match-end 0))))
     ((re-search-forward (eval-when-compile i/simple-template-token-separator-re) nil t)
      (goto-char (match-beginning 0))
      (buffer-substring-no-properties p (point)))
     (t
      (prog1 (buffer-substring-no-properties p (point-max))
        (goto-char (point-max)))))))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "i/simple-template-parse-buffer-get-token-and-advance")
      (expect '("The " "[%" "  " "%]" " " "[%#" " perfect " "%]" " " "[%=" " insider " "%]" ".")
        (i/with-point-buffer "The [%  %] [%# perfect %] [%= insider %]."
          (goto-char (point-min))
          (loop for token = (i/simple-template-parse-buffer-get-token-and-advance)
                while token
                collect token)))
      (desc "newline")
      (expect '("The " "[%" "  " "%]" " " "[%#" " \nperfect " "%]" " " "[%=" " outsider " "%]" ".")
        (i/with-point-buffer "The [%  %] [%# \nperfect %] [%= outsider %]."
          (goto-char (point-min))
          (loop for token = (i/simple-template-parse-buffer-get-token-and-advance)
                while token
                collect token)))
      (expect nil
        (i/with-point-buffer ""
          (goto-char (point-min))
          (loop for token = (i/simple-template-parse-buffer-get-token-and-advance)
                while token
                collect token)))
      (expect '("newline\n")
        (i/with-point-buffer "newline\n"
          (goto-char (point-min))
          (loop for token = (i/simple-template-parse-buffer-get-token-and-advance)
                while token
                collect token)))
      (expect '("[%" " (if flag " "%]" "\n" "[%" " ) " "%]")
        (let ((o "you")
              (flag t)
              (tmpl "\
\[% (if flag %]
\[% ) %]"))
          (i/with-point-buffer tmpl
            (goto-char (point-min))
            (loop for token = (i/simple-template-parse-buffer-get-token-and-advance)
                  while token
                  collect token))))
      )))

(defsubst i/simple-template-parse-buffer ()
  (let ((state 'text)
        (multiline_expression nil)
        (tree nil)
        (tag-end (eval-when-compile i/simple-template-tag-end))
        (tag-start (eval-when-compile i/simple-template-tag-start))
        (tag-comment-start (eval-when-compile i/simple-template-tag-comment-start))
        (expression-start (eval-when-compile i/simple-template-tag-expression-start)))
    (loop initially (goto-char (point-min))
          with tokens
          for token = (i/simple-template-parse-buffer-get-token-and-advance)
          while token
          do (i/xcase= token
               ((tag-end)
                (setq state 'text
                      multiline_expression nil))
               ((tag-start)
                (setq state 'code))
               ((tag-comment-start)
                (setq state 'comment))
               ((expression-start)
                (setq state 'expr))
               (otherwise
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
      (desc "i/simple-template-parse-buffer")
      (expect nil
        (i/with-point-buffer "[%# \nperfect %]"
          (i/simple-template-parse-buffer)))
      (expect '((text "The ") (code " aa ") (text " \n ") (text " ") (expr " insider ") (text "."))
        (i/with-point-buffer "The [% aa %] \n [%# \nperfect %] [%= insider %]."
          (i/simple-template-parse-buffer)))
      )))

(defsubst* i/simple-template-compile-aux (type value &key (insert-fn 'insert))
  (ecase type
    (text (prin1 `(,insert-fn ,value) (current-buffer)))
    (code (insert value))
    (expr (insert "(insert " value ")"))))
(defsubst* i/simple-template-compile (tree)
  (with-temp-buffer
    (insert "(progn ")
    (loop for (type value) in tree
          do (i/simple-template-compile-aux type value))
    (insert ")")
    (goto-char (point-min))
    (condition-case err
        (read (current-buffer))
      (error
       (error "\
ERROR occur during compiling template.\ntree: %S\ncompiled source: %S\nerror-message: %s"
              tree (buffer-substring-no-properties (point-min) (point-max))
              (error-message-string err))))))
(defvar i/*simple-template-output-buffer nil
  "this variable must be let bounded")

(defsubst* i/simple-template-buffer ()
  ;; this temp var name should be prefixed.
  ;; please see test -> (desc "Note, variable `i/--form' is seen inside tmplate.")
  (let ((i/--form (i/simple-template-compile
                    (i/simple-template-parse-buffer)))
        (i/--output-buffer
         (and (boundp 'i/*simple-template-output-buffer)
              (or (stringp (symbol-value 'i/*simple-template-output-buffer))
                  (bufferp (symbol-value 'i/*simple-template-output-buffer)))
              (get-buffer (symbol-value 'i/*simple-template-output-buffer)))))
    (cond
     (i/--output-buffer
      (with-current-buffer i/--output-buffer
        (eval i/--form)))
     (t
      (with-temp-buffer
        (eval i/--form)
        (buffer-string))))))

(defsubst* i/simple-template (file-or-list-of-file)
  (assert (not (null file-or-list-of-file)))
  (with-temp-buffer
    (etypecase file-or-list-of-file
      (list (dolist (file file-or-list-of-file)
              (insert-file-contents file)
              (goto-char (point-max))))
      (string (insert-file-contents file-or-list-of-file)))
    (i/simple-template-buffer)))

(defsubst* i/simple-template-string (str-or-list-of-string)
  (assert (not (null str-or-list-of-string)))
  (with-temp-buffer
    (etypecase str-or-list-of-string
      (list (insert (mapconcat 'identity str-or-list-of-string "")))
      (string (insert str-or-list-of-string)))
    (i/simple-template-buffer)))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/simple-template +++++")
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
          (i/simple-template-string tmpl)))

      (desc "Note, variable `i/--form' is seen inside tmplate.")
      (expect (error)
        (let ((i/--form "lexical"))
          (i/do-temp-file (tfile  "loving [%= i/--form %].")
            (i/simple-template tfile))))

      (desc "this gonna well")
      (expect "i miss lexical scope."
        (let ((other-var-name "lexical"))
          (i/do-temp-file (tfile  "i miss [%= other-var-name %] scope.")
            (i/simple-template tfile))))

      (desc "last newline")
      (expect "newline\n"
        (i/do-temp-file (tfile  "newline\n")
          (i/simple-template tfile)))

      (desc "basic file")
      (expect "loving you."
        (let ((o  "you"))
          (i/do-temp-file (tfile  "loving [%= o %].")
            (i/simple-template tfile))))
      (desc "allow newline inside tag")
      (expect "loving you."
        (let ((o "you"))
          (i/do-temp-file (tfile  "loving [%= \n  o \n %].")
            (i/simple-template tfile))))

      (desc "just string")
      (expect "loving you."
        (i/do-temp-file (tfile  "loving you.")
          (i/simple-template tfile)))

      (desc "[%# ... %] COMMENT")
      (expect "loving"
        (i/do-temp-file (tfile  "loving[%# COMMENT %]")
          (i/simple-template tfile)))

      (desc "[%# ... %] newline inside COMMENT ")
      (expect "loving"
        (i/do-temp-file (tfile  "loving[%# \n COMMENT \n  %]")
          (i/simple-template tfile)))

      (desc "call with nil")
      (expect (error)
        (i/do-temp-file (tfile  "loving[%# \n COMMENT \n  %]")
          (i/simple-template nil)))

      (desc "call with empty string")
      (expect (error)
        (i/simple-template ""))

      (desc "multiple file")
      (expect "1\n2"
        (let ((tmpl1 "1")
              (tmpl2 "2"))
          (i/do-temp-file (tfile1  "[%= tmpl1 %]\n")
            (i/do-temp-file (tfile2 "[%= tmpl2 %]")
              (i/simple-template (list tfile1 tfile2))))))

      (desc "+++++ i/simple-template-string +++++")
      (desc "call with nil")
      (expect (error)
        (i/simple-template-string nil))

      (desc "call with empty string")
      (expect ""
        (i/simple-template-string ""))


      (desc "i/*simple-template-output-buffer")
      (desc "No error even if `i/*simple-template-output-buffer' is unbound")
      (expect "loving you."
        (let ((i/*simple-template-output-buffer i/*simple-template-output-buffer))
          (makunbound 'i/*simple-template-output-buffer)
          (assert (not (boundp 'i/*simple-template-output-buffer)))
          (let ((tmpl  "loving you."))
            (i/simple-template-string tmpl))))

      (desc "widget.el support")
      (expect 'push-button
        (let ((insert-push-button
               (i/fn () (widget-create
                       'push-button
                       :action (lambda (widget &rest ignore))
                       :value "push-button")))
              (tmpl "`!!'[% (funcall insert-push-button) %]")
              (i/*simple-template-output-buffer
               (get-buffer-create "*simple-template-output-buffer test*")))
          (i/simple-template-string tmpl)
          (with-current-buffer "*simple-template-output-buffer test*"
            (goto-char (point-min))
            (i/goto-pointmark-and-delete)
            (prog1 (first (get-char-property (point) 'button))
              (kill-buffer "*simple-template-output-buffer test*")))))
      )))

;;;; i/try-these
(defmacro i/try-these (&rest forms)
  (declare (debug (&rest form)))
  `(or ,@(loop for form in forms
               collect `(ignore-errors ,form))))

(defmacro i/define-toggle-command (name var)
  `(defun ,name ()
     (interactive)
     (setq ,var
           (not ,var))
     (when (interactive-p)
       (message "%s: %s" (symbol-name ',var) ,var))))


;;;; Utils
(defun i/replace-region-aux (start end cb)
  (let* ((str (buffer-substring-no-properties start end))
         (rep-str (funcall cb str)))
    (when rep-str
      (delete-region start end)
      (insert rep-str))))

(defun i/empty-string-p (str)
  (or (string-equal "" str)
      (string-match (rx bos (+ space) eos)
                str)))

(defun i/rt (text face)
  "Put a face to the given text."
  (unless (stringp text) (setq text (format "%s" (or text ""))))
  (put-text-property 0 (length text) 'face face text)
  (put-text-property 0 (length text) 'font-lock-face face text)
  text)

(defsubst i/pput (text prop val)
  "Put a face to the given text.
v:0.04"
  (unless (stringp text) (setq text (format "%s" (or text ""))))
  (put-text-property 0 (length text) prop val text)
  text)

(defsubst* i/make-list (a)
  (if (listp a) a (list a)))

(defsubst* i/some (pred xs)
  (loop for x in xs
        thereis (funcall pred x)))

(defsubst* i/any-match (regexp-or-regexps str)
  (i/awhen (i/make-list regexp-or-regexps)
    (i/some (i/fn (string-match _ str)) it)))
(defalias 'i/some-match 'i/any-match)


(defsubst* i/every (pred xs)
  (loop for x in xs
        always (funcall pred x)))

(defsubst i/plist-delete (plist prop)
  "Delete property PROP from property list PLIST by side effect.
This modifies PLIST."
  ;; deal with prop at the start
  (while (eq (car plist) prop)
    (setq plist (cddr plist)))
  ;; deal with empty plist
  (when plist
    (let ((lastcell (cdr plist))
          (l (cddr plist)))
      (while l
        (if (eq (car l) prop)
            (progn
              (setq l (cddr l))
              (setcdr lastcell l))
          (setq lastcell (cdr l)
                l (cddr l))))))
  plist)

(defsubst i/nonempty-string-p (string)
  (and (stringp string) (not (string= string ""))))

(defsubst* i/acdrs (keys alist)
  (mapcar (i/fn (_) (i/acdr _ alist)) keys))

(defsubst* i/assocdrs (keys alist)
  (mapcar (i/fn (_) (assoc-default _ alist)) keys))

(defun i/sort-by-regexp (re los)
  (loop for s in los
        with matched
        with unmatched
        if (string-match re s)
        do (push s matched)
        else
        do (push s unmatched)
        finally return (nconc (nreverse matched)
                              (nreverse unmatched))))


(defsubst* i/avalue-bind-aux (clauses)
  (loop with bind-keys
        with alist-keys
        for c in clauses
        collect (etypecase c
                  (symbol c)
                  (cons (first c)))
        into bind-keys
        collect (etypecase c
                  (symbol c)
                  (cons (second c)))
        into alist-keys
        finally return (list bind-keys alist-keys)))

(defmacro i/avalue-bind (clause-keys alist &rest body)
  (declare
   (debug ((&rest &or
                  symbolp
                  (symbolp sexp))
           form body))
   (indent 2))
  (dbind (bind-keys alist-keys) (i/avalue-bind-aux clause-keys)
    `(dbind ,bind-keys (i/acdrs ',alist-keys ,alist)
       ,@body)))

(defsubst* i/assoc-value-bind-aux (clauses)
  (loop with bind-keys
        with alist-keys
        for c in clauses
        collect (etypecase c
                  (string (intern c))
                  (cons (first c)))
        into bind-keys
        collect (etypecase c
                  (string c)
                  (cons (second c)))
        into alist-keys
        finally return (list bind-keys alist-keys)))

(defmacro i/assoc-value-bind (clause-keys alist &rest body)
  (declare
   (debug ((&rest &or
                  stringp
                  (symbolp sexp))
           form body))
   (indent 2))
  (dbind (bind-keys alist-keys) (i/assoc-value-bind-aux clause-keys)
    `(dbind ,bind-keys (i/assoc-cdrs ',alist-keys ,alist)
       ,@body)))

(defun* i/positions->string (start end &key (buffer (current-buffer)))
  (with-current-buffer buffer
    (i/aand (buffer-substring-no-properties
           start end))))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (expect '("bob" "pswd")
        (i/awhen '((pass . "pswd")
                 (name . "bob"))
          (i/avalue-bind (name pass) it
            (list name pass)))
        )
      (expect '("bob" "pswd")
        (i/awhen '((pass . "pswd")
                 (name . "bob"))
          (i/avalue-bind ((__name name) (__pass pass)) it
            (list __name __pass)))
        )

      (expect '("bob" "pswd")
        (i/awhen '(("pass" . "pswd")
                   ("name" . "bob"))
          (i/assoc-value-bind ("name" "pass") it
            (list name pass)))
        )
      (expect '("bob" "pswd")
        (i/awhen '(("pass" . "pswd")
                   ("name" . "bob"))
          (i/assoc-value-bind ((__name "name") (__pass "pass")) it
            (list __name __pass)))
        ))))

(defun* i/cycle-list-gen
  (&key get-list-fn
        (cb 'identity)
        (cache t))
  (i/with-lexical-bindings (get-list-fn cb cache)
    (lexical-let* ((lst (cond
                         (cache (funcall get-list-fn))
                         (t nil)))
                   (c -1))
      (i/fn ()
        (and (not cache)
             (setq lst (funcall get-list-fn)))
        (and (>= (incf c) (length lst))
             (setq c 0))
        (i/aand (nth c lst)
              (funcall cb it))))))

(defmacro i/define-cycle-list-function (name &rest args)
  (declare (debug t))
  `(progn
     (defvar ,name nil)
     (setq ,name (i/cycle-list-gen ,@args))
     (defun ,name ()
       (funcall ,name))))

(defsubst i/fast-pp-alist (alist)
  (when alist
    ;(assert (and (consp alist) (consp (car alist))))
    (i/with-point-buffer (format "%S" alist)
      (down-list)
      (loop while (ignore-errors (forward-sexp)
                                 t)
            do (newline))
      (buffer-string))))






(defvar i/enable-font-lock nil)
(defun i/cl-function-syms ()
  (loop for sym being the symbols
        when (i/aand (first (plist-get (symbol-plist sym)
                                     'autoload))
                   (i/=~ (rx bol "cl" (or eol "-")
                           ) it))
        collect sym))


(defmacro* i/remove-if-any-match (re seqs &rest args)
  `(i/remif (i/fn (i/any-match ,re _)) ,seqs ,@args))


;; Todo
(defun i/remove-if-some-match (re seqs)
  (i/remove-if (lambda (s) (i/any-match re s)) seqs))

(defmacro* i/remove-if-not-any-match (re seqs &rest args)
  `(i/!remif (i/fn (i/any-match ,re _)) ,seqs ,@args))

; Todo
(defun i/remove-if-not-some-match (re seqs)
  (i/remove-if-not (lambda (s) (i/any-match re s)) seqs))

(defsubst* i/trim (s)
  "Remove whitespace at beginning and end of string."
  (i/aand (or (i/=~ "\\`[ \t\n\r]+" s
                ($sub "" 0 :fixedcase t :literal-string t))
              s)
          (or (i/=~ "[ \t\n\r]+\\'" it
                ($sub "" 0 :fixedcase t :literal-string t))
              it)
          it))

(defsubst* i/insert-each-line (los)
  (insert (mapconcat 'identity los "\n")))

(defun* i/directory-files-recursively
    (regexp &optional directory type (deep nil) &key (internal-current-deep 0))
  (flet ((any-match
          (regexp-or-regexps file-name)
          (when regexp-or-regexps
            (let ((regexps (if (listp regexp-or-regexps) regexp-or-regexps (list regexp-or-regexps))))
              (i/some
               (lambda (re)
                 (string-match re file-name))
               regexps))))
         (check-deep-p
          ()
          (and deep
               (> internal-current-deep deep))))
    (unless (check-deep-p)
      (let* ((directory (or directory default-directory))
             (predfunc (case type
                         ((d dir directory) 'file-directory-p)
                         ((f file) 'file-regular-p)
                         (otherwise 'identity)))
             (files (i/remove-if
                     (lambda (s)
                       (string-match (rx bol ".")
                                     (file-name-nondirectory s)))
                     (directory-files directory t nil t)))
             (regexps (if (listp regexp) regexp (list regexp))))
        (loop for file in files
              when (and (funcall predfunc file)
                        (any-match regexps (file-name-nondirectory file)))
              collect file into ret
              when (file-directory-p file)
              nconc (i/directory-files-recursively regexp file type deep
                                                   :internal-current-deep (1+ internal-current-deep)) into ret
              finally return ret)))))

(defun i/call-process-to-string (program &rest args)
  (with-temp-buffer
    (print args (create-file-buffer "i/call-process-to-string args"))
    (apply 'call-process program nil t nil args)
    (buffer-string)))

;;;; Http Utils
(eval-when-compile
  (require 'url-util))

(defsubst* i/http-hexify-string (string &optional (encoding 'utf-8))
  (mapconcat
   (lambda (byte)
     (if (memq byte url-unreserved-chars)
         (char-to-string byte)
       (format "%%%02x" byte)))
   (if (multibyte-string-p string)
       (encode-coding-string string encoding)
     string)
   ""))

(defsubst* i/http-hexify-region (s e)
  (interactive "r")
  (i/replace-region-aux
   s e
   (lambda (str) 
     (i/http-hexify-string str))))

(defsubst* i/escape-html-region (s e)
  (interactive "r")
  (i/replace-region-aux
   s e
   (lambda (str) 
     (i/escape-html str))))

(defun* i/http-build-query (params &optional (encoding 'utf-8))
  (let ((ret (loop for (k . v) in params
                   for k = (etypecase k
                             (symbol (symbol-name k))
                             (string k))
                   for v = (cond
                            ((stringp v) v)
                            ((and (listp v) (= 1 (length v)))
                             (car v))
                            (t v))
                   nconc
                   (etypecase v
                     (list (loop for v in v
                                 collect (mapconcat 'i/http-hexify-string (list k v) "=")))
                     (string (list (mapconcat 'i/http-hexify-string (list k v) "="))))
                   )))
    (mapconcat 'identity ret "&")))
(defvar i/escape-html-table
  '(
    ("&" . "&amp;")
    (">" . "&gt;")
    ("<" . "&lt;")
    ("\"" . "&quot;")
    ("'" . "&#39;")
    ))

(defvar i/escape-html-re
  (rx (group (or "&" ">" "<" "\"" "'"))))


(defun i/escape-html (str)
  (i/with-point-buffer str
    (goto-char (point-min))
    (loop while (re-search-forward i/escape-html-re nil t)
          do (replace-match (assoc-default (match-string 1) i/escape-html-table))
          finally return (buffer-substring-no-properties (point-min) (point-max)))))

;; (i/escape-html "><&&'=\"&&")
;;;; Buffer Utils
(defsubst* i/collect-matches
  (re &optional (count 0) (match-string-fn 'match-string)
      (point-min (point-min)) (point-max (point-max)))
  (save-excursion
    (loop initially (goto-char point-min)
          while (re-search-forward re point-max t)
          collect (funcall match-string-fn count))))

;;;; Mode
(defun i/set-minor-mode-mode-line (minor-mode str)
  (when (and (boundp 'minor-mode)
             (assq 'minor-mode minor-mode-alist))
    (setcar (cdr (assq 'minor-mode minor-mode-alist)) str)))

;;;; XXX
(defsubst* i/get-buffers-by-regexp (regexp)
  (loop for b being the buffers
        for bn = (buffer-name b)
        when (i/any-match regexp bn)
        collect b))

;;;; Utility functions
(defun i/remove-rassoc (value alist)
  "Delete by side effect any elements of ALIST whose cdr is `equal' to VALUE.
The modified ALIST is returned.  If the first member of ALIST has a car
that is `equal' to VALUE, there is no way to remove it by side effect;
therefore, write `(setq foo (remrassoc value foo))' to be sure of changing
the value of `foo'."
  (while (and (consp alist)
              (or (not (consp (car alist)))
                  (equal (cdr (car alist)) value)))
    (setq alist (cdr alist)))
  (if (consp alist)
      (let ((prev alist)
            (tail (cdr alist)))
        (while (consp tail)
          (if (and (consp (car tail))
                   (equal (cdr (car tail)) value))
              ;; `(setcdr CELL NEWCDR)' returns NEWCDR.
              (setq tail (setcdr prev (cdr tail)))
            (setq prev (cdr prev)
                  tail (cdr tail))))))
  alist)

(defun* i/assoc-remove-all (elem alist &key (test (lambda (elem _) (equal elem _))))
  (loop for dlst in alist
        for (key . val) = dlst
        unless (funcall test elem key)
        collect dlst))

(defun* i/assoc-remove-keys (elems alist &key (test (lambda (elem _) (equal elem _))))
  (loop for dlst in alist
        for (key . val) = dlst
        unless (member key elems)
        collect dlst))

(defun* i/assoc-remove-regexp (regexp alist)
  (i/assoc-remove-all regexp alist
                      :test (lambda (re elem)
                              (i/any-match (i/make-list re) elem))))
(defun* i/assq-remove-all (key alist)
  (i/assoc-remove-all key alist
                      :test (lambda (key elem)
                              (eq key elem))))
;;;; Time Utils

;;;; Markdown
(defun i/markdown->html (str-or-buffer)
  (let ((cmd "Markdown.pl"))
    (assert (executable-find cmd) t "Cant find %s. Please install Text::Markdown")
    (let ((s (etypecase str-or-buffer
               (string str-or-buffer)
               (buffer (with-current-buffer str-or-buffer
                         (buffer-string))))))
      (i/do-temp-file (tempfile s)
        (i/call-process-to-string cmd
                                  tempfile)))))

(defun i/markdown->html-region (s e)
  (interactive "r")
  (let ((cmd "Markdown.pl"))
    (assert (executable-find cmd) t "Cant find %s. Please install Text::Markdown")
    (i/do-temp-file (tempfile (buffer-substring s e))
      (i/call-process-to-string cmd
                                tempfile))))


(defvar i/markdown-extra-wrapper-program
  "\
\<?php
set_include_path(get_include_path().PATH_SEPARATOR.\"/Applications\");
include_once \"markdown.php\";
$input=($argc==1)? 'php://stdin': $argv[1];
$markdown=file_get_contents($input);
$html=Markdown($markdown);
echo($html);
?>
")
(defvar i/markdown-extra-path nil)

(defun i/markdown-extra->html (str-or-buffer)
  (let ((s (etypecase str-or-buffer
             (string str-or-buffer)
             (buffer (with-current-buffer str-or-buffer
                       (buffer-string))))))
    (i/do-temp-file (wrapper-cmd i/markdown-extra-wrapper-program)
      (i/do-temp-file (tempfile s)
        (let ((default-directory i/markdown-extra-path))
          (i/call-process-to-string "php"
                                    wrapper-cmd
                                    tempfile))))))



(defun i/markdown-extra->html-region (s e)
  (interactive "r")
  (i/markdown-extra->html (buffer-substring s e)))


(defun i/pandoc-markdown-extra->html (str-or-buffer)
  (let ((s (etypecase str-or-buffer
             (string str-or-buffer)
             (buffer (with-current-buffer str-or-buffer
                       (buffer-string))))))
    (i/do-temp-file (wrapper-cmd i/markdown-extra-wrapper-program)
      (i/do-temp-file (tempfile s)
        (let ((default-directory i/markdown-extra-path))
          (i/call-process-to-string "pandoc"
                                    "-f"
                                    "markdown_phpextra"
                                    "-t"
                                    "html"
                                    tempfile))))))
(defun i/pandoc-markdown->html-region (s e)
  (interactive "r")
  (let ((str (i/pandoc-markdown-extra->html (buffer-substring s e))))
    (delete-region s e)
    (insert str)))


(defun i/pandoc-html->markdown (str-or-buffer)
  (let ((s (etypecase str-or-buffer
             (string str-or-buffer)
             (buffer (with-current-buffer str-or-buffer
                       (buffer-string))))))
      (i/do-temp-file (tempfile s)
        (let ((default-directory i/markdown-extra-path))
          (i/call-process-to-string "pandoc"
                                    "-f"
                                    "html"
                                    "-t"
                                    "markdown_phpextra"
                                    tempfile)))))

(defun i/pandoc-html->markdown-region (s e)
  (interactive "r")
  (let ((str (i/pandoc-html->markdown (buffer-substring s e))))
    (delete-region s e)
    (insert str)))



;;;; Datetime
(defstruct (i/$datetime
            (:constructor nil)
            (:constructor i/--make-$datetime)
            (:copier i/$datetime-clone))
  time)

(defsubst* i/make-$datetime
  (&key
   year
   (month 1)
   (day 1)
   (hour 0)
   (minute 0)
   (second 0))
  (assert (integerp year) t "\
Mandatory parameter :year missing in call to `i/make-$datetime'. year is %s")
  (check-type month (integer 1 12))
  (check-type day (integer 1 31))
  (check-type hour (integer 0 23))
  (check-type minute (integer 0 59))
  (check-type second (integer 0 61))
  (let* ((date (list second minute hour day month year))
         (time (apply 'encode-time date)))
    (i/--make-$datetime
     :time time)))

(defun i/$datetime-now ()
  (i/--make-$datetime
   :time (current-time)))

(defsubst* i/$datetime-from-time (time)
  (assert (numberp (first time)))
  (i/--make-$datetime
   :time time))

(defsubst* i/$datetime-from-epoch (time)
  (i/$datetime-from-time
   (seconds-to-time time)))

(defsubst i/pretty-$datetime ($datetime)
  (check-type $datetime i/$datetime)
  (format-time-string
   "%Y年%m月%d日%H時%M分" (i/$datetime-time $datetime)))

(eval-when-compile
  (defvar i/--$datetime-generate-simple-getters-map
    '(
      (year . "%Y")
      (month . "%m")
      ((day day-of-month mday) . "%d")
      ((day-of-week dow wday) . "%w")
      (hour . "%H")
      (minute . "%M")
      (second . "%S")
      ((day-of-year doy) . "%j")
      ))
  (defmacro i/--$datetime-generate-simple-getters ()
    (let ((flatten-map
           (loop for (name . fm-str) in i/--$datetime-generate-simple-getters-map
                 if (consp name)
                 append (loop for n in name
                              collect (cons n fm-str))
                 else
                 collect (cons name fm-str))))
      `(progn
         ,@(loop for (fn-name . fm-str) in flatten-map
                 for full-fn-name-sym = (intern (format "i/$datetime-%s" fn-name))
                 collect `(defsubst* ,full-fn-name-sym ($datetime)
                            (check-type $datetime i/$datetime)
                            (string-to-number
                             (format-time-string ,fm-str
                                                 (i/$datetime-time $datetime)))))))))
;; define i/$datetime-year, i/$datetime-hour, etc...
(i/--$datetime-generate-simple-getters)

(defsubst* i/$datetime-ymd ($datetime &optional (separator "/"))
  (check-type $datetime i/$datetime)
  (check-type separator string)
  (mapconcat 'identity
             (mapcar 'number-to-string
                     (list (i/$datetime-year $datetime)
                           (i/$datetime-month $datetime)
                           (i/$datetime-day $datetime)))
             separator))

(defsubst* i/$datetime-leap-year-p ($datetime)
  (check-type $datetime i/$datetime)
  (let ((year (i/$datetime-year $datetime)))
    (or (and (zerop (% year 4))
             (not (zerop (% year 100))))
        (zerop (% year 400)))))
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (expect t
        (i/$datetime-leap-year-p
         (i/make-$datetime :year 2000)))
      (expect nil
        (i/$datetime-leap-year-p
         (i/make-$datetime :year 2001)))
      )))

;;;; add, subtract
(defsubst* i/$datetime-add
  ($datetime
   &key
   year
   month
   day
   hour
   minute
   second)
  (check-type $datetime i/$datetime)
  (let* ((time (i/$datetime-time $datetime))
         (date (decode-time time)))
    (and second
         (setf (nth 0 date)
               (+ (nth 0 date) second)))
    (and minute
         (setf (nth 1 date)
               (+ (nth 1 date) minute)))
    (and hour
         (setf (nth 2 date)
               (+ (nth 2 date) hour)))
    (and day
         (setf (nth 3 date)
               (+ (nth 3 date) day)))
    (and month
         (setf (nth 4 date)
               (+ (nth 4 date) month)))
    (and year
         (setf (nth 5 date)
               (+ (nth 5 date) year)))
    (setf (i/$datetime-time $datetime)
          (apply 'encode-time date))
    $datetime))

(defsubst* i/$datetime-subtract-keys
  ($datetime
   &key
   year
   month
   day
   hour
   minute
   second)
  (check-type $datetime i/$datetime)
  (let* ((time (i/$datetime-time $datetime))
         (date (decode-time time)))
    (and second
         (setf (nth 0 date)
               (- (nth 0 date) second)))
    (and minute
         (setf (nth 1 date)
               (- (nth 1 date) minute)))
    (and hour
         (setf (nth 2 date)
               (- (nth 2 date) hour)))
    (and day
         (setf (nth 3 date)
               (- (nth 3 date) day)))
    (and month
         (setf (nth 4 date)
               (- (nth 4 date) month)))
    (and year
         (setf (nth 5 date)
               (- (nth 5 date) year)))
    (setf (i/$datetime-time $datetime)
          (apply 'encode-time date))
    $datetime))

(defstruct (i/$duration
            (:constructor nil)
            (:constructor i/--make-$duration))
  difference-float)

(defsubst* i/$datetime-subtract-op ($datetime1 $datetime2)
  (check-type $datetime1 i/$datetime)
  (check-type $datetime2 i/$datetime)
  (i/--make-$duration
   :difference-float
   (- (float-time (i/$datetime-time $datetime1))
      (float-time (i/$datetime-time $datetime2)))))

(defun i/$datetime-subtract (&rest args)
  (cond
   ((i/$datetime-p (second args))
    (i/$datetime-subtract-op (first args) (second args)))
   (t
    (apply 'i/$datetime-subtract-keys args))))

(defsubst* i/$datetime-compare-aux (op $datetime1 $datetime2)
  (check-type $datetime1 i/$datetime)
  (check-type $datetime2 i/$datetime)
  (funcall op
           (float-time (i/$datetime-time $datetime1))
           (float-time (i/$datetime-time $datetime2))))
(defsubst* i/$datetime-< ($d1 $d2)
  (i/$datetime-compare-aux '< $d1 $d2))
(defsubst* i/$datetime-> ($d1 $d2)
  (i/$datetime-compare-aux '> $d1 $d2))

(defsubst* i/$duration-pretty-string-aux (diff-seconds)
  (cond
   ((< diff-seconds 60)
    (values 'second (round diff-seconds)))
   ((< diff-seconds 3600)
    (values 'minute (round (/ diff-seconds 60))))
   ((< diff-seconds (+ 86400 3600))
    (values 'hour (round (/ diff-seconds 3600))))
   ((< diff-seconds (+ 259200 86400))
    (values 'day (round (/ diff-seconds 86400))))
   (t
    nil)))

(defsubst* i/$duration-pretty-string-japanese ($duration)
  (check-type $duration i/$duration)
  (i/acond
    ((i/$duration-pretty-string-aux (i/$duration-difference-float
                                        $duration))
     (dbind (tag val) it
       (ecase tag
         (second (format "%s秒" val))
         (minute (format "%s分" val))
         (hour (format "%s時間" val))
         (day (format "%s日" val)))))
    (t (format "%s日" (round
                       (/ (i/$duration-difference-float
                           $duration)
                          86400))))))

;;;; XXX
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/$datetime-< +++++")
      (expect nil
        (i/$datetime-<
         (i/make-$datetime :year 2012
                              :month 7
                              :day 14)
         (i/make-$datetime :year 2012
                              :month 7
                              :day 12)))

      (desc "+++++ i/$datetime-> +++++")
      (expect t
        (i/$datetime->
         (i/make-$datetime :year 2012
                              :month 7
                              :day 14)
         (i/make-$datetime :year 2012
                              :month 7
                              :day 12)))

      (desc "+++++ i/$datetime-subtract +++++")
      (expect "2日"
        (i/$duration-pretty-string-japanese
         (i/$datetime-subtract
          (i/make-$datetime :year 2012
                               :month 7
                               :day 14)
          (i/make-$datetime :year 2012
                               :month 7
                               :day 12))))
      (expect "33日"
        (i/$duration-pretty-string-japanese
         (i/$datetime-subtract
          (i/make-$datetime :year 2012
                               :month 8
                               :day 14)
          (i/make-$datetime :year 2012
                               :month 7
                               :day 12))))
      (expect "2時間"
        (i/$duration-pretty-string-japanese
         (i/$datetime-subtract
          (i/make-$datetime :year 2012
                               :month 7
                               :day 14
                               :hour 18
                               :minute 0)
          (i/make-$datetime :year 2012
                               :month 7
                               :day 14
                               :hour 16
                               :minute 30))))
      (expect "30分"
        (i/$duration-pretty-string-japanese
         (i/$datetime-subtract
          (i/make-$datetime :year 2012
                               :month 7
                               :day 14
                               :hour 18
                               :minute 0)
          (i/make-$datetime :year 2012
                               :month 7
                               :day 14
                               :hour 17
                               :minute 30))))
      (expect "30秒"
        (i/$duration-pretty-string-japanese
         (i/$datetime-subtract
          (i/make-$datetime :year 2012
                               :month 7
                               :day 14
                               :hour 18
                               :minute 0)
          (i/make-$datetime :year 2012
                               :month 7
                               :day 14
                               :hour 17
                               :minute 59
                               :second 30))))
      )))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/$datetime-add +++++")
      (expect "2020年05月12日01時53分"
        (with-stub
          (stub current-time => '(19436 11933 218000))
          (let ((now (i/$datetime-now)))
            (i/$datetime-add now
                                :year 10)
            (i/$datetime-subtract now
                                     :day 2)
            (i/pretty-$datetime now))))

      (expect "2010年05月15日01時53分"
        (with-stub
          (stub current-time => '(19436 11933 218000))
          (let ((now (i/$datetime-now)))
            (i/pretty-$datetime now) ;;"2010年05月14日01時53分"
            (i/$datetime-add now
                                :day 1)
            (i/pretty-$datetime now)))
        )

      (desc "+++++ i/$datetime-ymd +++++")
      (expect "2010/5/14"
        (with-stub
          (stub current-time => '(19436 11933 218000))
          (let ((now (i/$datetime-now)))
            (i/$datetime-ymd now))))
      (expect "2010+5+14"
        (with-stub
          (stub current-time => '(19436 11933 218000))
          (let ((now (i/$datetime-now)))
            (i/$datetime-ymd now "+"))))

      )))






;;;; Datetime Test.
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/make-$datetime +++++")
      (expect (error)
        (i/make-$datetime))

      (desc "+++++ i/$datetime-now +++++")
      (expect t
        (i/$datetime-p (i/$datetime-now)))

      (desc "+++++ i/$datetime-year +++++")
      (expect (error)
        (i/$datetime-year
         'not-$datetime))
      (expect 2000
        (i/$datetime-year
         (i/make-$datetime :year 2000)))

      (desc "+++++ i/$datetime-month +++++")
      (expect 12
        (i/$datetime-month
         (i/make-$datetime :year 2000
                              :month 12)))

      (desc "+++++ i/$datetime-day +++++")
      (expect 30
        (i/$datetime-day
         (i/make-$datetime :year 2000
                              :month 12
                              :day 30)))
      (expect 30
        (i/$datetime-day-of-month
         (i/make-$datetime :year 2000
                              :month 12
                              :day 30)))
      (expect 30
        (i/$datetime-mday
         (i/make-$datetime :year 2000
                              :month 12
                              :day 30)))

      (desc "+++++ i/$datetime-day-of-week +++++")
      (desc "Note, Monday is 1")
      (expect 1
        (i/$datetime-day-of-week
         (i/make-$datetime :year 2010
                              :month 5
                              :day 10)))

      (desc "+++++ i/$datetime-hour +++++")
      (expect 9
        (i/$datetime-hour
         (i/make-$datetime :year 2010
                              :month 5
                              :day 10
                              :hour 9)))

      (desc "+++++ i/$datetime-minute +++++")
      (expect 7
        (i/$datetime-minute
         (i/make-$datetime :year 2010
                              :month 5
                              :day 10
                              :hour 9
                              :minute 7)))

      (desc "+++++ i/$datetime-second +++++")
      (expect 59
        (i/$datetime-second
         (i/make-$datetime :year 2010
                              :month 5
                              :day 10
                              :hour 9
                              :minute 7
                              :second 59)))

      (desc "+++++ i/$datetime-day-of-year +++++")
      (expect 365
        (i/$datetime-day-of-year
         (i/make-$datetime :year 2010
                              :month 12
                              :day 31)))
      (desc "leap year")
      (expect 366
        (i/$datetime-day-of-year
         (i/make-$datetime :year 2000
                              :month 12
                              :day 31)))

      (desc "+++++ i/pretty-$datetime +++++")
      (expect (error)
        (i/pretty-$datetime nil))
      (expect "2000年01月01日00時00分"
        (i/pretty-$datetime
         (i/make-$datetime :year 2000)))
      )))

(defsubst* i/days-ago-time (n time)
  (check-type n integer)
  (let ((date (decode-time time)))
    (setf (nth 3 date)
          (- (nth 3 date) n))
    (apply 'encode-time date)))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ Datetime +++++")
      (let* ((test-time '(19423 243 265000))
             (7-days-ago-time (i/days-ago-time 7 test-time))
             (6-days-ago-time '(19415 6131))
             (8-days-ago-time '(19412 29939)))
        (assert (equal "05/04/10" (format-time-string "%D" test-time)))
        (assert (equal "04/27/10" (format-time-string "%D" 7-days-ago-time)))
        (assert
         (eq t (time-less-p 7-days-ago-time 6-days-ago-time)))
        (assert
         (eq nil (time-less-p 7-days-ago-time 8-days-ago-time))))
      )))

(defun i/pretty-time-japanese (time)
  (assert (numberp (first time)))
  (format-time-string
   "%Y年%m月%d日%H時%M分" time))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/pretty-time-japanese +++++")
      (with-stub
        (stub current-time => '(19435 42791 41987))
        (i/pretty-time-japanese (current-time)))
      )))


;;;; symlink
(defun* i/get-symlink-target-recursively (f &optional (deep-limit 10))
  (unless (= deep-limit 0)
    (i/aif (file-symlink-p f)
        (i/get-symlink-target-recursively it (1- deep-limit))
      f)))

(defun i/normalize-file-name (path)
  (i/aand (expand-file-name path)
        (if (file-directory-p path)
            (concat (i/=~ "/*$" it ($sub ""))
                    "/")
          (i/=~ "/*$" it ($sub "")))))

;;;; Font lock support
(defgroup imakado nil
  "imakado.el"
  :prefix "i/"
  :group 'convenience)
(defface i/keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for macros are defined in imakado.el"
  :group 'imakado)

(defvar i/keyword-face 'i/keyword-face
  "Face name to use for function names.")

(defun i/font-lock-keyword-matcher2 (limit)
  (when i/enable-font-lock
    (let ((re (rx "("
                  (group (or "i/defcacheable"
                             "i/define-define-keymap" 
                             "i/define-with-all-slots-struct"
                             "i/define-with-struct-macro"
                             "i/define-macro-aliases"
                             "i/define-macro-alias"
                             "i/defclass+"
                             "i/define-defmethod"
                             "i/define-cycle-list-function"
                             ))
                  (+ space)
                  (group (+ (or (syntax symbol) (syntax word)))))))
      (when (re-search-forward re limit t)
        (set-match-data (match-data))
        t))))


(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '((i/font-lock-keyword-matcher1
      (1 i/keyword-face append))
     (i/font-lock-keyword-matcher2
      (1 i/keyword-face append)
      (2 font-lock-function-name-face append)))))

;;;; Aliases

(defun i/font-lock-keywords-1 ()
  (list
   "imakado-require-version"
   "i/gensym"
   "i/group"
   "i/allquote"
   "i/in-aux-test"
   "i/flatten"
   "i/subseq"
   "i/remove-if"
   "i/remove-if-not"
   "i/acdr"
   "acdr"
   "i/assq-rest"
   "i/with-gensyms"
   "i/with-lexical-bindings"
   "i/dirconcat"
   "i/remif-aux"
   "i/remif"
   "i/!remif"
   "i/aif"
   "i/awhen"
   "i/awhile"
   "i/aand"
   "i/acond"
   "i/alambda"
   "i/cond-let-aux-vars"
   "i/cond-let-aux-binds"
   "i/cond-let-aux-clause"
   "i/cond-let"
   "i/when-let"
   "i/fn-aux-anaph-arg-syms"
   "i/fn-aux-appear_?"
   "i/fn-aux"
   "i/fn"
   "i/define-macro-alias"
   "i/define-macro-aliases"
   "i/cars"
   "i/cdrs"
   "i/cadrs"
   "i/assoc-cdrs"
   "i/nths"
   "i/in"
   "i/inq"
   "i/in="
   "i/join+"
   "i/case-cond-clause-aux"
   "i/xcase"
   "i/xcase-clause-aux-test"
   "i/xcase-clause-aux"
   "i/xcase="
   "i/with-struct"
   "i/define-with-struct-macro"
   "i/with-struct-all-slots-get-getters-slot?"
   "i/with-struct-all-slots-get-all-slots"
   "i/define-with-all-slots-struct"
   "i/dolist-with-progress-reporter"
   "i/with-anaphoric-match-utilities"
   "i/=~"
   "i/case-regexp-aux"
   "i/case-regexp"
   "i/match-with-temp-buffer"
   "i/with-temp-buffer-file"
   "i/match-with-temp-buffer-file"
   "i/save-excursion-force"
   "i/goto-pointmark-and-delete"
   "i/with-point-buffer"
   "i/memoize"
   "i/--test-memoize-fn"
   "i/do-temp-file"
   "i/for-each-single-property-change"
   "i/require-methods-aux"
   "i/require-methods"
   "i/with-orefs"
   "i/require-slots"
   "i/simple-template-parse-buffer-get-token-and-advance"
   "i/simple-template-parse-buffer"
   "i/simple-template-compile-aux"
   "i/simple-template-compile"
   "i/simple-template-buffer"
   "i/simple-template"
   "i/simple-template-string"
   "i/try-these"
   "i/rt"
   "i/make-list"
   "i/some"
   "i/any-match"
   "i/every"
   "i/collect-matches"
   "i/get-buffers-by-regexp"
   "i/font-lock-keyword-matcher1"
   "i/font-lock-keyword-matcher2"
   "i/gensym"
   "i/group"
   "i/allquote"
   "i/flatten"
   "i/subseq"
   "i/remove-if"
   "i/remove-if-not"
   "i/acdr"
   "i/remif-aux"
   "i/goto-pointmark-and-delete"
   "i/simple-template-buffer"
   "i/rt"
   "i/make-list"
   "i/some"
   "i/any-match"
   "i/every"
   "i/collect-matches"
   "i/get-buffers-by-regexp"
   "i/imakado-require-version"
   "i/acdr"
   "i/with-gensyms"
   "i/with-lexical-bindings"
   "i/dirconcat"
   "i/remif"
   "i/!remif"
   "i/aif"
   "i/awhen"
   "i/awhile"
   "i/aand"
   "i/acond"
   "i/alambda"
   "i/cond-let"
   "i/when-let"
   "i/fn"
   "i/cars"
   "i/cdrs"
   "i/cadrs"
   "i/assoc-cdrs"
   "i/nths"
   "i/in"
   "i/inq"
   "i/in="
   "i/join+"
   "i/xcase"
   "i/xcase="
   "i/with-struct"
   "i/dolist-with-progress-reporter"
   "i/with-anaphoric-match-utilities"
   "i/=~"
   "i/case-regexp"
   "i/match-with-temp-buffer"
   "i/with-temp-buffer-file"
   "i/match-with-temp-buffer-file"
   "i/save-excursion-force"
   "i/with-point-buffer"
   "i/memoize"
   "i/do-temp-file"
   "i/for-each-single-property-change"
   "i/define-defmethod"
   "i/defclass+"
   "i/require-methods"
   "i/with-orefs"
   "i/require-slots"
   "i/simple-template"
   "i/simple-template-string"
   "i/try-these"
   "i/font-lock-keywords-1"
   "i/test-macro"
   ))

(i/defcacheable i/font-lock-keyword-matcher1-re ()
    (rx-to-string
     `(and "("
           (group "i/"
                  (+ (or (syntax word)
                         (syntax symbol))))
           symbol-end)))

(defun i/font-lock-keyword-matcher1 (limit)
  (when i/enable-font-lock
    (let ((re (i/font-lock-keyword-matcher1-re)))
      (when (re-search-forward re limit t)
        (set-match-data (match-data))
        t))))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (expect t
        (i/some 'evenp (number-sequence 0 10 2)))
      (expect nil
        (i/some 'oddp (number-sequence 0 10 2)))
      (expect t
        (i/every 'evenp (number-sequence 0 10 2)))
      (expect nil
        (i/every 'evenp '(2 4 5)))
      )))        
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "+++++ i/try-these +++++")
      (expect "ok"
        (i/try-these (error "aa")
                   (progn "ok")
                   (error "bb")))
      (expect "ok"
        (flet ((i/--try-these-test () (return nil)))
          (i/try-these (i/--try-these-test)
                     (progn "ok")
                     (error "bb"))))
      (expect nil
        (let ((called nil))
          (flet ((i/--try-these-test () (setq called t)))
            (i/try-these (error "aa")
                       (progn "ok")
                       (i/--try-these-test))
            called)))
      (expect t
        (let ((called nil))
          (flet ((i/--try-these-test () (setq called t)))
            (i/try-these (error "aa")
                       (progn (i/--try-these-test)
                              "ok")
                       "bb")
            called)))
      )))


;;;; singularize, Pluralize
(i/defcacheable i/plural-rules ()
  '(("atlas$" "atlases")
    ("beef$" "beefs")
    ("brother$" "brothers")
    ("child$" "children")
    ("corpus$" "corpuses")
    ("cows$" "cows")
    ("ganglion$" "ganglions")
    ("genie$" "genies")
    ("genus$" "genera")
    ("graffito$" "graffiti")
    ("hoof$" "hoofs")
    ("loaf$" "loaves")
    ("man$" "men")
    ("money$" "monies")
    ("mongoose$" "mongooses")
    ("move$" "moves")
    ("mythos$" "mythoi")
    ("numen$" "numina")
    ("occiput$" "occiputs")
    ("octopus$" "octopuses")
    ("opus$" "opuses")
    ("ox$" "oxen")
    ("penis$" "penises")
    ("person$" "people")
    ("sex$" "sexes")
    ("soliloquy$" "soliloquies")
    ("testis$" "testes")
    ("trilby$" "trilbys")
    ("turf$" "turfs")

    ("\\(.*[nrlm]ese\\)$" "\\1")
    ("\\(.*deer\\)$" "\\1")
    ("\\(.*fish\\)$" "\\1")
    ("\\(.*measles\\)$" "\\1")
    ("\\(.*ois\\)$" "\\1")
    ("\\(.*pox\\)$" "\\1")
    ("\\(.*sheep\\)$" "\\1")
    ("\\(Amoyese\\)$" "\\1")
    ("\\(bison\\)$" "\\1")
    ("\\(Borghese\\)$" "\\1")
    ("\\(bream\\)$" "\\1")
    ("\\(breeches\\)$" "\\1")
    ("\\(britches\\)$" "\\1")
    ("\\(buffalo\\)$" "\\1")
    ("\\(cantus\\)$" "\\1")
    ("\\(carp\\)$" "\\1")
    ("\\(chassis\\)$" "\\1")
    ("\\(clippers\\)$" "\\1")
    ("\\(cod\\)$" "\\1")
    ("\\(coitus\\)$" "\\1")
    ("\\(Congoese\\)$" "\\1")
    ("\\(contretemps\\)$" "\\1")
    ("\\(corps\\)$" "\\1")
    ("\\(debris\\)$" "\\1")
    ("\\(diabetes\\)$" "\\1")
    ("\\(djinn\\)$" "\\1")
    ("\\(eland\\)$" "\\1")
    ("\\(elk\\)$" "\\1")
    ("\\(equipment\\)$" "\\1")
    ("\\(Faroese\\)$" "\\1")
    ("\\(flounder\\)$" "\\1")
    ("\\(Foochowese\\)$" "\\1")
    ("\\(gallows\\)$" "\\1")
    ("\\(Genevese\\)$" "\\1")
    ("\\(Genoese\\)$" "\\1")
    ("\\(Gilbertese\\)$" "\\1")
    ("\\(graffiti\\)$" "\\1")
    ("\\(headquarters\\)$" "\\1")
    ("\\(herpes\\)$" "\\1")
    ("\\(hijinks\\)$" "\\1")
    ("\\(Hottentotese\\)$" "\\1")
    ("\\(information\\)$" "\\1")
    ("\\(innings\\)$" "\\1")
    ("\\(jackanapes\\)$" "\\1")
    ("\\(Kiplingese\\)$" "\\1")
    ("\\(Kongoese\\)$" "\\1")
    ("\\(Lucchese\\)$" "\\1")
    ("\\(mackerel\\)$" "\\1")
    ("\\(Maltese\\)$" "\\1")
    ("\\(media\\)$" "\\1")
    ("\\(mews\\)$" "\\1")
    ("\\(moose\\)$" "\\1")
    ("\\(mumps\\)$" "\\1")
    ("\\(Nankingese\\)$" "\\1")
    ("\\(news\\)$" "\\1")
    ("\\(nexus\\)$" "\\1")
    ("\\(Niasese\\)$" "\\1")
    ("\\(Pekingese\\)$" "\\1")
    ("\\(Piedmontese\\)$" "\\1")
    ("\\(pincers\\)$" "\\1")
    ("\\(Pistoiese\\)$" "\\1")
    ("\\(pliers\\)$" "\\1")
    ("\\(Portuguese\\)$" "\\1")
    ("\\(proceedings\\)$" "\\1")
    ("\\(rabies\\)$" "\\1")
    ("\\(rice\\)$" "\\1")
    ("\\(rhinoceros\\)$" "\\1")
    ("\\(salmon\\)$" "\\1")
    ("\\(Sarawakese\\)$" "\\1")
    ("\\(scissors\\)$" "\\1")
    ("\\(sea[- ]bass\\)$" "\\1")
    ("\\(series\\)$" "\\1")
    ("\\(Shavese\\)$" "\\1")
    ("\\(shears\\)$" "\\1")
    ("\\(siemens\\)$" "\\1")
    ("\\(species\\)$" "\\1")
    ("\\(swine\\)$" "\\1")
    ("\\(testes\\)$" "\\1")
    ("\\(trousers\\)$" "\\1")
    ("\\(trout\\)$" "\\1")
    ("\\(tuna\\)$" "\\1")
    ("\\(Vermontese\\)$" "\\1")
    ("\\(Wenchowese\\)$" "\\1")
    ("\\(whiting\\)$" "\\1")
    ("\\(wildebeest\\)$" "\\1")
    ("\\(Yengeese\\)$" "\\1")

    ("\\(s\\)tatus$" "statuses")
    ("\\(quiz\\)$" "quizzes")
    ("^\\(ox\\)$" "oxen")
    ("\\([m\\|l]\\)ouse$" "\\1ice")
    ("\\(matr\\|vert\\|ind\\)\\(ix\\|ex\\)$" "\\1ices")
    ("\\(x\\|ch\\|ss\\|sh\\)$" "\\1es")
    ("\\([^aeiouy]\\|qu\\)y$" "\\1ies")
    ("\\(hive\\)$" "hives")
    ("\\(\\([^f]\\)fe\\|\\([lr]\\)f\\)$" "\\1\\3ves")
    ("sis$" "ses")
    ("\\([ti]\\)um$" "\\1a")
    ("\\(p\\)erson$" "\\1eople")
    ("\\(m\\)an$" "\\1en")
    ("\\(c\\)hild$" "\\1hildren")
    ("\\(buffal\\|tomat\\)o$" "\\1\\2oes")
    ("\\(alumn\\|bacill\\|cact\\|foc\\|fung\\|nucle\\|radi\\|stimul\\|syllab\\|termin\\|vir\\)us$" "\\1")
    ("us$" "uses")
    ("\\(alias\\)$" "\\1es")
    ("\\(ax\\|cri\\|test\\)is$" "\\1es")
    ("s$" "s")
    ("$" "s"))
  )

(i/defcacheable i/singular-rules ()
  '(("atlases$" "atlas")
    ("beefs$" "beef")
    ("brothers$" "brother")
    ("children$" "child")
    ("corpuses$" "corpus")
    ("cows$" "cow")
    ("ganglions$" "ganglion")
    ("genies$" "genie")
    ("genera$" "genus")
    ("graffiti$" "graffito")
    ("hoofs$" "hoof")
    ("loaves$" "loaf")
    ("men$" "man")
    ("monies$" "money")
    ("mongooses$" "mongoose")
    ("moves$" "move")
    ("mythoi$" "mythos")
    ("numina$" "numen")
    ("occiputs$" "occiput")
    ("octopuses$" "octopus")
    ("opuses$" "opus")
    ("oxen$" "ox")
    ("penises$" "penis")
    ("people$" "person")
    ("sexes$" "sex")
    ("soliloquies$" "soliloquy")
    ("testes$" "testis")
    ("trilbys$" "trilby")
    ("turfs$" "turf")

    ("\\(.*[nrlm]ese\\)$" "\\1")
    ("\\(.*deer\\)$" "\\1")
    ("\\(.*fish\\)$" "\\1")
    ("\\(.*measles\\)$" "\\1")
    ("\\(.*ois\\)$" "\\1")
    ("\\(.*pox\\)$" "\\1")
    ("\\(.*sheep\\)$" "\\1")
    ("\\(Amoyese\\)$" "\\1")
    ("\\(bison\\)$" "\\1")
    ("\\(Borghese\\)$" "\\1")
    ("\\(bream\\)$" "\\1")
    ("\\(breeches\\)$" "\\1")
    ("\\(britches\\)$" "\\1")
    ("\\(buffalo\\)$" "\\1")
    ("\\(cantus\\)$" "\\1")
    ("\\(carp\\)$" "\\1")
    ("\\(chassis\\)$" "\\1")
    ("\\(clippers\\)$" "\\1")
    ("\\(cod\\)$" "\\1")
    ("\\(coitus\\)$" "\\1")
    ("\\(Congoese\\)$" "\\1")
    ("\\(contretemps\\)$" "\\1")
    ("\\(corps\\)$" "\\1")
    ("\\(debris\\)$" "\\1")
    ("\\(diabetes\\)$" "\\1")
    ("\\(djinn\\)$" "\\1")
    ("\\(eland\\)$" "\\1")
    ("\\(elk\\)$" "\\1")
    ("\\(equipment\\)$" "\\1")
    ("\\(Faroese\\)$" "\\1")
    ("\\(flounder\\)$" "\\1")
    ("\\(Foochowese\\)$" "\\1")
    ("\\(gallows\\)$" "\\1")
    ("\\(Genevese\\)$" "\\1")
    ("\\(Genoese\\)$" "\\1")
    ("\\(Gilbertese\\)$" "\\1")
    ("\\(graffiti\\)$" "\\1")
    ("\\(headquarters\\)$" "\\1")
    ("\\(herpes\\)$" "\\1")
    ("\\(hijinks\\)$" "\\1")
    ("\\(Hottentotese\\)$" "\\1")
    ("\\(information\\)$" "\\1")
    ("\\(innings\\)$" "\\1")
    ("\\(jackanapes\\)$" "\\1")
    ("\\(Kiplingese\\)$" "\\1")
    ("\\(Kongoese\\)$" "\\1")
    ("\\(Lucchese\\)$" "\\1")
    ("\\(mackerel\\)$" "\\1")
    ("\\(Maltese\\)$" "\\1")
    ("\\(media\\)$" "\\1")
    ("\\(mews\\)$" "\\1")
    ("\\(moose\\)$" "\\1")
    ("\\(mumps\\)$" "\\1")
    ("\\(Nankingese\\)$" "\\1")
    ("\\(news\\)$" "\\1")
    ("\\(nexus\\)$" "\\1")
    ("\\(Niasese\\)$" "\\1")
    ("\\(Pekingese\\)$" "\\1")
    ("\\(Piedmontese\\)$" "\\1")
    ("\\(pincers\\)$" "\\1")
    ("\\(Pistoiese\\)$" "\\1")
    ("\\(pliers\\)$" "\\1")
    ("\\(Portuguese\\)$" "\\1")
    ("\\(proceedings\\)$" "\\1")
    ("\\(rabies\\)$" "\\1")
    ("\\(rice\\)$" "\\1")
    ("\\(rhinoceros\\)$" "\\1")
    ("\\(salmon\\)$" "\\1")
    ("\\(Sarawakese\\)$" "\\1")
    ("\\(scissors\\)$" "\\1")
    ("\\(sea[- ]bass\\)$" "\\1")
    ("\\(series\\)$" "\\1")
    ("\\(Shavese\\)$" "\\1")
    ("\\(shears\\)$" "\\1")
    ("\\(siemens\\)$" "\\1")
    ("\\(species\\)$" "\\1")
    ("\\(swine\\)$" "\\1")
    ("\\(testes\\)$" "\\1")
    ("\\(trousers\\)$" "\\1")
    ("\\(trout\\)$" "\\1")
    ("\\(tuna\\)$" "\\1")
    ("\\(Vermontese\\)$" "\\1")
    ("\\(Wenchowese\\)$" "\\1")
    ("\\(whiting\\)$" "\\1")
    ("\\(wildebeest\\)$" "\\1")
    ("\\(Yengeese\\)$" "\\1")

    ("\\(s\\)tatuses$" "\\1\\2tatus")
    ("^\\(.*\\)\\(menu\\)s$" "\\1\\2")
    ("\\(quiz\\)zes$" "\\1")
    ("\\(matr\\)ices$" "\\1ix")
    ("\\(vert\\|ind\\)ices$" "\\1ex")
    ("^\\(ox\\)en" "\\1")
    ("\\(alias\\)\\(es\\)*$" "\\1")
    ("\\(alumn\\|bacill\\|cact\\|foc\\|fung\\|nucle\\|radi\\|stimul\\|syllab\\|termin\\|viri?\\)i$" "\\1us")
    ("\\(cris\\|ax\\|test\\)es$" "\\1is")
    ("\\(shoe\\)s$" "\\1")
    ("\\(o\\)es$" "\\1")
    ("ouses$" "ouse")
    ("uses$" "us")
    ("\\([m\\|l]\\)ice$" "\\1ouse")
    ("\\(x\\|ch\\|ss\\|sh\\)es$" "\\1")
    ("\\(m\\)ovies$" "\\1\\2ovie")
    ("\\(s\\)eries$" "\\1\\2eries")
    ("\\([^aeiouy]\\|qu\\)ies$" "\\1y")
    ("\\([lr]\\)ves$" "\\1f")
    ("\\(tive\\)s$" "\\1")
    ("\\(hive\\)s$" "\\1")
    ("\\(drive\\)s$" "\\1")
    ("\\([^f]\\)ves$" "\\1fe")
    ("\\(^analy\\)ses$" "\\1sis")
    ("\\(\\(a\\)naly\\|\\(b\\)a\\|\\(d\\)iagno\\|\\(p\\)arenthe\\|\\(p\\)rogno\\|\\(s\\)ynop\\|\\(t\\)he\\)ses$" "\\1\\2sis")
    ("\\([ti]\\)a$" "\\1um")
    ("\\(p\\)eople$" "\\1\\2erson")
    ("\\(m\\)en$" "\\1an")
    ("\\(c\\)hildren$" "\\1\\2hild")
    ("\\(n\\)ews$" "\\1\\2ews")
    ("^\\(.*us\\)$" "\\1")
    ("s$" "")
    ("$" ""))
  )

(defun i/pluralize (str)
  "Pluralize str"
  (interactive)
  (let ((result str))
    (loop for (from to)  in (i/plural-rules) do
          (unless (not (string-match from str))
            (setq result (replace-match to nil nil str))
            (return result)))))

(defun i/singularize (str)
  "Singularize str"
  (interactive)
  (let ((result str))
    (loop for (plural-re sing-re) in (i/singular-rules) do
          (unless (not (string-match plural-re str))
            (setq result (replace-match sing-re nil nil str))
            (return result)))))


;;;; Sort
;; (sort
(defun* i/sort-by-average 
    (lon &key (key 'identity) (average nil))
  (let* ((average (or average
                      (/ (loop for n in lon
                               sum (funcall key n))
                         (length lon))))
         (lon-copied (copy-sequence lon)))
    (sort lon-copied 
          (lambda (a b)
            (let* ((a (funcall key a))
                   (b (funcall key b))
                   (x (abs (- a average)))
                   (y (abs (- b average))))
              (if (= x y)
                  (< a b)
                (< x y))))
          )))

;;;; Misc
(defun* i/show-eol (los &key (temp-buffer-name "*i/temp*"))
  (i/aand (get-buffer-create temp-buffer-name)
        (with-current-buffer it
          (erase-buffer)
          (i/insert-each-line los)
          (current-buffer))
        (switch-to-buffer it)))


;;;; Tests
;; need el-expectations.el(written by rubikitch) to run.
;; http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "i/awhen")
      (expect "ya"
        (i/awhen (or nil "ya")
          it))
      (desc "i/awhile")
      (expect '(do you rock me?)
        (let ((l '(do you rock me?))
              (ret nil))
          (i/awhile (pop l)
            (push it ret))
          (nreverse ret)))
      (desc "i/aand")
      (expect 3
        (i/aand (1+ 0)
              (1+ it)
              (1+ it)))
      (desc "i/acond")
      (expect "cla1"
        (let ((ret nil))
          (i/acond
            (ret (push "ret is non-nil" ret))
            ((progn "cla1") (push it ret))
            (t (push "t" ret)))
          (car ret)))
      (desc "i/alambda")
      (expect 120
        (let ((factor (i/alambda (lst)
                        (cond
                         ((null lst) 1)
                         (t (* (car lst) (caller (cdr lst))))))))
          (funcall factor '(1 2 3 4 5))))

      (desc "i/with-struct")
      (expect 'st-a
        (defstruct (i/test-struct
                    (:constructor i/make-test-struct (a b)))
          a b)
        (let ((st (i/make-test-struct 'st-a 'st-b)))
          (i/with-struct (i/test-struct- a b) st
            a)))
      (desc "i/with-struct setf")
      (expect 'changed
        (defstruct (i/test-struct
                    (:constructor i/make-test-struct (a b)))
          a b)
        (let ((st (i/make-test-struct 'st-a 'st-b)))
          (i/with-struct (i/test-struct- a b) st
            (setf a 'changed))
          (i/with-struct (i/test-struct- a) st
            a)))

      (desc "i/define-with-all-slots-struct")
      (expect '(st-a st-b)
        (progn
          (defstruct (i/test-struct
                      (:constructor i/make-test-struct (a b)))
            a b)
          (i/define-with-all-slots-struct i/with-all-slots-test-struct i/test-struct-)
          (let ((st (i/make-test-struct 'st-a 'st-b)))
            (i/with-all-slots-test-struct st
              (list a b)))))
      (desc "i/define-with-all-slots-struct setf")
      (expect '(aa bb)
        (progn
          (defstruct (i/test-struct
                      (:constructor i/make-test-struct (a b)))
            a b)
          (i/define-with-all-slots-struct i/with-all-slots-test-struct i/test-struct-)
          (let ((st (i/make-test-struct 'st-a 'st-b)))
            (i/with-all-slots-test-struct st
              (setq a 'aa
                    b 'bb)
              (list a b)))))

      (desc "i/with-lexical-bindings")
      (expect '(var-a var-b changed)
        (let ((funcs (let ((a 'var-a)
                           (b 'var-b)
                           (c 'var-c))
                       (i/with-lexical-bindings (a b)
                         (list (lambda () a)
                               (lambda () b)
                               (lambda () c))))))
          (destructuring-bind (a b c) '(changed changed changed)
            (mapcar 'funcall funcs))))

      (desc "i/define-with-struct-macro")
      (expect 'st-a
        (defstruct (i/test-struct
                    (:constructor i/make-test-struct (a b)))
          a b)
        (i/define-with-struct-macro i/with-test-struct i/test-struct-)
        (let ((st (i/make-test-struct 'st-a 'st-b)))
          (i/with-test-struct (a) st
            a)))

      (desc "i/case-regexp-aux")
      (expect '(((i/=~ "aa" expr) (i/with-anaphoric-match-utilities expr "match aa")) ((i/=~ "bb" expr) (i/with-anaphoric-match-utilities expr "match bb")))
        (i/case-regexp-aux 'expr '(("aa" "match aa") ("bb" "match bb"))))

      (expect '((t nil))
        (i/case-regexp-aux 'expr '( (t) )))

      (desc "i/case-regexp")
      (expect "match huga"
        (i/case-regexp (concat "hu" "ga")
          ("^huga" (progn (message "%s" "match!!")
                          "match huga"))
          (t "otherwise")))
      (expect nil
        (i/case-regexp (concat "hu" "ga")
          ("huga" nil)
          (t "otherwise")))
      (expect nil
        (i/case-regexp (concat "hu" "ga")
          ("never match"
           (progn (message "%s" "match!!")
                  "match"))
          (t nil)))
      (expect nil
        (i/case-regexp (concat "hu" "ga")
          ("never match" (progn (message "%s" "match!!")
                                "match"))
          (t )))
      (expect nil
        (i/case-regexp (concat "hu" "ga")
          ("never match" (progn (message "%s" "match!!")
                                "match"))
          (otherwise nil)))
      (expect "huga"
        (i/case-regexp (concat "hu" "ga")
          ("^huga" $0)
          (t "otherwise")))

      (expect  "match huga"
        (i/case-regexp (concat "hu" "ga")
          ((rx "huga") "match huga")
          (t "otherwise")))

      (desc "i/define-define-keymap")
      (expect 'i/test-command
        (let ((i/test-keymap (make-sparse-keymap)))
          (flet ((i/test-command (&rest args) (interactive) (apply 'identity args)))
            (i/define-define-keymap i/define-key-test-keymap i/test-keymap)
            (i/define-key-test-keymap "<return>" 'i/test-command)
            (lookup-key i/test-keymap (kbd "<return>")))))

      (expect 'i/test-command
        (let ((i/test-keymap (make-sparse-keymap)))
          (flet ((i/test-command (&rest args) (interactive) (apply 'identity args)))
            (i/define-define-keymap i/define-key-test-keymap i/test-keymap)
            (i/define-key-test-keymap [f1] 'i/test-command)
            (lookup-key i/test-keymap [f1]))))

      (expect 'i/test-command
        (let ((i/test-keymap (make-sparse-keymap)))
          (flet ((i/test-command (&rest args) (interactive) (apply 'identity args)))
            (i/define-define-keymap i/define-key-test-keymap i/test-keymap)
            (i/define-key-test-keymap (progn nil "C-c a") (progn "ignore" 'i/test-command))
            (lookup-key i/test-keymap (progn nil "C-c a")))))

      (expect 'i/test-command
        (let ((i/test-keymap (make-sparse-keymap)))
          (flet ((i/test-command (&rest args) (interactive) (apply 'identity args)))
            (i/define-define-keymap i/define-key-test-keymap i/test-keymap :doc "define i/test-keymap key")
            (i/define-key-test-keymap "<return>" 'i/test-command)
            (lookup-key i/test-keymap (kbd "<return>")))))

      (desc "i/dolist-with-progress-reporter")
      (expect (regexp "[[:digit:]][[:digit:]]%")
        (flet ((message (&rest args) (print (apply 'format args))))
          (with-output-to-string
            (i/dolist-with-progress-reporter (v (number-sequence 0 5)) "msg: " nil 0.1
              (sit-for 0.1)))))
      (expect '(1 2 3)
        (let (ret)
          (i/dolist-with-progress-reporter (n '(1 2 3) (nreverse ret)) "" nil nil
            (push n ret))))
      (desc "i/cond-let")
      (expect "cd(d c nil)"
        (with-output-to-string
          (i/cond-let (((= 1 2) (x (princ 'a)) (y (princ 'b)))
                     ((= 1 1) (y (princ 'c)) (x (princ 'd)))
                     (t       (x (princ 'e)) (z (princ 'f))))
            (princ (list x y z)))))
      (expect 'aa
        (i/cond-let ((t (a 'aa)))
          a))

      (desc "i/when-let")
      (expect 'foo
        (i/when-let (var 'foo)
          var))
      (expect nil
        (i/when-let (var (progn nil))
          (progn 'foo)))

      (desc "i/in")
      (expect '(t nil)
        (let (var)
          (flet ((op () 'two))
            (list (i/in (op) 'one 'two (prog1 'three (setq var 'called)))
                  var))))

      (desc "i/group")
      (expect '((1 2) (3 4))
        (i/group '(1 2 3 4) 2))
      (expect '((1 2) (3))
        (i/group '(1 2 3) 2))
      (expect (error)
        (i/group '(1 2 3) 2 :error-check t))
      (expect '((1))
        (i/group '(1) 2))
      (expect (error)
        (i/group '(1) 2 :error-check t))


      (desc "i/define-macro-alias")
      (expect 'alias
        (defmacro i/test-macro (arg)
          (i/with-gensyms (a)
            `(let ((,a ,arg))
               ,a)))
        (i/define-macro-alias i/test-macro-alias i/test-macro)
        (i/test-macro-alias 'alias))

      (desc "i/in")
      (expect '(t nil)
        (let (called)
          (let ((op 'otherwise))
            (list
             (i/in op 't 'otherwise (prog1 'ya (setq called t)))
             called))))
      (desc "i/inq")
      (expect t
        (let ((op 'otherwise))
          (i/inq op t otherwise ya)))

      (desc "i/xcase")
      (expect '(first t nil)
        (let (called called2)
          (list
           (i/xcase 'key
             (((prog1 'key (setq called t))) 'first)
             ((prog1 'ya (setq called2 t)) 'second))
           called called2)))

      (desc "i/in")
      (expect t
        (i/in (prog1 'apple "apple") "banana" "apple" "pine" 'apple))
      (expect nil
        (i/in (progn 'ignore "apple") "banana" "apple" "pine"))
      (desc "i/in=")
      (expect t
        (i/in= (progn 'ignore "apple") "banana" "apple" "pine"))
      (expect t
        (i/in= (progn 'ignore 'apple) "banana" "apple" "pine" (prog1 'apple 'ignore)))

      (desc "i/join+")
      (expect "oh my god"
        (i/join+ (list "oh" "my" "god") (prog1 " " 'ignore)))


      (desc "i/flatten")
      (expect '(a b c d e)
        (i/flatten '(a (b (c d)) e)))

      (desc "i/subseq")
      (expect '(3 4 5 6 7)
        (i/subseq (number-sequence 0 10) 3 8))

      (desc "i/remove-if")
      (expect '(0 2 4 6 8 10)
        (i/remove-if 'oddp (number-sequence 0 10)))
      (desc "i/remove-if-not")
      (expect '(1 3 5 7 9)
        (i/remove-if-not 'oddp (number-sequence 0 10)))

      (desc "i/fn")
      (expect '(3 2 1)
        (sort (list 1 2 3)
              (i/fn (_ b) (> _ b))))
      (expect '(3 2 1)
        (sort (list 1 2 3)
              (i/fn (> _1 _2))))
      (expect '(0 1 2 3)
        (i/remove-if (i/fn (_) (> _ 3)) (number-sequence 0 10)))
      (expect '(0 1 2 3)
        (i/remove-if (i/fn (> _ 3)) (number-sequence 0 10)))
      (expect '(1 3)
        (apply (i/fn (list _1 _3)) '(1 2 3)))

      (expect t
        (funcall (i/fn (= 1 (length _)))
                 (list 'a)))
      (desc "i/fn error")
      (expect "cant use \"_\" and \"(_1 _2 ...)\" at the same time!!"
        (condition-case err
            (i/fn (list _ _1 _2))
          (error "%s" (error-message-string err))))

      (desc "i/fn cant use both _<n> and _")
      (expect (error)
        (sort (list 1 2 3)
              (i/fn (< _1 _))))

      (desc "i/fn _0 is bound to arglist")
      (expect '(a b (a b))
        (apply (i/fn (list _1
                         _2
                         _0))
               '(a b)))

      (desc "i/cars")
      (expect '("a" "c")
        (i/cars '( ("a" . "b") ("c" . "d"))))
      (desc "i/cdrs")
      (expect '("b" "d")
        (i/cdrs '( ("a" . "b") ("c" . "d"))))

      (desc "i/match-with-temp-buffer")
      (expect '("string" "string" "string")
        (i/match-with-temp-buffer ((rx "string") "string string string" )
          $m))
      (expect "string string string"
        (i/match-with-temp-buffer ((rx "ing") "string string string" (buffer-string))
          $m))
      (expect "  "
        (i/match-with-temp-buffer ((rx "string") "string string string" (buffer-string))
          ($sub "")))

      (desc "i/remif")
      (expect '("a" "b")
        (i/remif (i/fn (i/=~ (rx bol ".") _))
               (list "a" "b" ".hoge")))
      (expect '("a" "b")
        (i/remif (i/fn (i/=~ (rx bol ".") _))
               (list "a" "b" ".hoge")
               :key 'identity))
      (expect '("a" "b")
        (i/remif (i/fn (i/=~ (rx bol ".") _))
               (list "a" "b" ".hoge")
               :key (i/fn (a) a)))

      (desc "i/!remif")
      (expect '((1 . "huga"))
        (i/!remif (i/fn (i/=~ "huga" _))
                '( (1 . "huga")
                   (2 . "hoge")
                   (3 . "piyo") )
                :key 'cdr))
      (expect '(".hoge")
        (i/!remif (i/fn (i/=~ (rx bol ".") _))
                (list "a" "b" ".hoge")))
      (expect '(".hoge")
        (i/!remif (i/fn (i/=~ (rx bol ".") _))
                (list "a" "b" ".hoge")
                :key 'identity))
      (expect '(".hoge")
        (i/!remif (i/fn (i/=~ (rx bol ".") _))
                (list "a" "b" ".hoge")
                :key (i/fn (a) a)))

      (desc "cadr")
      (expect '("2" "4")
        (i/cadrs '( ("1" "2") ("3"  "4"))))

      (desc "i/with-point-buffer")
      (expect '("a" "b")
        (i/with-point-buffer "aaa`!!'bbb"
          (list
           (char-to-string (preceding-char))
           (char-to-string (following-char)))))

      (desc "i/assoc-cdrs")
      (expect '("kval" "vval")
        (i/assoc-cdrs '("k" "v")
                    '(("k" . "kval")
                      ("v" . "vval"))))
      (expect '("kval" "vval")
        (i/assoc-cdrs '( k v )
                    '((k . "kval")
                      (v . "vval"))))
      (expect '("kval" "vval" "def")
        (i/assoc-cdrs '( k v default)
                    '((k . "kval")
                      default
                      (v . "vval"))
                    'eq
                    "def"))
      (desc "i/nths")
      (expect '(2 2 2)
        (i/nths 2
              (list (number-sequence 0 10)
                    (number-sequence 0 10)
                    (number-sequence 0 10))))
      )))

(provide 'imakado)
;;; imakado.el ends here

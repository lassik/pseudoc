;;; xxx: you can't write interleaved control structures such as duff's
;;; device using this.

(defmacro vbind (pars args &body body)
  `(multiple-value-bind ,pars ,args ,@body))

(defmacro dbind (pars args &body body)
  `(destructuring-bind ,pars ,args ,@body))

(defun list1 (x)
  (if (listp x) x (list x)))

(defun sf (ctl &rest args)
  (apply #'format nil ctl args))

(defun sb ()
  (make-array 0
              :element-type 'character
              :fill-pointer 0
              :adjustable   t))

(defun s+ (&rest xs)
  (apply #'concatenate 'string (mapcar #'string (remove nil xs))))

(defun s+= (sb &rest xs)
  (dolist (x xs)
    (etypecase x
      (character (vector-push-extend x sb))
      (string (loop for remain from (length x) by -1
                    for c across x
                    do (vector-push-extend c sb remain)))
      (null)
      (symbol (s+= sb (symbol-name x))))))

(defun sindent (ncol string)
  (loop with ind = (make-string ncol :initial-element #\space)
        with new = (sb)
        as start = 0 then (1+ end)
        as end = (position #\newline string :start start)
        do (s+= new ind (subseq string start end))
        while end
        do (s+= new #\newline)
        finally (return new)))

(defun sdelim (delim list)
  (loop with new = (sb)
        for (x . rest) on list
        do (s+= new x (when rest delim))
        finally (return new)))

(defun intern+ (&rest xs)
  (intern (apply #'s+ xs)))

(defmacro with-syms (syms &body body)
  `(let ,(mapcar (lambda (sym)
                   (if (consp sym)
                       `(,(car sym) (intern+ ,@(cdr sym)))
                       `(,sym (gensym ,(symbol-name sym)))))
                 syms)
     ,@body))

(defmacro defdef (name doc)
  (with-syms (($var "*" name "*")
              ($def "DEF" name)
              ($get "GET" name)
              %def-name
              %def-pars
              %def-body
              %def-args
              %get-key
              %get-need-p)
    `(progn (defparameter ,$var (make-hash-table))
            (defmacro ,$def (,%def-name ,%def-pars &body ,%def-body)
              `(setf (gethash ',,%def-name ,',$var)
                     (lambda (,',%def-args)
                       (dbind ,,%def-pars ,',%def-args
                         ,@,%def-body))))
            (defun ,$get (,%get-key &optional (,%get-need-p nil))
              (or (values (gethash ,%get-key ,$var))
                  (if ,%get-need-p
                      (error "No such ~A: ~S" ,doc ,%get-key)
                      nil)))
            t)))

(defdef @type "type specifier")
(defdef @top  "top-level language construct")
(defdef @sub  "subordinate language construct")
(defdef @memb "class member")

(defmacro with-@context ((label) &body body)
  `(let ((*@context* ,label))
     (declare (special *@context*))
     ,@body))

(defun check@context (context &optional what)
  (when (not (equal *@context* context))
    (if what
        (error "~a must appear in ~a context" what context)
        (error "expecting ~a context" context))))

(defun eval@name (x)
  (if (symbolp x)
      (substitute-if #\_ (complement #'alphanumericp) (symbol-name x))
      (error "expected a name, but got ~s" x)))

(defun eval@type (x)
  (labels ((bad () (error "bad type specifier: ~s" x)))
    (let ((x (list1 x)))
      (if (null x)
          (bad)
          (let ((f (get@type (car x))))
            (if f
                (funcall f (cdr x))
                (if (cdr x)
                    (bad)
                    (eval@name (car x)))))))))

;(def@type |vec| (xxx-name targtype &optional (count nil count-p))
;  (sf "~a[~a]" (eval@nametype xxx-name targtype) (if count-p (sf "~d" count) "")))

(defun eval@nametype (nametype)
  (dbind (name type) nametype
    (cond ((and (consp type) (eql '|vec| (car type)))  ; xxx: kludge
           (dbind (v-elt-type &optional (v-length nil v-length-p)) (cdr type)
             (sf "~a ~a[~a]"
                 (eval@type v-elt-type)
                 (eval@name name)
                 (if v-length-p (eval@expr v-length) ""))))
          ((and (consp type) (eql '|fun| (car type)))
           (dbind (pars &rest rest) (cdr type)
             (eval@funp name pars (parse-fun-rest rest nil))))
          (t
           (let ((et (eval@type type)))
             (s+ et
                 (when (not (char= #\* (char et (- (length et) 1)))) " ")
                 (eval@name name)))))))

(defun eval@top (x)
  (if (atom x)
      (error "Atom at file top level: ~S" x)
      (funcall (get@top (car x) t)
               (cdr x))))

(defun eval@funcall (x)
  (let ((f (get@sub (car x))))
    (if f
        (funcall f (cdr x))
        (sf "~A(~A)"
            (eval@expr (car x))
            (sdelim "," (mapcar #'eval@expr (cdr x)))))))

(defun eval@internal-sub (x)
  (etypecase x
    (symbol (eval@name x))
    (string (sf "\"~A\"" x))
    (character (sf "'~A'" x))
    (integer (sf "~S" x))
    (float (sf "~S" x))
    (cons (eval@funcall x))))

(defun eval@expr (x)
  (with-@context ("expression")
    (eval@internal-sub x)))

(defun eval@statement (x)
  (with-@context ("statement")
    (eval@internal-sub x)))

(defun eval@body (x)
  (sf "{~%~A~%}" (sindent 2 (sf "~{~A;~^~%~}" (mapcar #'eval@statement x)))))

(defun eval@funp (name pars &optional (result nil result-supplied-p))
  (sf "~A(~A)~%"
      (if result-supplied-p
          (sdelim " " (nconc (mapcar #'eval@type (butlast result))
                             (list (eval@nametype (list name (car (last result)))))))
          (eval@name name))
      (if pars
          (sdelim ", " (mapcar #'eval@nametype pars))
          "void")))

(defun eval@bin (op xs)
  (assert (> (length xs) 1))
  (s+ "(" (sdelim op (mapcar #'eval@expr xs)) ")"))

(defun eval@binint (op xs)
  (loop with new = (sb)
        for first-p = t then nil
        for (x . rest) on xs
        do (s+= new
                (s+ (when (not first-p) op) (eval@expr x)))
        finally (return new)))

(defun @-read-string (s c)
  (declare (ignore c))
  (let ((r (sb))
        (esc nil))
    (loop (let ((c (read-char s t nil t)))
            (cond ((not esc)
                   (case c
                     (#\" (return r))
                     (#\\ (setf esc t))
                     (t   (s+= r c))))
                  (esc
                   (setf esc nil)
                   (s+= r
                        #\\
                        (ecase c
                          (#\\ #\\)
                          (#\" #\")
                          (#\t #\t)
                          (#\r #\r)
                          (#\n #\n)))))))))

(defmacro with-@-read-syntax (&body body)
  `(let ((*readtable* (copy-readtable nil nil)))
     (setf (readtable-case *readtable*) :preserve)
     (set-macro-character #\" #'@-read-string)
     ,@body))

(defparameter +file-prependage+
  '("/* this file is machine-generated */ "
    "#if !defined(__BORLANDC__) && !defined(__STRICT_ANSI__)"
    "typedef long long __int64_t;"
    "#endif"))

(defun eval@stream (istream ostream)
  (mapc (lambda (line) (write-line line ostream))
        +file-prependage+)
  (loop with eof = (gensym)
        as x = (with-@-read-syntax (read istream nil eof))
        while (not (eql eof x))
        do (format ostream "~A" (eval@top x))))

(def@top |include-global| (name)
  (when (not (stringp name))
    (error "include-global: expected string, got ~S" name))
  (sf "#include <~A>~%" name))

(def@top |include-local| (name)
  (when (not (stringp name))
    (error "include-local: expected string, got ~S" name))
  (sf "#include ~S~%" name))

(def@top |cplusplus-extern-c| (&body body)
  (s+ (sf "extern \"C\" {~%")
      (sdelim (sf "~%") (mapcar #'eval@top body))
      (sf "~%")
      "}  /* end of extern \"C\" */"))

(def@top |extern| (x)
  (assert (and (consp x)
               (or (eql (car x) '|var|)
                   (eql (car x) '|funp|))))
  (s+ "extern " (eval@top x)))

(def@top |type| (name expansion)
  (sf "typedef ~A;~%"
      (eval@nametype (list name expansion))))

(def@top |enum| (name &body values)
  (sf "enum ~A {~%~A~%}~%"
      (eval@name name)
      (sf "~{  ~A~^,~%~}" (mapcar #'eval@name values))))

(def@top |stu| (name &body slots)
  (sf "struct ~A {~%~A};~%"
      (eval@name name)
      (sf "~{  ~A;~%~}" (mapcar #'eval@nametype slots))))

(def@top |const| (name type value)
  (sf "const ~A = ~A;"
      (eval@nametype (list name type))
      (eval@expr value)))

(def@top |var| (name type &optional (initvalue nil initvalue-supplied-p))
  (sf "~A;~%"
      (s+ (eval@nametype (list name type))
          (when initvalue-supplied-p
            (sf " = ~A" (eval@expr initvalue))))))

(defun parse-fun-rest (rest want-body-p)
  (vbind (result body)
      (if (eql '=> (car rest))
          (dbind (=> (&rest the-result) &body the-body) rest
            (values the-result the-body))
          (values '(|void|) rest))
    (if (and body (not want-body-p))
        (error "Function body not expected.")
        (values result body))))

(def@top |funp| (name pars &rest rest)
  (s+ (eval@funp name pars (parse-fun-rest rest nil))
      ";"))

(def@top |fun| (name pars &rest rest)
  (vbind (result body) (parse-fun-rest rest t)
    (s+ (eval@funp name pars result)
        (eval@body body))))

(def@top |class| (name (&optional superclass) &body body)
  (s+ "class " (eval@name name)
      (when superclass (s+ ": public " (eval@name superclass)))
      (sf "~%{~%")
      (apply #'s+
             (let ((*class-name* name))
               (declare (special *class-name*))
               (mapcar
                (lambda (bodypart)
                  (etypecase bodypart
                    (keyword
                     (case bodypart
                       ((:|private| :|protected| :|public|)
                        (sf "~A:~%" (symbol-name bodypart)))
                       (t
                        (error "No such class member protection label: ~A"
                               bodypart))))
                    (cons
                     (s+ (sindent 2 (funcall (get@memb (car bodypart) t)
                                             (cdr bodypart)))
                         (sf "~%")))))
                (cons :|public|
                      body))))
      (sf "};~%")))

(def@memb |var| (name type)
  (sf "~A;"
      (eval@nametype (list name type))))

(def@memb |fun| (name pars &rest rest)
  (vbind (result body) (parse-fun-rest rest t)
    (s+ (eval@funp name pars result)
        (eval@body body))))

(def@memb |set| (name pars &rest rest)
  (vbind (result body) (parse-fun-rest rest t)
    (s+ (eval@funp (make-symbol (s+ "set-" name)) pars result)
        (eval@body body))))

(def@memb |birth| (pars &body body)
  (s+ (eval@funp *class-name* pars)
      (eval@body body)))

(def@memb |death| (&body body)
  (s+ "~" (eval@name *class-name*) "(void)" (sf "~%")
      (eval@body body)))

(defun eval@typeptr (targtype stars)
  (s+ (if targtype (eval@type targtype) "")
      (if targtype " " "")
      stars))

(def@type |stu| (name) (sf "struct ~A" (eval@name name)))
(def@type |enum| (name) (sf "enum ~A" (eval@name name)))
(def@type |p| (&optional targtype) (eval@typeptr targtype "*"))
(def@type |pp| (&optional targtype) (eval@typeptr targtype "**"))
(def@type |ppp| (&optional targtype) (eval@typeptr targtype "***"))
(def@type |char| () "char")
(def@type |bool| () "bool")
(def@type |uint8| () "unsigned char")
(def@type |uint16| () "unsigned short")
(def@type |uint32| () "unsigned long")
(def@type |uint64| () "unsigned __int64_t")
(def@type |sint8| () "signed char")
(def@type |sint16| () "signed short")
(def@type |sint32| () "signed long")
(def@type |sint64| () "signed __int64_t")
(def@type |uint| () "unsigned int")
(def@type |sint| () "signed int")
(def@type |int| () (error "Don't use the `int' type. Use `uint' or `sint'."))

(def@sub |cast| (type value)
  (sf "((~A)(~A))" (eval@type type) (eval@expr value)))

(def@sub |p+| (place)
  (sf "(&~A)" (eval@expr place)))

(def@sub |p-| (place)
  (sf "(*(~A))" (eval@expr place)))


(def@sub |p--| (place)
  (sf "(**(~A))" (eval@expr place)))

(def@sub |sizeof| (type)
  (sf "sizeof(~A)" (eval@type type)))

(def@sub |maxof| (type)
  (sf "maxof(~A)" (eval@type type)))

(def@sub |new| (class-name &rest initargs)
  (sf "new ~A(~A)"
      (eval@name class-name)
      (sdelim "," (mapcar #'eval@expr initargs))))

;;; xxx: what if <initvalue> supplied within expression, not statement?
;;; xxx: think <for(int var = foo)> vs. <if(int var = foo == bar)>
(def@sub |var| (name type &optional (initvalue nil initvalue-supplied-p))
  (s+ (eval@nametype (list name type))
      (when initvalue-supplied-p
        (s+ " = " (eval@expr initvalue)))))

(def@sub |for| (init step fini &body body)
  (sf "for(~a; ~a; ~a) ~a"
      (eval@expr init)
      (eval@expr step)
      (eval@expr fini)
      (eval@body body)))

(def@sub |while| (test &body body)
  (sf "while(~A) ~A"
      (eval@expr test)
      (eval@body body)))

(def@sub |dotimes| ((times &optional (var "dotimes_counter")) &body body)
  (sf "for(unsigned long ~A = 0; ~A < ~A; ++A) ~A"
      (eval@name var)
      (eval@name var)
      (eval@expr times)
      (eval@name var)
      (eval@body body)))

(def@sub |loop| (&body body)
  (sf "for(;;) ~A"
      (eval@body body)))

(def@sub |when| (test &body body)
  (sf "if(~A) ~A"
      (eval@expr test)
      (eval@body body)))

(def@sub |if| (test then else)
  (sf "(~A) ? ~A : ~A"
      (eval@expr test)
      (eval@expr then)
      (eval@expr else)))

(def@sub |cond| (&body clauses)
  (sf "~{~A~}"
      (loop for clause in clauses
            for had-some-p = nil then t
            collect (dbind (test &body body) clause
                      (s+ (if (eql '|else| test)
                              (if had-some-p
                                  "else"
                                  "if(0) ; else")
                              (s+ (when had-some-p "else ")
                                  (sf "if(~A)" (eval@expr test))))
                          " "
                          (eval@body body))))))

(def@sub |case| (key &body cases)
  (sf "switch (~A) {~{~A~}~%}~%"
      (eval@expr key)
      (mapcar (lambda (case)
                (dbind (select &body body) case
                  (sf "~A~A"
                      (if (eql '|else| select)
                          (sf "default:~%")
                          (sf "~{case ~A:~%~}"
                              (mapcar #'eval@expr (list1 select))))
                      (eval@body (nconc body `((|break|)))))))
              cases)))

(def@sub |return| (&optional (value nil value-supplied-p))
  (check@context "statement" "return")
  (s+ "return"
      (when value-supplied-p
        (sf "(~A)" (eval@expr value)))))

;;; xxx: warn if user tries to use (break) within (case), because that
;;; xxx: won't work.
(def@sub |break| ()
  (check@context "statement")
  "break")

(def@sub |next| ()
  (check@context "statement")
  "continue")

(def@sub |vref| (vector index) (sf "~a[~a]" (eval@expr vector) (eval@expr index)))

(def@sub |@|  (&rest xs) (eval@binint "."  xs))
(def@sub |->| (&rest xs) (eval@binint "->" xs))

(def@sub |=|  (&rest xs) (eval@bin " == " xs))
(def@sub |<|  (&rest xs) (eval@bin " < "  xs))
(def@sub |>|  (&rest xs) (eval@bin " > "  xs))
(def@sub |<=| (&rest xs) (eval@bin " <= " xs))
(def@sub |>=| (&rest xs) (eval@bin " >= " xs))

(def@sub |nil?|    (x) (sf "!~A"  (eval@expr x)))
(def@sub |not|     (x) (sf "!~A"  (eval@expr x)))
(def@sub |bit-not| (x) (sf "~~~A" (eval@expr x)))

(def@sub |and|      (&rest xs) (eval@bin " && " xs))
(def@sub |or|       (&rest xs) (eval@bin " || " xs))
(def@sub |bit-and|  (&rest xs) (eval@bin " & "  xs))
(def@sub |bit-or|   (&rest xs) (eval@bin " | "  xs))
(def@sub |bit-xor|  (&rest xs) (eval@bin " ^ "  xs))
(def@sub |bit-shl|  (&rest xs) (eval@bin " << " xs))
(def@sub |bit-shr|  (&rest xs) (eval@bin " >> " xs))
(def@sub |+|        (&rest xs) (eval@bin " + "  xs))
(def@sub |-|        (&rest xs) (eval@bin " - "  xs))
(def@sub |*|        (&rest xs) (eval@bin " * "  xs))
(def@sub |/|        (&rest xs) (eval@bin " / "  xs))
(def@sub |mod|      (&rest xs) (eval@bin " % "  xs))

(def@sub |inc!|     (place)       (sf "++~A" (eval@expr place)))
(def@sub |dec!|     (place)       (sf "--~A" (eval@expr place)))

(def@sub |set!|     (place value) (eval@bin " = "   (list place value)))
(def@sub |not!|     (place)       (eval@bin " = ! " (list place place)))
(def@sub |bit-not!| (place)       (eval@bin " = ~ " (list place place)))
(def@sub |bit-and!| (place other) (eval@bin " &= "  (list place other)))
(def@sub |bit-or!|  (place other) (eval@bin " |= "  (list place other)))
(def@sub |bit-xor!| (place other) (eval@bin " ^= "  (list place other)))
(def@sub |bit-shl!| (place other) (eval@bin " <<= " (list place other)))
(def@sub |bit-shr!| (place other) (eval@bin " >>= " (list place other)))
(def@sub |+!|       (place other) (eval@bin " += "  (list place other)))
(def@sub |-!|       (place other) (eval@bin " -= "  (list place other)))
(def@sub |*!|       (place other) (eval@bin " *= "  (list place other)))
(def@sub |/!|       (place other) (eval@bin " /= "  (list place other)))
(def@sub |mod!|     (place other) (eval@bin " %= "  (list place other)))

(eval@stream *standard-input* *standard-output*)

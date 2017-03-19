;;; DEFINITION AND READING OF THE LEX GRAMMAR

;; read characters while no special character, return list of read
;; characters and next charcter.  if string contains \ character, the
;; next character is read without interpretation.
(defun read-string (string &optional acc)
  (if (null string)
      (values (nreverse acc) nil)
      (let ((chr (first string)))
	(cond ((alphanumericp chr) (read-string (rest string) (cons chr acc)))
	      ((eql chr #\#) (read-string (rest (rest string)) (cons (second string) acc)))
	      (t (values (nreverse acc) string))))))
(read-string (coerce "ab#!cde" 'list) nil)

(defun nrprepend (from to)
  (cond ((null from) to)
	(t (nrprepend (rest from) (cons (first from) to)))))

(defun simplify-expr0 (expr)
;;  (format t "~&SE ~S" expr)
  (if (and (= (length expr) 2)
	   (equal (first expr) 'EXPR)			       
	   (equal (caadr expr) 'Q)
	   (equal (caddadr expr) 1)
	   (equal (caadadr expr) 'EXPR))
      (cadadr expr)
      expr))

(defun simplify-expr (expr)
;;  (format t "~&SE2 ~S" expr)
  (if (and (= (length expr) 2)
	   (equal (first expr) 'EXPR)			       
	   (equal (caadr expr) 'Q)
	   (equal (caddadr expr) 1))
      (cadadr expr)
      expr))
	
;; read expression inside parentheses, return parsed structures and
;; pointer to the rest of the input.  beginning and end of the string
;; can be considered as parentheses
(defun read-expr (string &optional acc subacc)
;;  (format t "~&RP: ~S acc=~S subacc=~S" string acc subacc)
  (cond ((and (null string) acc)
	 (values (list 'EXPR
		       (list 'Q
			     (cons 'OR (nreverse (cons (cons 'EXPR (nreverse subacc)) acc)))
			     1))))
	((or (null string) (eql (first string) #\)))
	 (values (if acc
		     (cons 'OR (nreverse (cons (cons 'EXPR (nreverse subacc)) acc)))
		     (cons 'EXPR (nreverse subacc)))
		 (rest string)))
	((eql (first string) #\|)
	 (read-expr (rest string) (cons (simplify-expr0 (cons 'EXPR (nreverse subacc))) acc) nil))
	(t (multiple-value-bind (result rest) (read-next string)
;;	     (format t "~&RP: MVB: ~S, ~S" result rest)
	     (if (equal (first result) 'S)
		 (read-expr rest acc (nrprepend (rest result) subacc))
		 (read-expr rest acc (cons result subacc)))))))


;; read bracketted expression, return parsed structures and pointer to
;; next character after the closing backet.
(defun read-brackets (string &optional acc)
  (if (null string)
      (values nil nil)			;not legal: no closing bracket
      (let ((1st (first string))
	    (2nd (second string))
	    (3rd (third string)))
;;	(format t "~&RB 1st:~S 2nd:~S 3rg:~S" 1st 2nd 3rd)
	(cond ((and (eql 2nd #\-) (not (eql 3rd #\])))
	       (read-brackets (cdddr string) (cons (list 'range 1st 3rd) acc)))
	      ((or (null acc) (not (eql 1st #\]))) ; [ can only be the first character
	       (read-brackets (rest string) (cons 1st acc)))
	      ((eql 1st #\]) (values (cons 'BR (nreverse acc)) (rest string)))))))
(print (parse-regex "tpe[][a-efr0-9g,-]f(g)?kjh"))
(print (parse-regex "(koko(lo)?(pu[0-5]*)+)|suso|vova(24|27)"))


;; if no quantifier present, returns 1, otherwise integer, ?, * or +
(defun read-quantifier (string &optional acc digit)
  (if (null string)
      (values 1 nil)
      (let ((chr (first string)))
	(if (not digit)
	    (cond ((or (eql chr #\?) (eql chr #\*) (eql chr #\+))
		   (values chr (rest string)))
		  ((eql chr #\{)
		   (read-quantifier (rest string) nil t))
		  (t (values 1 string)))
	    (cond ((digit-char-p chr)
		   (read-quantifier (rest string) (cons chr acc) t))
		  ((eql chr #\})
		   (values (parse-integer (coerce (nreverse acc) 'string)) (rest string)))
		  (t (values nil nil)))))))

(defun caddadr (l) (car (cddadr l)))
(defun caadadr (l) (car (cadadr l)))

(defun read-next (string)
  (if (null string)
      (values nil nil)
      (let ((chr (first string)))
	(cond ((eql chr #\()
	       (multiple-value-bind (result rest) (read-expr (rest string))
		 (multiple-value-bind (quantifier rest2) (read-quantifier rest)
;		   (format t "~&RN: RESULT=~S" result)
		   (values (list 'Q (simplify-expr result) quantifier) rest2))))
	      ((eql chr #\[)
	       (multiple-value-bind (result rest) (read-brackets (rest string))
		 (multiple-value-bind (quantifier rest2) (read-quantifier rest)
		   (values (list 'Q result quantifier) rest2))))
	      ((or (eql chr #\#) (alphanumericp chr))
	       (multiple-value-bind (result rest) (read-string string)
		 (multiple-value-bind (quantifier rest2) (read-quantifier rest)
;;		   (format t "~&RN: ~S -> ~A" result quantifier)
		   ;; !!! quantifier after string other than 1 becomes: abcd* -> {abc:1} {d:*}
		   (if (equal quantifier 1)
		       (values (list 'Q result quantifier) rest2)
		       (values (list 'S
				     (list 'Q (reverse (cdr (reverse result))) 1)
				     (list 'Q (list (car (reverse result))) quantifier))
			       rest2)))))
	      (t (values nil nil))))))

(defun parse-regex (regex)
  (read-expr (coerce regex 'list)))


(print (parse-regex "abc?|cd"))
(print (parse-regex "(abc?|cd)"))
(print (parse-regex "cd"))
(print (parse-regex "(cd)"))
(print (parse-regex "[cd]+"))
(print (parse-regex "(ab?c|def)?|cd"))
(print (parse-regex "ab(cd){15}de?f"))
(print (parse-regex "ab[cd]?f"))
(print (parse-regex "ab(cde?){15}de?f"))
(print (parse-regex "ab(c|d){15}de?f"))
(print (parse-regex "tpe[][a-efr0-9g,-]f(g)?kjh"))
(print (parse-regex "(koko(lo)?(pu[0-5]*)+)|suso|vova(24|27)"))
(print (parse-regex "(#+|#-)?[0-9]+"))

;;; grammar reader
;;; example:
;;;     number: (+|-)?[0-9]+
;;;     lower: <
;;;     greater: >
;;;     punct: ,|:|!|;
;;;     reserved: if|begin|end|var|else
;;;     ident: [a-z][a-z0-9]*
;;; output:
;;;     <number>={sign}?{digit}+; <sign>=+|-; digit=0|1|2...|9
;;;     <lower>='<'
;;;     <greater>='>'
;;;     <punct>='.'|':'|'!'|';'
;;;     <reserved=if|begin|end|var|else
;;;     ident={letter}{letdig}*; letter=a|b|c|d|...|z; letdig=0|1|2...|9
;;; with <rule>=var1|var2|var3 => <rule>=var1; <rule>=var2; rule=<var3>
(defmacro defgram (name param &rest body)
  (declare (ignore param))
  (let ((rgram nil))
    (loop
       for c = body then (cddr c)
       for n = (car c)
       for e = (cadr c)
       while c
       do (push (list n (parse-regex e)) rgram))
    `(defparameter ,name ',(nreverse rgram))))

(defgram tada ()
  number "(#+|#-)?[0-9]+"
  lower "#<"
  greater "#>"
  punct "#,|#:|#!|##"
  reserved "if|begin|end|var|else"
  ident "[a-z][a-z0-9]*"
 )

(print tada)

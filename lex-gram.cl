;;; DEFINITION AND READING OF THE LEX GRAMMAR

(defun nrprepend (from to)
  (cond ((null from) to)
	(t (nrprepend (rest from) (cons (first from) to)))))

(defun caddadr (l) (car (cddadr l)))
(defun caadadr (l) (car (cadadr l)))
(defun 2+ (x) (+ 2 x))

;; simplify (Q (EXPR X) 1) to X
(defun simplify-expr (expr)
;;  (format t "~&SE ~A" expr)
  (if (and (= (length expr) 3)
	   (equal (first expr) 'Q)
	   (equal (third expr) 1)
	   (equal (caadr expr) 'EXPR))
      (cadadr expr)
      expr))
	
;; read expression inside parentheses, return parsed structures and
;; pointer to the rest of the input.  beginning and end of the string
;; can be considered as parentheses
(defun read-expr (string &optional acc subacc)
;;  (format t "~&RE: ~S acc=~S subacc=~S" string acc subacc)
  (cond ((and (null string) acc)
	 (values `(EXPR (Q ,(cons 'OR (nreverse (cons (cons 'EXPR (nreverse subacc)) acc)))
			   1))))
	((or (null string) (eql (first string) #\)))
	 (values (if acc
		     (cons 'OR (nreverse (cons (cons 'EXPR (nreverse subacc)) acc)))
		     (cons 'EXPR (nreverse subacc)))
		 (rest string)))
	((eql (first string) #\|)
	 (read-expr (rest string) (cons (cons 'EXPR (simplify-expr (nreverse subacc))) acc) nil))
	((or (member (first string) '(#\? #\* #\+ #\{))) ;TODO "a{5}"
	 (multiple-value-bind (quantifier rest2) (read-quantifier string)
	   (read-expr rest2
		      acc
		      (cons `(Q (EXPR ,(first subacc)) ,quantifier)
			    (rest subacc)))))
	(t (multiple-value-bind (result rest) (read-next string)
;;	     (format t "~&RP: MVB: ~S, ~S" result rest)
	     (cond ((characterp result)
		    (read-expr rest acc (cons result subacc)))
		   (t (read-expr rest acc (cons result subacc))))))))

(defun char-inc (c)
  (code-char (1+ (char-code c))))

(defun make-range (c d &optional a)
  (if (char> c d)
      a					;do no reverse, will be reverse afterwards
      (make-range (char-inc c) d (cons c a))))

;; read bracketted expression, return parsed structures and pointer to
;; next character after the closing backet.
(defun read-brackets (string &optional acc)
  (if (null string)
      (values nil nil)			;not legal: no closing bracket
      (let ((1st (first string))
	    (2nd (second string))
	    (3rd (third string)))
	(cond ((and (eql 2nd #\-) (not (eql 3rd #\])))
	       (read-brackets (cdddr string) (nconc (make-range 1st 3rd) acc)))
	      ((or (null acc) (not (eql 1st #\]))) ; [ can only be the first character
	       (read-brackets (rest string) (cons 1st acc)))
	      ((eql 1st #\]) (values (cons 'BR (nreverse acc)) (rest string)))))))

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


(defun read-next (string)
;;  (format t "~&RN: ~S" string)
  (if (null string)
      (values nil nil)
      (let ((chr (first string)))
	(cond ((eql chr #\()
	       (multiple-value-bind (result rest) (read-expr (rest string))
		 (multiple-value-bind (quantifier rest2) (read-quantifier rest)
		   (values (list 'Q result quantifier) rest2))))
	      ((eql chr #\[)
	       (multiple-value-bind (result rest) (read-brackets (rest string))
		 (multiple-value-bind (quantifier rest2) (read-quantifier rest)
		   (values (list 'Q result quantifier) rest2))))
	      ((eql chr #\#)
	       (values (cadr string) (cddr string)))
	      ((alphanumericp chr)
	       (values chr (rest string)))
	      (t (values nil nil))))))

(defun parse-regex (regex)
  (read-expr (coerce regex 'list)))


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

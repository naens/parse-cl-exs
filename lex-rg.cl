;;;; Creating Regular Grammar from Tree Regex

(defun comb (&rest ns)
  (if (null ns)
      ""
      (read-from-string (format nil "~A~A" (first ns) (apply #'comb (rest ns))))))

(defun make-rule (term rhs)
  `(,term ,@rhs))

;; returns list of rules
(defun make-rules (lhs &rest rhs)
  (cond ((null lhs) nil)
	((listp lhs) (cons
		      (make-rule (car lhs) rhs)
		      (apply #'make-rules `(,(cdr lhs) . ,rhs))))
	((atom lhs) (list (make-rule lhs rhs)))
	(t 'ERROR-MAKE-RULES)))
(make-rules 's 'a 'b 'c)
(make-rules '(r s t e) 'a 'b 'c))

(defun make-br-rules (lhs chars rest &optional acc)
  (cond ((null chars) acc)
	(t (make-br-rules lhs (cdr chars) rest (nconc (make-rules lhs (car chars) rest) acc)))))
(make-br-rules '(a b) '(#\x #\y #\z) 'Q)

(defun re2rgsub (re curr pre c &optional r a)
  (let ((type (first re))
	(expr (rest re)))
;;    (format t "~&SUB[~S]: ~S" type expr)
    (case type
      (EXPR (re2rg expr curr pre c r a))
      (BR (nconc (make-br-rules curr expr r) a))
      (OR (labels ((do-ors (or-exprs acc oc)
;;		     (format t "~&DO-ORS: [ORS=~S ACC=~S]" or-exprs acc)
		     (if (null or-exprs)
			 acc
			 (do-ors (rest or-exprs)
			   (re2rg (rest (first or-exprs)) curr (comb pre oc) c r acc)
			   (1+ oc)))))
	    (do-ors expr a 1)))
      (otherwise
       (format t"~&SUB[~S]: Type not supported." type)
       a))))

;;; converts regular expression to regular grammar
;;; re: regular expression
;;; curr: current terminal to create rules
;;; pre: prefix for rule names
;;; c: counter for rule names
;;; r: rest terminal, inserted as rule at end
;;; a: accumulator of rules, used to generate result
(defun re2rg (re curr pre c &optional r a)
;;  (format t "~&re2rg: e=~S curr=~S pre=~S r=~A" (first re) curr pre r)
  (let ((e (first re))
	(n (comb pre #\- c)))
    (cond ((null re) (nconc (make-rules curr r) a))
	  ((characterp e)
	   (re2rg (rest re) n pre (1+ c) r (nconc (make-rules curr e n) a)))
	  ((and (equal (first e) 'Q) (equal (third e) #\?))
	   (let ((sub (re2rgsub (second e) curr (comb n "_") 1 n a)))
;;	     (format t "~&SUB[?]=~S" sub)
	     (re2rg (rest re) n pre (1+ c) r (nconc (make-rules curr n) sub))))
	  ((and (equal (first e) 'Q) (equal (third e) #\*))
	   (let* ((n2 (comb pre "-" (1+ c)))
		  (rules (nconc (make-rules n n2) (make-rules curr n)))
		  (sub (re2rgsub (second e) n (comb n "_") 1 n (nconc rules a))))
;;	     (format t "~&SUB[*]=~S" sub)
	     (re2rg (rest re) n2 pre (2+ c) r sub)))
	  ((and (equal (first e) 'Q) (equal (third e) #\+))
	   (let* ((n2 (comb pre "-" (1+ c)))
		  (rule (make-rules n n2))		    
		  (sub (re2rgsub (second e) (list n curr) (comb n "_") 1 n (nconc rule a))))
;;	     (format t "~&SUB[+]=~S" sub)
	     (re2rg (rest re) n2 pre (2+ c) r sub)))
	  ((and (equal (first e) 'Q) (equal (third e) 1))
	   (let ((sub (re2rgsub (second e) curr (comb n "_") 1 n a)))
;;	     (format t "~&SUB[1]=~S" sub)
	     (re2rg (rest re) n pre (1+ c) r sub)))
	  ((and (equal (first e) 'Q) (numberp (third e)))
	   (labels ((do-repeat (last from to acc)
		      (if (< from to)
			  acc
			  (let ((nn (if (= from to) curr (comb pre "[" c "]_" from "\\-0"))))
			    (do-repeat
			      nn
			      (1- from)
			      to
			      (re2rgsub (second e) nn (comb pre "[" c "]_" from) 1 last acc))))))
	     (format t "~&SUB[n=~S]=~S " (third e) (second e))
	     (re2rg (rest re) n pre (1+ c) r (do-repeat n (third e) 1 a))))
;;; TODO: ADD {n} Q=n support	  
	  (t (progn
	       (format t "~&UNSUPPORTED: ~S" e)
	       (re2rg (rest re) n pre (1+ c) r (nconc (make-rules curr 'N/A n) a)))))))

;; makes rg from regex string starting with terminal name
(defun expr2rg (expr name)
  (re2rg (cdr expr) 'START name 1 (list 'ACCEPT name)))

(defun str2rg (str name)
  (let ((re (parse-regex str)))
    (nreverse (expr2rg re name))))

(defun prs (rules)
  (loop
     for r in rules
     do (cond ((= (length r) 3) (format t "~&~12A ->     ~A ~A" (first r) (second r) (third r)))
	      ((= (length r) 2) (format t "~&~12A ->     ~A" (first r) (second r)))
	      (t (print r))))
  (format t "~%"))

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
(print (parse-regex "ab((c|de)+abc|123)*zaza"))
(print (parse-regex "(a|b)|(c|d)"))
(print (parse-regex "(#+|#-)?[0-9]+"))
(print (parse-regex "ab([c-e]+fg)*m"))
(print (parse-regex "[a-z][a-z0-9]*"))


(prs (str2rg "ab(ef)?c" 'S))
(prs (str2rg "ab(ef)+c" 'W))
(prs (str2rg "ab(ef)*c" 'W))
(prs (str2rg "ab(ef)c" 'W))
(prs (str2rg "ab(cd(ef)+h)*ij" 'S))
(prs (str2rg "cd(ef)+h" 'A))
(prs (str2rg "cd[ef]*h" 'A))
(prs (str2rg "cd[ef]+h" 'A))
(prs (str2rg "cd[ef]h" 'A))
(prs (str2rg "ab[efg]c" 'W))
(prs (str2rg "ab[efg]*c" 'W))
(prs (str2rg "ab[ef4-8]*c" 'W))
(prs (str2rg "(koko(lo)?(pu[0-5]*)+)|suso|vova(24|27)" 'K))
(prs (str2rg "ab(e|wf)*c" 'W))
(prs (str2rg "ab(e|wf)+c" 'W))
(prs (str2rg "o(ab){2}c" 'N))
(prs (str2rg "o(ab){1}c" 'N))
(prs (str2rg "ra{5}d" 'P))
(prs (str2rg "[a-d][a-d0-2]*" 'P))
(prs (str2rg "(a|b)?[0-2]+" 'P))
(print (parse-regex "(a|b)?[0-2]+"))


(defun gram2rg (gram &optional acc)
  (if (null gram)
      (nreverse acc)
      (let ((name (caar gram))
	    (expr (cadar gram)))
;;	(format t "~&~A: ~S" name expr)
	(gram2rg (cdr gram) (nconc (expr2rg expr name) acc)))))

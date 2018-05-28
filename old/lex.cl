
(load "data-types.cl")
(load "lex-gram.cl")
(load "lex-rg.cl")

;;; returns automaton to parse the grammar
;;; automaton is a function taking list of characters and returning list of term-items
(defun auto (gram)
  nil					;TODO
  )

;; lex function: takes lex grammar and string, returns the corresponding list of term-items
(defun lex (gram string)
  (let ((a (auto gram))
	(l (coerce string 'list)))
    (funcall a l)))

(defgram tada ()
  n "(#+|#-)?[0-9]+"
  l "#<" 
  g "#>"
  pt "#,|#:|#!|##"
  res "if|begin|end|var|else"
;;  ident "[a-z][a-z0-9]*"
  id "[a-d][a-d0-2]*"
 )

(print tada)

(defparameter tada-rg (gram2rg tada))
(prs tada-rg)

(defun get-rg-map (rg)
  (let ((rg-map (make-hash-table :test 'equalp)))
    (loop
       for rule in rg
       for trm = (first rule)
       for chr = (if (= (length rule) 2) nil (second rule))
       for key = (if (= (length rule) 2) (list trm) (list trm chr))
       for val = (if (= (length rule) 2) (second rule) (third rule))
       for old = (gethash key rg-map)
       for new = (union (list val) old)
       do (progn
	    (setf (gethash key rg-map) new)
	    (if (= (length rule) 3)
		(let ((chars (gethash trm rg-map)))
		  (if (not (member chr chars))
		      (setf (gethash trm rg-map) (cons chr chars)))))))
    rg-map))
    
(defun print-hash-entry (key value)
  (if (listp key)
      (format t "~&~18S ----->     ~S~%" key (first value))
      (format t "~&~S ~S~%" key value)))

(maphash #'print-hash-entry (get-rg-map tada-rg))

;; compare nodes in order to sort
(defun compare-nodes (n1 n2)
  (cond ((and (atom n1) (atom n2) (string< n1 n2)))
	((and (listp n1) (listp n2)) (string< (cadr n1) (cadr n2)))
	((listp n1) (if (string= (cadr n1) n2) 0 (string< (cadr n1) n2)))
	((listp n2) (if (string= n1 (cadr n2)) NIL (string< n1 (cadr n2))))))

;; finds connected nodes through char from nodes
;; char can be null
;; acc may contain no duplicates
;; node-map node+character->node-set: hashmap containing nodes
(defun find-connected (node-map nodes &optional acc)
  (cond ((null nodes) (sort acc #'compare-nodes))
	(t (let* ((n (car nodes))
		  (r (cdr nodes)))
	     (if (member n acc)
		 (find-connected node-map r acc)
		 (find-connected node-map (union (gethash (list n) node-map) r) (cons n acc)))))))

(defparameter nmap (make-hash-table :test 'equalp))
(setf (gethash '(A #\a) nmap) '(A F))   (setf (gethash '(A #\c) nmap) '(D))
(setf (gethash '(B #\a) nmap) '(A))     (setf (gethash '(B #\b) nmap) '(B))
(setf (gethash '(C #\a) nmap) '(B H))   (setf (gethash '(C #\b) nmap) '(C))
(setf (gethash '(D #\a) nmap) '(F C))   (setf (gethash '(D #\c) nmap) '(A B))
(setf (gethash '(E #\a) nmap) '(H))     (setf (gethash '(E #\b) nmap) '(B))
(setf (gethash '(G #\c) nmap) '(H C))   (setf (gethash '(B #\c) nmap) '(C))
(setf (gethash '(A) nmap) '(D H))
(setf (gethash '(B) nmap) '(F))
(setf (gethash '(C) nmap) '(E H))
(setf (gethash '(D) nmap) '(E))
(setf (gethash '(G) nmap) '(B))
(setf (gethash '(H) nmap) '(G))
(find-connected nmap '(A D))

(defun get-reachable (nodes node-map char)
  (let ((from-nodes (find-connected node-map nodes)))
    (format t "~&from-nodes: ~S" from-nodes)
    (if (null char)
	from-nodes
	(labels ((rec (from acc)
		   (cond ((null from) acc)
			 (t (let* ((key (list (car from) char))
				   (reachable (gethash key node-map))
				   (connected (find-connected node-map reachable)))
			      (rec (cdr from) (union connected acc)))))))
	  (sort (rec from-nodes nil) #'compare-nodes)))))
(get-reachable '(A C F) nmap #\b)

(defun get-nodeset-chars (nodeset node-map)
  (let ((result nil))
    (loop
       for node in nodeset
       do (setf result (union result (gethash node node-map))))
    result))

(defun get-accepting-name (node)
  (if (listp node)
      (cadr node)
      nil))

(get-accepting-name 'A)
(get-accepting-name '(A N))

(defun have-accepting (nodes)
  (let ((accepting-node (find-if #'get-accepting-name nodes)))
    (if accepting-node
	(second accepting-node)
	nil)))
(have-accepting '(A B C D (E R) F (G G)))

(defun make-accepting-state (name map)
  (lambda (action &optional input acc)
    (if (equal action 'get-map)
	map
	(cond ((null input) (values (list name acc) nil))
	      (t (let ((res (funcall (gethash (first input) map)
				     nil
				     (rest input)
				     (cons (first input) acc))))
		   (if res res (values (list acc) input))))))))

(defun make-non-accepting-state (map)
  (lambda (action &optional input acc)
    (if (equal action 'get-map)
	map
	(cond ((null input) nil)
	      (t (let ((res (funcall (gethash (first input) map)
				     nil
				     (rest input)
				     (cons (first input) acc))))
		   (if res res nil)))))))

(defun make-state (nodes)
  (let ((map (make-hash-table))
	(name (have-accepting nodes)))
    (if accepting
	(make-accepting-state name map))
	(make-non-accepting-state map)))
	  
(defun get-state-map (state)
  (funcall state 'get-map))

(defun rg2dfa (state nodes nodes-map ns2s-map)
  (loop
     for chr in (get-nodeset-chars nodes nodes-map)
     for nss = (get-reachable nodes nodes-map chr)
     for ch2n-map = (get-state-map state)
     for s = (gethash nss ns2s-map)
     do (if s
	    (setf (gethash chr ch2n-map) s)
	    (let ((ns (make-state nss)))
	      (setf (gethash chr ch2n-map) ns)
	      (setf (gethash nss ns2s-map) ns)
	      (rg2dfa ns nss nodes-map ns2s-map)))))

(defgram ex1 ()
  A "a(b(cb)*|c(bc)*)a"
  B "a[bcd]+b"
  )
;;(print ex1)

(defparameter ex1-rg (gram2rg ex1))
(prs ex1-rg)

(defparameter ex1-rg-map (get-rg-map ex1-rg))
(maphash #'print-hash-entry ex1-rg-map)


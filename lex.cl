;;;; Lex: uses regular grammar to transform a string to a list of terminals

(load "data-types.cl")

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
       for key = (if (= (length rule) 2) (first rule) (list (first rule) (second rule)))
       for val = (if (= (length rule) 2) (second rule) (third rule))
       for old = (gethash key rg-map)
       for new = (union (list val) old)
       do (setf (gethash key rg-map) new))
    rg-map))
    
(print (get-rg-map tada-rg))
(print tada-rg)
       
(defun print-hash-entry (key value)
  (format t "~&~18S ----->     ~S~%" key value))

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
		 (find-connected node-map (union (gethash n node-map) r) (cons n acc)))))))
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


(defparameter nmap (make-hash-table :test 'equalp))
(setf (gethash '(A #\a) nmap) '(A F))   (setf (gethash '(A #\c) nmap) '(D))
(setf (gethash '(B #\a) nmap) '(A))     (setf (gethash '(B #\b) nmap) '(B))
(setf (gethash '(C #\a) nmap) '(B H))   (setf (gethash '(C #\b) nmap) '(C))
(setf (gethash '(D #\a) nmap) '(F C))   (setf (gethash '(D #\c) nmap) '(A B))
(setf (gethash '(E #\a) nmap) '(H))     (setf (gethash '(E #\b) nmap) '(B))
(setf (gethash '(G #\c) nmap) '(H C))   (setf (gethash '(B #\c) nmap) '(C))
(setf (gethash 'A nmap) '(D H))
(setf (gethash 'B nmap) '(F))
(setf (gethash 'C nmap) '(E H))
(setf (gethash 'D nmap) '(E))
(setf (gethash 'G nmap) '(B))
(setf (gethash 'H nmap) '(G))


(defun get-nodeset-chars (nodeset)
  ...)

(defun rg2dfa (state nodes &optional acc)
  (let ((state-chars (get-nodeset-chars nodes)))
    ...))

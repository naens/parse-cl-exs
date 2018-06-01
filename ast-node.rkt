;;; Structure: ast-node
;;;
;;;     Represents AST nodes of different kinds:
;;;
;;;     - regular expression nodes
;;;     - tokens (terminal nodes)
;;;     - non-terminal AST nodes
;;;
;;; Fields:
;;;
;;;     name - The name (or type) of the element
;;;     value - The value of the element
;;;     sub-elements - list of sub-elements
;;;
(define-struct ast-node
  (name value sub-elements)
  #:methods gen:custom-write
  ((define (write-proc ast-node port write-mode)
     (let ((name (ast-node-name ast-node))
           (value (ast-node-value ast-node))
           (sub-elements (ast-node-sub-elements ast-node)))
       (cond (sub-elements
              (fprintf port
                       "{name=~a,value=~a,sub=~a}"
                       name
                       value
                       sub-elements))
             (#t
              (fprintf port
                       "{name=~a,value=~a}"
                       name
                       value)))))))


(print (make-ast-node "asdf" 15 #f))

(print (make-ast-node "asdf" 15 '(a b c d)))

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



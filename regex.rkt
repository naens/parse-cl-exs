;;;; Package: regex
;;;;    Parsing of Regular Expressions and create AST from them.

;;; TODO: use packages
;;; TODO: separator between rules: newline or semicolon?
;;;
;;; Function: parse
;;;
;;;     Parses a regular expression.
;;;
;;;     : regex = <disjunct> ('|', <disjunct>)*
;;;
;;;     Returns its AST.  The input string is a Common Lisp string, but
;;;     internally it transforms it in a list of characters and then
;;;     only works with lists.
;;;
;;; Parameters:
;;;
;;;     string - The input string representing the regular expression
;;;
;;; Returns:
;;;
;;;     The AST of the regular expression.
;;;
(define (parse string)
  ;; parse-char-list works with a list representation of the string
  (define (parse-regex char-list)
    'TODO)
  (parse-regex (string->list s)))


;;; Function: skip
;;;
;;;     Skips separator characters between tokens and syntax elements,
;;;     such as spaces and tabs.  The function should not be called
;;;     when reading strings or escaped characters.
;;;
;;;     In a future version might also skip comments if a syntax for
;;;     comments is defined.
;;;
;;; Parameters:
;;;
;;;     char-list - regular expression (or tail of it) in list form
;;;
;;; Returns:
;;;
;;;     The regular expression following the characters to be skipped.
;;;
(define (skip char-list)
  (cond ((null? char-list) #f)
        ((member (car char-list)
                 (list #\space #\tab))  ; add other characters if needed
         (skip (cdr char-list)))
        (#t char-list)))

;;; Function: parse-char
;;;
;;;     Parses a single character.
;;;
;;; Parameters:
;;;
;;;     char-list - list of characters to parse
;;;     character - expected character
;;;
;;; Returns:
;;;
;;;     ok - boolean: *#t* if matches, *#f* otherwise
;;;     tail - the rest of the char-list after the character
;;;
(define (parse-char char-list character)
  (cond ((null? char-list)
         (values #f #f))
        ((equal (car char-list))
         (values #t (cdr char-list)))
        (#t (values #f #f))))

;;; Function: parse-disjunct
;;;
;;;     Parses a disjunct.
;;;
;;;     : disjunct = <term> (',', <term>)*
;;;
;;; Parameters:
;;;
;;;     char-list - list of characters to parse
;;;
;;; Returns:
;;;
;;;     disjunct - AST node representing the parsed disjunct
;;;     tail - The rest of the regex
;;;
(define (parse-disjunct char-list)
  ;; parses a character list and returns a list of terms in reversed order
  (define (parse-terms char-list acc)
    (let-values (((term term-tail) (parse-term char-list)))
      (if term                 ; if we have a term
          (let-values (((comma-ok comma-tail)
                        (parse-char (skip term-tail) #\,)))
            (if comma-ok        ; if we have a comma
                (parse-terms comma-tail (cons term acc))
                (cons term acc)))
          #f)))
    ;; get list of terms
  (let ((terms-reversed (parse-terms char-list nil)))
    ;; create a disjunct AST node
    'TODO))

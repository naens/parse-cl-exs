;;;; Package: regex
;;;;    Parsing of Regular Expressions and create AST from them.


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
        ((equal? (car char-list) character)
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

;;; Function: parse-term
;;;
;;;     Parses a term.
;;;
;;;     : term = <conjunct> (('&' | '\'), <conjunct>)*
;;;

;;; Function: parse-conjunct
;;;
;;;     Parses a conjunct
;;;
;;;     : conjunct = '~',
;;;     :            (<range> | <string> | <rule> | '(' <regex ')'),
;;;     :            ('*', '?', '+')?
;;;

;;; Function: parse-range
;;;
;;;     Parses a range.
;;;
n;;;     : range = '..', <character> | <character>, '..', <character>?
;;;


;;; Function: parse-charlit
;;;
;;;     Parses a charater literal.
;;;
;;;     : character = <quote>, <char>, <quote> | <number> | <hex-number>
;;;
;;; Parameters:
;;;
;;;     char-list - list of characters to parse
;;;
;;; Returns:
;;;
;;;     character-node - AST node representing the parsed character
;;;     tail - The rest of the regex
;;;
(define (parse-charlit char-list)
  'TODO)


;;; Function: parse-string
;;;
;;;     Parses a string literal.
;;;
;;;     : string = <quote>, <char>*, <quote>
;;;
;;; Parameters:
;;;
;;;     char-list - list of characters to parse
;;;
;;; Returns:
;;;
;;;     character list - list of character AST nodes representing the
;;;     string.
;;;     tail - The rest of the regex
;;;
(define (parse-string char-list)
  ;; reads the character list and returns the list of ast char nodes
  ;; reads until quote or end-of-list
  (define (parse-str-rec chlst acc)
    (cond ((null? chlst) (values acc chlst))
          ((equal? (car chlst) #\') (values acc (cdr chlst)))
          (#t
           (let ((char-node (make-ast-node 'character (car chlst) #f)))
             (parse-str-rec (cdr chlst) (cons char-node acc))))))
  (let-values (((char-ok tail) (parse-char char-list #\')))
    (if char-ok
        (let-values (((nodes tail) (parse-str-rec tail (list))))
          (values (make-ast-node 'string  '() (reverse nodes)) tail))
        (values #f #f)))))

;; test parse-string
(let-values (((ast tail) (parse-string (string->list "'abc'; "))))
  (printf "~%=> ast=~s,  tail=~s" ast tail))

(let-values (((ast tail) (parse-string (string->list "''; "))))
  (printf "~%=> ast=~s,  tail=~s" ast tail))

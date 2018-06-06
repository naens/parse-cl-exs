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
;;;     : range = '..', <character> | <character>, '..', <character>?
;;;

;;; Function parse-number
;;;
;;;     Parses a integer number literal in hexadecimal or decimal format.
;;;     Can be positive or negative
;;;
;;; Parameters:
;;;
;;;     char-list - list of characters to parse
;;;
;;; Returns:
;;;
;;;     number - integer, corresponding to the number literal
;;;     tail - The rest of the regex
;;;
;;
;; number: hex: [-]0x[0-9a-fA-F]+ OR [0-9][0-9a-fA-F]*[hH]
;;         dec: [-] [0-9]+
;;
;; pseudocode:
;;     read sign
;;     if 0x then read hex
;;     otherwise
;;         read until end
;;         if [hH] then convert to hex
;;         if no [hH] then
;;             if contains no [a-fA-F] then convert to dec
;;             otherwise convert to dec before [a-fA-F], tail=first [a-fA-F]
(define (parse-number char-list)
  ;; returns true if character in range '0'-'9'
  (define (is-dec-digit c)
    (and (char>=? c #\0) (char<=? c #\9)))
  ;; returns true if character in range 'a'-'f' or 'A'-'F'
  (define (is-hex-digit c)
    (or
     (and (char>=? c #\a) (char<=? c #\f))
     (and (char>=? c #\A) (char<=? c #\F))))
  ;; converts '0'-'9' to 0-9
  (define (dec-to-num c)
    (- (char->integer c) (char->integer #\0)))
  ;; converts 'a'-'f','A'-'F' to 10-15
  (define (hex-to-num c)
    (if (and (char>=? c #\a) (char<=? c #\f))
        (+ 10 (- (char->integer c) (char->integer #\a)))
        (+ 10 (- (char->integer c) (char->integer #\A)))))
  ;; creates an ast-node from digits
  (define (read-num-tail sign char-list)
    (let-values (((digit-values num-tail ls dec-only)
                  (read-num char-list '() '() '())))
      (cond ((null? digit-values)
             (values #f #f))        ; no hex digits -> not a number
            ((or (equal? (car num-tail) #\h) ; digits and [hH] -> hex
                 (equal? (car num-tail) #\H))
             (values (make-ast-node 'number (number-from-digits sign digit-values 16))))
            ((null? ls)             ; no [hH], no a-f -> normal decimal
             (values (make-ast-node 'number (number-from-digits sign digit-values 10)) num-tail))
            ((null? dec-only)       ; no [hH], has a-f, but no 0-9 -> not a number
             (values #f #f))
            (#t                     ; no [hH], has a-f, has 0-9 -> number from [0-9]+
             (values (make-ast-node 'number (number-from-digits sign dec-only 10)) ls)))))
  ;; reads number, converts characters to digits
  ;; returns
  ;;   * the list of digits
  ;;   * the tail
  ;;   * the start of [a-fA-F]
  ;;   * the list of digits from the start to first [a-fA-F]
  (define (read-num char-list digit-values letters-start dec-only)
    (cond ((null? char-list)
           (values digit-values '() letters-start) dec-only)
          ((is-dec-digit (car char-list))
           (read-num (cdr char-list)
                     (cons (dec-to-num (car char-list)) digit-values)
                     letters-start
                     (if (null? letters-start)
                         (cons (car char-list) dec-only)
                         dec-only)))
          ((is-hex-digit (car char-list))
           (read-num (cdr char-list)
                     (cons (hex-to-num (car char-list)))
                     (if (null? letters-start) char-list letters-start)
                     dec-only))
          (#t (values digit-values char-list letters-start dec-only))))
  (let-values (((sign sign-tail) (parse-char char-list #\-)))
    (let ((tail (if sign sign-tail char-list)))
      (let-values (((fst fst-tail) (parse-char tail #\0)))
        (if fst                             ; start with 0
            (let-values (((sec sec-tail) (parse-char fst-tail #\x)))
              (if sec                       ; start with 0x
                  (let-values (((digit-values num-tail ls dec-only)
                                (read-num sec-tail '() '() '())))
                    (if (null? digit-values) ;no digits -> 0, x belongs to tail
                        (values (make-ast-node 'number 0 sec-tail) #f)
                        (number-from-digits sign digit-values 16))) ; 0x[0-9a-fA-f]+ number
                  (read-num-tail sign fst-tail)))
            (read-num-tail sign tail))))))

(parse-number (string->list "0"))

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
  ;; is either a single character or two quotes
  (define (parse-chlit-inquotes chlist)
    (let-values (((quote1-ok quote1-tail) (parse-char chlist #\')))
      (if quote1-ok
          (let-values (((quote2-ok quote2-tail) (parse-char quote1-tail #\')))
            (if quote2-ok
                (values (make-ast-node 'character #\' #f) quote2-tail)   ; quote literal
                (values #f #f)))                                         ; invalid char lit
          (values (make-ast-node 'character (car char-list) #f) chlist)))) ; valid character
  (let-values (((quote-ok quote-tail) (parse-char char-list #\')))
    (if quote-ok
        (let-values (((chlit-node inquote-tail) (parse-chlit-inquotes quote-tail)))
          (if chlit-node
              (let-values (((endquote-ok endquote-tail) (parse-char char-list #\')))
                (if endquote-ok
                    (values chlit-node endquote-tail)
                    (values #f #f)))    ; no quote after literal
              (values #f #f)))          ; no valid character literal after first quote
        (let-values (((number number-tail) (parse-number char-list)))
          (if number
              (values (make-ast-node 'number number #f) number-tail)
              (values #f #f))))))      ; not a character literal



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

;;;; Package: regex
;;;;    Parsing of Regular Expressions and create AST from them

;;; TODO: use packages
;;;
;;; Function: regex@parse
;;;
;;;     Parses a regular expression:
;;;     : <disjunct> ('|', <disjunct>)*
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
;;;     The AST of the regular expression
;;;
(defun regex@parse (string)
  ;; parse-char-list works with a list representation of the string
  (labels ((parse-regex (char-list)
             'TODO))
    (parse-regex (coerce s 'list))))


;;; Function: regex@skip-char
;;;
;;;     Determines whether the character should be read outside strings.
;;;
;;; Parameters:
;;;
;;;     character - input character to test
;;;
;;; Returns:
;;;
;;;     t if the character is skippable, nil otherwise
;;;
(defun regex@skip-char (character)
  (member character (list #\Space #\Tab  #\Linefeed  #\Return  #\Newline)))

;;; Function: regex@parse-char
;;;
;;;     Parses a single character
;;;
;;; Parameters:
;;;
;;;     char-list - list of characters to parse
;;;     character - expected character
;;;
;;; Returns:
;;;
;;;     ok - boolean, t if matches, nil otherwise
;;;     tail - the rest of the char-list after the character
;;;
(defun regex@parse-char (char-list character)
  (cond ((null char-list)
         (values nil nil))
        ((regex@skip-char (car char-list))
         (regex@parse-char (cdr char-list) character))
        ((equal (car char-list))
         (values t (cdr char-list)))
        (t (values nil nil))))

;;; Function: regex@parse-disjunct
;;;
;;;     Parses a disjunct:
;;;     : <term> (',', <term>)*
;;;
;;; Parameters:
;;;
;;;     char-list - list of characters to parse
;;;
;;; Returns:
;;;
;;;     A regex AST node representing the parsed disjunct.
;;;
(defun regex@parse-disjunct (char-list)
  ;; parses a character list and returns a list of terms in reversed order
  (labels ((parse-terms (char-list acc)
             (let ((term (parse-term)))
               (if (null term)          ; if we have a term
                   nil
                   (multiple-value-bind (ok tail)
                       (regex@parse-char char-list #\,)
                     (if ok             ; if we have a comma
                         (parse-terms tail (cons term acc))
                         (list term)))))))
    ;; get list of terms
    (let ((terms-reversed (parse-terms char-list nil)))
      ;; create a disjunct AST node
      'TODO)))

           

             
  

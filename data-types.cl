;;;; Data Types for Parsing ;;;;


;;; Types with substrings

(defstruct term-item
  name
  substring)

(defstruct ast-node
  non-term-name
  rule
  children)				; children: ast-nodes or term-items


;;; Data Types for Grammars (lex, ebnf)

(defstruct rule
  name
  elements)

(defstruct element
  item				        ; lex: char or range; ebnf: terminal or non-terminal
  quantifier)         ; quantifier: int, ?, *, +

;;; lex elements can have classes or ranges as items.
;;; when converted to structs, expanded to different rules.
(defstruct range      ; ranges for lexer (ex: from 0 to 9 for a digit...)
  chr-from
  chr-to)

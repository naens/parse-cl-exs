;;;; Data Types for Parsing ;;;;


;;; 1. Types with substrings

;; for terminal strings
(defstruct term-item
  name
  substring)

;; for parsing output: ast
(defstruct ast-node
  non-term-name
  rule
  children)				;children: ast-nodes or ast-leaves

(defstruct ast-leaf
  term-name
  substring)

;;; 2. Data Types for Grammars
;;; Grammar types: lex, bnf, ebnf
;;; lex rules and ebnf rules can have zero-elements expressions
(defstruct rule
  name
  elements)

;;; quantifier: int, ?, *, +
;;; lex grammars can have classes or ranges as items.
;;; when converted to structs, expanded to different rules.
(defstruct element
  item				        ;lex: char; bnf and ebnf: terminal or non-terminal
  quantifier)				;default:1, bnf: only 1 is possible

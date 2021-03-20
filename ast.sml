structure AST =
struct
datatype exp = Id of string
| BinExp of  binop * exp * exp 
| UnExp of  unop * exp 
| Const of const
| TermExp of  exp * terminal
| ParenExp of lparen * exp * rparen
| ConditionExp of  exp  * exp  * exp
| CombExp of exp * exp
and binop = OR | AND | XOR | EQUALS | IMPLIES
and unop = NOT
and terminal = TERMINAL
and const = TRUE | FALSE
and lparen = LPAREN
and rparen = RPAREN
end
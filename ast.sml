structure AST =
struct
type id = string


datatype type1 = INT | BOOL | ARROW of type1 * type1

datatype decl = ValDecl of id * exp


and exp = NExp of int
| BExp of bool
| BinExp of  binop * exp * exp
| UnExp of  unop * exp 
| TermExp of  exp * terminal
| ParenExp of lparen * exp * rparen
| ConditionExp of  exp  * exp  * exp
| CombExp of exp * exp
| LetExp of decl * exp
| VarExp of id
| FnExp of id * type1 * type1 * exp
| FunExp of id * id * type1 * type1 * exp
| AppExp of id * exp


and binop = OR | AND | XOR | EQUALS | IMPLIES | PLUS | MINUS | TIMES | LESSTHAN | GREATERTHAN
and unop = NOT | NEGATE 
and terminal = TERMINAL
and lparen = LPAREN
and rparen = RPAREN
end
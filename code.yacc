(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

%%
(* required declarations *)
%name code

%term
  ID of string 
| OR | AND | NOT | XOR  | RPAREN | LPAREN | IMPLIES 
| IF | THEN | ELSE | EQUALS | TRUE  | FALSE  | TERMINAL  | EOF

%nonterm FORMULA of AST.exp | STATEMENT of AST.exp |  START of AST.exp | PROGRAM of AST.exp

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)


%right IF THEN ELSE
%right IMPLIES 
%left AND OR  XOR EQUALS
%right NOT

%start START

%verbose

%%
START: PROGRAM (PROGRAM1)
PROGRAM:      STATEMENT (STATEMENT1)
        | STATEMENT PROGRAM (AST.CombExp(STATEMENT, PROGRAM))
    STATEMENT : FORMULA TERMINAL ((AST.TermExp(FORMULA1, AST.TERMINAL)))

    FORMULA: LPAREN FORMULA RPAREN ((AST.ParenExp(AST.LPAREN, FORMULA1, AST.RPAREN)))
            |IF FORMULA THEN FORMULA ELSE FORMULA  ((AST.ConditionExp(FORMULA1, FORMULA2, FORMULA3)))
            | FORMULA AND FORMULA  (AST.BinExp(AST.AND, FORMULA1,  FORMULA2))
            | FORMULA OR FORMULA  (AST.BinExp(AST.OR, FORMULA1,  FORMULA2))
            | FORMULA XOR FORMULA (AST.BinExp(AST.XOR, FORMULA1,  FORMULA2))
            | TRUE (AST.Const(AST.TRUE))
            | FALSE (AST.Const(AST.FALSE))
            | FORMULA EQUALS FORMULA (AST.BinExp(AST.EQUALS, FORMULA1,  FORMULA2))
            | FORMULA IMPLIES FORMULA (AST.BinExp(AST.IMPLIES, FORMULA1,  FORMULA2))
            | NOT FORMULA (AST.UnExp(AST.NOT, FORMULA1))


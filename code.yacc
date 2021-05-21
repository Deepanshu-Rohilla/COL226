(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

%%
(* required declarations *)
%name code

%term
  ID of string | NUM of int
| OR | AND | NOT | XOR  | RPAREN | LPAREN | IMPLIES | PLUS | MINUS | NEGATE | LESSTHAN | GREATERTHAN
| IF | THEN | ELSE | EQUALS | TRUE  | FALSE  | TERMINAL  | EOF | FI | LET | IN | END | TIMES | FN | FUN
| VAL  | EQ | INT | BOOL | COLON | ARROW

%nonterm EXP of AST.exp | STATEMENT of AST.exp |  START of AST.exp 
| PROGRAM of AST.exp | DECL of AST.decl | TYPE of AST.type1

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)


%right IF THEN ELSE FI LET IN END 
%left FN FUN ARROW
%right IMPLIES 
%left AND OR XOR EQUALS LESSTHAN GREATERTHAN
%left PLUS MINUS 
%left TIMES 
%right NOT NEGATE

%start START

%verbose

%%
START: PROGRAM (PROGRAM1)
DECL:  ID EQ EXP (AST.ValDecl(ID, EXP))
TYPE: INT (AST.INT) | BOOL (AST.BOOL) | TYPE ARROW TYPE (AST.ARROW(TYPE1,TYPE2)) 
PROGRAM:      STATEMENT (STATEMENT1)
        | STATEMENT PROGRAM (AST.CombExp(STATEMENT, PROGRAM))
    STATEMENT : EXP TERMINAL ((AST.TermExp(EXP1, AST.TERMINAL)))

    EXP: LPAREN EXP RPAREN ((AST.ParenExp(AST.LPAREN, EXP1, AST.RPAREN)))
            | IF EXP THEN EXP ELSE EXP FI ((AST.ConditionExp(EXP1, EXP2, EXP3)))
            | EXP AND EXP  (AST.BinExp(AST.AND, EXP1,  EXP2))
            | EXP OR EXP  (AST.BinExp(AST.OR, EXP1,  EXP2))
            | EXP XOR EXP (AST.BinExp(AST.XOR, EXP1,  EXP2))
            | TRUE (AST.BExp(true))
            | FALSE (AST.BExp(false))
            | EXP PLUS EXP (AST.BinExp(AST.PLUS, EXP1,  EXP2))
            | EXP MINUS EXP (AST.BinExp(AST.MINUS, EXP1,  EXP2))
            | EXP TIMES EXP (AST.BinExp(AST.TIMES, EXP1,  EXP2))
            | NUM (AST.NExp(NUM1))
            | EXP EQUALS EXP (AST.BinExp(AST.EQUALS, EXP1,  EXP2))
            | EXP GREATERTHAN EXP (AST.BinExp(AST.GREATERTHAN, EXP1,  EXP2))
            | EXP LESSTHAN EXP (AST.BinExp(AST.LESSTHAN, EXP1,  EXP2))
            | EXP IMPLIES EXP (AST.BinExp(AST.IMPLIES, EXP1,  EXP2))
            | NOT EXP (AST.UnExp(AST.NOT, EXP1))
            | NEGATE EXP (AST.UnExp(AST.NEGATE, EXP1))
            | LET DECL IN EXP END (AST.LetExp(DECL, EXP))
            | ID (AST.VarExp(ID1))
            | FN LPAREN ID COLON TYPE RPAREN COLON TYPE ARROW EXP (AST.FnExp(ID1,TYPE1,TYPE2,EXP1))
            | FUN ID LPAREN ID COLON TYPE RPAREN COLON TYPE ARROW EXP (AST.FunExp(ID1,ID2,TYPE1,TYPE2,EXP1))
            | LPAREN ID EXP RPAREN (AST.AppExp(ID1, EXP1))


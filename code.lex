structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
  val line = ref 0
  val column = ref 0
  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e,l,c) => (
  print("Error at " ^  (Int.toString l) ^ " : " ^ (Int.toString l) ^ " with the error message: Error at " ^ e ^ "\n");
  raise invalidTokenErr("Error at " ^  (Int.toString l) ^ " : " ^ (Int.toString l) ^ " with the error message: Error at " ^ e ^ "\n")
  )
  
%%
%header (functor codeLexFun(structure Tokens:code_TOKENS));
alpha=[A-Za-z];
integer = [0-9];
ws = [\ \t];
%%
\n       => (line := (!line) + 1;
            column := 0;
            pos := (!pos) + 1; lex());
{ws}+    => (lex());
"NOT"    => (column := (!column) + size yytext;
              print("NOT\"NOT\"");
            Tokens.NOT(!line,!column));
"="    => (column := (!column) + size yytext;
              print("EQ\"EQ\"");
            Tokens.EQ(!line,!column));
"PLUS"    => (column := (!column) + size yytext;
              print("PLUS\"PLUS\"");
            Tokens.PLUS(!line,!column));
"MINUS"    => (column := (!column) + size yytext;
              print("MINUS\"MINUS\"");
            Tokens.MINUS(!line,!column));
"TIMES"    => (column := (!column) + size yytext;
              print("TIMES\"TIMES\"");
            Tokens.TIMES(!line,!column));
"NEGATE"    => (column := (!column) + size yytext;
              print("NEGATE\"NEGATE\"");
            Tokens.NEGATE(!line,!column)); 
"LESSTHAN"    => (column := (!column) + size yytext;
              print("LESSTHAN\"LESSTHAN\"");
            Tokens.LESSTHAN(!line,!column));   
"GREATERTHAN"    => (column := (!column) + size yytext;
              print("GREATERTHAN\"GREATERTHAN\"");
            Tokens.GREATERTHAN(!line,!column));  
"AND"    => (column := (!column) + size yytext;
              print("AND\"AND\"");
            Tokens.AND(!line,!column));
"OR"    => (column := (!column) + size yytext;
              print("OR\"OR\"");
            Tokens.OR(!line,!column));
"XOR"    => (column := (!column) + size yytext;
              print("XOR\"XOR\"");
            Tokens.XOR(!line,!column));
"EQUALS"    => (column := (!column) + size yytext;
                print("EQUALS\"EQUALS\"");
            Tokens.EQUALS(!line,!column));
"IMPLIES"    => (column := (!column) + size yytext;
                  print("IMPLIES\"IMPLIES\"");
            Tokens.IMPLIES(!line,!column));
"if"    => (column := (!column) + size yytext;
            print("if\"if\"");
            Tokens.IF(!line,!column));
"then"    => (column := (!column) + size yytext;
              print("then\"then\"");
            Tokens.THEN(!line,!column));
"else"    => (column := (!column) + size yytext;
              print("else\"else\"");
            Tokens.ELSE(!line,!column));
"fi"    => (column := (!column) + size yytext;
              print("fi\"fi\"");
            Tokens.FI(!line,!column));
"let"    => (column := (!column) + size yytext;
              print("let\"let\"");
            Tokens.LET(!line,!column));
"in"    => (column := (!column) + size yytext;
              print("in\"in\"");
            Tokens.IN(!line,!column));
"end"    => (column := (!column) + size yytext;
              print("end\"end\"");
            Tokens.END(!line,!column));
"fn"    => (column := (!column) + size yytext;
              print("fn\"fn\"");
            Tokens.FN(!line,!column));
"fun"    => (column := (!column) + size yytext;
              print("fun\"fun\"");
            Tokens.FUN(!line,!column));
"int"    => (column := (!column) + size yytext;
              print("int\"int\"");
            Tokens.INT(!line,!column));
"bool"    => (column := (!column) + size yytext;
              print("bool\"bool\"");
            Tokens.BOOL(!line,!column));
":"    => (column := (!column) + size yytext;
              print(":\":\"");
            Tokens.COLON(!line,!column));
"=>"    => (column := (!column) + size yytext;
              print("arrow\"arrow\"");
            Tokens.ARROW(!line,!column));
"TRUE"    => (column := (!column) + size yytext;
                print("TRUE\"TRUE\"");
            Tokens.TRUE(!line,!column));
"FALSE"    => (column := (!column) + size yytext;
                print("FALSE\"FALSE\"");
            Tokens.FALSE(!line,!column)); 
"("      => (column := (!column) + size yytext;
              print("LPAREN\"(\"");
            Tokens.LPAREN(!line,!column));
")"      => (column := (!column) + size yytext;
              print("RPAREN\")\"");
              Tokens.RPAREN(!line,!column));
";"      => (column := (!column) + size yytext;
              print("TERMINAL\";\"");
              Tokens.TERMINAL(!line,!column));
{integer}+ => (column := (!column) + size yytext;
                print("NUM \""^ yytext ^ "\"");
            Tokens.NUM(valOf(Int.fromString(yytext)),!line,!column));
{alpha}+ => (column := (!column) + size yytext;
                print("ID \""^ yytext ^ "\"");
            Tokens.ID(yytext,!line,!column));
.     => (error (yytext,!line,!column);
             lex());



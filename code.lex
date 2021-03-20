structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
  val line = ref 0
  val column = ref 0
  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e,l,c) => raise invalidTokenErr("Error at " ^  (Int.toString l) ^ " : " ^ (Int.toString l) ^ " with the error message: Error at " ^ e ^ "\n")
  
%%
%header (functor codeLexFun(structure Tokens:code_TOKENS));
alpha=[A-Za-z];
ws = [\ \t];
%%
\n       => (line := (!line) + 1;
            column := 0;
            pos := (!pos) + 1; lex());
{ws}+    => (lex());
"NOT"    => (column := (!column) + size yytext;
              print("NOT\"NOT\"");
            Tokens.NOT(!line,!column));
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
"IF"    => (column := (!column) + size yytext;
            print("IF\"IF\"");
            Tokens.IF(!line,!column));
"THEN"    => (column := (!column) + size yytext;
              print("THEN\"THEN\"");
            Tokens.THEN(!line,!column));
"ELSE"    => (column := (!column) + size yytext;
              print("ELSE\"ELSE\"");
            Tokens.ELSE(!line,!column));

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
{alpha}+ => (column := (!column) + size yytext;
                print("ID \""^ yytext ^ "\"");
            Tokens.ID(yytext,!line,!column));
.     => (error (yytext,!line,!column);
             lex());





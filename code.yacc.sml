functor codeLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : code_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\017\000\003\000\016\000\005\000\015\000\006\000\027\000\
\\008\000\014\000\012\000\013\000\000\000\
\\001\000\002\000\017\000\003\000\016\000\005\000\015\000\008\000\014\000\
\\010\000\026\000\012\000\013\000\000\000\
\\001\000\002\000\017\000\003\000\016\000\005\000\015\000\008\000\014\000\
\\011\000\029\000\012\000\013\000\000\000\
\\001\000\002\000\017\000\003\000\016\000\005\000\015\000\008\000\014\000\
\\012\000\013\000\015\000\012\000\000\000\
\\001\000\004\000\010\000\007\000\009\000\009\000\008\000\013\000\007\000\
\\014\000\006\000\000\000\
\\001\000\016\000\000\000\000\000\
\\032\000\000\000\
\\033\000\004\000\010\000\007\000\009\000\009\000\008\000\013\000\007\000\
\\014\000\006\000\000\000\
\\034\000\000\000\
\\035\000\000\000\
\\036\000\000\000\
\\037\000\002\000\017\000\003\000\016\000\005\000\015\000\008\000\014\000\
\\012\000\013\000\000\000\
\\038\000\000\000\
\\039\000\000\000\
\\040\000\000\000\
\\041\000\000\000\
\\042\000\000\000\
\\043\000\000\000\
\\044\000\002\000\017\000\003\000\016\000\005\000\015\000\008\000\014\000\
\\012\000\013\000\000\000\
\\045\000\000\000\
\"
val actionRowNumbers =
"\004\000\006\000\007\000\003\000\
\\016\000\015\000\004\000\004\000\
\\004\000\008\000\009\000\004\000\
\\004\000\004\000\004\000\004\000\
\\001\000\000\000\019\000\017\000\
\\018\000\014\000\012\000\013\000\
\\004\000\010\000\002\000\004\000\
\\011\000\005\000"
val gotoT =
"\
\\001\000\003\000\002\000\002\000\003\000\029\000\004\000\001\000\000\000\
\\000\000\
\\001\000\003\000\002\000\002\000\004\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\016\000\000\000\
\\001\000\017\000\000\000\
\\001\000\018\000\000\000\
\\000\000\
\\000\000\
\\001\000\019\000\000\000\
\\001\000\020\000\000\000\
\\001\000\021\000\000\000\
\\001\000\022\000\000\000\
\\001\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\026\000\000\000\
\\000\000\
\\000\000\
\\001\000\028\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 30
val numrules = 14
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ID of unit ->  (string) | PROGRAM of unit ->  (AST.exp)
 | START of unit ->  (AST.exp) | STATEMENT of unit ->  (AST.exp)
 | FORMULA of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result = AST.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 15) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "OR"
  | (T 2) => "AND"
  | (T 3) => "NOT"
  | (T 4) => "XOR"
  | (T 5) => "RPAREN"
  | (T 6) => "LPAREN"
  | (T 7) => "IMPLIES"
  | (T 8) => "IF"
  | (T 9) => "THEN"
  | (T 10) => "ELSE"
  | (T 11) => "EQUALS"
  | (T 12) => "TRUE"
  | (T 13) => "FALSE"
  | (T 14) => "TERMINAL"
  | (T 15) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 
1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PROGRAM PROGRAM1, PROGRAM1left, 
PROGRAM1right)) :: rest671)) => let val  result = MlyValue.START (fn _
 => let val  PROGRAM1 = PROGRAM1 ()
 in (PROGRAM1)
end)
 in ( LrTable.NT 2, ( result, PROGRAM1left, PROGRAM1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.STATEMENT STATEMENT1, STATEMENT1left, 
STATEMENT1right)) :: rest671)) => let val  result = MlyValue.PROGRAM
 (fn _ => let val  STATEMENT1 = STATEMENT1 ()
 in (STATEMENT1)
end)
 in ( LrTable.NT 3, ( result, STATEMENT1left, STATEMENT1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.PROGRAM PROGRAM1, _, PROGRAM1right)) :: ( _,
 ( MlyValue.STATEMENT STATEMENT1, STATEMENT1left, _)) :: rest671)) =>
 let val  result = MlyValue.PROGRAM (fn _ => let val  (STATEMENT as 
STATEMENT1) = STATEMENT1 ()
 val  (PROGRAM as PROGRAM1) = PROGRAM1 ()
 in (AST.CombExp(STATEMENT, PROGRAM))
end)
 in ( LrTable.NT 3, ( result, STATEMENT1left, PROGRAM1right), rest671)

end
|  ( 3, ( ( _, ( _, _, TERMINAL1right)) :: ( _, ( MlyValue.FORMULA 
FORMULA1, FORMULA1left, _)) :: rest671)) => let val  result = 
MlyValue.STATEMENT (fn _ => let val  FORMULA1 = FORMULA1 ()
 in ((AST.TermExp(FORMULA1, AST.TERMINAL)))
end)
 in ( LrTable.NT 1, ( result, FORMULA1left, TERMINAL1right), rest671)

end
|  ( 4, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.FORMULA 
FORMULA1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.FORMULA (fn _ => let val  FORMULA1 = FORMULA1
 ()
 in ((AST.ParenExp(AST.LPAREN, FORMULA1, AST.RPAREN)))
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.FORMULA FORMULA3, _, FORMULA3right)) :: _ ::
 ( _, ( MlyValue.FORMULA FORMULA2, _, _)) :: _ :: ( _, ( 
MlyValue.FORMULA FORMULA1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671
)) => let val  result = MlyValue.FORMULA (fn _ => let val  FORMULA1 = 
FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 val  FORMULA3 = FORMULA3 ()
 in ((AST.ConditionExp(FORMULA1, FORMULA2, FORMULA3)))
end)
 in ( LrTable.NT 0, ( result, IF1left, FORMULA3right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _ ::
 ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671)) =>
 let val  result = MlyValue.FORMULA (fn _ => let val  FORMULA1 = 
FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in (AST.BinExp(AST.AND, FORMULA1,  FORMULA2))
end)
 in ( LrTable.NT 0, ( result, FORMULA1left, FORMULA2right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _ ::
 ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671)) =>
 let val  result = MlyValue.FORMULA (fn _ => let val  FORMULA1 = 
FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in (AST.BinExp(AST.OR, FORMULA1,  FORMULA2))
end)
 in ( LrTable.NT 0, ( result, FORMULA1left, FORMULA2right), rest671)

end
|  ( 8, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _ ::
 ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671)) =>
 let val  result = MlyValue.FORMULA (fn _ => let val  FORMULA1 = 
FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in (AST.BinExp(AST.XOR, FORMULA1,  FORMULA2))
end)
 in ( LrTable.NT 0, ( result, FORMULA1left, FORMULA2right), rest671)

end
|  ( 9, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.FORMULA (fn _ => (AST.Const(AST.TRUE)))
 in ( LrTable.NT 0, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 10, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.FORMULA (fn _ => (AST.Const(AST.FALSE)))
 in ( LrTable.NT 0, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _
 :: ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671))
 => let val  result = MlyValue.FORMULA (fn _ => let val  FORMULA1 = 
FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in (AST.BinExp(AST.EQUALS, FORMULA1,  FORMULA2))
end)
 in ( LrTable.NT 0, ( result, FORMULA1left, FORMULA2right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _
 :: ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671))
 => let val  result = MlyValue.FORMULA (fn _ => let val  FORMULA1 = 
FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in (AST.BinExp(AST.IMPLIES, FORMULA1,  FORMULA2))
end)
 in ( LrTable.NT 0, ( result, FORMULA1left, FORMULA2right), rest671)

end
|  ( 13, ( ( _, ( MlyValue.FORMULA FORMULA1, _, FORMULA1right)) :: ( _
, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.FORMULA (fn _ => let val  FORMULA1 = FORMULA1 ()
 in (AST.UnExp(AST.NOT, FORMULA1))
end)
 in ( LrTable.NT 0, ( result, NOT1left, FORMULA1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : code_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun TERMINAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
end
end

structure codeLrVals = codeLrValsFun(structure Token = LrParser.Token)
structure codeLex = codeLexFun(structure Tokens = codeLrVals.Tokens);
structure codeParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = codeLrVals.ParserData
     	       structure Lex = codeLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,line,col) = (
					 print("Syntax error at line "^Int.toString(line) ^  ":" ^ Int.toString(col) ^ s);
					 raise parseErr("Syntax error at line "^Int.toString(line) ^  ":" ^ Int.toString(col) ^ s)
				 )
				
		in
		    codeParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer fileName =
    let val inStream = TextIO.openIn fileName;

    	val lexer = codeParser.makeLexer(fn n => if TextIO.endOfStream inStream
                                                     then ""
                                                     else TextIO.inputN (inStream,n))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = codeLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = codeParser.Stream.get lexer
    in
        if codeParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer


fun parse fileName = 
	(parseString(fileName);
	print("")
	) handle parseErr msg => print(msg) | 
	invalidTokenErr msg => print(msg) 

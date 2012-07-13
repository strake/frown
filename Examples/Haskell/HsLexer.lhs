-----------------------------------------------------------------------------
--
-- (c) The GHC Team, 1997-2000
--
-- Lexer for Haskell.
--
-----------------------------------------------------------------------------
-- Heavily modified to work with Frown, Ralf Hinze, 2001

ToDo: Parsing floats is a *real* hack...
ToDo: Introduce different tokens for decimal, octal and hexadecimal (?)
ToDo: FloatTok should have three parts (integer part, fraction, exponent)
ToDo: Use a lexical analyser generator (lx?)

> --module HsLexer (Token(..), get, isSymbol) where
> module HsLexer (get, isSymbol) where
> 
> import HsParseMonad
> import HsParseUtils
> import HsSyn(SrcLoc(..))
> --import FiniteMap as FM
> 
> import Char
> 
> reserved_ops :: [(String, Token)] -- FM.FiniteMap Char Token
> -- reserved_ops = FM.fromList [
> reserved_ops = [
>  ( "..", DotDot ),    
>  ( "::", DoubleColon ),
>  ( "=",  Equals ),    
>  ( "\\", Backslash ), 
>  ( "|",  Bar ),       
>  ( "<-", LeftArrow ), 
>  ( "->", RightArrow ),
>  ( "@",  At ),        
>  ( "~",  Tilde ),     
>  ( "=>", DoubleArrow ),
>  ( "-",  Minus ),			--ToDo: shouldn't be here
>  ( "!",  Exclamation )		--ditto
>  ]
> 
> reserved_ids :: [(String, Token)] -- FM.FiniteMap Char Token
> -- reserved_ids = FM.fromList [
> reserved_ids = [
>  ( "_",         Underscore ),
>  ( "case",      KW_Case ),     
>  ( "class",     KW_Class ),    
>  ( "data",      KW_Data ),     
>  ( "default",   KW_Default ),  
>  ( "deriving",  KW_Deriving ), 
>  ( "do",        KW_Do ),       
>  ( "else",      KW_Else ),     
>  ( "if",    	KW_If ),       
>  ( "import",    KW_Import ),   
>  ( "in", 	KW_In ),       
>  ( "infix", 	KW_Infix ),    
>  ( "infixl", 	KW_InfixL ),   
>  ( "infixr", 	KW_InfixR ),   
>  ( "instance",  KW_Instance ), 
>  ( "let", 	KW_Let ),      
>  ( "module", 	KW_Module ),   
>  ( "newtype",   KW_NewType ),  
>  ( "of", 	KW_Of ),       
>  ( "then", 	KW_Then ),     
>  ( "type", 	KW_Type ),     
>  ( "where", 	KW_Where ),    
>  ( "as", 	KW_As ),       
>  ( "qualified", KW_Qualified ),
>  ( "hiding", 	KW_Hiding )
>  ]
> 
> isIdent  c = isAlpha c || isDigit c || c == '\'' || c == '_'
> isSymbol c = elem c ":!#$%&*+./<=>?@\\^|-~"
> isWhite  c = elem c " \n\r\t\v\f"
> 
> data IntKind
>         = Decimal     (String,String)
>         | Octal       (String,String)
>         | Hexadecimal (String,String)
> 
> tAB_LENGTH = 8 :: Int

The source location, (y,x), is the coordinates of the previous token.
col is the current column in the source file.  If col is 0, we are
somewhere at the beginning of the line before the first token.

Setting col to 0 is used in two places: just after emitting a virtual
close brace due to layout, so that next time through we check whether
we also need to emit a semi-colon, and at the beginning of the file,
to kick off the lexer.

> get :: Lex Token
> get =  Lex lexer
> 
> lexer :: (Token -> P a) -> P a
> lexer cont input (SrcLoc y x) col =
>          if col == 0
>             then tab y x True  input
>             else tab y col False input -- throw away old x
>   where
>    	-- move past whitespace and comments
>         tab y x bol [] = 
>         	cont EOF [] (SrcLoc y x) col
>         tab y x bol ('\t':s) =
>         	tab y (nextTab x) bol s
>         tab y x bol ('\n':s) =
>                 newLine cont s y
>         tab y x bol ('-':'-':s) = 
>         	newLine cont (drop 1 (dropWhile (/= '\n') s)) y
>         tab y x bol ('{':'-':s) = nestedComment tab y x bol s
>         tab y x bol (c:s)
>         	| isWhite c = tab y (x+1) bol s
>         	| otherwise = 
>         		if bol 	then lexBOL   cont (c:s) (SrcLoc y x) x
>         			else lexToken cont (c:s) (SrcLoc y x) x
> 
> 	  newLine cont s y =  tab (y+1) 1 True s
> 
> nextTab x = x + (tAB_LENGTH - (x-1) `mod` tAB_LENGTH)

When we are lexing the first token of a line, check whether we need to
insert virtual semicolons or close braces due to layout.

> lexBOL :: (Token -> P a) -> P a
> lexBOL cont s loc@(SrcLoc y x) col context =
>         if need_close_curly then 
>                 -- trace "layout: inserting '}'\n" $
>         	-- Set col to 0, indicating that we're still at the
>         	-- beginning of the line, in case we need a semi-colon too.
>         	-- Also pop the context here, so that we don't insert
>         	-- another close brace before the parser can pop it.
>         	cont VRightCurly s loc 0 (tail context)
>         else if need_semi_colon then
>                 --trace "layout: inserting ';'\n" $
>         	cont SemiColon s loc col context
>         else
>         	lexToken cont s loc col context
>  where
>         need_close_curly =
>         	case context of
>         		[] -> False
>         		(i:_) -> case i of
>         			    NoLayout -> False
>         			    Layout n -> x < n
>         need_semi_colon =
>         	case context of
>         		[] -> False
>         		(i:_) -> case i of
>         			    NoLayout -> False
>         			    Layout n -> x == n
> 
> lexToken :: (Token -> P a) -> P a
> lexToken cont (c:s) loc@(SrcLoc y x') x =
>    -- trace ("lexer: y="++show y++" x="++show x++"\n") $ 
>    case c of
>         -- First the special symbols
>         '(' -> special LeftParen
>         ')' -> special RightParen
>         ',' -> special Comma
>         ';' -> special SemiColon
>         '[' -> special LeftSquare
>         ']' -> special RightSquare
>         '`' -> special BackQuote
>         '{' -> \ctxt -> special LeftCurly (NoLayout : ctxt)
>         '}' -> \stk -> case stk of
>                         (_:ctxt) -> special RightCurly ctxt -- pop context on '}'
>                         []       -> error "Internal error: empty context in lexToken"
> 
>         '\'' -> lexChar cont s loc (x+1)
>         '\"' -> lexString cont s loc (x+1)
> 
>         c | isLower c || c == '_' ->
>         	let 
>         	    (idtail, rest) = span isIdent s
>         	    id = c:idtail
>         	    l_id = 1 + length idtail
>         	in
>         	case lookup id reserved_ids of
>         		Just keyword -> forward l_id keyword rest
>         		Nothing -> forward l_id (VarId id) rest
> 
>           | isUpper c ->
>         	let
>         	    (contail, rest) = span isIdent s
>         	    l_con = 1 + length contail
>         	    con = c:contail
>         	in
>         	case rest of
>         	    '.':c1:s1 
>         	     | isLower c1 ->	-- qualified varid?
>         		let
>         		    (idtail, rest1) = span isIdent s1
>         		    id = c1:idtail
>         		    l_id = 1 + length idtail
>         		in
>         		case lookup id reserved_ids of
>         		   -- cannot qualify a reserved word
>         		   Just keyword ->
>         			forward l_con (ConId con) rest
>         		   Nothing ->
>         			forward (l_con+l_id) (QVarId (con, id))
>         				rest1
> 
>         	     | isUpper c1 ->	-- qualified conid?
>         		let 
>         		    (con1,rest1) = span isIdent s1
>         		    l_con1 = 1 + length con1
>         		in
>         		forward (l_con+l_con1) (QConId (con, (c1:con1))) rest1
> 
>         	     | isSymbol c1 ->	-- qualified symbol?
>         		let
>         		    (symtail, rest1) = span isSymbol s1
>         		    sym = c1 : symtail
>         		    l_sym = 1 + length symtail
>         		in
>         		case lookup sym reserved_ops of
>         		    -- cannot qualify a reserved operator
>         		    Just _  -> 
>         			forward l_con (ConId con) rest
>         		    Nothing -> case c of
>         				':' -> forward (l_con+l_sym) 
>         					(QConSym (con, sym)) rest1
>         				_   -> forward (l_con+l_sym)
>         					(QVarSym (con, sym)) rest1
> 
>         	    _ -> -- not a qualified object
>         		forward l_con (ConId con) rest
> 
>           | isSymbol c ->
>         	let
>         	    (symtail, rest) = span isSymbol s
>         	    sym = c : symtail
>         	    l_sym = 1 + length symtail
>         	in
>         	case lookup sym reserved_ops of
>         	    Just t  -> forward l_sym t rest
>         	    Nothing -> case c of
>         			':' -> forward l_sym (ConSym sym) rest
>         			_   -> forward l_sym (VarSym sym) rest
> 
>           | isDigit c ->
>         	case lexInt (c:s) of
>         	    Decimal     (n,rest) ->
>         		case rest of
>         		    ('.':c2:rest2) | isDigit c2 ->
>         			case lexFloatRest (c2:rest2) of
>                                   Nothing -> \ stk -> fail "Illegal float"
>                                   Just (n2,rest3) -> let f = n ++ ('.':n2)
>         			                       in forward (length f) (FloatTok f) rest3
>                     	    _ -> forward (length n) (IntTok n) rest
>         	    Octal       (n,rest) -> forward (length n) (IntTok n) rest
>         	    Hexadecimal (n,rest) -> forward (length n) (IntTok n) rest
> 
>           | otherwise ->
>         	\ stk -> fail ("illegal character \'" ++ show c ++ "\'\n")
> 
>  where special t = forward 1 t s
>        forward n t s = cont t s loc (x+n)
> 
>        lexFloatRest r = case span isDigit r of
>         		      (r2, 'e':r3) -> lexFloatExp (r2 ++ "e") r3
>         		      (r2, 'E':r3) -> lexFloatExp (r2 ++ "e") r3
>         		      f@(r2,   r3) -> Just f
> 
>        lexFloatExp r1 ('-':r2) = lexFloatExp2 (r1 ++ "-") r2
>        lexFloatExp r1 ('+':r2) = lexFloatExp2 (r1 ++ "+") r2
>        lexFloatExp r1      r2  = lexFloatExp2 r1          r2
> 
>        lexFloatExp2 r1 r2 = case span isDigit r2 of
>                                 ("", _ ) -> Nothing
>                                 (ds, r3) -> Just (r1++ds,r3)
> 			      
> lexToken _ _ _ _ = error "Internal error: empty input in lexToken"
> 
> lexInt ('0':o:d:r) | toLower o == 'o' && isOctDigit d = let (ds,rs)= span isOctDigit r in Octal       ('0':'o':d:ds,rs)
> lexInt ('0':x:d:r) | toLower x == 'x' && isHexDigit d = let (ds,rs)= span isHexDigit r in Hexadecimal ('0':'x':d:ds,rs)
> lexInt r					        = Decimal (span isDigit r)
> 
> lexChar :: (Token -> P a) -> P a
> lexChar cont s loc x stk = case s of
>                     '\\':s -> escapeChar s >>= \(e, s', i) ->
>                               charEnd e s' loc (x + i) stk
>                     c:s    -> charEnd c s  loc (x + 1) stk
>                     []     -> error "Internal error: lexChar"
> 
>   where charEnd c ('\'':s) loc x = cont (Character c) s loc (x+1)
>         charEnd c s        loc x = \ stk -> fail "Improperly terminated character constant"
> 
> lexString :: (Token -> P a) -> P a
> lexString cont s loc@(SrcLoc y _) x = loop "" s x y
>   where
>      loop e s x y = case s of
>             '\\':'&':s  -> loop e s (x+2) y
>             '\\':c:s | isSpace c -> stringGap e s (x+2) y
>         	     | otherwise -> \ stk -> escapeChar (c : s) >>= \(e', s', i) ->
>         		                     loop (e':e) s' (x + i) y stk
>             '\"':s -> cont (StringTok (reverse e)) s loc (x+1)
>             c:s		-> loop (c:e) s (x+1) y
>             []          -> \ stk -> fail "Improperly terminated string"
> 
>      stringGap e s x y = case s of
>         	'\n':s -> stringGap e s 1 (y+1)
>         	'\\':s -> loop e s (x+1) y
>         	c:s' | isSpace c -> stringGap e s' (x+1) y
>         	     | otherwise -> 
>         	       \ stk -> fail "Illegal character in string gap"
>               []     -> error "Internal error: stringGap"

ToDo: \o, \x, \<octal> things.

> escapeChar :: String -> Result (Char, String, Int)
> escapeChar s = case s of

Production charesc from section B.2 (Note: \& is handled by caller)

>   'a':s 	  -> return ('\a',s,2)
>   'b':s 	  -> return ('\b',s,2)
>   'f':s 	  -> return ('\f',s,2)
>   'n':s 	  -> return ('\n',s,2)
>   'r':s 	  -> return ('\r',s,2)
>   't':s 	  -> return ('\t',s,2)
>   'v':s 	  -> return ('\v',s,2)
>   '\\':s        -> return ('\\',s,2)
>   '"':s         -> return ('\"',s,2)
>   '\'':s        -> return ('\'',s,2)

Production ascii from section B.2

>   '^':x@(c:s)   -> cntrl x
>   'N':'U':'L':s -> return ('\NUL',s,4)
>   'S':'O':'H':s -> return ('\SOH',s,4)
>   'S':'T':'X':s -> return ('\STX',s,4)
>   'E':'T':'X':s -> return ('\ETX',s,4)
>   'E':'O':'T':s -> return ('\EOT',s,4)
>   'E':'N':'Q':s -> return ('\ENQ',s,4)
>   'A':'C':'K':s -> return ('\ACK',s,4)
>   'B':'E':'L':s -> return ('\BEL',s,4)
>   'B':'S':s     -> return ('\BS', s,3)
>   'H':'T':s  	  -> return ('\HT', s,3)
>   'L':'F':s 	  -> return ('\LF', s,3)
>   'V':'T':s 	  -> return ('\VT', s,3)
>   'F':'F':s 	  -> return ('\FF', s,3)
>   'C':'R':s 	  -> return ('\CR', s,3)
>   'S':'O':s 	  -> return ('\SO', s,3)
>   'S':'I':s 	  -> return ('\SI', s,3)
>   'D':'L':'E':s -> return ('\DLE',s,4)
>   'D':'C':'1':s -> return ('\DC1',s,4)
>   'D':'C':'2':s -> return ('\DC2',s,4)
>   'D':'C':'3':s -> return ('\DC3',s,4)
>   'D':'C':'4':s -> return ('\DC4',s,4)
>   'N':'A':'K':s -> return ('\NAK',s,4)
>   'S':'Y':'N':s -> return ('\SYN',s,4)
>   'E':'T':'B':s -> return ('\ETB',s,4)
>   'C':'A':'N':s -> return ('\CAN',s,4)
>   'E':'M':s     -> return ('\EM', s,3)
>   'S':'U':'B':s -> return ('\SUB',s,4)
>   'E':'S':'C':s -> return ('\ESC',s,4)
>   'F':'S':s     -> return ('\FS', s,3)
>   'G':'S':s     -> return ('\GS', s,3)
>   'R':'S':s     -> return ('\RS', s,3)
>   'U':'S':s     -> return ('\US', s,3)
>   'S':'P':s     -> return ('\SP', s,3)
>   'D':'E':'L':s -> return ('\DEL',s,4)

Escaped numbers are missing here..

>   _             -> fail "Illegal escape sequence"

Production cntrl from section B.2

> cntrl :: String -> Result (Char, String, Int)
> cntrl (c   :s) | isUpper c = return (chr (ord c - ord 'A'), s,2)
> cntrl ('@' :s)             = return ('\^@', s,2)
> cntrl ('[' :s)             = return ('\^[', s,2)
> cntrl ('\\':s)             = return ('\^\', s,2)
> cntrl (']' :s)             = return ('\^]', s,2)
> cntrl ('^' :s)             = return ('\^^', s,2)
> cntrl ('_' :s)             = return ('\^_', s,2)
> cntrl _                    = fail "Illegal control character"
> 
> nestedComment cont y x bol s =
>    case s of
>       '-':'}':s -> cont y (x+2) bol s
>       '{':'-':s -> nestedComment (nestedComment cont) y (x+2) bol s
>       '\t':s    -> nestedComment cont y (nextTab x) bol s
>       '\n':s    -> nestedComment cont (y+1) 1 True s
>       c:s       -> nestedComment cont y (x+1) bol s
>       []        -> error "Internal error: nestedComment"
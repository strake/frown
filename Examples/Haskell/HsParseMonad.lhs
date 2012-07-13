-----------------------------------------------------------------------------
--
-- (c) The GHC Team 1997-2000
--
-- Monad for the Haskell parser.
--
-----------------------------------------------------------------------------
-- Heavily modified to work with Frown, Ralf Hinze, 2001

> module HsParseMonad           (  Token(..), Result(..), LexContext(..), P, Lex(..), unLex
>                               ,  runTokens, runHsModule
>                               ,  parseError, getSrcLoc, getContext, pushContext, popContext)
> where
>
> import HsSyn
> import Monad

> data Token 
>         = VarId String
>         | QVarId (String,String)
> 	| ConId String
>         | QConId (String,String)
>         | VarSym String
>         | ConSym String
>         | QVarSym (String,String)
>         | QConSym (String,String)
> 	| IntTok String
>         | FloatTok String
> 	| Character Char
>         | StringTok String
> 
> -- Symbols
> 
> 	| LeftParen
> 	| RightParen
> 	| SemiColon
>         | LeftCurly
>         | RightCurly
>         | VRightCurly			-- a virtual close brace
>         | LeftSquare
>         | RightSquare
> 	| Comma
>         | Underscore
>         | BackQuote
> 
> -- Reserved operators
> 
> 	| DotDot
> 	| DoubleColon
> 	| Equals
> 	| Backslash
> 	| Bar
> 	| LeftArrow
> 	| RightArrow
> 	| At
> 	| Tilde
> 	| DoubleArrow
> 	| Minus
> 	| Exclamation
> 
> -- Reserved Ids
> 
> 	| KW_As       
> 	| KW_Case     
> 	| KW_Class    
> 	| KW_Data     
> 	| KW_Default  
> 	| KW_Deriving 
> 	| KW_Do       
> 	| KW_Else     
>         | KW_Hiding
> 	| KW_If       
> 	| KW_Import   
> 	| KW_In       
> 	| KW_Infix    
> 	| KW_InfixL   
> 	| KW_InfixR   
> 	| KW_Instance 
> 	| KW_Let      
> 	| KW_Module   
> 	| KW_NewType  
> 	| KW_Of       
> 	| KW_Then     
> 	| KW_Type     
> 	| KW_Where    
> 	| KW_Qualified
>       | EOF
>         deriving (Eq,Show)

A simple exception monad.

> data Result a                 =  Fail String | Return a
>                                  deriving (Show)

> instance Monad Result where
>     Fail s   >>= _k           =  Fail s
>     Return a >>= k            =  k a
>     return                    =  Return
>     fail                      =  Fail

> instance MonadPlus Result where
>     mzero                     =  fail ""
>     Fail _s `mplus` m         =  m
>     Return a `mplus` _m       =  Return a

> data LexContext               =  NoLayout | Layout Int
> 	                           deriving (Eq, Ord, Show)
>
> type Contexts                 =  [LexContext]

A state monad (in CPS style).

> type P a
>     =  String			-- input string
>     -> SrcLoc			-- location of last token read
>     -> Int			-- current column
>     -> Contexts		-- layout information
>     -> Result a

> {- non Haskell 98

> newtype Lex a                 =  Lex (forall ans . (a -> P ans) -> P ans)
> unLex (Lex x)                 =  x

> run                           :: Lex a -> P a
> run (Lex p)                   =  p (\a -> \i l c s -> Return a)

> -}

> newtype Lex a                 =  Lex ((a -> P Answer) -> P Answer)
> unLex (Lex x)                 =  x

> data Answer                   =  Tokens [Token] | HsModule' HsModule
>
> runTokens                     :: Lex [Token] -> P [Token]
> runTokens (Lex p) i l c s     =  p (\a -> \i l c s -> Return (Tokens a)) i l c s >>= \ (Tokens a) -> return a
>
> runHsModule                   :: Lex HsModule -> P HsModule
> runHsModule (Lex p) i l c s   =  p (\a -> \i l c s -> Return (HsModule' a)) i l c s >>= \ (HsModule' a) -> return a

> instance Monad Lex where
>     return a                  =  Lex (\ cont -> cont a)
>     m >>= k                   =  Lex (\ cont -> unLex m (\ a -> unLex (k a) cont))
>     fail s                    =  Lex (\ cont i l c stk -> fail s)

> parseError                    :: String -> Lex a
> parseError s                  =  Lex (\ cont r (SrcLoc y x) c stk -> 
>                                         fail (show y ++ ":" ++ show x ++ ": " ++ s))

Accessing the state.

> getSrcLoc                     :: Lex SrcLoc
> getSrcLoc                     =  Lex (\cont i l c s -> cont l i l c s)

> getContext                    :: Lex Contexts
> getContext                    =  Lex (\cont i l c s -> cont s i l c s)

> pushContext                   :: LexContext -> Lex ()
> pushContext ctxt              = 
>     Lex (\cont i l c s -> cont () i l c (ctxt:s))

> popContext                    :: Lex ()
> popContext                    =
>     Lex (\ cont i l c stk ->
>            case stk of
>                []      -> error "Internal error: empty context in popContext"
>   	         (_ : s) -> cont () i l c s)

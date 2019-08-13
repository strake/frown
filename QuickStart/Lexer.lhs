> module Lexer where
> import Data.Char
> import Data.Maybe
>
> data Terminal  =  DO
>                |  ELSE
>                |  END
>                |  FUNCTION
>                |  IF
>                |  IN
>                |  LET
>                |  THEN
>                |  VAR
>                |  WHILE
>                |  ASSIGN
>                |  COLON
>                |  COMMA
>                |  CPAREN
>                |  DIV
>                |  EQU
>                |  LST
>                |  MINUS
>                |  OPAREN
>                |  PLUS
>                |  SEMI
>                |  TIMES
>                |  ID   String
>                |  INT  String
>                   deriving (Show)
>
> lexer                         :: String -> [Terminal]
> lexer []                      =  []
> lexer (':' : '=' : cs)        =  ASSIGN  : lexer cs
> lexer (':' : cs)              =  COLON   : lexer cs
> lexer (',' : cs)              =  COMMA   : lexer cs
> lexer (')' : cs)              =  CPAREN  : lexer cs
> lexer ('/' : cs)              =  DIV     : lexer cs
> lexer ('=' : cs)              =  EQU     : lexer cs
> lexer ('<' : '=' : cs)        =  LST     : lexer cs
> lexer ('-' : cs)              =  MINUS   : lexer cs
> lexer ('(' : cs)              =  OPAREN  : lexer cs
> lexer ('+' : cs)              =  PLUS    : lexer cs
> lexer (';' : cs)              =  SEMI    : lexer cs
> lexer ('*' : cs)              =  TIMES   : lexer cs
> lexer (c   : cs) | isAlpha c  =  t : lexer cs'
>   where (n, cs')              =  span isAlphaNum cs
>         t                     =  fromMaybe (ID (c : n)) (lookup (c : n) keywords)
> lexer (c   : cs) | isDigit c  =  INT (c : n) : lexer cs'
>   where (n, cs')              =  span isDigit cs
> lexer (_   : cs)              =            lexer cs

> keywords  =  [  ("do",        DO)
>              ,  ("else",      ELSE)
>              ,  ("end",       END)
>              ,  ("function",  FUNCTION)
>              ,  ("if",        IF)
>              ,  ("in",        IN)
>              ,  ("let",       LET)
>              ,  ("then",      THEN)
>              ,  ("var",       VAR)
>              ,  ("while",     WHILE) ]
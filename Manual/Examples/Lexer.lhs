> module Lexer  (module Terminal1, module Lexer) where
> import Char
> import Terminal1
>
> lexer                         ::  String -> [Terminal]
> lexer []                      =   []
> lexer ('+'  : cs)             =   Addop Plus    : lexer cs
> lexer ('-'  : cs)             =   Addop Minus   : lexer cs
> lexer ('*'  : cs)             =   Mulop Times   : lexer cs
> lexer ('/'  : cs)             =   Mulop Divide  : lexer cs
> lexer ('='  : cs)             =   Equal         : lexer cs
> lexer ('('  : cs)             =   LParen        : lexer cs
> lexer (')'  : cs)             =   RParen        : lexer cs
> lexer (c : cs)
>     | isAlpha c               =   let (s, cs') = span isAlphaNum  cs  in  ident    (c : s) : lexer cs'
>     | isDigit c               =   let (s, cs') = span isDigit     cs  in  numeral  (c : s) : lexer cs'
>     | otherwise               =   lexer cs
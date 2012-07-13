> module Terminal1 where
> import Maybe
>
> data Op                       =   Plus | Minus | Times | Divide
>                                   deriving (Show)
>
> name                          ::  Op -> String
> name Plus                     =   "+"
> name Minus                    =   "-"
> name Times                    =   "*"
> name Divide                   =   "/"
>
> app                           ::  Op -> (Int -> Int -> Int)
> app Plus                      =   (+)
> app Minus                     =   (-)
> app Times                     =   (*)
> app Divide                    =   div
>
> data Terminal                 =   Numeral Int
>                               |   Ident String
>                               |   Addop Op
>                               |   Mulop Op
>                               |   KWLet
>                               |   KWIn
>                               |   Equal
>                               |   LParen
>                               |   RParen
>                               |   EOF
>                                   deriving (Show)
>
> ident, numeral                ::  String -> Terminal
> ident   s                     =   fromMaybe (Ident s) (lookup s keywords)
> numeral s                     =   Numeral (read s)
>
> keywords                      ::  [(String, Terminal)]
> keywords                      =   [ ("let", KWLet), ("in", KWIn) ]
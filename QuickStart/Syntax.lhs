> module Syntax
> where
>
> type Ident    =  String
> type TyIdent  =  String
>
> data Expr  =  Var      Ident
>            |  Block    [Expr]
>            |  Int      String
>            |  Un       UnOp Expr
>            |  Call     Ident [Expr]
>            |  Bin      Expr  BinOp Expr
>            |  Assign   Ident Expr
>            |  IfThen   Expr Expr
>            |  IfElse   Expr Expr Expr
>            |  While    Expr Expr
>            |  Let      [Decl] [Expr]
>               deriving (Show)
>
> data Decl    =  Variable  Ident Expr
>              |  Function  Ident [Ident] Expr
>               deriving (Show)
>
> data UnOp  =  Neg
>               deriving (Show)
> data BinOp =  Add | Sub | Mul | Div | Leq | Eq
>               deriving (Show)
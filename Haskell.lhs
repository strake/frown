%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%   Frown --- An LALR(k) parser generator for Haskell 98                      %
%   Copyright (C) 2001-2005 Ralf Hinze                                        %
%                                                                             %
%   This program is free software; you can redistribute it and/or modify      %
%   it under the terms of the GNU General Public License (version 2) as       %
%   published by the Free Software Foundation.                                %
%                                                                             %
%   This program is distributed in the hope that it will be useful,           %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of            %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             %
%   GNU General Public License for more details.                              %
%                                                                             %
%   You should have received a copy of the GNU General Public License         %
%   along with this program; see the file COPYING.  If not, write to          %
%   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,          %
%   Boston, MA 02111-1307, USA.                                               %
%                                                                             %
%   Contact information                                                       %
%   Email:      Ralf Hinze <ralf@cs.uni-bonn.de>                              %
%   Homepage:   http://www.informatik.uni-bonn.de/~ralf/                      %
%   Paper mail: Dr. Ralf Hinze                                                %
%               Institut für Informatik III                                   %
%               Universität Bonn                                              %
%               Römerstraße 164                                               %
%               53117 Bonn, Germany                                           %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> module Haskell                -- (  module Haskell, module Atom  )
> where
> import Base
> import Lexer2
> import Atom                   hiding (  string  )
> import qualified Atom
> import Prettier
> import Data.Char
> import Prelude                hiding (  concat, (<>)  )

%-------------------------------=  --------------------------------------------
\section{Expressions}
%-------------------------------=  --------------------------------------------

> type Quoted                   =  [Token]
>
> type Type                     =  Expr
>
> type Pat                      =  Expr
>
> data Expr                     =  Var Ident
>                               |  Con Ident
>                               |  Literal Literal
>                               |  Tuple [Expr]
>                               |  List [Expr]
>                               |  Case Expr [(Pat, Expr)]
>                               |  Let [Decl] Expr
>         		        |  Fun [Pat] Expr
>                               |  App Expr Expr
>                               |  Infix Expr Ident Expr
>                               |  Quoted Quoted

Specifically for patterns.

>         		        |  As Ident Pat
>                               |  Guard Pat Expr
>                               |  TypeOf Quoted Quoted -- deprecated
>                                  deriving (Eq, Ord, Show)

> instance Pretty Expr where
>     prettyPrec d (Var i)      =  prettyPrec d i
>     prettyPrec d (Con i)      =  prettyPrec d i
>     prettyPrec d (Literal l)  =  prettyPrec d l
>     prettyPrec _d (Tuple es)  =  parens (sepBy (char ',' <> nl) es)
>     prettyPrec _d (List es)   =  brackets (sepBy (char ',' <> nl) es)
>     prettyPrec d (Case e cs)	=  condParens (d > 1)
>                               $  block 4 (string "case " <> nest 5 (pretty e) <> string " of"
>                                           <+> string "{" <> nl
>                                           <> intersperse sep
>                                               [ block 4 (pretty p <> nl <> string "->" <> sp <> nest 3 (pretty e'))
>                                               | (p, e') <- cs ] <+> string "}")
>         where sep		=  string ";" <> nl
>     prettyPrec d (Let ds e)   =  condParens (d > 1)
>                               $  group (string "let " <> string "{"
>                                         <> nest 6 (nl <> sepBy sep ds)
>                                         <> nest 4 (nl <> string "}") <> nl
>                                         <> string "in" <> nest 3 (sp <> pretty e))
>         where sep		=  string ";" <> nl
>     prettyPrec d (Fun ps e)	=  condParens (d > 1)
>                               $  block 4 (string "\\" <> nl <> prettyMany ps <> nl
>                                           <> string "->" <> nl <> pretty e)
>     prettyPrec d (App e1 e2)  =  condParens (d > 9)
>                               $  block 4 (prettyPrec 9 e1 <> nl <> prettyPrec 10 e2)
>     prettyPrec d (Infix e1 op e2)
>				=  condParens (d > 8)
>                               $  block 4 (prettyPrec 9 e1 <> nl <> pretty op <> nl <> prettyPrec rprec e2)
>         where rprec           =  if op == ident "->" then 8 else 9
>     prettyPrec _d (As i p)    =  block 4 (prettyPrec 10 i <> string "@" -- TODO: parens
>                                           <> nest (length (Atom.string i) + 1) (prettyPrec 10 p))
>     prettyPrec _d (Guard p e) =  block 4 (pretty p <+> string "|" <+> pretty e)
> {-
>     prettyPrec d (TypeOf _ _) =  string "-"
>     prettyPrec d (Quoted _)   =  string "-"
> -}
>     prettyPrec d (TypeOf ts us)
>                               =  condParens (d > 9)
>                               $  block 4 (quote ts <> nl <> string "::" <> nl <> quote us)
>     prettyPrec _d (Quoted [t])
>                               =  condParens (not (isVarid t || isConid t))
>                               $  string (toString t)
>     prettyPrec _d (Quoted ts) =  parens (concat [ string (toString t) | t <- ts ])

Smart constructors.

> con, var                      :: String -> Expr
> con (c : s)                   =  Con (ident (toUpper c : s))
> con _                         =  impossible "Haskell.con"
> var (c : s)                   =  Var (ident (toLower c : s))
> var _                         =  impossible "Haskell.var"
>
> stringLiteral                 :: String -> Expr
> stringLiteral s               =  Literal (stringLit s)
>
> tuple                         :: [Expr] -> Expr
> tuple [e]                     =  e
> tuple es                      =  Tuple es
>
> hsInfix                       :: Expr -> String -> Expr -> Expr
> hsInfix e1 s e2               =  Infix e1 (ident s) e2
>
> quote                         :: [Token] -> Doc
> quote ts                      =  concat [ string (toString t) | t <- ts ]

%-------------------------------=  --------------------------------------------
\section{Declarations and Bindings}
%-------------------------------=  --------------------------------------------

> data Decl                     =  DataDecl Type [(Ident, [Type])]
>                               |  TypeDecl Type Type
>                               |  FunBind Expr Expr
>                               |  Sig [Ident] Type
>                               |  Empty
>                               |  AComment [String]
>                               |  Raw String
>                                  deriving (Eq, Ord, Show)

> instance Pretty Decl where
>     prettyPrec _d (DataDecl t cs)
>                               =  string "data" <+> nest 5 (pretty t)
>                               <> block 4 (nl <> equals
>                                           <+> intersperse (nl <> string "|" <> sp) (map prettyConstruct cs))
>     prettyPrec _d (TypeDecl t t')
>                               =  string "type" <+> nest 5 (pretty t)
>                               <> block 4 (nl <> equals <+> pretty t')
>     prettyPrec _d (FunBind lhs rhs)
>                               =  pretty lhs
>                               <> block 4 (nl <> equals <+> nest 2 (pretty rhs))
>     prettyPrec _d (Sig ids t) =  block 4 (intersperse (string "," <> nl) (map pretty ids)
>                                           <> nl <> string "::" <> sp <> pretty t)
>     prettyPrec _d Empty       =  empty
>     prettyPrec _d (AComment ss)
>                               =  group (string "{-" <> intersperse nl [ nest 2 (string s) | s <- ss ] <> string "-}")
>     prettyPrec _d (Raw s)     =  string s

> prettyConstruct               :: (Pretty a) => (Ident, [a]) -> Doc
> prettyConstruct (c, [])	=  pretty c
> prettyConstruct (c, ts)	=  block 5 (pretty c <> nl <> prettyMany ts)

%-------------------------------=  --------------------------------------------
\section{Shorthands}
%-------------------------------=  --------------------------------------------

> local                         :: [Decl] -> Expr -> Expr
> local [] e                    =  e
> local ds e                    =  Let ds e

Abbreviations for Haskell names.

> anon                          :: Pat
> anon                          =  var "_"

> hsUndefined, hsReturn, hsFail, hsPutStrLn, hsShow, hsNil, hsHead, hsTail
>                               :: Expr
> hsUndefined                   =  var "undefined"
> hsReturn                      =  var "return"
> hsFail                        =  var "fail"
> hsPutStrLn                    =  var "putStrLn"
> hsShow                        =  var "show"
> hsNil                         =  con "[]"
> hsHead                        =  var "head"
> hsTail                        =  var "tail"

Abbreviations for Haskell operators.

> infix 7 <=>>
> (<=>>)                        :: Expr -> Expr -> Expr
> e1 <=>> e2                    =  hsInfix e1 "=>" e2
>
> infixr 8 <->>
> (<->>)                        :: [Expr] -> Expr -> Expr
> [] <->> e'                    =  e'
> (e : es) <->> e'              =  hsInfix e "->" (es <->> e')

> infixl 9 <$>
> (<$>)                         :: Expr -> [Expr] -> Expr
> e <$> es                      =  foldl App e es
> -- e <$> []                      =  e
> -- e <$> es                      =  apply e es

> infixr 5 <:>
> (<:>)                         :: Expr -> Expr -> Expr
> e1 <:> e2                     =  hsInfix e1 ":" e2
>
> infixr 5 <++>
> (<++>)                        :: Expr -> Expr -> Expr
> e1 <++> e2                    =  hsInfix e1 "++" e2
>
> infixl 1 <>>>, <>>=>
> (<>>>), (<>>=>)               :: Expr -> Expr -> Expr
> e1 <>>> e2                    =  hsInfix e1 ">>" e2
> e1 <>>=> e2                   =  hsInfix e1 ">>=" e2

> infixr 3 <&&>
> (<&&>)                        :: Expr -> Expr -> Expr
> e1 <&&> e2                    =  hsInfix e1 "&&" e2

Float guarded patterns to the top-level.

> funbind                       :: Expr -> Expr -> Decl
> funbind lhs rhs               =  FunBind (guard lhs' gs) rhs
>   where (lhs', gs)            =  guards lhs

> switch                        :: Expr -> [(Pat, Expr)] -> Expr
> switch e cs                   =  Case e [ (guard p' gs, e') | (p, e') <- cs, let (p', gs) =  guards p ]

> guards                        :: Pat -> (Pat, [Expr])
> guards (Var s)                =  (Var s, [])
> guards (Con s)                =  (Con s, [])
> guards (Literal s)            =  (Literal s, [])
> guards (Tuple ps)             =  (Tuple (map fst ps'), concatMap snd ps')
>   where ps'                   =  map guards ps
> guards (List ps)              =  (List (map fst ps'), concatMap snd ps')
>   where ps'                   =  map guards ps
> guards (App p q)              =  (App p' q', gp ++ gq)
>   where (p', gp)              =  guards p
>         (q', gq)              =  guards q
> guards (As x p)               =  (As x p', gp)
>   where (p', gp)              =  guards p
> guards (Infix p x q)          =  (Infix p' x q', gp ++ gq)
>   where (p', gp)              =  guards p
>         (q', gq)              =  guards q
> guards (Guard p q)            =  (p, [App q p]) -- TODO: `|p|' should be an expression
> --  where [Varid s]             =  filter (not . isWhite) qs -- Hack: should introduce an as-pattern with a fresh variable
> guards (Quoted hs)            =  (Quoted hs, [])
> guards e                      =  impossible ("Haskell.guards: " ++ show e)

> guard                        :: Expr -> [Expr] -> Expr
> guard e []                   =  e
> guard e gs                   =  Guard e (foldr1 (<&&>) gs)

%-------------------------------=  --------------------------------------------
\section{Helper functions}
%-------------------------------=  --------------------------------------------

Should go to |Pretty|:

> equals                        :: Doc
> equals                        =  string "="

TODO: better names.

> sepBy                         :: (Pretty a) => Doc -> [a] -> Doc
> sepBy sep as                  =  intersperse sep (map pretty as)
>
> prettyMany                    :: (Pretty a) => [a] -> Doc
> prettyMany as                 =  intersperse nl (map (prettyPrec 10) as)

> replace                       :: Expr -> Expr -> Expr
> replace _e (Var s)            =  Var s
> replace _e (Con s)            =  Con s
> replace _e (Literal s)        =  Literal s
> replace e (Tuple ps)          =  Tuple (map (replace e) ps)
> replace e (List  ps)          =  List  (map (replace e) ps)
> replace e (App p q)           =  App (replace e p) (replace e q)
> replace e (Guard p q)         =  Guard (replace e p) q
> replace e (TypeOf _ _)        =  e
> replace e (Quoted _)          =  e
> replace _ _                   =  impossible "Haskell.replace"

> combine                       :: Expr -> [Expr] -> Expr
> combine p vs                  =  fst (combine' p vs)
>
> combine'                      :: Expr -> [Expr] -> (Expr, [Expr])
> combine' (Var s) vs           =  (Var s, vs)
> combine' (Con s) vs           =  (Con s, vs)
> combine' (Literal s) vs       =  (Literal s, vs)
> combine' (Tuple ps) vs        =  let (ps', vs') = combineMany' ps vs
>                                  in  (Tuple ps', vs')
> combine' (List  ps) vs        =  let (ps', vs') = combineMany' ps vs
>                                  in  (List ps', vs')
> combine' (App p q) vs         =  let (p', vs1) = combine' p vs
>                                      (q', vs2) = combine' q vs1
> 
>                                  in  (App p' q', vs2)
> combine' (Guard p q) vs       =  let (p', vs') = combine' p vs
>                                  in  (Guard p' q, vs')
> combine' (TypeOf _ _) vs      =  (head vs, tail vs)
> combine' (Quoted _) vs        =  (head vs, tail vs)
> combine' _ _                  =  impossible "Haskell.combine'"

> combineMany'                  :: [Expr] -> [Expr] -> ([Expr], [Expr])
> combineMany' [] vs            =  ([], vs)
> combineMany' (p : ps) vs      =  let (p', vs1) = combine' p vs
>                                      (ps', vs2) = combineMany' ps vs1
>                                  in  (p' : ps', vs2)

> quotes                        :: Pat -> [Expr]
> quotes (Var _s)               =  []
> quotes (Con _s)               =  []
> quotes (Literal _s)           =  []
> quotes (Tuple ps)             =  concatMap quotes ps
> quotes (List ps)              =  concatMap quotes ps
> quotes (App p q)              =  quotes p ++ quotes q
> quotes (Guard p _q)           =  quotes p
> quotes (Quoted hs)            =  [Quoted hs]
> quotes _                      =  impossible "Haskell.quotes"

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

> module Grammar                (  module Grammar  )
> where
> --import Lexer
> import Atom                   hiding (  string  )
> import Haskell
> import Prettier               hiding (  strip  )
> import qualified Prettier as PP
> import Base
> import Prelude                hiding (  concat, (<>)  )

%-------------------------------=  --------------------------------------------
\section{Modifier}
%-------------------------------=  --------------------------------------------

The terminal symbols on the RHS may have a modifier such as |insert|
or |prec|.

> data Modifier                 =  Copy | Insert | Delete | Prec
>                                  deriving (Eq, Ord, Show)

%-------------------------------=  --------------------------------------------
\section{Associativity and precedence}
%-------------------------------=  --------------------------------------------

A symbol may have an associativity and a precedence (or priority)
attached to it.

> data Assoc                    =  LeftAssoc  { aprec :: Int }
>                               |  RightAssoc { aprec :: Int }
>                               |  NonAssoc   { aprec :: Int }
>                               |  Unspecified
>                                  deriving (Eq, Ord, Show)
>
> instance Pretty Assoc where
>     pretty (LeftAssoc i)      =  block 4 (string "left" </> pretty i)
>     pretty (RightAssoc i)     =  block 4 (string "right" </> pretty i)
>     pretty (NonAssoc i)       =  block 4 (string "nonassoc" </> pretty i)
>     pretty Unspecified        =  string "-"
>
> prec                          :: Assoc -> Maybe Int
> prec (LeftAssoc  i)           =  Just i
> prec (RightAssoc i)           =  Just i
> prec (NonAssoc   i)           =  Just i
> prec Unspecified              =  Nothing

%-------------------------------=  --------------------------------------------
\section{Grammar symbols}
%-------------------------------=  --------------------------------------------

A symbol is either a terminal or a nonterminal.

> data Symbol                   =  Terminal    { number     :: Int
>                                              , pattern    :: Pat
>                                              , attributes :: [Expr]
>                                              , types      :: [Type]
>                                              , isEOF      :: Bool
>                                              , modifier   :: Modifier
>                                              , assoc      :: Assoc
>                                              , shorthand  :: Maybe Literal }
>                               |  Nonterminal { number     :: Int
>                                              , name       :: Ident
>                                              , arguments  :: [Symbol]
>                                              , attributes :: [Expr]
>                                              , types      :: [Type]
>                                              , isStart    :: Bool }
>                                  deriving (Show)

> instance Eq Symbol where
>     Terminal { number = n1, modifier = m1 } == Terminal { number = n2, modifier = m2 }
>                               =  n1 == n2 && m1 == m2
>     Nonterminal { number = n1, arguments = args1 } == Nonterminal { number = n2, arguments = args2 }
>                               =  n1 == n2 && and [ a1 == a2 | (a1, a2) <- zip args1 args2 ]
>     _ == _                    =  False
>
> instance Ord Symbol where
>     compare (Terminal { number = n1, modifier = e1 }) (Terminal { number = n2, modifier = e2 })
>                               =  compare (n1, e1) (n2, e2)
>     compare (Terminal {}) (Nonterminal {})
>                               =  LT
>     compare (Nonterminal {}) (Terminal {})
>                               =  GT
>     compare (Nonterminal { number = n1, arguments = args1  }) (Nonterminal { number = n2, arguments = args2 })
>                               =  compare n1 n2 `cmp` foldr cmp EQ [ compare a1 a2 | (a1, a2) <- zip args1 args2 ]
>
> cmp                                  :: Ordering -> Ordering -> Ordering
> cmp LT _ord                          =  LT
> cmp EQ ord                           =  ord
> cmp GT _ord                          =  GT
>
> instance Pretty Symbol where
>     prettyPrec _d (Terminal{ pattern = p, modifier = Copy})
>                               =  pretty (strip p)
>     prettyPrec _d (Terminal{ pattern = p, modifier = Insert})
>                               =  block 4 (string "insert" </> pretty (strip p))
>     prettyPrec _d (Terminal{ pattern = p, modifier = Delete})
>                               =  block 4 (string "delete" </> pretty (strip p))
>     prettyPrec _d (Terminal{ pattern = p, modifier = Prec})
>                               =  block 4 (string "prec" </> pretty (strip p))
>     prettyPrec d (Nonterminal{ name = n, arguments = args, attributes = attrs })
>                               =  block 4 (nt <> concat [ nl <> string "{" <> pretty a <> string "}" | a <- attrs ])
>       where nt | null args    =  pretty n
>                | otherwise    =  condParens (d > 9)
>                               $  pretty n <> concat [ nl <> prettyPrec 10 a | a <- args ]

> strip                         :: Pat -> Pat
> --strip p                       =  replace (Var "-") p
> strip p                       =  p

> nonterminal, terminal         :: Symbol -> Bool
> nonterminal (Terminal    {})  =  False
> nonterminal (Nonterminal {})  =  True
>
> terminal (Terminal    {})     =  True
> terminal (Nonterminal {})     =  False

%-------------------------------=  --------------------------------------------
\section{Productions and grammars}
%-------------------------------=  --------------------------------------------

Productions.

> type Prec                     =  Maybe Int
>
> data Rule                     =  Rule { rnumber :: Int
>                                       , rlhs    :: Symbol
>                                       , rrhs    :: [Symbol]
>                                       , rprec   :: Prec }
>                                  deriving (Eq, Ord, Show)
>
> instance Pretty Rule where
>     prettyPrec d (Rule i n r p)
>                               =  condParens (d > 9)
>                               $  group (PP.string s
>                                         <> nest (length s) (pretty n <+> PP.string ":"
>                                                             <+> intersperse (PP.string "," <> nl)
>                                                                     (map pretty r))
>                                                             <+> prettyPrec 10 p)
>         where s               =  "[" ++ rjustify 3 (show i) ++ "] "

Grammars.

> data Grammar                  =  Grammar { terminals    :: [Symbol]
>                                          , nonterminals :: [Symbol]
>                                          , startSymbols :: [Symbol]
>                                          , productions  :: [Rule] }
>                                  deriving (Eq, Ord, Show)
>
> instance Pretty Grammar where
>     pretty g                  =  header "Terminals"
>                                  <> pretty (terminals g) <> nl <> nl
>                                  <> header "Nonterminals"
>                                  <> pretty (nonterminals g) <> nl <> nl
>                                  <> header "Start symbols"
>                                  <> pretty (startSymbols g) <> nl <> nl
>                                  <> header "Productions"
>                                  <> pretty (productions g)

> symbols                       :: Grammar -> [Symbol]
> symbols g                     =  terminals g ++ nonterminals g

> productionsOf                 :: Grammar -> Symbol -> [Rule]
> productionsOf g n             =  [ p | p@(Rule _ n' _ _) <- productions g, n' == n ]

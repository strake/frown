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

%-------------------------------=  --------------------------------------------
\section{|Generate.lhs|}
%-------------------------------=  --------------------------------------------

> module Generate
> where
> import Atom
> import Lexer2
> import Haskell
> import Grammar
> import LR0
> import qualified OrdUniqListSet as Set
> import OrdUniqListSet         (  Set  )
> import Base
> --import Char                   (  toLower, toUpper  )
> import Data.List                   (  intersperse  )

Symbols.

> argsOf                        :: Symbol -> [Expr]
>-- argsOf                        =  map fst . quotesOf . pattern
> argsOf                        =  attributes
>
> typesOf                       :: Symbol -> [Type]
>-- typesOf                       =  map snd . quotesOf . pattern
> typesOf                       =  types

> fresh, anonymous, bottoms     :: Symbol -> Pat
> fresh v                       =  combine (pattern v) [ v_i i | i <- [1 ..] ]
> anonymous v                   =  replace anon (pattern v)
> bottoms v                     =  replace hsUndefined (pattern v)

> genVars                       :: Symbol -> [Expr]
> genVars v                     =  [ v_i i | i <- [1 .. length (typesOf  v)] ]

Monadic actions.

> --eval args e                   =  foldr eval e (zip [1 ..] args)
> --    where eval (i, ts) x      =  ts <>>=> Fun [v_i i] x

> evaluate                      :: [Pat] -> ([Pat] -> Expr) -> Expr
> evaluate args cont            =  eval ([ (v_i i, arg) | (i, arg) <- zip [1 ..] args]) []
>   where eval [] es            =  cont es
>         eval ((v, ts) : as) es=  case monadic ts of
>                                      Nothing  -> eval as (es ++ [ts])
>                                      Just ts' -> ts' <>>=> Fun [v] (eval as (es ++ [v]))

> monadic                       :: Expr -> Maybe Expr
> monadic (TypeOf (Varsym "%" : ts) us)
>                               =  Just (TypeOf ts us)
> monadic (Quoted (Varsym "%" : ts))
>                               =  Just (Quoted ts)
> monadic _                     =  Nothing

Expected tokens.

> expected                      :: Set Symbol -> Expr
> expected lookahead            =  List [ Literal s | t <- Set.toList lookahead, Just s <- [shorthand t] ]
> --expected lookahead            =  List [ bottoms t | t <- Set.toList lookahead ]

Helper functions.

> unCon, unVar                  :: Expr -> Ident
> unCon (Con x)                 =  x
> unCon _                       =  impossible "Generate.unCon"
> unVar (Var x)                 =  x
> unVar _                       =  impossible "Generate.unVar"

> mangle                        :: Symbol -> String
> mangle v                      =  show (number v)     -- number
> smangle                       :: State -> String
> smangle s                     =  show (snumber s)
> imangle                       :: Item -> String
> imangle i                     =  show (inumber i)
> pmangle                       :: Action -> String
> pmangle p                     =  show (pnumber p)

> vmangle                       :: Int -> Symbol -> String
> vmangle i v                   =  concat (glue (safeName v : map (vmangle (i + 1)) (safeArguments v)))
>   where
>   glue                        =  intersperse (replicate i '_')
>
>   safeName                    :: Symbol -> String
>   safeName (Terminal { number = n })
>                               =  show n
>   safeName (Nonterminal { name = s, types = ts })
>                               =  string s ++ "'" ++ show (length ts)
>
>   safeArguments               :: Symbol -> [Symbol]
>   safeArguments (Terminal {}) =  []
>   safeArguments (Nonterminal { arguments = vs })
>                               =  vs

> asPat                         :: Expr -> Pat -> Expr
> asPat (Var x) p               =  As x p
> asPat _ _                     =  impossible "Generate.asPat"

> {-
> consOf                        :: Expr -> Ident
> consOf (Var s)                =  s
> consOf (App p _p')            =  consOf p
> consOf _                      =  impossible "Generate.consOf"

> hasHash                       :: Expr -> Bool
> hasHash e                     =  head (consOf e) == '#'

> rmHash                        :: Expr -> Ident
> rmHash e                      =  tail (consOf e)
> -}

Names. NB. |v_i| should only be used if there is \emph{no} danger of
name capture.

> v_i                           :: Int -> Expr
> v_i i                         =  var ("v" ++ show i)

User supplied functions.

> hsGet, hsFrown, result_tcon, terminal_tcon
>                               :: Expr
> hsGet                         =  var "get"
> hsFrown                       =  var "frown"
> result_tcon                   =  con "Result"
> terminal_tcon                 =  con "Terminal"

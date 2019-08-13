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

> module Atom                   (  SrcLoc(..), showLoc
>                               ,  Ident, ident, identAt, prime, string, isPrimed, identSrcLoc
>                               ,  Literal, numeral, charLit, stringLit, numeralAt, charLitAt, stringLitAt, litSrcLoc  )
> where
> import Prettier               hiding (  string  )
> import qualified Prettier as PP
> import Prelude hiding         ((<>))

> data SrcLoc                   =  At Int Int
>                               |  Unknown
>                               deriving (Show)
>
> instance Pretty SrcLoc where
>     prettyPrec _d (At ln col) =  PP.string (showLoc ln col)
>     prettyPrec _d Unknown     =  PP.string "<unknown>"
>
> showLoc                       :: Int -> Int -> String
> showLoc line col              =  "(line "  ++ show line ++ ", column " ++ show col ++ ")"

%-------------------------------=  --------------------------------------------
\section{Identifier}
%-------------------------------=  --------------------------------------------

> data Ident                    =  Ident String SrcLoc
>                               |  Prime Ident
>
> instance Eq Ident where
>     Ident s1 _ == Ident s2 _  =  s1 == s2
>     Prime i1   == Prime i2    =  i1 == i2
>     _          == _           =  False
>
> instance Ord Ident where
>     compare (Ident s1 _) (Ident s2 _)
>                               =  compare s1 s2
>     compare (Ident _ _) (Prime _)
>                               =  LT
>     compare (Prime _) (Ident _ _)
>                               =  GT
>     compare (Prime i1) (Prime i2)
>                               =  compare i1 i2
>
> instance Show Ident where
>     showsPrec d (Ident s _)   =  showParen (d > 9) (showString "ident " . shows s)
>     showsPrec d (Prime i)     =  showParen (d > 9) (showString "prime " . showsPrec 10 i)
>
> instance Pretty Ident where
>     prettyPrec _d (Ident s _) =  PP.string s
>     prettyPrec d (Prime i)    =  prettyPrec d i <> char '\''
>
> ident                         :: String -> Ident
> ident s                       =  Ident s Unknown
>
> identAt                       :: String -> SrcLoc -> Ident
> identAt                       =  Ident
>
> prime                         :: Ident -> Ident
> prime                         =  Prime
>
> string                        :: Ident -> String
> string (Ident s _)            =  s
> string (Prime i)              =  string i
>
> isPrimed                      :: Ident -> Bool
> isPrimed (Prime _i)           =  True
> isPrimed _                    =  False
>
> identSrcLoc                   :: Ident -> SrcLoc
> identSrcLoc (Ident _ loc)     =  loc
> identSrcLoc (Prime i)         =  identSrcLoc i

%-------------------------------=  --------------------------------------------
\section{Literal}
%-------------------------------=  --------------------------------------------

> data Literal                  =  Numeral String SrcLoc
>				|  Char    String SrcLoc
>				|  String  String SrcLoc
>
> str                           :: Literal -> String
> str (Numeral s _)             =  s
> str (Char    s _)             =  s
> str (String  s _)             =  s
>
> instance Eq Literal where
>     l1 == l2                  =  str l1 == str l2
>
> instance Ord Literal where
>     compare l1 l2             =  compare (str l1) (str l2)
>
> instance Show Literal where
>     showsPrec d (Numeral s _) =  showParen (d > 9) (showString "numeral " . shows s)
>     showsPrec d (Char    s _) =  showParen (d > 9) (showString "charLit " . shows s)
>     showsPrec d (String  s _) =  showParen (d > 9) (showString "stringLit " . shows s)
>
> instance Pretty Literal where
>     prettyPrec _d (Numeral s _) =  PP.string s
>     prettyPrec _d (Char    s _) =  PP.string s
>     prettyPrec _d (String  s _) =  PP.string s
>
> numeral, charLit, stringLit   :: String -> Literal
> numeral   s                   =  Numeral s Unknown
> charLit   s                   =  Char    s Unknown
> stringLit s                   =  String  s Unknown
>
> numeralAt, charLitAt, stringLitAt   :: String -> SrcLoc -> Literal
> numeralAt                     =  Numeral
> charLitAt                     =  Char
> stringLitAt                   =  String
>
> litSrcLoc                     :: Literal -> SrcLoc
> litSrcLoc (Numeral _ loc)     =  loc
> litSrcLoc (Char    _ loc)     =  loc
> litSrcLoc (String  _ loc)     =  loc

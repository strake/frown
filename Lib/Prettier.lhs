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
\section{Pretty printing combinators}
%-------------------------------=  --------------------------------------------

> module Prettier (
>     Doc,
>     -- primitive combinators
>     empty, string, newline, (<>), nest, group, fillgroup, oneliner,
>     -- derived combinators
>     text, int, integer, float, double, char, sp, nl, (<+>), (</>),
>     block, parens, brackets, braces, condParens, concat, stack, strip,
>     intersperse, header,
>     -- rendering a document
>     Width, Style(Page, OneLine), render,
>     Pretty(pretty, prettyPrec, prettyList) )
> where
> import Prelude hiding(concat)

Todo: add Jeff Lewis's |tab| and |indent| combinators (see email).
Todo: add ribbon width (maximum number of characters on a line).
Todo: add laws for |fillgroup| which satisfies |fillgroup (newline s)
= string s <||> newline s| (|group| breaks consistently whereas
|fillgroup| tries to fill the lines).

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Interface}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

The type of documents.

< data Doc

Primitive combinators.

> empty                         :: Doc
> string                        :: String -> Doc
> newline                       :: String -> Doc
> infixr 6 <>
> (<>)                          :: Doc -> Doc -> Doc
> nest                          :: Int -> Doc -> Doc
> group                         :: Doc -> Doc
> fillgroup                     :: Doc -> Doc
> oneliner                      :: Doc -> Doc

The argument of |newline| is the replacement string if it is placed on
one line.

Derived combinators.

> text                          :: String -> Doc
> int                           :: Int -> Doc
> integer                       :: Integer -> Doc
> float                         :: Float -> Doc
> double                        :: Double -> Doc
> char                          :: Char -> Doc
> sp, nl                        :: Doc
> block                         :: Int -> Doc -> Doc
> infixr 6 <+>, </>
> (<+>), (</>)                  :: Doc -> Doc -> Doc
> parens, brackets, braces      :: Doc -> Doc
> condParens                    :: Bool -> Doc -> Doc
> concat, stack, strip          :: [Doc] -> Doc
> intersperse                   :: Doc -> [Doc] -> Doc
> header                        :: String -> Doc

Rendering a document.

> type Width                    =  Int
> data Style                    =  Page Width | OneLine
> render                        :: Style -> Doc -> String

A type class for pretty printing.

> class (Show a) => Pretty a where
>       pretty                  :: a -> Doc
>	prettyPrec       	:: Int -> a -> Doc
>	prettyList		:: [a] -> Doc
>
>       pretty a                =  prettyPrec 0 a
>       prettyPrec d a          =  string (showsPrec d a "")
>	prettyList as		=  brackets ( -- or |fillgroup| instead of |group|?
>                                    intersperse (char ',' <> nl) (map pretty as))

Note that the default methods are defined in terms of each
other. Hence the user must provide either |pretty| or |prettyPrec|.

Nearly all of the predefined types are declared instances of |Pretty|.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Axiomatisation}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

The following laws are adequate to reduce any document to the form
(here |<||>| denotes the union of documents):
\[
    |d1 <||> cdots <||> dn|,
\]
where each |di| has the form
\[
    |string s0 <> nest i1 (newline r1) <> string s1 <> cdots <> nest ik (newline rk) <> string sk|.
\]

Laws for |string|:
%
\begin{eqnarray}
    |string ""| & = & |empty| \\
    |string (s ++ t)| & = & |string s <> string t|
\end{eqnarray}
%
Laws for |nest|:
%
\begin{eqnarray}
    |nest 0 d| & = & |d| \\
    |nest (i + j) d| & = & |nest i (nest j d)| \\
    |nest i empty| & = & |empty| \\
    |nest i (string s)| & = & |string s| \\
    |nest i (d1 <> d2)| & = & |nest i d1 <> nest i d2|
\end{eqnarray}
%
Definition of |group|:
%
\begin{eqnarray}
    |group d| & = & |oneliner d <||> d|
\end{eqnarray}
%
Note that |<||>| is not accessible to the user.
Laws for |<||>|:
%
\begin{eqnarray}
    |(d1 <||> d2) <> d3| & = & |(d1 <> d3) <||> (d2 <> d3)| \\
    |d1 <> (d2 <||> d3)| & = & |(d1 <> d2) <||> (d1 <> d3)| \\
    |nest i (d1 <||> d2)| & = & |nest i d1 <||> nest i d2|
\end{eqnarray}
%
Laws for |oneliner|:
%
\begin{eqnarray}
    |oneliner empty| & = & |empty| \\
    |oneliner (string s)| & = & |string s| \\
    |oneliner (newline s)| & = & |string s| \\
    |oneliner (d1 <> d2)| & = & |oneliner d1 <> oneliner d2| \\
    |oneliner (nest i d)| & = & |oneliner d| \\
    |oneliner (d1 <||> d2)| & = & |oneliner d1|
\end{eqnarray}
%
This implies, in particular, that |oneliner (group d) = oneliner d|.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Example uses}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> {-

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Terms}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data	Term	                =  Term String [Term]

> prTerm (Term n [])	        =  string n
> prTerm (Term n ts)	        =  parens (group (nest 2 (stack (string n : map prTerm ts))))

> szTerm (Term n ts)	        =  length n + length ts + sum (map szTerm ts)

> mkTerm 0 i		        =  Term (mkName i) []
> mkTerm (d+1) i		=  Term (mkName i) (map (mkTerm d) (randoms i))

> mkName i		        =  [ 'x' | j <- [1..i] ]

> randoms i		        =  [ i * j `mod` p | j <- [2 .. i `mod` p] ]
>     where p                   =  7

> teststring w d i	        =  render (Page w) (prTerm (mkTerm d i))
> testshow w d i		=  putStrLn (teststring w d i)
> test w d i		        =  length (teststring w d i)

Pretty> test 60 8 3
11402
(377229 reductions, 640227 cells, 7 garbage collections)
Pretty> test 60 9 3
28253
(901055 reductions, 1519954 cells, 16 garbage collections)

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{C programs}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

General aestethic rule: to obtain nice indentation add a |nl|
before calling |pp| for a subcomponent or more general:
|pp (... e ...) = ... nl <> string s <> sp <> nest (length s + 1) (pp e)|.

> type Expr		        =  String
>
> data Stat		        =  Expr Expr	        -- expression statement
>				|  While Expr Stat	-- iteration statement
>				|  If Expr Stat Stat	-- selection statement
>				|  Block [Stat]		-- compound statement
>				|  Return Expr		-- jump statement

> ppExpr                        :: Expr -> Doc
> ppExpr e		        =  string e

> ppStat			:: Stat -> Doc
> ppStat (Expr e)	        =  ppExpr e <> string ";"
> ppStat (While e s)	        =  group (string "while" <+> parens (ppExpr e)
>				          <> nest 4 (nl <> ppStat s))
> ppStat (If e s1 s2)	        =  group (string "if" <+> parens (ppExpr e)
>				          <> nest 4 (nl <> ppStat s1) <> nl
>				          <> string "else"
>				          <> nest 4 (nl <> ppStat s2))
> ppStat (Block ss)	        =  group (string "{"
>				          <> nest 4 (concat [ nl <> ppStat s | s <- ss ]) <> nl
>				          <> string "}")
> ppStat (Return e)	        =  string "return" <+> ppExpr e <> string ";"

> binsearch			:: Stat
> binsearch			=
>     Block [
>         Expr "low = 0",
>         Expr "high = n - 1",
>         While "low <= high" (
>             Block [
>                 Expr "mid = (low + high) / 2",
>                 If "x < v[mid]" (
>                     Expr "high = mid - 1" ) (
>                 If "x > v[mid]" (
>                     Expr "low = mid + 1" ) (
>                     Return "mid" ))
>             ]),
>         Return "-1"
>     ]

> -}

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Implementation}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> --instance Show Doc where
> --    show d                    =  render (Page 79) d

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Primitive functions}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> infixr 6 :<>
>
> type Length                   =  Int
> data Doc                      =  Empty
>                               |  Doc :<> Doc
>                               |  Nest Int Doc
>                               |  String  Length String
>                               |  Newline Length String
>                               |  Group Doc
>                               |  FillGroup Doc
>                                  deriving (Show)

The length of the string is cached in |String| and |Newline|.

> empty                         =  Empty
> (<>)                          =  (:<>)
> nest                          =  Nest
> string  s                     =  String  (length s) s
> newline s                     =  Newline (length s) s
> group                         =  Group
> fillgroup                     =  FillGroup

> oneliner Empty                =  Empty
> oneliner (d1 :<> d2)          =  oneliner d1 <> oneliner d2
> oneliner (Nest _i d)          =  oneliner d
> oneliner (String l s)         =  String l s
> oneliner (Newline l s)        =  String l s
> oneliner (Group d)            =  oneliner d
> oneliner (FillGroup d)        =  oneliner d

Simple documents.
    			 
> data Simple                   =  Nil
>                               |  StringApp Int String Simple
>                               |  NestLineApp Int Simple

> layout                        :: Simple -> String
> layout Nil                    =  ""
> layout (StringApp _l s d)     =  s ++ layout d
> layout (NestLineApp i d)      =  '\n' : replicate i ' ' ++ layout d

> fits                          :: Width -> Simple -> Bool
> fits w _d
>     | w < 0                   =  False
> fits _w Nil                   =  True
> fits w (StringApp l _s d)     =  fits (w - l) d
> fits _w (NestLineApp _i _d)   =  True

Nested documents.
    			 
> data Item                     =  Std !Int Doc | Grp !Int Doc | Fill !Int Doc

TODO: |Grp| needs no |i| argument.

> best                                  :: Width -> Int -> [Item] -> Simple
> best _w _k []                         =  Nil
> best w k (Std _i Empty : ds)          =  best w k ds
> best w k (Std i (d1 :<> d2) : ds)     =  best w k (Std i d1 : Std i d2 : ds)
> best w k (Std i (Nest j d) : ds)      =  best w k (Std (i + j) d : ds)
> best w k (Std _i (String l s) : ds)   =  StringApp l s (best w (k + l) ds)
> best w _k (Std i (Newline _l _s) : ds)=  NestLineApp i (best w i ds)
> best w k (Std i (Group d) : ds)       =  better w k (best w k (Grp i d : ds))
>                                                     (best w k (Std i d : ds))
> best w k (Std i (FillGroup d) : ds)   =  best w k (Fill i d : ds)

The following equations correspond to the ones
above except for the two last ones.

> best w k (Grp _i Empty : ds)          =  best w k ds
> best w k (Grp i (d1 :<> d2) : ds)     =  best w k (Grp i d1 : Grp i d2 : ds)
> best w k (Grp i (Nest _j d) : ds)     =  best w k (Grp i d : ds)
> best w k (Grp _i (String l s) : ds)   =  StringApp l s (best w (k + l) ds)
> best w k (Grp _i (Newline l s) : ds)  =  StringApp l s (best w (k + l) ds)
> best w k (Grp i (Group d) : ds)       =  best w k (Grp i d : ds)
> best w k (Grp i (FillGroup d) : ds)   =  best w k (Grp i d : ds)

Again, the following equations correspond to the ones
above except for the two last ones.

> best w k (Fill _i Empty : ds)         =  best w k ds
> best w k (Fill i (d1 :<> d2) : ds)    =  best w k (Fill i d1 : Fill i d2 : ds)
> best w k (Fill i (Nest j d) : ds)     =  best w k (Fill (i + j) d : ds)
> best w k (Fill _i (String l s) : ds)  =  StringApp l s (best w (k + l) ds)
> best w k (Fill i (Newline l s) : ds)  =  better w k (StringApp l s (best w (k + l) ds))
>                                                     (NestLineApp i (best w i ds))
> best w k (Fill i (Group d) : ds)      =  better w k (best w k (Grp  i d : ds))
>                                                     (best w k (Fill i d : ds))
> best w k (Fill i (FillGroup d) : ds)  =  best w k (Fill i d : ds)

Choosing between two layouts.

> better                        :: Width -> Int -> Simple -> Simple -> Simple
> better w k d1 d2              =  if fits (w - k) d1 then d1 else d2

Rendering.

> render (Page w) d             =  layout (best w 0 [Std 0 d])
> render OneLine  d             =  layout (best maxBound 0 [Grp 0 d])

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Derived functions}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> text                          =  string

> int i                         =  string (show i)
> integer i                     =  string (show i)
> float f                       =  string (show f)
> double d                      =  string (show d)
> char c                        =  string [c]

> nl                            =  newline " "
> sp                            =  string " "

> block i d                     =  group (nest i d)

> parens   d                    =  block 1 (string "(" <> d <> string ")")
> brackets d                    =  block 1 (string "[" <> d <> string "]")
> braces   d                    =  block 1 (string "{" <> d <> string "}")

> condParens False d            =  d
> condParens True d             =  parens d

> concat                        =  foldr (<>) empty

> intersperse _sep []                   =  empty
> intersperse _sep [d]                  =  d
> intersperse sep (d : ds@(_ : _))      =  d <> sep <> intersperse sep ds

> d1 </> d2		        =  d1 <> nl <> d2
> d1 <+> d2		        =  d1 <> sp <> d2
		
> stack		                =  foldr1 (</>)
> strip		                =  foldr1 (<+>)

> header s                      =  string s <> nl <> string (replicate (length s) '-') <> nl <> nl

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Class instances}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

 instance Show (a -> b) where
       showsPrec _d _f         =  showString "<function>"

 instance Pretty (a -> b)

> instance Pretty Bool
> instance Pretty Char where
>	prettyList s		=  string (show s)
> instance Pretty Int
> instance Pretty Integer
> instance Pretty Float
> instance Pretty Double
> instance (Pretty a) => Pretty [a] where
>	prettyPrec _d as	=  prettyList as
> instance Pretty ()
> instance (Pretty a, Pretty b) => Pretty (a, b) where
>       prettyPrec _d (a, b)	=  parens (
>                                      pretty a <> char ',' <> nl <>
>                                      pretty b)
> instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
>       prettyPrec _d (a, b, c)	=  parens (
>                                      pretty a <> char ',' <> nl <>
>                                      pretty b <> char ',' <> nl <>
>                                      pretty c)
> instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
>       prettyPrec _d (a, b, c, d)
>                          	=  parens (
>                                      pretty a <> char ',' <> nl <>
>                                      pretty b <> char ',' <> nl <>
>                                      pretty c <> char ',' <> nl <>
>                                      pretty d)
> instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e)
>                               => Pretty (a, b, c, d, e) where
>       prettyPrec _d (a, b, c, d, e)
>                          	=  parens (
>                                      pretty a <> char ',' <> nl <>
>                                      pretty b <> char ',' <> nl <>
>                                      pretty c <> char ',' <> nl <>
>                                      pretty d <> char ',' <> nl <>
>                                      pretty e)
>
> instance (Pretty a) => Pretty (Maybe a) where
>     prettyPrec _d Nothing     =  string "Nothing"
>     prettyPrec d (Just a)     =  condParens (d > 9) (
>                                      block 4 (string "Just" </> prettyPrec 10 a))


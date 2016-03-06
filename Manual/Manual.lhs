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

%let fast                       =  True

\documentclass[a4paper,fleqn]{report}

\usepackage[utf8]{inputenc}
\usepackage{a4wide}
\usepackage{graphicx}
\usepackage{calc}
\usepackage{ulem}\normalem
\usepackage{mflogo}
\usepackage{hevea}
\usepackage{verbatim}

\newcommand{\oldfrown}{\frown}
%\renewcommand{\frown}{\texttt{frown :-(}}
%\newcommand{\Frown}{\textsl{Frown}\hspace{-0.35em}\llap{\raisebox{0.8ex}{$\frown$}}\hspace{0.35em}}
%\newcommand{\Frown}{\textsf{Fr\t{o}wn}}
%\newcommand{\Frown}{\textsf{\bfseries Frown}}
\newcommand{\Frown}{\textsf{Frown}}

\newtheorem{remark}{Remark}

%-------------------------------=  --------------------------------------------

%include Manual.fmt

%-------------------------------=  --------------------------------------------
%if False

> module Manual
> where
> import FMP
> import FMPPicture
> import FMPMatrix

> --orange                        =  Color 1.0 0.5 0.0
> lightblue                     =  Color 0.66 0.66 1.0

> lineStyle                     =  setArrowHeadStyle ahLine

> farrow a b                    =  arrow a b
>                               #  setArrowHead (defaultArrowHead # lineStyle)

> edge a l b d                  =  farrow (ref a) (ref b)
>                               #  setLabel 0.5 d (math l)
>
> loopS s                       =   ref (s <+ S) + vec (-0.25 * width s, 0)
>                               ... ref (s <+ S) + vec (0, -0.4 * width s)
>                               ... ref (s <+ S) + vec (0.25 * width s, 0)
>                               #  setArrowHead (defaultArrowHead # lineStyle)

> array as                      =
>     "\\begin{array}{@{}l@{\\mbox{}\\shortrightarrow\\mbox{}}l@{}}"
>     ++ concat (intersperse "\\\\" [ concat (intersperse "&" a) | a <- as])
>     ++ "\\end{array}"

> intersperse                   :: a -> [a] -> [a]
> intersperse _s []             =  []
> intersperse s (a : x)         =  a : intersperse1 x
>     where intersperse1 []     =  []
>           intersperse1 (b : y)=  s : b : intersperse1 y

%endif
%-------------------------------=  --------------------------------------------

\begin{document}

\title{%
{\fontencoding{OT1}\fontfamily{cmss}\fontseries{bx}\fontshape{n}\fontsize{41pt}{60pt}\selectfont Frown} \\[2mm]
An LALR($k$) Parser Generator for Haskell \\[4mm]
\small version 0.6 (andromeda release) \\[1cm]
\includegraphics{Pics/Frown.ps}}

\author
{RALF HINZE
\\Institut für Informatik III
\\Universität Bonn
\\Römerstraße 164
\\53117 Bonn
\\Germany
\\\texttt{ralf\symbol{64}cs.uni-bonn.de}
\\@http://www.cs.uni-bonn.de/~ralf/@}

\date{1st November 2005}

\maketitle

\begin{abstract}
\Frown\ is an LALR($k$) parser generator for Haskell~98 written in
Haskell~98.

\vspace*{2mm}
\noindent
Its salient features are:
%
\begin{itemize}
\item The generated parsers are time and space efficient. On the downside,
  the parsers are quite large.

\item \Frown\ generates four different types of parsers. As a common
  characteristic, the parsers are \emph{genuinely functional} (ie
  `table-free'); the states of the underlying LR automaton are encoded
  as mutually recursive functions. Three output formats use a typed
  stack representation, one format due to Ross Paterson
  (@code=stackless@) works even without a stack.
  
\item Encoding states as functions means that each state can be
  treated individually as opposed to a table driven-approach, which
  necessitates a uniform treatment of states. For instance, look-ahead
  is only used when necessary to resolve conflicts.

\item \Frown\ comes with debugging and tracing facilities; the standard
  output format due to Doaitse Swierstra (@code=standard@) may be
  useful for teaching LR parsing.
  
\item Common grammatical patterns such as repetition of symbols can
  be captured using \emph{rule schemata}. There are several predefined
  rule schemata.
  
\item Terminal symbols are arbitrary variable-free Haskell patterns
  or guards.  Both terminal and nonterminal symbols may have an
  arbitrary number of synthesized attributes.

\item \Frown\ comes with extensive documentation; several example
  grammars are included.
\end{itemize}
Furthermore, \Frown\ supports the use of monadic lexers, monadic
semantic actions, precedences and associativity, the generation of
backtracking parsers, multiple start symbols, error reporting and a
weak form of error correction.
\end{abstract}

\tableofcontents

%================================  ============================================
\chapter{Introduction}
\label{sec:introduction}
%================================  ============================================

\Frown\ is an LALR($k$) parser generator for Haskell~98 written in
Haskell~98.

The work on \Frown\ started as an experiment in generating genuinely
functional LR parsers. The first version was written within three
days---yes, Haskell is a wonderful language for rapid prototyping.
Since then \Frown\ has gone through several cycles of reorganization
and rewriting. It also grew considerably: dozens of features were
added, examples were conceived and tested, and this manual was
written. In the end, \Frown\ has become a useable tool. I hope you
will find it useful, too.

%-------------------------------=  --------------------------------------------
\section{Obtaining and installing \protect\Frown}
\label{sec:install}
%-------------------------------=  --------------------------------------------

\paragraph{Obtaining \Frown}

The parser generator is available from
%
\begin{quote}
@http://www.informatik.uni-bonn.de/~ralf/frown@.
\end{quote}
%
The bundle includes the sources and the complete documentation (dvi,
ps, PDF, and HTML).

\paragraph{Requirements}

You should be able to build \Frown\ with every Haskell~98-compliant
compiler.  You have to use a not too ancient compiler as there have
been some changes to the Haskell language in Sep.~2001 (GHC 5.02 and
later versions will do).

The Haskell interpreter Hugs~98 is needed for running the testsuite.

Various tools are required to generate the documentation from scratch:
@lhs2TeX@, \LaTeX, functional \MP, \hevea\ and  \hacha. Note, however, that
the bundle already includes the complete documentation.

\paragraph{Installation}

Unzip and untar the bundle. This creates a directory called @Frown@.
Enter this directory.
%
\begin{verbatim}
ralf> tar xzf frown.tar.gz
ralf> cd Frown
\end{verbatim}
%
The documentation resides in the directory @Manual@; example grammars
can be found in @Examples@ and @Manual/Examples@ (the files ending in
@.g@ and @.lg@).

You can install \Frown\ using either traditional makefiles or Cabal.

\paragraph{Using makefiles}

Optionally, edit the @Makefile@ to specify destinations for the binary
and the documentation (this information is only used by @make
install@). Now, you can trigger
%
\begin{verbatim}
ralf/Frown> make
\end{verbatim}
%
which compiles \Frown\ generating an executable called @frown@ (to use
\Frown\ you only need this executable). Optionally, continue with
%
\begin{verbatim}
ralf/Frown> make install
\end{verbatim}
%
to install the executable and the documentation.

For reference, here is a list of possible targets:
%
\begin{description}
\item[@make@] \mbox{}\\
  Compiles \Frown\ generating an executable called @frown@ (to use \Frown\ 
  you only need this executable).

\item[@make install@] \mbox{}\\
  Compiles \Frown\ and installs the executable and the documentation.

\item[@make test@] \mbox{}\\
  Runs the testsuite.\footnote{There are some known problems. The
    format @code=stackless@ behaves differently for @Loop.g@ (the
    generated parser is less strict than the standard one). Also,
    @Empty.g@ does not work yet. Finally, error reports may differ for
    different formats and for optimized and unoptimized versions (as some
    parsers perform additional reductions before an error is
    reported).}

\item[@make man@] \mbox{}\\
  Generates the documentation in various formats (dvi, ps, PDF, and
  HTML).
  
\item[@make clean@] \mbox{}\\
  Removes some temporary files.
  
\item[@make distclean@] \mbox{}\\
  Removes all files except the ones that are included in the distribution.
\end{description}

\paragraph{Using Cabal}

Alternatively, you can build \Frown\ using Cabal (version 1.1.3 or
later), Haskell's Common Architecture for Building Applications and
Libraries.

For a global install, type:
%
\begin{verbatim}
ralf/Frown> runhaskell Setup.hs configure --ghc
ralf/Frown> runhaskell Setup.hs build
ralf/Frown> runhaskell Setup.hs install
\end{verbatim}
%
If you want to install \Frown\ locally, use (you may wish to replace
@$HOME@ by a directory of your choice):
%
\begin{verbatim}
ralf/Frown> runhaskell Setup.hs configure --ghc --prefix=$HOME
ralf/Frown> runhaskell Setup.hs build
ralf/Frown> runhaskell Setup.hs install --user
\end{verbatim}

\paragraph{Usage}

The call
\begin{verbatim}
ralf/Frown> frown -h
\end{verbatim}
displays the various options. For more information consult this
manual.

%-------------------------------=  --------------------------------------------
\section{Reporting bugs}
%-------------------------------=  --------------------------------------------

Bug reports should be send to Ralf Hinze (\verb|ralf@cs.uni-bonn.de|).
The report should include all information necessary to reproduce the
bug: the compiler used to compile \Frown, the grammar source file (and
possibly auxiliary Haskell source files), and the command-line
invocation of \Frown.

Suggestions for improvements or request for features should also be
sent to the above address.

%-------------------------------=  --------------------------------------------
%\newpage
\section{License}
%-------------------------------=  --------------------------------------------

\Frown\ is distributed under the GNU general public licence
(version~2).

\verbatiminput{../COPYRIGHT}

%-------------------------------=  --------------------------------------------
\section{Credits}
%-------------------------------=  --------------------------------------------

\Frown\ wouldn't have seen the light of day without the work of Ross
Paterson and Doaitse Swierstra. Ross invoked my interest in LR parsing
and he devised the @--code=stackless@ and @--code=gvstack@ output
formats. Doaitse invented the @--code=standard@ format, which was
historically the first format \Frown\ supported.

A big thank you goes to Andres Löh and Ross Paterson for various bug
reports and suggestions for improvement.

%================================  ============================================
\chapter{Quick start}
%================================  ============================================

First install \Frown\ as described in Sec.~\ref{sec:install}. Then enter
the directory @QuickStart@.
%
\begin{verbatim}
ralf/Frown> cd QuickStart
\end{verbatim}
%
The file @Tiger.lg@, listed in Fig.~\ref{fig:tiger}, contains a
medium-sized grammar for an imperative language.
%
\begin{figure}
\scriptsize
%include ../QuickStart/Tiger.lg
\caption{\label{fig:tiger}A sample \Frown\ grammar file.}
\end{figure}
%
Now, type
%
\begin{verbatim}
ralf/Frown/QuickStart> frown -v Tiger.lg
ralf/Frown/QuickStart> hugs Tiger.hs
...
Tiger> exp [ID "a", PLUS, ID "b", TIMES, INT "2"] >>= print
Bin (Var "a") Add (Bin (Var "b") Mul (Int "2"))
Tiger> tc "fib.tig"
...
\end{verbatim}
%
The call @frown -v Tiger.lg@ generates a Haskell parser which can then
be loaded into @hugs@ (or @ghci@). The parser has type |exp :: (Monad
m) => [Terminal] -> m Expr|, that is, the parser is a computation that
takes a list of terminals as input and returns an expression.

\vspace*{2mm}
More examples can be found in the directory @Manual/Examples@:
%
\begin{description}
\item[@Paren1.lg@] \mbox{}\\
well-balanced parentheses: a pure grammar (see Sec.~\ref{sec:pure});

\item[@Paren2.lg@] \mbox{}\\
an extension of @Paren1.lg@ illustrating the definition of attributes
(see Sec.~\ref{sec:attributes});

\item[@Calc.lg@] \mbox{}\\
a simple evaluator for arithmetic expressions: a parser that
interfaces with a separate lexer (see Sec.~\ref{sec:lexer});

\item[@MCalc.lg@] \mbox{}\\
a variant of the desktop calculator (@Calc.lg@) that prints all
intermediate results: illustrates monadic actions (see
Sec.~\ref{sec:monadicactions});

\item[@Let1.lg@] \mbox{}\\
an ambiguous expression grammar: illustrates backtracking parsers (see
Sec.~\ref{sec:backtracking});

\item[@Let2.lg@] \mbox{}\\
an expression grammar: illustrates the use of precedences and
associativity (see Sec.~\ref{sec:prec});

\item[@Let3.lg@] \mbox{}\\
a variant of the expression grammar: shows how to simulate inherited
attributes using a reader monad (see Sec.~\ref{sec:reader});

\item[@Let4.lg@] \mbox{}\\
an expression grammar: illustrates a parser
that interfaces with a monadic lexer (see Sec.~\ref{sec:mlexer});

\item[@Let5.lg@] \mbox{}\\
a variant of @Let4.lg@ with better error reporting (see
Sec.~\ref{sec:reporting});

\item[@Let6.lg@] \mbox{}\\
a variant of @Let5.lg@ with even better error reporting: prints a
list of expected tokens upon error (see Sec.~\ref{sec:expected});

\item[@Let7.lg@] \mbox{}\\
yet another variant of the expression grammar: illustrates a simple
form of error correction (see Sec.~\ref{sec:errorcorrection});

\item[@Let8.lg@] \mbox{}\\
variant of @Let7.lg@ that notifies the user of corrections (see
Sec.~\ref{sec:errorcorrection});

\item[@VarCalc.lg@] \mbox{}\\
a variant of the desktop calculator (@Calc.lg@) that works without a
separate lexer: illustrates guards (see Sec.~\ref{sec:terminal2});

\item[@Paren3.lg@] \mbox{}\\
illustrates the tracing facilities (see Sec.~\ref{sec:tracing});

\item[@VarParen.lg@] \mbox{}\\
illustrates irrefutable patterns on the right-hand side of productions
(see Sec.~\ref{sec:irrefutable});

\item[@RepMin.lg@] \mbox{}\\
a solution to the rep-min problem: illustrates how to simulate
inherited attributes using functional attributes (see
Sec.~\ref{sec:inherited}).
\end{description}

%================================  ============================================
\chapter{Tour de \protect\Frown}
%================================  ============================================

This chapter introduces \Frown\ by means of example.

%-------------------------------=  --------------------------------------------
\section{Preliminaries: monads}
%-------------------------------=  --------------------------------------------

Some elementary knowledge of \technical{monads} is helpful in order to
use \Frown\ effectively. For the most basic applications, however, one
can possibly do without.  This section summarizes the relevant facts.

In Haskell, the concept of a monad is captured by the following
class definition.

< class Monad m where
<     return                    ::  a -> m a
<     (>>=)                     ::  m a -> (a -> m b) -> m b
<     (>>)                      ::  m a -> m b -> m b
<     fail                      ::  String -> m a
<
<     m >> n                    =   m >>= const n
<     fail s                    =   error s

The essential idea of monads is to distinguish between
\technical{computations} and \technical{values}. This distinction is
reflected on the type level: an element of |m a| represents a
computation that yields a value of type |a|. The trivial or pure
computation that immediately returns the value |a| is denoted |return
a|. The operator |(>>=)|, commonly called `bind', combines two
computations: |m >>= k| applies |k| to the result of the computation
|m|. The derived operation |(>>)| provides a handy shortcut if one is
not interested in the result of the first computation. Finally, the
operation |fail| is useful for signaling error conditions (a common
thing in parsing).

Framing the concept of a monad as a type class is sensible for at
least two interrelated reasons. First, we can use the same names
(|return|, `|>>=|', and |fail|) for wildly different computational
structures.\footnote{In fact, we can use the same notation, the
  so-called \technical{|do|-notation}, for different monads (cf
  Haskell Report \S{3.14}).} Second, by overloading a function with
the monad class we effectively parameterize the function by
computational structures, that is, we can call the same function with
different instances of monads obtaining different computational
effects.

The following instance declaration (@Result.lhs@) defines a simple
monad that we will use intensively in the sequel (the monad can be
seen as a simplified term implementation of the basic monad
operations).
%
%include Examples/Result.lhs
%
In monad speak, this is an \technical{exception monad}: a computation
in |Result| either succeeds gracefully yielding a value |a|
(represented by the term |Return a|) or it fails with an error message
|s| (represented by |Fail s|). That's all we initially need for
\Frown: parsing a given input either succeeds producing a semantic
value (sometimes called an attribution) or it fails (hopefully, with a
clear indication of the syntax error).

%-------------------------------=  --------------------------------------------
\section{Basic features}
%-------------------------------=  --------------------------------------------

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Pure grammars}
\label{sec:pure}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Let's start with a simple example. The following complete \Frown\
source file (@Paren1.lg@\footnote{The source files of the examples are
located in the directory @Manual/Examples@.}) defines the language of
well-balanced parentheses. The specification of the grammar is
enclosed in special curly braces `|%{ ldots }%|'. The remainder
contains Haskell source code, that is, a module header and a function
declaration.
%
%include Examples/Paren1.lg
%
The part enclosed in special curly braces comprises the typical
ingredients of a \technical{context-free grammar}: a declaration of
the \technical{terminal symbols}, a declaration of the
\technical{nonterminal symbols}, and finally the
\technical{productions} or \technical{grammar rules}.

In general, the terminal symbols are given by Haskell patterns of the
same type. Here, we have two character patterns of type |Char|.

Nonterminals are just identifiers starting with a lower-case letter.
By convention, the first nonterminal is also the start symbol of
the grammar (this default can be overwritten, see~Sec.~\ref{sec:multiple}).

Productions have the general form |n : v_1, ldots, v_k;| where |n| is
a nonterminal and |v_1|, \ldots, |v_k| are symbols. Note that the
symbols are separated by commas and terminated by a semicolon. The
mandatory trailing semicolon helps to identify so-called
\technical{$\epsilon$-productions}, productions with an empty right-hand
side, such as |paren {-"\;"-} : {-"\;"-} ;|.

As a shorthand, we allow to list several alternative right-hand sides
separated by a vertical bar. Thus, the above productions could have
been written more succinctly as

< paren                         :  ;
<                               |  paren, '(', paren, ')';

The two styles can be arbitrarily mixed. In fact, it is not even
required that productions with the same left-hand side are grouped
together (though it is good style to do so).

Now, assuming that the above grammar resides in a file called
@Paren.g@ we can generate a Haskell parser by issuing the command
\begin{quote}
@frown Paren.g@
\end{quote}
This produces a Haskell source file named @Paren.hs@ that contains
among other things the function

< paren                         :: (Monad m) => [Char] -> m () {-","-}

which recognizes the language generated by the start symbol of the
same name.  Specifically, if |inp| is a list of characters, then
|paren inp| is a computation that either succeeds indicating that
|inp| is a well-formed parentheses or fails indicating that |inp|
isn't well-formed. Here is a short interactive session using the
Haskell interpreter Hugs (type @hugs Paren.hs@ at the command line).
%
%include Examples/Paren1.session
%
Note that we have to specify the result type of the expressions in
order to avoid an unresolved overloading error. Or to put it
differently, we have to specify the monad, in which the parsing
process takes place.  Of course, we are free to assign |paren| a more
constrained type by placing an appropriate type signature in the
Haskell section of the grammar file:

< paren                         :: [Char] -> Result () {-"."-}

By the way, since the nonterminal |paren| carries no semantic value,
the type of the computation is simply |Result ()| where the empty
tuple type `|()|' serves as a dummy type. In the next section we will
show how to add attributes or semantic values to nonterminals.

Every once in a while parsing fails. In this case, \Frown\ calls a
user-supplied function named, well, |frown| (note that you must supply
this function). In our example, |frown| has type

< frown                         :: (Monad m) => [Char] -> m a

The error function |frown| is passed the remaining input as an
argument, that you can give an indication of the location of the
syntax error (more on error reporting in
Sec.~\ref{sec:error-reporting}). Note that |frown| must be polymorphic
in the result type.

\begin{remark}
  For those of you who are knowledgable and/or interested in
  LR~parsing, Fig.~\ref{fig:ex1} displays the Haskell file that is
  generated by @frown Paren.g@\footnote{Actually, the file is
  generated using @frown --suffix Paren.g@, see
  Sec.~\ref{sec:options}.}. For each state |i| of the underlying
  LR\emph{(}$0$\emph{)} automaton, displayed in Fig.~\ref{fig:auto1},
  there is one function called |parse_i|. All these functions take two
  arguments: the remaining input and a stack that records the
  transitions of the LR\emph{(}$0$\emph{)} machine. The reader is
  invited to trace the parse of |"(())()"|.
%
\begin{figure}

< module Paren where
< import Result
<
< {- frown :-( -}
<
< data Stack                     =  Empty
<                                |  T_1_2 Stack
<                                |  T_2_3 Stack
<                                |  T_2_5 Stack
<                                |  T_4_5 Stack
<                                |  T_4_6 Stack
<                                |  T_5_4 Stack
<
< data Nonterminal               =  Paren
<
< paren tr                       =  parse_1 tr Empty >>= (\ Paren -> return ())
<
< parse_1 ts st                  =  parse_2 ts (T_1_2 st)
<
< parse_2 tr@[] st               =  parse_3 tr (T_2_3 st)
< parse_2 ('(' : tr) st          =  parse_5 tr (T_2_5 st)
< parse_2 ts st                  =  frown ts
<
< parse_3 ts (T_2_3 (T_1_2 st))  =  return Paren
<
< parse_4 ('(' : tr) st          =  parse_5 tr (T_4_5 st)
< parse_4 (')' : tr) st          =  parse_6 tr (T_4_6 st)
< parse_4 ts st                  =  frown ts
<
< parse_5 ts st                  =  parse_4 ts (T_5_4 st)
<
< parse_6 ts (T_4_6 (T_5_4 (T_2_5 (T_1_2 st)))) = 
<                                =  parse_2 ts (T_1_2 st)
< parse_6 ts (T_4_6 (T_5_4 (T_4_5 (T_5_4 st))))
<                                =  parse_4 ts (T_5_4 st)
<
< {- )-: frown -}
<
< frown _                        =  fail "syntax error"

\caption{\label{fig:ex1}A \protect\Frown\ generated parser.}
\end{figure}
%if False

> automaton1                    =
>     draw [
>         edge ("S1" <+ E) "P" ("S2" <+ W) S,
>         edge ("S2" <+ E) "\\mbox{\\tt\\$}" ("S3" <+ W) S,
>         edge ("S2" <+ C) "\\mbox{\\tt(}" ("S4" <+ C) SE,
>         edge ("S4" <+ E) "P" ("S5" <+ W) S,
>         edge ("S5" <+ E) "\\mbox{\\tt)}" ("S6" <+ W) S,
>         edge ("S5" <+ C) "\\mbox{\\tt(}" ("S4" <+ C) S # setStartAngle (-140)
>     ]
>     states1

> states1                       =
>     matrixSepBy 35 20
>     [[state1 # setName "S1", state2 # setName "S2", state3 # setName "S3"]
>     ,[state4 # setName "S4", state5 # setName "S5", state6 # setName "S6"]]
>   where
>   state1                      =  toPicture (
>     rbox 10 (math ("1\\colon" ++ array [ ["S","\\mbox{}\\cdot P\\mbox{\\tt\\$}"]
>                                        , ["P","\\mbox{}\\cdot\\mbox{}"]
>                                        , ["P","\\mbox{}\\cdot P\\mbox{\\tt(}P\\mbox{\\tt)}"] ]))
>     # setBGColor lightblue)
>   state2                      =  toPicture (
>     rbox 10 (math ("2\\colon" ++ array [ ["S","P\\cdot\\mbox{\\tt\\$}"]
>                                        , ["P","P\\cdot\\mbox{\\tt(}P\\mbox{\\tt)}"] ]))
>     # setBGColor yellow)
>   state3                      =  toPicture (
>     rbox 10 (math ("3\\colon" ++ array [ ["S","P\\mbox{\\tt\\$}\\cdot\\mbox{}"] ]))
>     # setBGColor lightblue)
>   state4                      =  toPicture (
>     rbox 10 (math ("5\\colon" ++ array [ ["P","P\\mbox{\\tt(}\\cdot P\\mbox{\\tt)}"]
>                                        , ["P","\\mbox{}\\cdot"]
>                                        , ["P","\\mbox{}\\cdot P\\mbox{\\tt(}P\\mbox{\\tt)}"] ]))
>     # setBGColor lightblue)
>   state5                      =  toPicture (
>     rbox 10 (math ("4\\colon" ++ array [ ["P", "P\\mbox{\\tt(}P\\cdot\\mbox{\\tt)}"]
>                                        , ["P","P\\cdot\\mbox{\\tt(}P\\mbox{\\tt)}"] ]))
>     # setBGColor yellow)
>   state6                      =  toPicture (
>     rbox 10 (math ("6\\colon" ++ array [ ["P", "P\\mbox{\\tt(}P\\mbox{\\tt)}\\cdot\\mbox{}"] ]))
>     # setBGColor lightblue)

%endif
\begin{figure}
\begin{center}
%if fast
\includegraphics{auto.1}
%else
\perform{generate "auto" 1 automaton1}
%endif
\end{center}
\caption{\label{fig:auto1}The LR($0$) automaton underlying the parser of Fig.~\ref{fig:ex1}.}
\end{figure}
%
\end{remark}

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Attributes}
\label{sec:attributes}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Now, let's augment the grammar of Sec.~\ref{sec:pure} by semantic
values (@Paren2.lg@).  Often, the parser converts a given input into
some kind of tree representation (the so-called \technical{abstract
syntax tree}).  To represent nested parentheses we simply use binary
trees (an alternative employing |n|-ary trees is given in
Sec.~\ref{sec:irrefutable}).
%
%include Examples/Paren2.lg
%
Attributes are always given in curly braces. When we declare a
nonterminal, we have to specify the types of its attributes as in
|paren {Tree}|. The rules of the grammar can be seen as functions from
the right-hand side to the left-hand side. On the right-hand side,
Haskell variables are used to name the values of attributes. The
values of the attributes on the left-hand side are then given by
Haskell expressions, in which the variables of the right-hand side may
occur free. The Haskell expressions can be arbitrary, except that they
must not be layout-sensitive.

In general, a nonterminal may have an arbitrary number of attributes
(see Sec.~\ref{sec:multattr} for an example). Note that \Frown\ only
supports so-called \technical{synthesized attributes}
(\technical{inherited attributes} can be simulated, however, with the
help of a reader monad, see Sec.~\ref{sec:reader}, or with functional
attributes, see Sec.~\ref{sec:inherited}).

The parser generated by \Frown\ now has type

< paren                         :: (Monad m) => [Char] -> m Tree {-"."-}

The following interactive session illustrates its use.
%
%include Examples/Paren2.session
%

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Interfacing with a lexer}
\label{sec:lexer}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

The parsers of the two previous sections take a list of characters as
input.  In practice, a parser usually does not work on character
streams directly. Rather, it is prefaced by a lexer that first
converts the characters into a list of so-called
\technical{tokens}. The separation of the lexical analysis from the
syntax analysis usually leads to a clearer design and as a benevolent
side-effect it also improves efficiency (Sec.~\ref{sec:terminal2}
shows how to combine lexing and parsing in \Frown, though).

A simple token type is shown in Fig~\ref{fig:terminal}
(@Terminal1.lhs@). (Note that the type comprises more constructors
than initially needed.)
%
\begin{figure}
%include Examples/Terminal1.lhs
\caption{\label{fig:terminal}The type of terminals (@Terminal1.lhs@).}
\end{figure}

Fig.~\ref{fig:lexer} (@Lexer.lhs@) displays a simple lexer for
arithmetic expressions, which are built from numerals using the
arithmetic operators `@+@', `@-@', `@*@', and `@/@'.
\begin{figure}
%include Examples/Lexer.lhs
\caption{\label{fig:lexer}A simple lexer for arithmetic expressions (@Lexer.lhs@).}
\end{figure}

The following grammar, which builds upon the lexer, implements a
simple evaluator for arithmetic expressions (@Calc.lg@).
%
%include Examples/Calc.lg
%
The terminal declaration now lists patterns of type |Terminal|. Note
that terminals may also carry semantic values. The single argument of
|Numeral|, for instance, records the numerical value of the numeral.

When declaring a terminal we can optionally define a shortcut using an
|as|-clause as, for example, in |LParen as "("|. The shortcut can be
used in the productions possibly improving their readability.

Here is an example session demonstrating the evaluator.
%
%include Examples/Calc.session
%

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Monadic actions}
\label{sec:monadicactions}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

The expression that determines the value of an attribute is usually a
pure one. It is, however, also possible to provide a monadic action
that \emph{computes} the value of the attribute. The computation lives
in the underlying parsing monad. Monadic actions are enclosed in `|{%
ldots }|' braces and have type |m t| where |m| is the type of the
underlying monad and |t| is the type of attributes.

As an example, the following variant of the desktop calculator
(@MCalc.lg@) prints all intermediate results (note that we only list
the changes to the preceeding example).
%
%include Examples/MCalc.lg
%
The following session illustrates its working.
%
%subst char a           = "\texttt{" a "}"
%include Examples/MCalc.session
%subst char a           = "\texttt{''" a "''}"
%

In general, monadic actions are useful for performing `side-effects' (for example,
in order to parse @%include@ directives)
and for interaction with a monadic lexer (see Sec.~\ref{sec:mlexer}).

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Backtracking parsers}
\label{sec:backtracking}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

In the previous examples we have encoded the precedences of the
operators (`|*|' binds more tightly than `|+|') into the productions
of the grammar. However, this technique soon becomes unwieldy for a
larger expression language. So let's start afresh. The grammar file
shown in Fig.~\ref{fig:let1} (@Let1.lg@) uses only a single
nonterminal for expressions (we have also extended expressions by
local definitions).
%
\begin{figure}
%include Examples/Let1.lg
\caption{\label{fig:let1}An ambiguous grammar (@Let1.lg@).}
\end{figure}
%
Also note that the grammar has no |Nonterminal| declaration. Rather,
the terminal symbols are declared by supplying type signatures before
the respective rules. Generally, type signatures are preferable to a
|Nonterminal| declaration if the grammar is long.

Of course, the rewritten grammar is no longer LALR($k$) simply because
it is ambiguous. For instance, `|1+2*3|' can be parsed as |Bin (Const
1) Plus (Bin (Const 2) Times (Const 3))| or as |Bin (Bin (Const 1)
Plus (Const 2)) Times (Const 3)|. \Frown\ is also unhappy with the
grammar: it reports six shift/reduce conflicts:
%
\begin{verbatim}
* warning: 6 shift/reduce conflicts
\end{verbatim}
%
This means that \Frown\ wasn't able to produce a deterministic parser.
Or rather, it produced a deterministic parser by making some arbitrary
choices to avoid non-determinism (shifts are preferred to reductions,
see Sec.~\ref{sec:prec}). However, we can also instruct \Frown\ to
produce a non-deterministic parser, that is, one that generates all
possible parses of a given input. We do so by supplying the option
@--backtrack@:
\begin{quote}
@frown --backtrack Let.g@
\end{quote}
The generated parser |expr| now has type

< expr                          :: (MonadPlus m) => [Terminal] -> m Expr {-"."-}

Note that the underlying monad must be an instance of |MonadPlus|
(defined in the standard library |Monad|). The list monad and the
|Maybe| monad are both instances of |MonadPlus|. The following session
shows them in action.
%
%include Examples/Let1.session
%
The list monad supports `deep backtracking': all possible parses are
returned (beware: the number grows exponentionally). The |Maybe| monad
implements `shallow backtracking': it commits to the first solution
(yielding the same results as the parser generated \emph{without} the
option @--backtrack@).

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Precedences and associativity}
\label{sec:prec}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Instead of resorting to a backtracking parser we may also help \Frown\ 
to generate the `right' deterministic parser by assigning
\technical{precedences} to terminal symbols. The understand the
working of precedences it is necessary to provide some background of
the underlying parsing technique.

LR parsers work by repeatedly performing two operations:
\technical{shifts} and \technical{reductions}. A shift moves a
terminal from the input onto the stack, the auxiliary data structure
maintained by the parser. A reduction replaces a top segment of the
stack matching the right-hand side of a production by its left-hand side.
Parsing succeeds if the input is empty and the stack consists of a
start symbol only. As an example, consider parsing `|N*N+N|'.
\[
\begin{array}{r||r@@{\qquad}l}
\hline\hline
             & N*N+N        & \text{shift} \\
    N        & \mbox{}*N+N  & \text{reduce by |e : N;|} \\
    e        & \mbox{}*N+N  & \text{shift} \\
   e*\mbox{} & N+N          & \text{shift} \\
  e*N        & \mbox{}+N    & \text{reduce by |e : N;|} \\
  e*e        & \mbox{}+N    & \\
\hline\hline
\end{array}
\]
At this point, there are two possibilities: we can either perform a
reduction (using the production |e : e, *, e;|) or shift the next
input symbol. Both choices are viable.
\[
\begin{array}[t]{r||r@@{\qquad}l}
\hline\hline
  e*e        & \mbox{}+N    & \text{reduce by |e : e, *, e;|} \\
    e        & \mbox{}+N    & \text{shift} \\
   e+\mbox{} & N            & \text{shift} \\
  e+N        &              & \text{reduce by |e : N;|} \\
  e+e        &              & \text{reduce by |e : e, +, e;|} \\
    e        &              & \\
\hline\hline
\end{array}
\qquad
\begin{array}[t]{r||r@@{\qquad}l}
\hline\hline
  e*e         & \mbox{}+N    & \text{shift} \\
  e*e+\mbox{} & N            & \text{shift} \\
  e*e+N       &              & \text{reduce by |e : N;|} \\
  e*e+e       &              & \text{reduce by |e : e, +, e;|} \\
  e*e         &              & \text{reduce by |e : e, * , e;|} \\
    e         &              & \\
\hline\hline
\end{array}
\]
Alas, the two choices also result in different parse trees. By default,
\Frown\ prefers shifts to reductions. As a consequence, |N*N+N| is parsed
as |N*(N+N)|, that is, `|+|' binds more tightly than `|*|'.

Now, we can direct the resolution of conflicts by assigning
\technical{precedences} and \technical{associativity} to terminal
symbols. The following declarations will do in our example (@Let2.g@).
%
%include Examples/Let2.lg
%
Thus, `|*|' takes precedence over `|+|', which in turn binds more
tightly than `|in|'. For instance, |let a = 4 in a + 2| is parsed as
|let a = 4 in (a + 2)|. A conflict between two symbols of equal
precedence is resolved using \technical{associativity}: the succession
|1+2+3| of left-associative operators is grouped as |(1+2)+3|;
likewise for right-associative operators; sequences of non-associative
operators are not well-formed.

Given the fixity declarations above \Frown\ now produces the `right'
deterministic parser, which can be seen in action below.
%
%include Examples/Let2.session
%

In general, a conflict between the actions `reduce by rule |r|' and
`shift terminal |t|' is resolved as follows (the precedence of a rule
is given by the precedence of the rightmost terminal on the right-hand
side):
%
\begin{center}
\begin{tabular}{lll@@{\qquad\qquad}l}\hline\hline
condition         &              & action & example \\\hline\hline
|prec r < prec t| &              & shift  & reduce by |e:e,+,e;|  versus shift |*|  \\\hline
                  & |left t|     & reduce & reduce by |e:e,*,e;|  versus shift |*|  \\
|prec r = prec t| & |right t|    & shift  & reduce by |e:e,++,e;| versus shift |++| \\
                  & |nonassoc t| & fail   & reduce by |e:e,==,e;| versus shift |==| \\\hline
|prec r > prec t| &              & reduce & reduce by |e:e,*,e;|  versus shift |+|  \\
\hline\hline
\end{tabular}
\end{center}

Just in case you may wonder: there are no shift/shift conflicts by
construction; reduce/reduce conflicts cannot be cured using
precedences and associativity.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Multiple start symbols}
\label{sec:multiple}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

A grammar may have several start symbols. In this case, \Frown\ 
generates multiple parsers, one for each start symbol (actually, these
are merely different entry points into the LR($0$)
automaton\footnote{There is, however,  a small cost involved: for each start
  symbol |s| \Frown\ silently introduces a new symbol |s'| and a new
  rule |s' : s, EOF|. This increases the size of the automaton by a
  few states.}). We mark a symbol as a start symbol simply by putting
an asterix before its declaration (either in a |Nonterminal|
declaration or in a separate type signature). Consider our previous
example: most likely we want parsers both for expressions and
declarations. Thus, we write

< *expr  {Expr};
< *decl  {Decl};

and get parsers of type.

< expr                          :: (Monad m) => [Terminal] -> m Expr
< decl                          :: (Monad m) => [Terminal] -> m Decl {-"."-}

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Monadic attributes}
\label{sec:reader}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

This section does not introduce any new features of \Frown\ and can be
safely skipped on first reading. Its purpose is to show how to
simulate inherited attributes using a reader monad (see also
Sec.~\ref{sec:inherited}). Generally, inherited attributes are used to
pass context information down the parse tree. As an example, consider
implementing an evaluator for arithmetic expressions that include
variables and |let|-bindings (@Let3.lg@). To determine the value of a
variable we need to pass down an environment that records the values
of bound variables.  The reader monad displayed in Fig.~\ref{fig:reader}
(@Reader.lhs@) serves exactly this purpose.
%
\begin{figure}
%include Examples/Reader.lhs
\caption{\label{fig:reader}The reader monad (@Reader.lhs@).}
\end{figure}
%
%include Examples/Let3.lg
%
Let's see the evaluator in action.
%
%include Examples/Let3.session
%

%-------------------------------=  --------------------------------------------
\section{Error reporting and correction}
\label{sec:error-reporting}
%-------------------------------=  --------------------------------------------

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Monadic lexers}
\label{sec:mlexer}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

The chances that parsing succeeds are probably smaller than the
chances that it fails. Good error messages are indispensable to turn
the latter into the former case. Up to now we only produced the rather
uninformative message |"syntax error"|.  Fortunately, we are in a good
position to do better. LR parsing has the nice property that it
detects a syntax error at the earliest possible moment: parsing fails
as soon as the input cannot be extended to a legal sentence of the
grammar. For instance, the syntax error in $|let|\; a = 4 * (7 + 1 -
1\;|in|\;a * a$ is detected after reading the keyword `|in|'.

Now, all we have to do is to keep track of context information: the
current line and column number and possibly the filename. This section
prepares the ground for maintaining state information; the parser that
actually keeps track of line numbers etc is only shown in the next
section.

Unsurprisingly, to maintain state information we employ monads again.
This time, we require a state monad. The natural place for maintaining
information about line numbers etc is, of course, the lexer.
Consequently, we turn the stream-based lexer of type |String ->
[Terminal]| into a monadic one of type

< get                           :: M Terminal

where |M| is the state monad. The idea is that each time |get| is
called it returns the next token and updates its internal state.

The first version of the monadic lexer shown in Fig.~\ref{fig:mlexer1}
(@MLexer1.lhs@) has no internal state apart from the input stream,
that is, it provides no additional functionality compared to the
stream-based lexer.
%
\begin{figure}
%include Examples/MLexer1.lhs
\caption{\label{fig:mlexer1}A monadic lexer for the |let| language (@MLexer1.lhs@).}
\end{figure}
%
Note that we use a continuation-based state monad, |Lex m|, which
requires local universal quantification (a non-Haskell~98 feature).
Actually, |Lex| is even a \technical{monad transformer} so that we can
freely choose a base monad (such as |Result| or |IO|). Of course, an
`ordinary' state monad would do, as well.  The monadic lexer |get|
incorporates more or less the stream-based lexer. We only changed the
recursive calls to lexer (ie |t : lexer cs|) into invocations of the
continuation (ie |cont t cs|). The error routine |frown| now has type

< frown                         :: (Monad m) => Terminal -> Lex m a {-","-}

that is, |frown| is no longer passed the remaining input but only the
look-ahead token.

The changes to the grammar are minor: we have to declare an `end of
file' token marked by a star (@Let4.lg@)
%
%include Examples/Let4.lg
%
When we generate the Haskell parser we must supply the option @--lexer@
to inform \Frown\ that we use a monadic lexer.
\begin{quote}
@frown --lexer Let.g@
\end{quote}

For completeness, here is an interactive session (note that in the
case of error the look-ahead token is \emph{not} displayed).
%
%subst char a           = "\texttt{" a "}"
%include Examples/Let4.session
%subst char a           = "\texttt{''" a "''}"
%

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Error reporting}
\label{sec:reporting}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

The monadic lexer shown in Fig.~\ref{fig:mlexer2} (@MLexer2.lhs@)
builds upon the one given in the previous section. The state monad
|Lex m| has been extended to keep track of the current line number and
the current line itself. The current line is displayed in case of a
lexical or syntax error. As an aside, note that the column number can
be recreated from the rest of the input and the length of the current
line.
%
\begin{figure}
%include Examples/MLexer2.lhs
\caption{\label{fig:mlexer2}A monadic lexer for the |let| language featuring good error reports (@MLexer2.lhs@).}
\end{figure}
% include Examples/Let5.lg

The following session shows the new lexer in action.
%
%subst char a           = "\texttt{" a "}"
%include Examples/Let5.session
%subst char a           = "\texttt{''" a "''}"
%
In the case of a lexical error the cursor `@^@' points at the
offending character. In the case of a syntax error the cursor points
at the \emph{last} character of the offending token (recall that the
part of the input up to and including this token is the shortest
prefix of the input that cannot be extended to a legal sentence of the
grammar).

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Expected tokens}
\label{sec:expected}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

We can do even better! We can instruct \Frown\ to pass a list of
\technical{expected} tokens to the error routine |frown| (by supplying
the option @--expected@).
%
\begin{quote}
@frown --lexer --expected Let.g@
\end{quote}

\Frown\ uses the shortcuts given in the terminal declaration for generating
lists of expected tokens. This means, in particular, that a token is
\emph{not} included in such a list if it does not have a shortcut. In
our running example, we want every token to be listed. Therefore, we
add shortcuts for every terminal symbol (@Let6.lg@).
%
%include Examples/Let6.lg
%
The error routine |frown| now takes an additional argument of type
|[String]| (@MLexer3.lhs@).
%
%include Examples/MLexer3.lhs
%
The interactive session listed in Fig.~\ref{fig:session} is a bit
longer than usual to illustrate the quality of the error messages.
%
\begin{figure}
%subst char a           = "\texttt{" a "}"
%include Examples/Let6.session
%subst char a           = "\texttt{''" a "''}"
\caption{\label{fig:session}A session full of syntax errors.}
\end{figure}

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Error correction}
\label{sec:errorcorrection}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

So far we have content ourselves with reporting syntax errors. To a
limited extent it is also possible to \technical{correct} errors.
Consider the last rule of the following grammar (@Let7.lg@).
%
%include Examples/Let7.lg
%
The symbol |insert ")"| instructs \Frown\ to automatically insert a
|")"| token \emph{if parsing would otherwise fail}. The special symbol
|insert ")"| can be seen as being defined by the $\epsilon$-production
|insert ")" {-"\;"-} : {-"\;"-};|. The difference to an `ordinary'
user-defined $\epsilon$-production is that the rule is only applied if
every other action would fail.

The following session shows the error correction in action.
%
%subst char a           = "\texttt{" a "}"
%include Examples/Let7.session
%subst char a           = "\texttt{''" a "''}"
%
In the last query the missing parenthesis `$)$' is inserted just
before the keyword `|in|'. This may or may not what the user intended!

It is generally a good idea to notify the user if a token is inserted.
This is relatively easy to accomplish using monadic actions
(@Let8.lg@). The parsing monad is now |Lex IO|; the monad transformer
|Lex| proves its worth.
%
%include Examples/Let8.lg
%
Let's repeat the last query of the previous session.
%
%subst char a           = "\texttt{" a "}"
%include Examples/Let8.session
%subst char a           = "\texttt{''" a "''}"
%
The reader is invited to extend the code so that the current source
location is additionally printed (informing the user \emph{where} the
token has been inserted).

%if False
TODO

!!Beware.

< expr  {Expr};
< expr  {Let d e}               :  "let", decl {d}, insert "in", expr {e};
<       {Bin e1 op e2}          |  expr {e1}, Addop {op}, expr {e2};
<       {Bin e1 op e2}          |  expr {e1}, Mulop {op}, expr {e2};
<       {Const n}               |  Numeral {n};
<       {Var s}                 |  Ident {s};
<       {e}                     |  "(", expr {e}, ")";
<       {e}                     |  "(", expr {e}, insert ")";

\begin{verbatim}
* warning: 3 error-correction conflicts
\end{verbatim}
%endif

%-------------------------------=  --------------------------------------------
\section{Advanced features}
%-------------------------------=  --------------------------------------------

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Rule schemes}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

When we define grammars we often find ourselves repeatedly writing
similar rules. A common pattern is the \technical{repetition} of
symbols. As an example, the following rules define a repetition of |t|
symbols.

< ts                            :  ;
< ts                            :  ts, t;

As an aside, note that the second rule is intentionally
\technical{left-recursive}.  LR parsers prefer left to right
recursion: the rules above use constant stack space whereas the
right-recursive variant requires space linear in the length of the
input.

Now, \Frown\ allows to capture recurring patterns using so-called
\technical{rule schemes}. Here is the scheme for a repetition of
symbols (of arity |0|).

< many x                        <-  x;
< many x                        :   ;
<                               |   many x, x;

The first line contains |many|'s type signature: it simply says that
neither |many| nor |many|'s argument |x| possess attributes. Given
this scheme we can simply write |many t| for a repetition of |t|
symbols.

The rule for repetition becomes more interesting if the argument
possesses an attribute (is of arity~|1|). In this case, |many|
returns a list of semantic values.

< many x {[a]}                  <-  x {a};
< many x {[]}                   <-  ;
< many x {as ++ [a]}            <-  many as {as}; x {a};

(The use of list concatenation `|++|' in the second rule incurs a
runtime penalty which we will cure later.) The first line contains
again the type signature, which we may read as a conditional clause:
if |x| has one attribute of type |a|, then |many x| has one attribute
of type |[a]|. This schemes comes in handy if we extend our little
expression language by applications and abstractions (we assume that
the abstract syntax has been extended suitably; |aexpr| denotes atomic
expressions).

< expr {App e es}               :  aexpr {e}, many aexpr {es};
< expr {Abs (i : is) e}         :  "\\", Ident {i}, many (Ident {}) {is}, ".", expr {e};

Note that if we pass terminal symbols as arguments to rule schemes
they must be written with (empty) curly braces---\Frown\ can only
identify terminal symbols, ie patterns, if they have exactly the same
syntactic form as in the terminal declaration. Think of `|{}|' as a
placeholder.

In the above definition of |many| we have used list concatenation to
append an element to a list. The following improved definition does
away with this linear-time operation employing Hughes' efficient
sequence type \cite{Hug86Nov}.

< many x  {[a]}                  <-  x {a};
< many x  {s []}                 :   many' x {s};
<
< many' x  {[a] -> [a]}          <-  x {a};
< many' x  {\ as -> as}          :   ;
<          {\ as -> s (a : as)}  |   many' x {s}, x {a};

These schemata are predefined in \Frown. There is a caveat, however:
the singleton production |many x {-"\;"-} : {-"\;"-} many' x| may
introduce a shift/reduce conflict, see Sec.~\ref{sec:conflicts}.

Actually, both the |many| scheme with no attributes and the scheme
above with one attribute are predefined. In general, it is possible to
use the same name for schemes and nonterminals of different arity. The
only restriction is that the arity of the scheme must determine the
arity of its arguments.

Another useful variation of |many| is |sepBy x sep| which denotes a
list of |x| symbols separated by |sep| symbols (|sepBy| and |sepBy1|
are predefined, as well).

< sepBy x sep  {[a]}            <-  x {a}, sep;
< sepBy x sep  {[]}             :   ;
<              {as}             |   sepBy1 x sep {as};
<
< sepBy1 x sep  {[a]}           <-  x {a}, sep;
< sepBy1 x sep  {[a]}           :   x {a};
<               {as ++ [a]}     |   sepBy1 x sep {as}, sep, x {a};

This scheme is useful for adding tuples to our expression language.

< expr {Tuple es}               :  "(", sepBy expr "," {es}, ")";

For a complete list of predefined schemes see
Sec.~\ref{sec:stdenv}.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{A second look at terminal symbols}
\label{sec:terminal2}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

The terminal symbols of a grammar are given by Haskell
\technical{patterns}. Up to now we have seen only simple
patterns. Patterns, however, may also be nested or even
overlapping. In the latter case, one should be careful to list
specific patterns before general ones in a |Terminal| declaration
(\Frown\ preserves the relative ordering of patterns when generating
|case| expressions). Here is a simple example.

< Terminal                      =  Ident "if"    as "if"
<                               |  Ident "then"  as "then"
<                               |  Ident "else"  as "else"
<                               |  Ident {String};
<                               |  ldots

Note that keywords are declared just by listing them before the
general pattern for identifiers.

Alternatively, terminal symbols can be specifed using so-called
\technical{guards}, Boolean functions of type |Terminal -> Bool|.
Guards are most useful for defining character classes as in the
following example.

< Terminal                      =  guard { isAlpha } as "alpha"
<                               |  ldots

A guard is introduced by the keyword |guard|, followed by its Haskell
definition, followed by the mandatory shortcut. The shortcut can then
be used as a terminal symbol \emph{of arity~|1|}: its attribute of
type |Terminal| is the very input symbol that matched the guard.

< ident {String};
< ident {c : cs}  :  "alpha" {c}, many "alpha" {cs};


Using guards one can quite easily define character-based grammars that
include lexical syntax (that is, whose parsers combine lexing and
parsing). Fig.~\ref{fig:varcalc} lists a variant of the desktop
calculator that works without a separate lexer.
%
\begin{figure}
%include Examples/VarCalc.lg
\caption{\label{fig:varcalc}A variant of the desktop calculator that includes
lexical syntax (@VarCalc.lhs@).}
\end{figure}
%
Note that the type |Terminal| must be defined in the Haskell section.
The reader may wish to extend the grammar so that two tokens can be
separated by white space.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Look-ahead}
\label{sec:look-ahead}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

\Todo{type grammar.}

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Debugging and tracing}
\label{sec:tracing}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

\Todo{@--prefix@ und @--suffix@.}

\Todo{@--debug@ und @--pagewidth@.}

%include Examples/Paren3.lg

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Output formats and optimizations}
\label{sec:optimizations}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

\Todo{optimizations (@--optimize@).}

\Todo{which format benefits from GHC extensions (@--ghc@)?}

\Todo{@NOINLINE@ pragmas (@--noinline@).}

\Todo{@--signature@.}

%================================  ============================================
\chapter{Tips and tricks}
%================================  ============================================

%-------------------------------=  --------------------------------------------
\section{Irrefutable patterns}
\label{sec:irrefutable}
%-------------------------------=  --------------------------------------------

Irrefutable patterns on the RHS (@VarParen.lg@):
%
%include Examples/VarParen.lg
%

%-------------------------------=  --------------------------------------------
\section{Inherited attributes}
\label{sec:inherited}
%-------------------------------=  --------------------------------------------

Shows how to simulate inherited attributes: |expr| has type |Integer
-> (Tree Integer, Integer)|, it takes the global minimum to the
rep-min tree (with all elements replaced by the minimum) and the local
minimum (@RepMin.lg@).
%
%include Examples/RepMin.lg
%
!avoid layout-sensitive code!

%-------------------------------=  --------------------------------------------
\section{Dealing with conflicts}
\label{sec:conflicts}
%-------------------------------=  --------------------------------------------

< many' x : many x;

%-------------------------------=  --------------------------------------------
\section{Multiple attributes}
\label{sec:multattr}
%-------------------------------=  --------------------------------------------

%================================  ============================================
\chapter{Reference manual}
%================================  ============================================

%-------------------------------=  --------------------------------------------
\section{Lexical syntax of \protect\Frown}
%-------------------------------=  --------------------------------------------

\Todo{that of Haskell including comments.}

\Todo{Literate grammar file (Bird tracks)}.

%-------------------------------=  --------------------------------------------
\section{Syntax of \protect\Frown}
%-------------------------------=  --------------------------------------------

%
%include GParser2.lg
%

%-------------------------------=  --------------------------------------------
\section{Predefined schemes}
\label{sec:stdenv}
%-------------------------------=  --------------------------------------------

%include ../Stdenv.lg

%-------------------------------=  --------------------------------------------
\section{Output formats}
\label{sec:formats}
%-------------------------------=  --------------------------------------------

\Todo{Used type names: |Result| and |Terminal|.}

\Todo{Used function names: |frown|. For each start symbol a parser.}

The @code=standard@ format is due to Doaitse Swierstra \cite{DuS00Fun}.

The @code=stackless@ format is due to Ross Paterson \cite{HiP05Typ}.

The @code=gvstack@ format is also due to Ross Paterson.

%
\begin{figure}

< module Paren where
< import Result
< 
< {- frown :-( -}
<
< data Stack                      =  Empty | T_1 State Stack
<  
< data State                      =  S_1 | S_2 | S_3 | S_4 | S_5 | S_6
<  
< data Nonterminal                =  Paren
<  
< paren tr                        =  parse_1 tr Empty >>= (\ Paren -> return ())
<  
< parse_1 ts st                   =  reduce_2 ts S_1 st
<  
< parse_2 tr@[] st                =  parse_3 tr (T_1 S_2 st)
< parse_2 ('(' : tr) st           =  parse_5 tr (T_1 S_2 st)
< parse_2 ts st                   =  frown ts
<  
< parse_3 ts st                   =  reduce_1 ts st
<  
< parse_4 ('(' : tr) st           =  parse_5 tr (T_1 S_4 st)
< parse_4 (')' : tr) st           =  parse_6 tr (T_1 S_4 st)
< parse_4 ts st                   =  frown ts
<  
< parse_5 ts st                   =  reduce_2 ts S_5 st
<  
< parse_6 ts st                   =  reduce_3 ts st
<  
< reduce_1 ts (T_1 _ (T_1 s st))  =  return Paren
<  
< reduce_2 ts s st                =  goto_5 s ts (T_1 s st)
<  
< reduce_3 ts (T_1 _ (T_1 _ (T_1 _ (T_1 s st))))
<                                 =  goto_5 s ts (T_1 s st)
<  
< goto_5 S_1                      =  parse_2
< goto_5 S_5                      =  parse_4
<  
< {- )-: frown -}
<
< frown _                         =  fail "syntax error"

\caption{\label{fig:ex1cc}@frown --code=compact Paren.g@.}
\end{figure}

%
\begin{figure}

< module Paren where
< import Result
<
< {- frown :-( -}
<
< paren tr                      =  state_1 (\ _ -> return ()) tr
<  
< state_1 k_1_0 ts              =  let { goto_paren = state_2 k_1_0 (reduce_3 goto_paren) }
<                                  in reduce_2 goto_paren ts
<  
< state_2 k_1_1 k_3_1 ts        =  case ts of {  tr@[]     -> state_3 k_1_1 tr;
<                                                '(' : tr  -> state_5 k_3_1 tr;
<                                                _         -> frown ts }
<  
< state_3 k_1_2 ts              =  k_1_2 ts
<  
< state_4 k_3_1 k_3_3 ts        =  case ts of {  '(' : tr  -> state_5 k_3_1 tr;
<                                                ')' : tr  -> state_6 k_3_3 tr;
<                                                _         -> frown ts }
<  
< state_5 k_3_2 ts              =  let { goto_paren = state_4 (reduce_3 goto_paren) k_3_2 }
<                                  in reduce_2 goto_paren ts
<  
< state_6 k_3_4 ts              =  k_3_4 ts
<  
< reduce_2 g ts                 =  g ts
<  
< reduce_3 g ts                 =  g ts
<
< {- )-: frown -}
<
< frown _                       =  fail "syntax error"

\caption{\label{fig:ex1cs}@frown --code=stackless Paren.g@.}
\end{figure}

\begin{figure}

< module Paren1 where
< import Result
< 
< {- frown :-( -}
<  
< data Nonterminal              =   Paren' | Paren
< 
< type Parser                   =   [Terminal] -> Result Nonterminal
< 
< type VStack vs v              =   ((vs, Nonterminal -> Parser), v)
< 
< paren tr                      =   state_1 () tr >>= (\ Paren' -> return ())
< 
< state_1                       ::  vs -> Parser
< state_1                       =   state action_1 goto_1
< action_1 t                    =   reduce_2
< goto_1 Paren                  =   goto state_2 ()
< 
< 
< state_2                       ::  VStack vs () -> Parser
< state_2                       =  state action_2 undefined
< action_2 t                    =  case t of {  '('  -> shift state_5 ();
<                                               '$'  -> shift state_3 ();
<                                               _    -> error }
< 
< state_3                       ::  VStack (VStack vs ()) () -> Parser
< state_3                       =   state action_3 undefined
< action_3 t                    =   reduce_1
< 
< state_4                       ::  VStack (VStack (VStack vs ()) ()) () -> Parser
< state_4                       =   state action_4 undefined
< action_4 t                    =   case t of {  '('  -> shift state_5 ();
<                                                ')'  -> shift state_6 ();
<                                                _    -> error }
< 
< state_5                       ::  VStack (VStack vs ()) () -> Parser
< state_5                       =   state action_5 goto_5
< action_5 t                    =   reduce_2
< goto_5 Paren                  =   goto state_4 ()
< 
< state_6                       ::  VStack (VStack (VStack (VStack vs ()) ()) ()) () -> Parser
< state_6                       =   state action_6 undefined
< action_6 t                    =   reduce_3
< 
< 
< reduce_1 (((((_, g), ()), _), ()), _) ts 
<                               =   accept Paren' ts
< 
< reduce_2 (_, g) ts            =   g Paren ts
< 
< reduce_3 (((((((((_, g), ()), _), ()), _), ()), _), ()), _) ts 
<                               =   g Paren ts
< 
< state action goto vs ts       =   let { gs = (vs, g); g v = goto v gs } in action (head ts) gs ts
< 
< shift state v vs ts           =   state (vs, v) (tail ts)
< 
< shift' state v vs ts          =   state (vs, v) ts
< 
< accept v _                    =   return v
< 
< goto state v vs               =   state (vs, v)
< 
< error gs ts                   =   frown ts
<
< {- )-: frown -}
< 
< frown _                       =   fail "syntax error"

\caption{\label{fig:ex1cg}@frown --code=gvstack Paren.g@ (requires an explicit EOF symbol).}
\end{figure}

%-------------------------------=  --------------------------------------------
\section{Invocation and options}
\label{sec:options}
%-------------------------------=  --------------------------------------------

@Usage: frown [option ...] file.g ...@

\begin{description}
\item[@-b@ or @--backtrack@] \mbox{}\\
generate a backtracking parser (see Sec.~\ref{sec:backtracking})

\item[@-cc@, @-ccompact@ or @--code=compact@] \mbox{}\\
  (see Sec.~\ref{sec:optimizations} and~\ref{sec:formats})

\item[@-cg@, @-cgvstack@ or @--code=gvstack@] \mbox{}\\
  (see Sec.~\ref{sec:optimizations} and~\ref{sec:formats})

\item[@-cs@, @-cstackless@ or @--code=stackless@] \mbox{}\\
  (see Sec.~\ref{sec:optimizations} and~\ref{sec:formats})

\item[@-cstandard@ or @--code=standard@] \mbox{}\\
  (see Sec.~\ref{sec:optimizations} and~\ref{sec:formats})

\item[@--copying@] \mbox{}\\
display details of copying

\item[@-d@ or @--debug@] \mbox{}\\
  emit debugging information (see Sec.~\ref{sec:tracing})

\item[@-e@ or @--expected@] \mbox{}\\
  pass a list of expected terminals to `|frown|' (see Sec.~\ref{sec:expected})

\item[@-g@ or @--ghc@] \mbox{}\\
  use GHC extensions (see Sec.~\ref{sec:optimizations})

\item[@-h@, @-?@ or @--help@] \mbox{}\\

\item[@-i@ or @--info@] \mbox{}\\
  put additional information into generated file (see Sec.~\ref{sec:tracing})

\item[@-k[nat]@ or @--lookahead[=nat]@] \mbox{}\\
  use $k$ tokens of look-ahead (see Sec.~\ref{sec:look-ahead})

\item[@-l@ or @--lexer@] \mbox{}\\
  use a monadic lexer (|get :: M Terminal|) (see Sec.~\ref{sec:mlexer})

\item[@-n@ or @--noinline@] \mbox{}\\
  generate @NOINLINE@ pragmas (see Sec.~\ref{sec:optimizations})

\item[@-O@ or @--optimize @] \mbox{}\\
  optimize parser (see Sec.~\ref{sec:optimizations})

\item[@-p[nat]@ or @--pagewidth[=nat]@] \mbox{}\\
  use the specified pagewidth for pretty printing (see Sec.~\ref{sec:tracing})

\item[@--prefix[=string]@] \mbox{}\\
  use prefix for \Frown-generated variables (see Sec.~\ref{sec:tracing})

\item[@-sm@, @-smono@ or @--signature=mono@] \mbox{}\\
  add monomorphic type signatures (see Sec.~\ref{sec:optimizations})

\item[@-sp@, @-spoly@ or @--signature=poly@] \mbox{}\\
  add polymorphic type signatures (see Sec.~\ref{sec:optimizations})

\item[@--suffix[=string]@] \mbox{}\\
  use suffix for frown generated variables (see Sec.~\ref{sec:tracing})

\item[@-t@ or @--trace@] \mbox{}\\
  insert calls to tracing routines (`|shift|', `|reduce|' and `|accept|')
  (see Sec.~\ref{sec:tracing})

\item[@-v@ or @--verbose@] \mbox{}\\
  be verbose

\item[@--version@] \mbox{}\\
  print version information

\item[@--warranty@] \mbox{}\\
  display details of warranty
\end{description}

\bibliographystyle{plain}
\bibliography{Manual}

\newpage
\begin{center}
\includegraphics{Pics/tuberling.ps}
\end{center}

\end{document}
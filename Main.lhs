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

Compile me with

        ghc -O -o frown --make -package util -package rh Main.lhs

        hmake -nhc98 -98 -ILib -PLib Main

> module Main
> where
> import Lexer2
> --import Quote
> import GParser2
> import Convert
> import LR0
> import Lookahead
> import Case
> -- import Optimize
> import qualified Standard
> import qualified Compact
> import qualified GVStack
> import qualified Stackless
> import Base
> import Options
> import Prettier               hiding (  string, concat  )
> import qualified Prettier as PP
> import qualified SearchTree as ST
> import Control.Applicative
> import Control.Monad                  (  when  )
> import System.Directory
> import System.Environment
> import System.Exit
> import System.IO
> import System.Console.GetOpt
> import Prelude                        hiding ( (<>) )

%-------------------------------=  --------------------------------------------
\section{The main program}
%-------------------------------=  --------------------------------------------

> main                          :: IO ()
> main                          =  getOpt Permute options <$> getArgs >>= \ case
>                                      (opts, fnames, []  )
>                                          | Copying `elem` opts ->
>                                              putStrLn copying
>                                          | Warranty `elem` opts ->
>                                              putStrLn warranty
>                                          | Help `elem` opts ->
>                                              putStrLn (usageInfo header options)
>                                          | Version `elem` opts ->
>                                              putStrLn license
>                                          | [] <- fnames ->
>                                              getContents >>= frown False opts >>= putStr
>                                          | otherwise ->
>                                              mapM_ (frownFile opts) fnames
>                                      (_, _, errors) ->
>                                          panic (concat errors ++ usageInfo header options)
>     where header              =  "Usage: frown [option ...] file.[l]g ..."

> license                       :: String
> license                       =
>     "Frown :-(version 0.6), Copyright (C) 2001-2005 Ralf Hinze\n\n\
>     \Frown comes with ABSOLUTELY NO WARRANTY; for details type `frown --warranty'.\n\
>     \This is free software, and you are welcome to redistribute it\n\
>     \under certain conditions; type `frown --copying' for details."

%-------------------------------=  --------------------------------------------
\section{File handling}
%-------------------------------=  --------------------------------------------

> haskell                       :: FilePath -> FilePath
> haskell fname
>     | ext `elem` ["g", "lg"]  =  base ++ "hs"
>     | otherwise               =  fname ++ ".hs"
>     where (base, ext)         =  revBreak (== '.') fname

> revBreak                      :: (a -> Bool) -> [a] -> ([a], [a])
> revBreak p as                 =  (reverse as2, reverse as1)
>     where (as1, as2)          =  break p (reverse as)

> safeWriteFile                 :: FilePath -> String -> IO ()
> safeWriteFile fname contents  =  do exists <- doesFileExist fname
>                                     when exists (safeRenameFile 0 fname)
>                                     writeFile fname contents

> safeRenameFile                :: Int -> FilePath -> IO ()
> safeRenameFile i fname        =  do let fname' = fname ++ show i
>                                     exists <- doesFileExist fname'
>                                     if exists then safeRenameFile (i + 1) fname
>                                               else renameFile fname fname'

%-------------------------------=  --------------------------------------------
\section{Putting everything together}
%-------------------------------=  --------------------------------------------

Should go to |Pretty|.

> indent                        :: String -> Doc -> Doc
> indent s d                    =  PP.string s <> group (nest (length s) d)

> frownFile                     :: [Flag] -> FilePath -> IO ()
> frownFile opts fname          =
>     verbose opts ("*** Processing " ++ fname) >>
>     case literate of { True -> unlit; _ -> id; } <$> readFile fname >>= frown literate opts >>=
>     (verbose opts ("* Writing to " ++ haskell fname) >>) . safeWriteFile (haskell fname)
>     where literate = snd (revBreak (== '.') fname) == "lg"

> frown                         :: Bool -> [Flag] -> [Char] -> IO [Char]
> frown literate opts raw       =
>     do
>
>--        ts <- tokenize opts raw
>--        debug "Token list" (pretty ts)
>
>--        ts' <- unquotify opts ts
>--        debug "Quote" (pretty ts')
>
>        (l, ds, r) <- parse opts raw
>        debug "abstract syntax" (pretty ds)
>
>        (g, maxRHS) <- convert opts ds
>        debug "Grammar" (pretty g)
>
>        (states, initials, gotoTable, reachable) <- lr0automaton opts g
>        debugAuto
>            <- debug "LR(0) automaton" (
>                   header "States"
>                   <> intersperse nl [ indent (rjustify 5 (show n) ++ ": ") (pretty q)
>                                     | State n q <- states ] <> nl <> nl
>                   <> header "Initial states"
>                   <> pretty initials <> nl <> nl
>                   <> header "Goto table"
>                   <> pretty gotoTable)
>
>        -- let nStates      = length states
>        -- let nShiftStates = length [ s | s <- states, not (or [ nonterminal v | (s1, v, s2) <- gotoTable, s1 == s ]) ]
>        -- putStrLn (show nShiftStates ++ " of " ++ show nStates ++ " states.")
>
>        table <- lr0info opts states gotoTable
>        debug "LR(0) information" (
>            header "Shift and reduce table"
>            <> pretty table)
>
>        let k = lookahead opts
>
>        kActionTable   <- klookahead opts g gotoTable table
>        debugkActionTable
>            <- debug "k-lookahead information" (
>                  header ("Action table augmented by LA(" ++ show k ++ ") information")
>                  <> ppActionTable kActionTable)
>
>        let optkActionTable =  kActionTable
> {- not up to date
>        (optkActionTable, d_optkActionTable) <-
>            if Optimize `elem` opts then
>                do verb "Eliminating single reductions ..."
>                   let optkActionTable = optimize kActionTable
>                   d_optkActionTable <- debug "optimized reductions" (
>                                          header ("optimized action table")
>                                          <> ppActionTable optkActionTable)
>                   return (optkActionTable, d_optkActionTable)
>            else
>                return (kActionTable, "")
> -}
>        branchTable <- branchLogic opts optkActionTable
>        debugBranchTable <- debug "branch logic" (
>            PP.concat [ header ("State " ++ show (snumber s))
>                        <> pretty branch <> nl <> nl
>                      | (s, branch) <- ST.toList branchTable ])
>
>        prg <- if Code GVStack `elem` opts then
>                   GVStack.generate opts g initials gotoTable branchTable -- optkActionTable
>               else if Code Stackless `elem` opts then
>                   Stackless.generate opts g initials gotoTable branchTable -- optkActionTable
>               else if Code Compact `elem` opts then
>                   Compact.generate opts g initials reachable gotoTable branchTable -- optkActionTable
>               else
>                   Standard.generate opts initials gotoTable branchTable -- optkActionTable
>
>        return
>            ("" -- "{-# OPTIONS -fno-warn-unused-binds #-}\n\n"
>             ++ concatMap toString l
>             ++ "\n\n{-\n\n"
>             ++ (if Info `elem` opts then debugAuto         else "")
>--             ++ (if Info `elem` opts then debugkActionTable else "")
>             ++ (if Info `elem` opts then debugBranchTable else "")
>             ++ "\n\n-}\n\n"
>             ++ begin
>             ++ prettyProgram prg
>             ++ end
>             ++ concatMap toString r)
>     where
>     pageWidth                 =  head ([ w | Pagewidth w <- opts ] ++ [80]) `max` 40
>     prettyProgram ds
>       | literate              =  unlines [ "  " ++ l | l <- lines str ]
>       | otherwise             =  str
>       where str               =  concat [ render (Page pageWidth) (pretty d) ++ "\n" | d <- ds ]
>
>     debug s a                 =  when (Debug `elem` opts) (hPutStr stderr t) >> return t
>       where t                 =  cjustifyWith '*' pageWidth (" " ++ s++ " ") ++ "\n\n"
>                               ++ render (Page pageWidth) a ++ "\n\n"
>
>     verb                      =  verbose opts

> begin, end                    :: String
> begin                         =  "\n\n{- frown :-( -}\n\n"
> end                           =  "\n\n{- )-: frown -}\n\n"

%-------------------------------=  --------------------------------------------
\section{A simple unlitifier}
%-------------------------------=  --------------------------------------------

Stolen from Happy.

At the beginning of a line.

> unlit, unlit1, unlit2         :: String -> String
> unlit ('>' : ' ' : r)         =  ' ' : ' ' : unlit1 r
> unlit ('>' : '\t' : r)        =  ' ' : '\t' : unlit1 r
> unlit ('>' : '\n' : r)        =  ' ' : '\n' : unlit r
> unlit ('>' : r)               =  ' ' : unlit1 r
> unlit ('\n' : r)              =  '\n' : unlit r
> unlit r                       =  unlit2 r

Within program text.

> unlit1 ('\n' : r)             =  '\n' : unlit r
> unlit1 (c : r)                =  c : unlit1 r
> unlit1 []                     =  []

Within a literate comment.

> unlit2 ('\n' : r)             =  '\n' : unlit r
> unlit2 (_ : r)                =  unlit2 r
> unlit2 []                     =  []

%-------------------------------=  --------------------------------------------
\section{License and warranty}
%-------------------------------=  --------------------------------------------

> copying                       :: String
> copying                       =
>     "\t\t    GNU GENERAL PUBLIC LICENSE\n\
>     \\t\t       Version 2, June 1991\n\
>     \\n\
>     \ Copyright (C) 1989, 1991 Free Software Foundation, Inc.\n\
>     \                          59 Temple Place - Suite 330\n\
>     \                          Boston, MA 02111-1307, USA.\n\
>     \ Everyone is permitted to copy and distribute verbatim copies\n\
>     \ of this license document, but changing it is not allowed.\n\
>     \\n\
>     \\t\t\t    Preamble\n\
>     \\n\
>     \  The licenses for most software are designed to take away your\n\
>     \freedom to share and change it.  By contrast, the GNU General Public\n\
>     \License is intended to guarantee your freedom to share and change free\n\
>     \software--to make sure the software is free for all its users.  This\n\
>     \General Public License applies to most of the Free Software\n\
>     \Foundation's software and to any other program whose authors commit to\n\
>     \using it.  (Some other Free Software Foundation software is covered by\n\
>     \the GNU Library General Public License instead.)  You can apply it to\n\
>     \your programs, too.\n\
>     \\n\
>     \  When we speak of free software, we are referring to freedom, not\n\
>     \price.  Our General Public Licenses are designed to make sure that you\n\
>     \have the freedom to distribute copies of free software (and charge for\n\
>     \this service if you wish), that you receive source code or can get it\n\
>     \if you want it, that you can change the software or use pieces of it\n\
>     \in new free programs; and that you know you can do these things.\n\
>     \\n\
>     \  To protect your rights, we need to make restrictions that forbid\n\
>     \anyone to deny you these rights or to ask you to surrender the rights.\n\
>     \These restrictions translate to certain responsibilities for you if you\n\
>     \distribute copies of the software, or if you modify it.\n\
>     \\n\
>     \  For example, if you distribute copies of such a program, whether\n\
>     \gratis or for a fee, you must give the recipients all the rights that\n\
>     \you have.  You must make sure that they, too, receive or can get the\n\
>     \source code.  And you must show them these terms so they know their\n\
>     \rights.\n\
>     \\n\
>     \  We protect your rights with two steps: (1) copyright the software, and\n\
>     \(2) offer you this license which gives you legal permission to copy,\n\
>     \distribute and/or modify the software.\n\
>     \\n\
>     \  Also, for each author's protection and ours, we want to make certain\n\
>     \that everyone understands that there is no warranty for this free\n\
>     \software.  If the software is modified by someone else and passed on, we\n\
>     \want its recipients to know that what they have is not the original, so\n\
>     \that any problems introduced by others will not reflect on the original\n\
>     \authors' reputations.\n\
>     \\n\
>     \  Finally, any free program is threatened constantly by software\n\
>     \patents.  We wish to avoid the danger that redistributors of a free\n\
>     \program will individually obtain patent licenses, in effect making the\n\
>     \program proprietary.  To prevent this, we have made it clear that any\n\
>     \patent must be licensed for everyone's free use or not licensed at all.\n\
>     \\n\
>     \  The precise terms and conditions for copying, distribution and\n\
>     \modification follow.\n\
>     \\f\n\
>     \\t\t    GNU GENERAL PUBLIC LICENSE\n\
>     \   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION\n\
>     \\n\
>     \  0. This License applies to any program or other work which contains\n\
>     \a notice placed by the copyright holder saying it may be distributed\n\
>     \under the terms of this General Public License.  The \"Program\", below,\n\
>     \refers to any such program or work, and a \"work based on the Program\"\n\
>     \means either the Program or any derivative work under copyright law:\n\
>     \that is to say, a work containing the Program or a portion of it,\n\
>     \either verbatim or with modifications and/or translated into another\n\
>     \language.  (Hereinafter, translation is included without limitation in\n\
>     \the term \"modification\".)  Each licensee is addressed as \"you\".\n\
>     \\n\
>     \Activities other than copying, distribution and modification are not\n\
>     \covered by this License; they are outside its scope.  The act of\n\
>     \running the Program is not restricted, and the output from the Program\n\
>     \is covered only if its contents constitute a work based on the\n\
>     \Program (independent of having been made by running the Program).\n\
>     \Whether that is true depends on what the Program does.\n\
>     \\n\
>     \  1. You may copy and distribute verbatim copies of the Program's\n\
>     \source code as you receive it, in any medium, provided that you\n\
>     \conspicuously and appropriately publish on each copy an appropriate\n\
>     \copyright notice and disclaimer of warranty; keep intact all the\n\
>     \notices that refer to this License and to the absence of any warranty;\n\
>     \and give any other recipients of the Program a copy of this License\n\
>     \along with the Program.\n\
>     \\n\
>     \You may charge a fee for the physical act of transferring a copy, and\n\
>     \you may at your option offer warranty protection in exchange for a fee.\n\
>     \\n\
>     \  2. You may modify your copy or copies of the Program or any portion\n\
>     \of it, thus forming a work based on the Program, and copy and\n\
>     \distribute such modifications or work under the terms of Section 1\n\
>     \above, provided that you also meet all of these conditions:\n\
>     \\n\
>     \    a) You must cause the modified files to carry prominent notices\n\
>     \    stating that you changed the files and the date of any change.\n\
>     \\n\
>     \    b) You must cause any work that you distribute or publish, that in\n\
>     \    whole or in part contains or is derived from the Program or any\n\
>     \    part thereof, to be licensed as a whole at no charge to all third\n\
>     \    parties under the terms of this License.\n\
>     \\n\
>     \    c) If the modified program normally reads commands interactively\n\
>     \    when run, you must cause it, when started running for such\n\
>     \    interactive use in the most ordinary way, to print or display an\n\
>     \    announcement including an appropriate copyright notice and a\n\
>     \    notice that there is no warranty (or else, saying that you provide\n\
>     \    a warranty) and that users may redistribute the program under\n\
>     \    these conditions, and telling the user how to view a copy of this\n\
>     \    License.  (Exception: if the Program itself is interactive but\n\
>     \    does not normally print such an announcement, your work based on\n\
>     \    the Program is not required to print an announcement.)\n\
>     \\f\n\
>     \These requirements apply to the modified work as a whole.  If\n\
>     \identifiable sections of that work are not derived from the Program,\n\
>     \and can be reasonably considered independent and separate works in\n\
>     \themselves, then this License, and its terms, do not apply to those\n\
>     \sections when you distribute them as separate works.  But when you\n\
>     \distribute the same sections as part of a whole which is a work based\n\
>     \on the Program, the distribution of the whole must be on the terms of\n\
>     \this License, whose permissions for other licensees extend to the\n\
>     \entire whole, and thus to each and every part regardless of who wrote it.\n\
>     \\n\
>     \Thus, it is not the intent of this section to claim rights or contest\n\
>     \your rights to work written entirely by you; rather, the intent is to\n\
>     \exercise the right to control the distribution of derivative or\n\
>     \collective works based on the Program.\n\
>     \\n\
>     \In addition, mere aggregation of another work not based on the Program\n\
>     \with the Program (or with a work based on the Program) on a volume of\n\
>     \a storage or distribution medium does not bring the other work under\n\
>     \the scope of this License.\n\
>     \\n\
>     \  3. You may copy and distribute the Program (or a work based on it,\n\
>     \under Section 2) in object code or executable form under the terms of\n\
>     \Sections 1 and 2 above provided that you also do one of the following:\n\
>     \\n\
>     \    a) Accompany it with the complete corresponding machine-readable\n\
>     \    source code, which must be distributed under the terms of Sections\n\
>     \    1 and 2 above on a medium customarily used for software interchange; or,\n\
>     \\n\
>     \    b) Accompany it with a written offer, valid for at least three\n\
>     \    years, to give any third party, for a charge no more than your\n\
>     \    cost of physically performing source distribution, a complete\n\
>     \    machine-readable copy of the corresponding source code, to be\n\
>     \    distributed under the terms of Sections 1 and 2 above on a medium\n\
>     \    customarily used for software interchange; or,\n\
>     \\n\
>     \    c) Accompany it with the information you received as to the offer\n\
>     \    to distribute corresponding source code.  (This alternative is\n\
>     \    allowed only for noncommercial distribution and only if you\n\
>     \    received the program in object code or executable form with such\n\
>     \    an offer, in accord with Subsection b above.)\n\
>     \\n\
>     \The source code for a work means the preferred form of the work for\n\
>     \making modifications to it.  For an executable work, complete source\n\
>     \code means all the source code for all modules it contains, plus any\n\
>     \associated interface definition files, plus the scripts used to\n\
>     \control compilation and installation of the executable.  However, as a\n\
>     \special exception, the source code distributed need not include\n\
>     \anything that is normally distributed (in either source or binary\n\
>     \form) with the major components (compiler, kernel, and so on) of the\n\
>     \operating system on which the executable runs, unless that component\n\
>     \itself accompanies the executable.\n\
>     \\n\
>     \If distribution of executable or object code is made by offering\n\
>     \access to copy from a designated place, then offering equivalent\n\
>     \access to copy the source code from the same place counts as\n\
>     \distribution of the source code, even though third parties are not\n\
>     \compelled to copy the source along with the object code.\n\
>     \\f\n\
>     \  4. You may not copy, modify, sublicense, or distribute the Program\n\
>     \except as expressly provided under this License.  Any attempt\n\
>     \otherwise to copy, modify, sublicense or distribute the Program is\n\
>     \void, and will automatically terminate your rights under this License.\n\
>     \However, parties who have received copies, or rights, from you under\n\
>     \this License will not have their licenses terminated so long as such\n\
>     \parties remain in full compliance.\n\
>     \\n\
>     \  5. You are not required to accept this License, since you have not\n\
>     \signed it.  However, nothing else grants you permission to modify or\n\
>     \distribute the Program or its derivative works.  These actions are\n\
>     \prohibited by law if you do not accept this License.  Therefore, by\n\
>     \modifying or distributing the Program (or any work based on the\n\
>     \Program), you indicate your acceptance of this License to do so, and\n\
>     \all its terms and conditions for copying, distributing or modifying\n\
>     \the Program or works based on it.\n\
>     \\n\
>     \  6. Each time you redistribute the Program (or any work based on the\n\
>     \Program), the recipient automatically receives a license from the\n\
>     \original licensor to copy, distribute or modify the Program subject to\n\
>     \these terms and conditions.  You may not impose any further\n\
>     \restrictions on the recipients' exercise of the rights granted herein.\n\
>     \You are not responsible for enforcing compliance by third parties to\n\
>     \this License.\n\
>     \\n\
>     \  7. If, as a consequence of a court judgment or allegation of patent\n\
>     \infringement or for any other reason (not limited to patent issues),\n\
>     \conditions are imposed on you (whether by court order, agreement or\n\
>     \otherwise) that contradict the conditions of this License, they do not\n\
>     \excuse you from the conditions of this License.  If you cannot\n\
>     \distribute so as to satisfy simultaneously your obligations under this\n\
>     \License and any other pertinent obligations, then as a consequence you\n\
>     \may not distribute the Program at all.  For example, if a patent\n\
>     \license would not permit royalty-free redistribution of the Program by\n\
>     \all those who receive copies directly or indirectly through you, then\n\
>     \the only way you could satisfy both it and this License would be to\n\
>     \refrain entirely from distribution of the Program.\n\
>     \\n\
>     \If any portion of this section is held invalid or unenforceable under\n\
>     \any particular circumstance, the balance of the section is intended to\n\
>     \apply and the section as a whole is intended to apply in other\n\
>     \circumstances.\n\
>     \\n\
>     \It is not the purpose of this section to induce you to infringe any\n\
>     \patents or other property right claims or to contest validity of any\n\
>     \such claims; this section has the sole purpose of protecting the\n\
>     \integrity of the free software distribution system, which is\n\
>     \implemented by public license practices.  Many people have made\n\
>     \generous contributions to the wide range of software distributed\n\
>     \through that system in reliance on consistent application of that\n\
>     \system; it is up to the author/donor to decide if he or she is willing\n\
>     \to distribute software through any other system and a licensee cannot\n\
>     \impose that choice.\n\
>     \\n\
>     \This section is intended to make thoroughly clear what is believed to\n\
>     \be a consequence of the rest of this License.\n\
>     \\f\n\
>     \  8. If the distribution and/or use of the Program is restricted in\n\
>     \certain countries either by patents or by copyrighted interfaces, the\n\
>     \original copyright holder who places the Program under this License\n\
>     \may add an explicit geographical distribution limitation excluding\n\
>     \those countries, so that distribution is permitted only in or among\n\
>     \countries not thus excluded.  In such case, this License incorporates\n\
>     \the limitation as if written in the body of this License.\n\
>     \\n\
>     \  9. The Free Software Foundation may publish revised and/or new versions\n\
>     \of the General Public License from time to time.  Such new versions will\n\
>     \be similar in spirit to the present version, but may differ in detail to\n\
>     \address new problems or concerns.\n\
>     \\n\
>     \Each version is given a distinguishing version number.  If the Program\n\
>     \specifies a version number of this License which applies to it and \"any\n\
>     \later version\", you have the option of following the terms and conditions\n\
>     \either of that version or of any later version published by the Free\n\
>     \Software Foundation.  If the Program does not specify a version number of\n\
>     \this License, you may choose any version ever published by the Free Software\n\
>     \Foundation.\n\
>     \\n\
>     \  10. If you wish to incorporate parts of the Program into other free\n\
>     \programs whose distribution conditions are different, write to the author\n\
>     \to ask for permission.  For software which is copyrighted by the Free\n\
>     \Software Foundation, write to the Free Software Foundation; we sometimes\n\
>     \make exceptions for this.  Our decision will be guided by the two goals\n\
>     \of preserving the free status of all derivatives of our free software and\n\
>     \of promoting the sharing and reuse of software generally.\n\
>     \\n"
>     ++ warranty ++
>     "\n\n\
>     \\t\t     END OF TERMS AND CONDITIONS\n\
>     \\f\n\
>     \\t    How to Apply These Terms to Your New Programs\n\
>     \\n\
>     \  If you develop a new program, and you want it to be of the greatest\n\
>     \possible use to the public, the best way to achieve this is to make it\n\
>     \free software which everyone can redistribute and change under these terms.\n\
>     \\n\
>     \  To do so, attach the following notices to the program.  It is safest\n\
>     \to attach them to the start of each source file to most effectively\n\
>     \convey the exclusion of warranty; and each file should have at least\n\
>     \the \"copyright\" line and a pointer to where the full notice is found.\n\
>     \\n\
>     \    <one line to give the program's name and a brief idea of what it does.>\n\
>     \    Copyright (C) 19yy  <name of author>\n\
>     \\n\
>     \    This program is free software; you can redistribute it and/or modify\n\
>     \    it under the terms of the GNU General Public License as published by\n\
>     \    the Free Software Foundation; either version 2 of the License, or\n\
>     \    (at your option) any later version.\n\
>     \\n\
>     \    This program is distributed in the hope that it will be useful,\n\
>     \    but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
>     \    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
>     \    GNU General Public License for more details.\n\
>     \\n\
>     \    You should have received a copy of the GNU General Public License\n\
>     \    along with this program; see the file COPYING.  If not, write to\n\
>     \    the Free Software Foundation, Inc., 59 Temple Place - Suite 330,\n\
>     \    Boston, MA 02111-1307, USA.\n\
>     \\n\
>     \Also add information on how to contact you by electronic and paper mail.\n\
>     \\n\
>     \If the program is interactive, make it output a short notice like this\n\
>     \when it starts in an interactive mode:\n\
>     \\n\
>     \    Gnomovision version 69, Copyright (C) 19yy name of author\n\
>     \    Gnomovision comes with ABSOLUTELY NO WARRANTY; for details type `show w'.\n\
>     \    This is free software, and you are welcome to redistribute it\n\
>     \    under certain conditions; type `show c' for details.\n\
>     \\n\
>     \The hypothetical commands `show w' and `show c' should show the appropriate\n\
>     \parts of the General Public License.  Of course, the commands you use may\n\
>     \be called something other than `show w' and `show c'; they could even be\n\
>     \mouse-clicks or menu items--whatever suits your program.\n\
>     \\n\
>     \You should also get your employer (if you work as a programmer) or your\n\
>     \school, if any, to sign a \"copyright disclaimer\" for the program, if\n\
>     \necessary.  Here is a sample; alter the names:\n\
>     \\n\
>     \  Yoyodyne, Inc., hereby disclaims all copyright interest in the program\n\
>     \  `Gnomovision' (which makes passes at compilers) written by James Hacker.\n\
>     \\n\
>     \  <signature of Ty Coon>, 1 April 1989\n\
>     \  Ty Coon, President of Vice\n\
>     \\n\
>     \This General Public License does not permit incorporating your program into\n\
>     \proprietary programs.  If your program is a subroutine library, you may\n\
>     \consider it more useful to permit linking proprietary applications with the\n\
>     \library.  If this is what you want to do, use the GNU Library General\n\
>     \Public License instead of this License."

> warranty                      :: String
> warranty                      =
>     "\t\t\t    NO WARRANTY\n\
>     \\n\
>     \  11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY\n\
>     \FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN\n\
>     \OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES\n\
>     \PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED\n\
>     \OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF\n\
>     \MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS\n\
>     \TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE\n\
>     \PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,\n\
>     \REPAIR OR CORRECTION.\n\
>     \\n\
>     \  12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING\n\
>     \WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR\n\
>     \REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,\n\
>     \INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING\n\
>     \OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED\n\
>     \TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY\n\
>     \YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER\n\
>     \PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE\n\
>     \POSSIBILITY OF SUCH DAMAGES."

%-------------------------------=  --------------------------------------------
\section{Todo}
%-------------------------------=  --------------------------------------------

Bugs
----

o  Optimizer does not work with `monadic actions' (see MCalc.g). Currently,
   the optimizer (single reductions) is disabled.

o  Lookahead does not work with `--lexer'.

Required
--------

o  Manual!!

Optional
--------

o  recognize \begin{code}..\end{code} sections

o  --expected=symbols, --expected=terminals: for the former pass the
   set of symbols following the dots to the error routine.

o  Add calls to `impossible'; for backtracking parsers use a hard/final
   failure function instead (make --expected sense in conjunction with
   --backtrack?).

o  add a flag --semicolons (--no-offside, --no-layout) to produce code that
   uses explicit layout

o  Pretty printing of abstract syntax and grammar; pretty printing of
   error messages.

o  Errors and warnings:

   1. States that are neither reduce or shift states correspond to
      unreachable noterminals in the grammar.

      S -> E, EOF;
      E -> E + T;
      T -> NAT;

      The production `|E -> T|' is missing.

   2. Symbols that are declared more than once.

o  Tracing:

   use user-defined functions `shift', `reduce' and `accept' and pass
   *all* the information to these functions (stack, input, and action).

   ODER

   So viele Informationen übergeben, daß der Stack rekonstruiert werden
   kann.

Experimental
------------

o  external parsers, modularization

o  Lookahead: alternative approach for loop detection: before an e-reduction
   is applied the stack is checked whether it contains already the transition
   and if so it is pruned.

o  Optimierungen:
   -  chain reductions (see Optimize.lhs; including dead code elimination)
   -  multiple shifts

o  implement |Delete| qualifier!

o  Introduce abbreviations for patterns (as in Happy):

                                 | let add op = ADDOP (op :: Addop)
                                 | let id  s  = IDENT (s :: String);

   |add op| serves as an abbreviation for |ADDOP op|.

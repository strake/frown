






























 module Stdenv
 where
 import Lexer2
 import Atom
 import Haskell                hiding (  Decl  )
 import Grammar
 import GParser2

 stdenv                        :: [Decl]
 stdenv                        =
     [TypeSig (App (Var (ident "opt")) (Var (ident "x")),[]) [(Var (ident "x"),[])] False,Production (App (Var (ident "opt")) (Var (ident "x")),[]) [],Production (App (Var (ident "opt")) (Var (ident "x")),[]) [(Copy,Nonterm (Var (ident "x"),[]))],TypeSig (App (Var (ident "opt")) (Var (ident "x")),[[Conid "Maybe",White " ",Varid "a"]]) [(Var (ident "x"),[[Varid "a"]])] False,Production (App (Var (ident "opt")) (Var (ident "x")),[[Conid "Nothing"]]) [],Production (App (Var (ident "opt")) (Var (ident "x")),[[Conid "Just",White " ",Varid "a"]]) [(Copy,Nonterm (Var (ident "x"),[[Varid "a"]]))],TypeSig (App (Var (ident "many")) (Var (ident "x")),[]) [(Var (ident "x"),[])] False,Production (App (Var (ident "many")) (Var (ident "x")),[]) [],Production (App (Var (ident "many")) (Var (ident "x")),[]) [(Copy,Nonterm (App (Var (ident "many")) (Var (ident "x")),[])),(Copy,Nonterm (Var (ident "x"),[]))],TypeSig (App (Var (ident "many1")) (Var (ident "x")),[]) [(Var (ident "x"),[])] False,Production (App (Var (ident "many1")) (Var (ident "x")),[]) [(Copy,Nonterm (Var (ident "x"),[])),(Copy,Nonterm (App (Var (ident "many")) (Var (ident "x")),[]))],TypeSig (App (Var (ident "many")) (Var (ident "x")),[[LeftBracket,Varid "a",RightBracket]]) [(Var (ident "x"),[[Varid "a"]])] False,Production (App (Var (ident "many")) (Var (ident "x")),[[Varid "s",White " ",LeftBracket,RightBracket]]) [(Copy,Nonterm (App (Var (ident "many'")) (Var (ident "x")),[[Varid "s"]]))],TypeSig (App (Var (ident "many'")) (Var (ident "x")),[[LeftBracket,Varid "a",RightBracket,White " ",Varsym "->",White " ",LeftBracket,Varid "a",RightBracket]]) [(Var (ident "x"),[[Varid "a"]])] False,Production (App (Var (ident "many'")) (Var (ident "x")),[[Varsym "\\",White " ",Varid "as",White " ",Varsym "->",White " ",Varid "as"]]) [],Production (App (Var (ident "many'")) (Var (ident "x")),[[Varsym "\\",White " ",Varid "as",White " ",Varsym "->",White " ",Varid "s",White " ",LeftParen,Varid "a",White " ",Consym ":",White " ",Varid "as",RightParen]]) [(Copy,Nonterm (App (Var (ident "many'")) (Var (ident "x")),[[Varid "s"]])),(Copy,Nonterm (Var (ident "x"),[[Varid "a"]]))],TypeSig (App (Var (ident "many1")) (Var (ident "x")),[[LeftBracket,Varid "a",RightBracket]]) [(Var (ident "x"),[[Varid "a"]])] False,Production (App (Var (ident "many1")) (Var (ident "x")),[[Varid "a",White " ",Consym ":",White " ",Varid "as"]]) [(Copy,Nonterm (Var (ident "x"),[[Varid "a"]])),(Copy,Nonterm (App (Var (ident "many")) (Var (ident "x")),[[Varid "as"]]))],TypeSig (App (App (Var (ident "sepBy")) (Var (ident "x"))) (Var (ident "sep")),[]) [(Var (ident "x"),[]),(Var (ident "sep"),[])] False,Production (App (App (Var (ident "sepBy")) (Var (ident "x"))) (Var (ident "sep")),[]) [],Production (App (App (Var (ident "sepBy")) (Var (ident "x"))) (Var (ident "sep")),[]) [(Copy,Nonterm (App (App (Var (ident "sepBy1")) (Var (ident "x"))) (Var (ident "sep")),[]))],TypeSig (App (App (Var (ident "sepBy1")) (Var (ident "x"))) (Var (ident "sep")),[]) [(Var (ident "x"),[]),(Var (ident "sep"),[])] False,Production (App (App (Var (ident "sepBy1")) (Var (ident "x"))) (Var (ident "sep")),[]) [(Copy,Nonterm (Var (ident "x"),[]))],Production (App (App (Var (ident "sepBy1")) (Var (ident "x"))) (Var (ident "sep")),[]) [(Copy,Nonterm (App (App (Var (ident "sepBy1")) (Var (ident "x"))) (Var (ident "sep")),[])),(Copy,Nonterm (Var (ident "sep"),[])),(Copy,Nonterm (Var (ident "x"),[]))],TypeSig (App (App (Var (ident "sepBy")) (Var (ident "x"))) (Var (ident "sep")),[[LeftBracket,Varid "a",RightBracket]]) [(Var (ident "x"),[[Varid "a"]]),(Var (ident "sep"),[])] False,Production (App (App (Var (ident "sepBy")) (Var (ident "x"))) (Var (ident "sep")),[[LeftBracket,RightBracket]]) [],Production (App (App (Var (ident "sepBy")) (Var (ident "x"))) (Var (ident "sep")),[[Varid "as"]]) [(Copy,Nonterm (App (App (Var (ident "sepBy1")) (Var (ident "x"))) (Var (ident "sep")),[[Varid "as"]]))],TypeSig (App (App (Var (ident "sepBy1")) (Var (ident "x"))) (Var (ident "sep")),[[LeftBracket,Varid "a",RightBracket]]) [(Var (ident "x"),[[Varid "a"]]),(Var (ident "sep"),[])] False,Production (App (App (Var (ident "sepBy1")) (Var (ident "x"))) (Var (ident "sep")),[[Varid "s",White " ",LeftBracket,RightBracket]]) [(Copy,Nonterm (App (App (Var (ident "sepBy1'")) (Var (ident "x"))) (Var (ident "sep")),[[Varid "s"]]))],TypeSig (App (App (Var (ident "sepBy1'")) (Var (ident "x"))) (Var (ident "sep")),[[LeftBracket,Varid "a",RightBracket,White " ",Varsym "->",White " ",LeftBracket,Varid "a",RightBracket]]) [(Var (ident "x"),[[Varid "a"]]),(Var (ident "sep"),[])] False,Production (App (App (Var (ident "sepBy1'")) (Var (ident "x"))) (Var (ident "sep")),[[Varsym "\\",White " ",Varid "as",White " ",Varsym "->",White " ",Varid "a",White " ",Consym ":",White " ",Varid "as"]]) [(Copy,Nonterm (Var (ident "x"),[[Varid "a"]]))],Production (App (App (Var (ident "sepBy1'")) (Var (ident "x"))) (Var (ident "sep")),[[Varsym "\\",White " ",Varid "as",White " ",Varsym "->",White " ",Varid "s",White " ",LeftParen,Varid "a",White " ",Consym ":",White " ",Varid "as",RightParen]]) [(Copy,Nonterm (App (App (Var (ident "sepBy1'")) (Var (ident "x"))) (Var (ident "sep")),[[Varid "s"]])),(Copy,Nonterm (Var (ident "sep"),[])),(Copy,Nonterm (Var (ident "x"),[[Varid "a"]]))],TypeSig (App (App (Var (ident "optSepBy")) (Var (ident "x"))) (Var (ident "sep")),[]) [(Var (ident "x"),[]),(Var (ident "sep"),[])] False,Production (App (App (Var (ident "optSepBy")) (Var (ident "x"))) (Var (ident "sep")),[]) [],Production (App (App (Var (ident "optSepBy")) (Var (ident "x"))) (Var (ident "sep")),[]) [(Copy,Nonterm (Var (ident "x"),[]))],Production (App (App (Var (ident "optSepBy")) (Var (ident "x"))) (Var (ident "sep")),[]) [(Copy,Nonterm (App (App (Var (ident "optSepBy")) (Var (ident "x"))) (Var (ident "sep")),[])),(Copy,Nonterm (Var (ident "sep"),[]))],Production (App (App (Var (ident "optSepBy")) (Var (ident "x"))) (Var (ident "sep")),[]) [(Copy,Nonterm (App (App (Var (ident "optSepBy")) (Var (ident "x"))) (Var (ident "sep")),[])),(Copy,Nonterm (Var (ident "sep"),[])),(Copy,Nonterm (Var (ident "x"),[]))],TypeSig (App (App (Var (ident "optSepBy")) (Var (ident "x"))) (Var (ident "sep")),[[LeftBracket,Varid "a",RightBracket]]) [(Var (ident "x"),[[Varid "a"]]),(Var (ident "sep"),[])] False,Production (App (App (Var (ident "optSepBy")) (Var (ident "x"))) (Var (ident "sep")),[[Varid "s",White " ",LeftBracket,RightBracket]]) [(Copy,Nonterm (App (App (Var (ident "optSepBy'")) (Var (ident "x"))) (Var (ident "sep")),[[Varid "s"]]))],TypeSig (App (App (Var (ident "optSepBy'")) (Var (ident "x"))) (Var (ident "sep")),[[LeftBracket,Varid "a",RightBracket,White " ",Varsym "->",White " ",LeftBracket,Varid "a",RightBracket]]) [(Var (ident "x"),[[Varid "a"]]),(Var (ident "sep"),[])] False,Production (App (App (Var (ident "optSepBy'")) (Var (ident "x"))) (Var (ident "sep")),[[Varsym "\\",White " ",Varid "as",White " ",Varsym "->",White " ",Varid "as"]]) [],Production (App (App (Var (ident "optSepBy'")) (Var (ident "x"))) (Var (ident "sep")),[[Varsym "\\",White " ",Varid "as",White " ",Varsym "->",White " ",Varid "a",White " ",Consym ":",White " ",Varid "as"]]) [(Copy,Nonterm (Var (ident "x"),[[Varid "a"]]))],Production (App (App (Var (ident "optSepBy'")) (Var (ident "x"))) (Var (ident "sep")),[[Varsym "\\",White " ",Varid "as",White " ",Varsym "->",White " ",Varid "s",White " ",Varid "as"]]) [(Copy,Nonterm (App (App (Var (ident "optSepBy'")) (Var (ident "x"))) (Var (ident "sep")),[[Varid "s"]])),(Copy,Nonterm (Var (ident "sep"),[]))],Production (App (App (Var (ident "optSepBy'")) (Var (ident "x"))) (Var (ident "sep")),[[Varsym "\\",White " ",Varid "as",White " ",Varsym "->",White " ",Varid "s",White " ",LeftParen,Varid "a",White " ",Consym ":",White " ",Varid "as",RightParen]]) [(Copy,Nonterm (App (App (Var (ident "optSepBy'")) (Var (ident "x"))) (Var (ident "sep")),[[Varid "s"]])),(Copy,Nonterm (Var (ident "sep"),[])),(Copy,Nonterm (Var (ident "x"),[[Varid "a"]]))]]











































  module GParser2               (  Term, Nonterm, parse, Sym(..), Decl(..)  )
  where
  import Lexer2
  import Grammar
  import Atom
  import Haskell                hiding (  Decl, guard  )
  import Base                   hiding (  Result  )
  import qualified Base
  import Prettier               (  Pretty  )
  import Options
  import Control.Category.Unicode
  import Control.Applicative    hiding (  (<$>)  )
  import Control.Monad (ap)
  import Data.Char hiding ( isSymbol )
  import Data.List
  import System.IO





  type Term                     =  Pat
 
  type Nonterm                  =  (Expr, [Quoted])
 
  data Sym                      =  Term Term | Nonterm Nonterm
                                   deriving (Show)
 
  type AnnTerm                  =  (Term, Bool, Assoc, Maybe Literal)
 
  data Decl                     =  Terminals    [AnnTerm]
                                |  Nonterminals [(Nonterm, Bool)]
                                |  Fixity       Assoc Term
                                |  TypeSig      Nonterm [Nonterm] Bool
                                |  Production   Nonterm [(Modifier, Sym)]
                                   deriving (Show)
 
  instance Pretty Decl





  type Terminal                 =  Token
 
  type Result a                 =  Lex Base.Result a



  type Answer                   =  ([Token], [Decl], [Token])
 
  -- file                          :: Result Answer
 
  

{-



-}



{- frown :-( -}

  type Parser__ = Terminal -> Result Nonterminal__
  
  data Nonterminal__ = File__ Answer
  
  file
      = (get >>= state_1__ (\ v1 _ -> return (File__ v1)))
            >>=
            (\ (File__ v1) -> return (v1))
  
  state_1__ :: (Answer -> Parser__) -> Parser__
  state_1__ k_1_0__ t__
      = let {
              goto_file'1__ v1 = state_2__ (k_1_0__ v1);
              goto_many'1_33__ v1 = state_4__ (reduce_2__ goto_file'1__ v1);
              goto_many''1_33__ v1
                  = state_12__ (reduce_6__ goto_many'1_33__ v1)
                        (reduce_9__ goto_many''1_33__ v1)
            }
        in reduce_8__ goto_many''1_33__ t__
  
  state_2__ :: Parser__ -> Parser__
  state_2__ k_1_1__ t__
      = case t__ of { EOF -> get >>= k_1_1__; _ -> frown ["<end of input>"] t__ }
  
  state_4__
      :: ((()) -> ([Decl]) -> (()) -> ([(Terminal)]) -> Parser__) -> Parser__
  state_4__ k_2_1__ t__
      = let {
              goto_open'1__ v1 = state_5__ (k_2_1__ v1)
            }
        in reduce_3__ goto_open'1__ t__
  
  state_5__ :: (([Decl]) -> (()) -> ([(Terminal)]) -> Parser__) -> Parser__
  state_5__ k_2_2__ t__
      = case t__ of {
            LeftSpecial -> get >>= state_6__ k_2_2__;
            _ -> frown ["%{"] t__ }
  
  state_6__ :: (([Decl]) -> (()) -> ([(Terminal)]) -> Parser__) -> Parser__
  state_6__ k_2_3__ t__
      = let {
              goto_declarations'1__ v1 = state_7__ (k_2_3__ v1);
              goto_many'1_decl'1__ v1 = reduce_5__ goto_declarations'1__ v1;
              goto_many''1_decl'1__ v1
                  = state_13__ (reduce_7__ goto_many'1_decl'1__ v1)
                        (reduce_11__ goto_many''1_decl'1__ v1)
            }
        in reduce_10__ goto_many''1_decl'1__ t__
  
  state_7__ :: ((()) -> ([(Terminal)]) -> Parser__) -> Parser__
  state_7__ k_2_4__ t__
      = let {
              goto_close'1__ v1 = state_8__ (k_2_4__ v1)
            }
        in reduce_4__ goto_close'1__ t__
  
  state_8__ :: (([(Terminal)]) -> Parser__) -> Parser__
  state_8__ k_2_5__ t__
      = case t__ of {
            RightSpecial -> get >>= state_9__ k_2_5__;
            _ -> frown ["}%"] t__ }
  
  state_9__ :: (([(Terminal)]) -> Parser__) -> Parser__
  state_9__ k_2_6__ t__
      = let {
              goto_many'1_33__ v1 = k_2_6__ v1;
              goto_many''1_33__ v1
                  = state_12__ (reduce_6__ goto_many'1_33__ v1)
                        (reduce_9__ goto_many''1_33__ v1)
            }
        in reduce_8__ goto_many''1_33__ t__
  
  state_12__ :: Parser__ -> (Terminal -> Parser__) -> Parser__
  state_12__ k_6_1__ k_9_1__ t__
      = case t__ of { v1 | notSpecial v1 -> get >>= k_9_1__ v1; _ -> k_6_1__ t__ }
  
  state_13__ :: Parser__ -> (([Decl]) -> Parser__) -> Parser__
  state_13__ k_7_1__ k_11_1__ t__
      = let {
              goto_decl'1__ v1 = k_11_1__ v1;
              goto_terminals'1__ v1 = reduce_12__ goto_decl'1__ v1;
              goto_nonterminals'1__ v1 = reduce_13__ goto_decl'1__ v1;
              goto_fixity'1__ v1 = reduce_14__ goto_decl'1__ v1;
              goto_signature'1__ v1 = reduce_15__ goto_decl'1__ v1;
              goto_productions'1__ v1 = reduce_16__ goto_decl'1__ v1;
              goto_nonterminal'1__ v1
                  = state_45__ (reduce_23__ goto_signature'1__ v1)
                        (reduce_28__ goto_productions'1__ v1);
              goto_expr0'1__ v1
                  = state_65__ (reduce_31__ goto_nonterminal'1__ v1);
              goto_varid'1__ v1 = state_67__ (reduce_32__ goto_expr0'1__ v1);
              goto_srcloc'1__ v1 = state_86__ (reduce_42__ goto_varid'1__ v1)
            }
        in case t__ of {
               Conid "Terminal"
                   -> get >>= state_21__ (reduce_17__ goto_terminals'1__);
               Conid "Nonterminal"
                   -> get >>= state_25__ (reduce_18__ goto_nonterminals'1__);
               Varid "left" -> get >>= state_29__ (reduce_19__ goto_fixity'1__);
               Varid "right" -> get >>= state_33__ (reduce_20__ goto_fixity'1__);
               Varid "nonassoc"
                   -> get >>= state_37__ (reduce_21__ goto_fixity'1__);
               Varid v1 -> reduce_47__ goto_srcloc'1__ t__;
               Consym "::"
                   -> get
                          >>=
                          state_41__ (reduce_22__ goto_signature'1__)
                              (reduce_24__ goto_signature'1__);
               Varsym "*" -> get >>= state_51__ (reduce_25__ goto_signature'1__);
               RightSpecial -> k_7_1__ t__;
               _
                   -> frown
                              ["Terminal",
                               "Nonterminal",
                               "left",
                               "right",
                               "nonassoc",
                               "<variable>",
                               "::",
                               "*",
                               "}%"]
                          t__ }
  
  state_21__ :: (([(AnnTerm)]) -> Parser__) -> Parser__
  state_21__ k_17_1__ t__
      = case t__ of {
            Varsym "=" -> get >>= state_22__ k_17_1__;
            _ -> frown ["="] t__ }
  
  state_22__ :: (([(AnnTerm)]) -> Parser__) -> Parser__
  state_22__ k_17_2__ t__
      = let {
              goto_term'1__ v1 = reduce_86__ goto_sepBy1''1_term'1_17__ v1;
              goto_mark'1__ v1
                  = state_137__ (reduce_88__ goto_term'1__ v1)
                                (reduce_89__ goto_term'1__ v1)
                            (reduce_90__ goto_term'1__ v1)
                        (reduce_91__ goto_term'1__ v1);
              goto_sepBy'1_term'1_17__ v1 = state_23__ (k_17_2__ v1);
              goto_sepBy1'1_term'1_17__ v1
                  = reduce_71__ goto_sepBy'1_term'1_17__ v1;
              goto_sepBy1''1_term'1_17__ v1
                  = state_128__ (reduce_80__ goto_sepBy1'1_term'1_17__ v1)
                        (reduce_87__ goto_sepBy1''1_term'1_17__ v1)
            }
        in case t__ of {
               Varsym "*" -> get >>= reduce_93__ goto_mark'1__;
               Semicolon -> reduce_70__ goto_sepBy'1_term'1_17__ t__;
               _ -> reduce_92__ goto_mark'1__ t__ }
  
  state_23__ :: Parser__ -> Parser__
  state_23__ k_17_3__ t__
      = case t__ of { Semicolon -> get >>= k_17_3__; _ -> frown [";"] t__ }
  
  state_25__ :: (([((Nonterm, Bool))]) -> Parser__) -> Parser__
  state_25__ k_18_1__ t__
      = case t__ of {
            Varsym "=" -> get >>= state_26__ k_18_1__;
            _ -> frown ["="] t__ }
  
  state_26__ :: (([((Nonterm, Bool))]) -> Parser__) -> Parser__
  state_26__ k_18_2__ t__
      = let {
              goto_mark'1__ v1 = state_158__ (reduce_100__ goto_nonterm'1__ v1);
              goto_nonterm'1__ v1 = reduce_98__ goto_sepBy1''1_nonterm'1_17__ v1;
              goto_sepBy'1_nonterm'1_17__ v1 = state_27__ (k_18_2__ v1);
              goto_sepBy1'1_nonterm'1_17__ v1
                  = reduce_73__ goto_sepBy'1_nonterm'1_17__ v1;
              goto_sepBy1''1_nonterm'1_17__ v1
                  = state_129__ (reduce_81__ goto_sepBy1'1_nonterm'1_17__ v1)
                        (reduce_99__ goto_sepBy1''1_nonterm'1_17__ v1)
            }
        in case t__ of {
               Varid v1 -> reduce_92__ goto_mark'1__ t__;
               Varsym "*" -> get >>= reduce_93__ goto_mark'1__;
               Semicolon -> reduce_72__ goto_sepBy'1_nonterm'1_17__ t__;
               _ -> frown ["<variable>", "*", ";"] t__ }
  
  state_27__ :: Parser__ -> Parser__
  state_27__ k_18_3__ t__
      = case t__ of { Semicolon -> get >>= k_18_3__; _ -> frown [";"] t__ }
  
  state_29__ :: (String -> Term -> Parser__) -> Parser__
  state_29__ k_19_1__ t__
      = case t__ of {
            Numeral v1 -> get >>= state_30__ (k_19_1__ v1);
            _ -> frown ["<numeral>"] t__ }
  
  state_30__ :: (Term -> Parser__) -> Parser__
  state_30__ k_19_2__ t__
      = let {
              goto_terminal'1__ v1 = state_31__ (k_19_2__ v1);
              goto_pat'1__ v1 = reduce_33__ goto_terminal'1__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1
                  = state_70__ (reduce_34__ goto_terminal'1__ v1)
                        (reduce_38__ goto_apat'1__ v1);
              goto_srcloc'1__ v1
                  = state_89__ (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_31__ :: Parser__ -> Parser__
  state_31__ k_19_3__ t__
      = case t__ of { Semicolon -> get >>= k_19_3__; _ -> frown [";"] t__ }
  
  state_33__ :: (String -> Term -> Parser__) -> Parser__
  state_33__ k_20_1__ t__
      = case t__ of {
            Numeral v1 -> get >>= state_34__ (k_20_1__ v1);
            _ -> frown ["<numeral>"] t__ }
  
  state_34__ :: (Term -> Parser__) -> Parser__
  state_34__ k_20_2__ t__
      = let {
              goto_terminal'1__ v1 = state_35__ (k_20_2__ v1);
              goto_pat'1__ v1 = reduce_33__ goto_terminal'1__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1
                  = state_70__ (reduce_34__ goto_terminal'1__ v1)
                        (reduce_38__ goto_apat'1__ v1);
              goto_srcloc'1__ v1
                  = state_89__ (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_35__ :: Parser__ -> Parser__
  state_35__ k_20_3__ t__
      = case t__ of { Semicolon -> get >>= k_20_3__; _ -> frown [";"] t__ }
  
  state_37__ :: (String -> Term -> Parser__) -> Parser__
  state_37__ k_21_1__ t__
      = case t__ of {
            Numeral v1 -> get >>= state_38__ (k_21_1__ v1);
            _ -> frown ["<numeral>"] t__ }
  
  state_38__ :: (Term -> Parser__) -> Parser__
  state_38__ k_21_2__ t__
      = let {
              goto_terminal'1__ v1 = state_39__ (k_21_2__ v1);
              goto_pat'1__ v1 = reduce_33__ goto_terminal'1__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1
                  = state_70__ (reduce_34__ goto_terminal'1__ v1)
                        (reduce_38__ goto_apat'1__ v1);
              goto_srcloc'1__ v1
                  = state_89__ (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_39__ :: Parser__ -> Parser__
  state_39__ k_21_3__ t__
      = case t__ of { Semicolon -> get >>= k_21_3__; _ -> frown [";"] t__ }
  
  state_41__
      :: (Nonterm -> ([Nonterm]) -> Parser__) -> (Nonterm -> Parser__) -> Parser__
  state_41__ k_22_1__ k_24_1__ t__
      = let {
              goto_nonterminal'1__ v1 = state_42__ (k_22_1__ v1);
              goto_expr0'1__ v1
                  = state_65__ (reduce_31__ goto_nonterminal'1__ v1);
              goto_varid'1__ v1 = state_67__ (reduce_32__ goto_expr0'1__ v1);
              goto_srcloc'1__ v1 = state_86__ (reduce_42__ goto_varid'1__ v1)
            }
        in case t__ of {
               Varid v1 -> reduce_47__ goto_srcloc'1__ t__;
               Varsym "*" -> get >>= state_48__ k_24_1__;
               _ -> frown ["<variable>", "*"] t__ }
  
  state_42__ :: (([Nonterm]) -> Parser__) -> Parser__
  state_42__ k_22_2__ t__
      = let {
              goto_premise'1__ v1 = state_43__ (k_22_2__ v1)
            }
        in case t__ of {
               Varsym "<-" -> get >>= state_54__ (reduce_27__ goto_premise'1__);
               Semicolon -> reduce_26__ goto_premise'1__ t__;
               _ -> frown ["<-", ";"] t__ }
  
  state_43__ :: Parser__ -> Parser__
  state_43__ k_22_3__ t__
      = case t__ of { Semicolon -> get >>= k_22_3__; _ -> frown [";"] t__ }
  
  state_45__
      :: (([Nonterm]) -> Parser__)
          ->
          (([((Modifier, Sym))]) -> ([([Quoted], [(Modifier, Sym)])]) -> Parser__)
              ->
              Parser__
  state_45__ k_23_1__ k_28_1__ t__
      = let {
              goto_premise'1__ v1 = state_46__ (k_23_1__ v1)
            }
        in case t__ of {
               Consym ":" -> get >>= state_56__ k_28_1__;
               Varsym "<-" -> get >>= state_54__ (reduce_27__ goto_premise'1__);
               Semicolon -> reduce_26__ goto_premise'1__ t__;
               _ -> frown [":", "<-", ";"] t__ }
  
  state_46__ :: Parser__ -> Parser__
  state_46__ k_23_2__ t__
      = case t__ of { Semicolon -> get >>= k_23_2__; _ -> frown [";"] t__ }
  
  state_48__ :: (Nonterm -> Parser__) -> Parser__
  state_48__ k_24_2__ t__
      = let {
              goto_nonterminal'1__ v1 = state_49__ (k_24_2__ v1);
              goto_expr0'1__ v1
                  = state_65__ (reduce_31__ goto_nonterminal'1__ v1);
              goto_varid'1__ v1 = state_67__ (reduce_32__ goto_expr0'1__ v1);
              goto_srcloc'1__ v1 = state_86__ (reduce_42__ goto_varid'1__ v1)
            }
        in reduce_47__ goto_srcloc'1__ t__
  
  state_49__ :: Parser__ -> Parser__
  state_49__ k_24_3__ t__
      = case t__ of { Semicolon -> get >>= k_24_3__; _ -> frown [";"] t__ }
  
  state_51__ :: (Nonterm -> Parser__) -> Parser__
  state_51__ k_25_1__ t__
      = let {
              goto_nonterminal'1__ v1 = state_52__ (k_25_1__ v1);
              goto_expr0'1__ v1
                  = state_65__ (reduce_31__ goto_nonterminal'1__ v1);
              goto_varid'1__ v1 = state_67__ (reduce_32__ goto_expr0'1__ v1);
              goto_srcloc'1__ v1 = state_86__ (reduce_42__ goto_varid'1__ v1)
            }
        in reduce_47__ goto_srcloc'1__ t__
  
  state_52__ :: Parser__ -> Parser__
  state_52__ k_25_2__ t__
      = case t__ of { Semicolon -> get >>= k_25_2__; _ -> frown [";"] t__ }
  
  state_54__ :: (([(Nonterm)]) -> Parser__) -> Parser__
  state_54__ k_27_1__ t__
      = let {
              goto_nonterminal'1__ v1
                  = reduce_108__ goto_sepBy1''1_nonterminal'1_22__ v1;
              goto_expr0'1__ v1
                  = state_65__ (reduce_31__ goto_nonterminal'1__ v1);
              goto_varid'1__ v1 = state_67__ (reduce_32__ goto_expr0'1__ v1);
              goto_srcloc'1__ v1 = state_86__ (reduce_42__ goto_varid'1__ v1);
              goto_sepBy1'1_nonterminal'1_22__ v1 = k_27_1__ v1;
              goto_sepBy1''1_nonterminal'1_22__ v1
                  = state_131__ (reduce_83__ goto_sepBy1'1_nonterminal'1_22__ v1)
                        (reduce_109__ goto_sepBy1''1_nonterminal'1_22__ v1)
            }
        in reduce_47__ goto_srcloc'1__ t__
  
  state_56__
      :: (([((Modifier, Sym))]) -> ([([Quoted], [(Modifier, Sym)])]) -> Parser__)
          ->
          Parser__
  state_56__ k_28_2__ t__
      = let {
              goto_symbol'1__ v1 = reduce_101__ goto_sepBy1''1_symbol'1_22__ v1;
              goto_nonterminal'1__ v1 = reduce_107__ goto_symbol'1__ v1;
              goto_expr0'1__ v1
                  = state_65__ (reduce_31__ goto_nonterminal'1__ v1);
              goto_terminal'1__ v1 = reduce_106__ goto_symbol'1__ v1;
              goto_pat'1__ v1 = reduce_33__ goto_terminal'1__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_varid'1__ v1 = state_67__ (reduce_32__ goto_expr0'1__ v1);
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1
                  = state_70__ (reduce_34__ goto_terminal'1__ v1)
                        (reduce_38__ goto_apat'1__ v1);
              goto_srcloc'1__ v1
                  = state_87__ (reduce_42__ goto_varid'1__ v1)
                                    (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1);
              goto_sepBy'1_symbol'1_22__ v1 = state_57__ (k_28_2__ v1);
              goto_sepBy1'1_symbol'1_22__ v1
                  = reduce_75__ goto_sepBy'1_symbol'1_22__ v1;
              goto_sepBy1''1_symbol'1_22__ v1
                  = state_130__ (reduce_82__ goto_sepBy1'1_symbol'1_22__ v1)
                        (reduce_102__ goto_sepBy1''1_symbol'1_22__ v1)
            }
        in case t__ of {
               Varid "insert"
                   -> get >>= state_163__ (reduce_103__ goto_symbol'1__);
               Varid "delete"
                   -> get >>= state_165__ (reduce_104__ goto_symbol'1__);
               Varid "prec" -> get >>= state_167__ (reduce_105__ goto_symbol'1__);
               Semicolon -> reduce_74__ goto_sepBy'1_symbol'1_22__ t__;
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_57__ :: (([([Quoted], [(Modifier, Sym)])]) -> Parser__) -> Parser__
  state_57__ k_28_3__ t__
      = case t__ of {
            Semicolon -> get >>= state_58__ k_28_3__;
            _ -> frown [";"] t__ }
  
  state_58__ :: (([([Quoted], [(Modifier, Sym)])]) -> Parser__) -> Parser__
  state_58__ k_28_4__ t__
      = let {
              goto_alts'1__ v1 = k_28_4__ v1;
              goto_attributes'1__ v1 = state_60__ (reduce_30__ goto_alts'1__ v1);
              goto_haskell'1__ v1
                  = state_95__ (reduce_49__ goto_attributes'1__ v1);
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               Varsym "|" -> reduce_48__ goto_attributes'1__ t__;
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_29__ goto_alts'1__ t__ }
  
  state_60__
      :: (([((Modifier, Sym))]) -> ([([Quoted], [(Modifier, Sym)])]) -> Parser__)
          ->
          Parser__
  state_60__ k_30_1__ t__
      = case t__ of {
            Varsym "|" -> get >>= state_61__ k_30_1__;
            _ -> frown ["|"] t__ }
  
  state_61__
      :: (([((Modifier, Sym))]) -> ([([Quoted], [(Modifier, Sym)])]) -> Parser__)
          ->
          Parser__
  state_61__ k_30_2__ t__
      = let {
              goto_symbol'1__ v1 = reduce_101__ goto_sepBy1''1_symbol'1_22__ v1;
              goto_nonterminal'1__ v1 = reduce_107__ goto_symbol'1__ v1;
              goto_expr0'1__ v1
                  = state_65__ (reduce_31__ goto_nonterminal'1__ v1);
              goto_terminal'1__ v1 = reduce_106__ goto_symbol'1__ v1;
              goto_pat'1__ v1 = reduce_33__ goto_terminal'1__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_varid'1__ v1 = state_67__ (reduce_32__ goto_expr0'1__ v1);
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1
                  = state_70__ (reduce_34__ goto_terminal'1__ v1)
                        (reduce_38__ goto_apat'1__ v1);
              goto_srcloc'1__ v1
                  = state_87__ (reduce_42__ goto_varid'1__ v1)
                                    (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1);
              goto_sepBy'1_symbol'1_22__ v1 = state_62__ (k_30_2__ v1);
              goto_sepBy1'1_symbol'1_22__ v1
                  = reduce_75__ goto_sepBy'1_symbol'1_22__ v1;
              goto_sepBy1''1_symbol'1_22__ v1
                  = state_130__ (reduce_82__ goto_sepBy1'1_symbol'1_22__ v1)
                        (reduce_102__ goto_sepBy1''1_symbol'1_22__ v1)
            }
        in case t__ of {
               Varid "insert"
                   -> get >>= state_163__ (reduce_103__ goto_symbol'1__);
               Varid "delete"
                   -> get >>= state_165__ (reduce_104__ goto_symbol'1__);
               Varid "prec" -> get >>= state_167__ (reduce_105__ goto_symbol'1__);
               Semicolon -> reduce_74__ goto_sepBy'1_symbol'1_22__ t__;
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_62__ :: (([([Quoted], [(Modifier, Sym)])]) -> Parser__) -> Parser__
  state_62__ k_30_3__ t__
      = case t__ of {
            Semicolon -> get >>= state_63__ k_30_3__;
            _ -> frown [";"] t__ }
  
  state_63__ :: (([([Quoted], [(Modifier, Sym)])]) -> Parser__) -> Parser__
  state_63__ k_30_4__ t__
      = let {
              goto_alts'1__ v1 = k_30_4__ v1;
              goto_attributes'1__ v1 = state_60__ (reduce_30__ goto_alts'1__ v1);
              goto_haskell'1__ v1
                  = state_95__ (reduce_49__ goto_attributes'1__ v1);
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               Varsym "|" -> reduce_48__ goto_attributes'1__ t__;
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_29__ goto_alts'1__ t__ }
  
  state_65__ :: (([Quoted]) -> Parser__) -> Parser__
  state_65__ k_31_1__ t__
      = let {
              goto_attributes'1__ v1 = k_31_1__ v1;
              goto_haskell'1__ v1
                  = state_95__ (reduce_49__ goto_attributes'1__ v1);
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_48__ goto_attributes'1__ t__ }
  
  state_67__ :: (([(Expr)]) -> Parser__) -> Parser__
  state_67__ k_32_1__ t__
      = let {
              goto_many'1_aexpr0'1__ v1 = k_32_1__ v1;
              goto_many''1_aexpr0'1__ v1
                  = state_102__ (reduce_53__ goto_many'1_aexpr0'1__ v1)
                        (reduce_56__ goto_many''1_aexpr0'1__ v1)
            }
        in reduce_55__ goto_many''1_aexpr0'1__ t__
  
  state_70__ :: (([Token]) -> ([Quoted]) -> Parser__) -> Parser__ -> Parser__
  state_70__ k_34_1__ k_38_1__ t__
      = let {
              goto_haskell'1__ v1 = state_72__ (k_34_1__ v1);
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> k_38_1__ t__ }
  
  state_71__
      :: (([Token]) -> ([Quoted]) -> Parser__)
          ->
          Parser__ -> (Term -> Parser__) -> Parser__
  state_71__ k_34_1__ k_38_1__ k_89_3__ t__
      = let {
              goto_haskell'1__ v1 = state_72__ (k_34_1__ v1);
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               Varsym "=" -> get >>= state_140__ k_89_3__;
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> k_38_1__ t__ }
  
  state_72__ :: (([Quoted]) -> Parser__) -> Parser__
  state_72__ k_34_2__ t__
      = let {
              goto_attributes'1__ v1 = k_34_2__ v1;
              goto_haskell'1__ v1
                  = state_95__ (reduce_49__ goto_attributes'1__ v1);
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_48__ goto_attributes'1__ t__ }
  
  state_75__ :: (([(Pat)]) -> Parser__) -> Parser__ -> Parser__
  state_75__ k_36_1__ k_37_1__ t__
      = let {
              goto_apat'1__ v1
                  = state_119__ (reduce_66__ goto_many1'1_apat'1__ v1);
              goto_conid'1__ v1 = reduce_37__ goto_apat'1__ v1;
              goto_literal'1__ v1 = reduce_38__ goto_apat'1__ v1;
              goto_srcloc'1__ v1
                  = state_89__ (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1);
              goto_many1'1_apat'1__ v1 = k_36_1__ v1
            }
        in case t__ of {
               Conid v1 -> reduce_47__ goto_srcloc'1__ t__;
               Numeral v1 -> reduce_47__ goto_srcloc'1__ t__;
               Char v1 -> reduce_47__ goto_srcloc'1__ t__;
               String v1 -> reduce_47__ goto_srcloc'1__ t__;
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> k_37_1__ t__ }
  
  state_79__ :: (([(Pat)]) -> Parser__) -> Parser__
  state_79__ k_39_1__ t__
      = let {
              goto_pat'1__ v1 = reduce_125__ goto_sepBy1''1_pat'1_22__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1 = reduce_38__ goto_apat'1__ v1;
              goto_srcloc'1__ v1
                  = state_89__ (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1);
              goto_sepBy'1_pat'1_22__ v1 = state_80__ (k_39_1__ v1);
              goto_sepBy1'1_pat'1_22__ v1
                  = reduce_79__ goto_sepBy'1_pat'1_22__ v1;
              goto_sepBy1''1_pat'1_22__ v1
                  = state_133__ (reduce_85__ goto_sepBy1'1_pat'1_22__ v1)
                        (reduce_126__ goto_sepBy1''1_pat'1_22__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               RightParen -> reduce_78__ goto_sepBy'1_pat'1_22__ t__;
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_80__ :: Parser__ -> Parser__
  state_80__ k_39_2__ t__
      = case t__ of { RightParen -> get >>= k_39_2__; _ -> frown [")"] t__ }
  
  state_82__ :: (([(Pat)]) -> Parser__) -> Parser__
  state_82__ k_40_1__ t__
      = let {
              goto_pat'1__ v1 = reduce_125__ goto_sepBy1''1_pat'1_22__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1 = reduce_38__ goto_apat'1__ v1;
              goto_srcloc'1__ v1
                  = state_89__ (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1);
              goto_sepBy'1_pat'1_22__ v1 = state_83__ (k_40_1__ v1);
              goto_sepBy1'1_pat'1_22__ v1
                  = reduce_79__ goto_sepBy'1_pat'1_22__ v1;
              goto_sepBy1''1_pat'1_22__ v1
                  = state_133__ (reduce_85__ goto_sepBy1'1_pat'1_22__ v1)
                        (reduce_126__ goto_sepBy1''1_pat'1_22__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               RightBracket -> reduce_78__ goto_sepBy'1_pat'1_22__ t__;
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_83__ :: Parser__ -> Parser__
  state_83__ k_40_2__ t__
      = case t__ of { RightBracket -> get >>= k_40_2__; _ -> frown ["]"] t__ }
  
  state_86__ :: (String -> Parser__) -> Parser__
  state_86__ k_42_1__ t__
      = case t__ of {
            Varid v1 -> get >>= k_42_1__ v1;
            _ -> frown ["<variable>"] t__ }
  
  state_87__
      :: (String -> Parser__)
          ->
          (String -> Parser__)
              ->
              (String -> Parser__)
                  ->
                  (String -> Parser__) -> (String -> Parser__) -> Parser__
  state_87__ k_42_1__ k_43_1__ k_44_1__ k_45_1__ k_46_1__ t__
      = case t__ of {
            Conid v1 -> get >>= k_43_1__ v1;
            Varid v1 -> get >>= k_42_1__ v1;
            Numeral v1 -> get >>= k_45_1__ v1;
            Char v1 -> get >>= k_46_1__ v1;
            String v1 -> get >>= k_44_1__ v1;
            _
                -> frown
                           ["<constructor>",
                            "<variable>",
                            "<numeral>",
                            "<char literal>",
                            "<string literal>"]
                       t__ }
  
  state_89__
      :: (String -> Parser__)
          ->
          (String -> Parser__)
              ->
              (String -> Parser__) -> (String -> Parser__) -> Parser__
  state_89__ k_43_1__ k_44_1__ k_45_1__ k_46_1__ t__
      = case t__ of {
            Conid v1 -> get >>= k_43_1__ v1;
            Numeral v1 -> get >>= k_45_1__ v1;
            Char v1 -> get >>= k_46_1__ v1;
            String v1 -> get >>= k_44_1__ v1;
            _
                -> frown
                           ["<constructor>",
                            "<numeral>",
                            "<char literal>",
                            "<string literal>"]
                       t__ }
  
  state_91__
      :: (String -> Parser__)
          ->
          (String -> Parser__) -> (String -> Parser__) -> Parser__
  state_91__ k_44_1__ k_45_1__ k_46_1__ t__
      = case t__ of {
            Numeral v1 -> get >>= k_45_1__ v1;
            Char v1 -> get >>= k_46_1__ v1;
            String v1 -> get >>= k_44_1__ v1;
            _ -> frown ["<numeral>", "<char literal>", "<string literal>"] t__ }
  
  state_95__ :: (([Quoted]) -> Parser__) -> Parser__
  state_95__ k_49_1__ t__
      = let {
              goto_attributes'1__ v1 = k_49_1__ v1;
              goto_haskell'1__ v1
                  = state_95__ (reduce_49__ goto_attributes'1__ v1);
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_48__ goto_attributes'1__ t__ }
  
  state_97__ :: (([([Token] -> [Token])]) -> (()) -> Parser__) -> Parser__
  state_97__ k_50_1__ t__
      = case t__ of {
            LeftCurly -> get >>= state_98__ k_50_1__;
            _ -> frown ["{"] t__ }
  
  state_98__ :: (([([Token] -> [Token])]) -> (()) -> Parser__) -> Parser__
  state_98__ k_50_2__ t__
      = let {
              goto_many'1_hs'1__ v1 = state_99__ (k_50_2__ v1);
              goto_many''1_hs'1__ v1
                  = state_103__ (reduce_54__ goto_many'1_hs'1__ v1)
                        (reduce_63__ goto_many''1_hs'1__ v1)
            }
        in reduce_62__ goto_many''1_hs'1__ t__
  
  state_99__ :: ((()) -> Parser__) -> Parser__
  state_99__ k_50_3__ t__
      = let {
              goto_hsClose'1__ v1 = state_100__ (k_50_3__ v1)
            }
        in reduce_52__ goto_hsClose'1__ t__
  
  state_100__ :: Parser__ -> Parser__
  state_100__ k_50_4__ t__
      = case t__ of { RightCurly -> get >>= k_50_4__; _ -> frown ["}"] t__ }
  
  state_102__ :: Parser__ -> (Expr -> Parser__) -> Parser__
  state_102__ k_53_1__ k_56_1__ t__
      = let {
              goto_aexpr0'1__ v1 = k_56_1__ v1;
              goto_varid'1__ v1 = reduce_57__ goto_aexpr0'1__ v1;
              goto_conid'1__ v1 = reduce_58__ goto_aexpr0'1__ v1;
              goto_literal'1__ v1 = reduce_59__ goto_aexpr0'1__ v1;
              goto_srcloc'1__ v1
                  = state_87__ (reduce_42__ goto_varid'1__ v1)
                                    (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1)
            }
        in case t__ of {
               Conid v1 -> reduce_47__ goto_srcloc'1__ t__;
               Varid v1 -> reduce_47__ goto_srcloc'1__ t__;
               Numeral v1 -> reduce_47__ goto_srcloc'1__ t__;
               Char v1 -> reduce_47__ goto_srcloc'1__ t__;
               String v1 -> reduce_47__ goto_srcloc'1__ t__;
               LeftParen -> get >>= state_108__ (reduce_60__ goto_aexpr0'1__);
               LeftBracket -> get >>= state_111__ (reduce_61__ goto_aexpr0'1__);
               _ -> k_53_1__ t__ }
  
  state_103__ :: Parser__ -> (([Token] -> [Token]) -> Parser__) -> Parser__
  state_103__ k_54_1__ k_63_1__ t__
      = let {
              goto_hs'1__ v1 = k_63_1__ v1
            }
        in case t__ of {
               LeftCurly -> get >>= state_116__ (reduce_65__ goto_hs'1__);
               RightCurly -> k_54_1__ t__;
               v1 | notBrace v1 -> get >>= reduce_64__ goto_hs'1__ v1;
               _ -> frown ["{", "}", "not brace"] t__ }
  
  state_108__ :: (([(Expr)]) -> Parser__) -> Parser__
  state_108__ k_60_1__ t__
      = let {
              goto_expr'1__ v1 = reduce_110__ goto_sepBy1''1_expr'1_22__ v1;
              goto_aexpr'1__ v1 = reduce_112__ goto_expr'1__ v1;
              goto_varid'1__ v1
                  = state_178__ (reduce_113__ goto_expr'1__ v1)
                        (reduce_115__ goto_aexpr'1__ v1);
              goto_conid'1__ v1
                  = state_180__ (reduce_114__ goto_expr'1__ v1)
                        (reduce_116__ goto_aexpr'1__ v1);
              goto_literal'1__ v1 = reduce_117__ goto_aexpr'1__ v1;
              goto_srcloc'1__ v1
                  = state_87__ (reduce_42__ goto_varid'1__ v1)
                                    (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_120__ goto_aexpr'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1);
              goto_sepBy'1_expr'1_22__ v1 = state_109__ (k_60_1__ v1);
              goto_sepBy1'1_expr'1_22__ v1
                  = reduce_77__ goto_sepBy'1_expr'1_22__ v1;
              goto_sepBy1''1_expr'1_22__ v1
                  = state_132__ (reduce_84__ goto_sepBy1'1_expr'1_22__ v1)
                        (reduce_111__ goto_sepBy1''1_expr'1_22__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_185__ (reduce_118__ goto_aexpr'1__);
               RightParen -> reduce_76__ goto_sepBy'1_expr'1_22__ t__;
               LeftBracket -> get >>= state_188__ (reduce_119__ goto_aexpr'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_109__ :: Parser__ -> Parser__
  state_109__ k_60_2__ t__
      = case t__ of { RightParen -> get >>= k_60_2__; _ -> frown [")"] t__ }
  
  state_111__ :: (([(Expr)]) -> Parser__) -> Parser__
  state_111__ k_61_1__ t__
      = let {
              goto_expr'1__ v1 = reduce_110__ goto_sepBy1''1_expr'1_22__ v1;
              goto_aexpr'1__ v1 = reduce_112__ goto_expr'1__ v1;
              goto_varid'1__ v1
                  = state_178__ (reduce_113__ goto_expr'1__ v1)
                        (reduce_115__ goto_aexpr'1__ v1);
              goto_conid'1__ v1
                  = state_180__ (reduce_114__ goto_expr'1__ v1)
                        (reduce_116__ goto_aexpr'1__ v1);
              goto_literal'1__ v1 = reduce_117__ goto_aexpr'1__ v1;
              goto_srcloc'1__ v1
                  = state_87__ (reduce_42__ goto_varid'1__ v1)
                                    (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_120__ goto_aexpr'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1);
              goto_sepBy'1_expr'1_22__ v1 = state_112__ (k_61_1__ v1);
              goto_sepBy1'1_expr'1_22__ v1
                  = reduce_77__ goto_sepBy'1_expr'1_22__ v1;
              goto_sepBy1''1_expr'1_22__ v1
                  = state_132__ (reduce_84__ goto_sepBy1'1_expr'1_22__ v1)
                        (reduce_111__ goto_sepBy1''1_expr'1_22__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_185__ (reduce_118__ goto_aexpr'1__);
               LeftBracket -> get >>= state_188__ (reduce_119__ goto_aexpr'1__);
               RightBracket -> reduce_76__ goto_sepBy'1_expr'1_22__ t__;
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_112__ :: Parser__ -> Parser__
  state_112__ k_61_2__ t__
      = case t__ of { RightBracket -> get >>= k_61_2__; _ -> frown ["]"] t__ }
  
  state_116__ :: (([([Token] -> [Token])]) -> Parser__) -> Parser__
  state_116__ k_65_1__ t__
      = let {
              goto_many'1_hs'1__ v1 = state_117__ (k_65_1__ v1);
              goto_many''1_hs'1__ v1
                  = state_103__ (reduce_54__ goto_many'1_hs'1__ v1)
                        (reduce_63__ goto_many''1_hs'1__ v1)
            }
        in reduce_62__ goto_many''1_hs'1__ t__
  
  state_117__ :: Parser__ -> Parser__
  state_117__ k_65_2__ t__
      = case t__ of { RightCurly -> get >>= k_65_2__; _ -> frown ["}"] t__ }
  
  state_119__ :: (([((Pat))]) -> Parser__) -> Parser__
  state_119__ k_66_1__ t__
      = let {
              goto_many'1_apat'1__ v1 = k_66_1__ v1;
              goto_many''1_apat'1__ v1
                  = state_121__ (reduce_67__ goto_many'1_apat'1__ v1)
                        (reduce_69__ goto_many''1_apat'1__ v1)
            }
        in reduce_68__ goto_many''1_apat'1__ t__
  
  state_121__ :: Parser__ -> (Pat -> Parser__) -> Parser__
  state_121__ k_67_1__ k_69_1__ t__
      = let {
              goto_apat'1__ v1 = k_69_1__ v1;
              goto_conid'1__ v1 = reduce_37__ goto_apat'1__ v1;
              goto_literal'1__ v1 = reduce_38__ goto_apat'1__ v1;
              goto_srcloc'1__ v1
                  = state_89__ (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               Conid v1 -> reduce_47__ goto_srcloc'1__ t__;
               Numeral v1 -> reduce_47__ goto_srcloc'1__ t__;
               Char v1 -> reduce_47__ goto_srcloc'1__ t__;
               String v1 -> reduce_47__ goto_srcloc'1__ t__;
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> k_67_1__ t__ }
  
  state_128__ :: Parser__ -> (AnnTerm -> Parser__) -> Parser__
  state_128__ k_80_1__ k_87_1__ t__
      = case t__ of {
            Varsym "|" -> get >>= state_135__ k_87_1__;
            Semicolon -> k_80_1__ t__;
            _ -> frown ["|", ";"] t__ }
  
  state_129__ :: Parser__ -> (((Nonterm, Bool)) -> Parser__) -> Parser__
  state_129__ k_81_1__ k_99_1__ t__
      = case t__ of {
            Varsym "|" -> get >>= state_156__ k_99_1__;
            Semicolon -> k_81_1__ t__;
            _ -> frown ["|", ";"] t__ }
  
  state_130__ :: Parser__ -> (((Modifier, Sym)) -> Parser__) -> Parser__
  state_130__ k_82_1__ k_102_1__ t__
      = case t__ of {
            Comma -> get >>= state_161__ k_102_1__;
            Semicolon -> k_82_1__ t__;
            _ -> frown [",", ";"] t__ }
  
  state_131__ :: Parser__ -> (Nonterm -> Parser__) -> Parser__
  state_131__ k_83_1__ k_109_1__ t__
      = case t__ of {
            Comma -> get >>= state_172__ k_109_1__;
            Semicolon -> k_83_1__ t__;
            _ -> frown [",", ";"] t__ }
  
  state_132__ :: Parser__ -> (Expr -> Parser__) -> Parser__
  state_132__ k_84_1__ k_111_1__ t__
      = case t__ of { Comma -> get >>= state_175__ k_111_1__; _ -> k_84_1__ t__ }
  
  state_133__ :: Parser__ -> (Pat -> Parser__) -> Parser__
  state_133__ k_85_1__ k_126_1__ t__
      = case t__ of { Comma -> get >>= state_197__ k_126_1__; _ -> k_85_1__ t__ }
  
  state_135__ :: (AnnTerm -> Parser__) -> Parser__
  state_135__ k_87_2__ t__
      = let {
              goto_term'1__ v1 = k_87_2__ v1;
              goto_mark'1__ v1
                  = state_137__ (reduce_88__ goto_term'1__ v1)
                                (reduce_89__ goto_term'1__ v1)
                            (reduce_90__ goto_term'1__ v1)
                        (reduce_91__ goto_term'1__ v1)
            }
        in case t__ of {
               Varsym "*" -> get >>= reduce_93__ goto_mark'1__;
               _ -> reduce_92__ goto_mark'1__ t__ }
  
  state_137__
      :: (Assoc -> Term -> Parser__)
          ->
          (Assoc -> Literal -> Term -> Parser__)
              ->
              (Assoc -> Term -> Literal -> Parser__)
                  ->
                  (Assoc -> ([Token]) -> Literal -> Parser__) -> Parser__
  state_137__ k_88_1__ k_89_1__ k_90_1__ k_91_1__ t__
      = let {
              goto_assoc'1__ v1
                  = state_138__ (k_88_1__ v1) (k_89_1__ v1) (k_90_1__ v1)
                        (k_91_1__ v1)
            }
        in case t__ of {
               Varid "left" -> get >>= state_149__ (reduce_95__ goto_assoc'1__);
               Varid "right" -> get >>= state_151__ (reduce_96__ goto_assoc'1__);
               Varid "nonassoc"
                   -> get >>= state_153__ (reduce_97__ goto_assoc'1__);
               _ -> reduce_94__ goto_assoc'1__ t__ }
  
  state_138__
      :: (Term -> Parser__)
          ->
          (Literal -> Term -> Parser__)
              ->
              (Term -> Literal -> Parser__)
                  ->
                  (([Token]) -> Literal -> Parser__) -> Parser__
  state_138__ k_88_2__ k_89_2__ k_90_2__ k_91_2__ t__
      = let {
              goto_terminal'1__ v1 = state_139__ (k_88_2__ v1) (k_90_2__ v1);
              goto_pat'1__ v1 = reduce_33__ goto_terminal'1__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1
                  = state_71__ (reduce_34__ goto_terminal'1__ v1)
                            (reduce_38__ goto_apat'1__ v1)
                        (k_89_2__ v1);
              goto_srcloc'1__ v1
                  = state_89__ (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               Varid "guard" -> get >>= state_144__ k_91_2__;
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_139__ :: Parser__ -> (Literal -> Parser__) -> Parser__
  state_139__ k_88_3__ k_90_3__ t__
      = case t__ of {
            Varid "as" -> get >>= state_142__ k_90_3__;
            _ -> k_88_3__ t__ }
  
  state_140__ :: (Term -> Parser__) -> Parser__
  state_140__ k_89_4__ t__
      = let {
              goto_terminal'1__ v1 = k_89_4__ v1;
              goto_pat'1__ v1 = reduce_33__ goto_terminal'1__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1
                  = state_70__ (reduce_34__ goto_terminal'1__ v1)
                        (reduce_38__ goto_apat'1__ v1);
              goto_srcloc'1__ v1
                  = state_89__ (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_142__ :: (Literal -> Parser__) -> Parser__
  state_142__ k_90_4__ t__
      = let {
              goto_literal'1__ v1 = k_90_4__ v1;
              goto_srcloc'1__ v1
                  = state_91__ (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1)
            }
        in reduce_47__ goto_srcloc'1__ t__
  
  state_144__ :: (([Token]) -> Literal -> Parser__) -> Parser__
  state_144__ k_91_3__ t__
      = let {
              goto_haskell'1__ v1 = state_145__ (k_91_3__ v1);
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in reduce_51__ goto_hsOpen'1__ t__
  
  state_145__ :: (Literal -> Parser__) -> Parser__
  state_145__ k_91_4__ t__
      = case t__ of {
            Varid "as" -> get >>= state_146__ k_91_4__;
            _ -> frown ["as"] t__ }
  
  state_146__ :: (Literal -> Parser__) -> Parser__
  state_146__ k_91_5__ t__
      = let {
              goto_literal'1__ v1 = k_91_5__ v1;
              goto_srcloc'1__ v1
                  = state_91__ (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1)
            }
        in reduce_47__ goto_srcloc'1__ t__
  
  state_149__ :: (String -> Parser__) -> Parser__
  state_149__ k_95_1__ t__
      = case t__ of {
            Numeral v1 -> get >>= k_95_1__ v1;
            _ -> frown ["<numeral>"] t__ }
  
  state_151__ :: (String -> Parser__) -> Parser__
  state_151__ k_96_1__ t__
      = case t__ of {
            Numeral v1 -> get >>= k_96_1__ v1;
            _ -> frown ["<numeral>"] t__ }
  
  state_153__ :: (String -> Parser__) -> Parser__
  state_153__ k_97_1__ t__
      = case t__ of {
            Numeral v1 -> get >>= k_97_1__ v1;
            _ -> frown ["<numeral>"] t__ }
  
  state_156__ :: (((Nonterm, Bool)) -> Parser__) -> Parser__
  state_156__ k_99_2__ t__
      = let {
              goto_mark'1__ v1 = state_158__ (reduce_100__ goto_nonterm'1__ v1);
              goto_nonterm'1__ v1 = k_99_2__ v1
            }
        in case t__ of {
               Varid v1 -> reduce_92__ goto_mark'1__ t__;
               Varsym "*" -> get >>= reduce_93__ goto_mark'1__;
               _ -> frown ["<variable>", "*"] t__ }
  
  state_158__ :: (Nonterm -> Parser__) -> Parser__
  state_158__ k_100_1__ t__
      = let {
              goto_nonterminal'1__ v1 = k_100_1__ v1;
              goto_expr0'1__ v1
                  = state_65__ (reduce_31__ goto_nonterminal'1__ v1);
              goto_varid'1__ v1 = state_67__ (reduce_32__ goto_expr0'1__ v1);
              goto_srcloc'1__ v1 = state_86__ (reduce_42__ goto_varid'1__ v1)
            }
        in reduce_47__ goto_srcloc'1__ t__
  
  state_161__ :: (((Modifier, Sym)) -> Parser__) -> Parser__
  state_161__ k_102_2__ t__
      = let {
              goto_symbol'1__ v1 = k_102_2__ v1;
              goto_nonterminal'1__ v1 = reduce_107__ goto_symbol'1__ v1;
              goto_expr0'1__ v1
                  = state_65__ (reduce_31__ goto_nonterminal'1__ v1);
              goto_terminal'1__ v1 = reduce_106__ goto_symbol'1__ v1;
              goto_pat'1__ v1 = reduce_33__ goto_terminal'1__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_varid'1__ v1 = state_67__ (reduce_32__ goto_expr0'1__ v1);
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1
                  = state_70__ (reduce_34__ goto_terminal'1__ v1)
                        (reduce_38__ goto_apat'1__ v1);
              goto_srcloc'1__ v1
                  = state_87__ (reduce_42__ goto_varid'1__ v1)
                                    (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               Varid "insert"
                   -> get >>= state_163__ (reduce_103__ goto_symbol'1__);
               Varid "delete"
                   -> get >>= state_165__ (reduce_104__ goto_symbol'1__);
               Varid "prec" -> get >>= state_167__ (reduce_105__ goto_symbol'1__);
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_163__ :: (Term -> Parser__) -> Parser__
  state_163__ k_103_1__ t__
      = let {
              goto_terminal'1__ v1 = k_103_1__ v1;
              goto_pat'1__ v1 = reduce_33__ goto_terminal'1__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1
                  = state_70__ (reduce_34__ goto_terminal'1__ v1)
                        (reduce_38__ goto_apat'1__ v1);
              goto_srcloc'1__ v1
                  = state_89__ (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_165__ :: (Term -> Parser__) -> Parser__
  state_165__ k_104_1__ t__
      = let {
              goto_terminal'1__ v1 = k_104_1__ v1;
              goto_pat'1__ v1 = reduce_33__ goto_terminal'1__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1
                  = state_70__ (reduce_34__ goto_terminal'1__ v1)
                        (reduce_38__ goto_apat'1__ v1);
              goto_srcloc'1__ v1
                  = state_89__ (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_167__ :: (Term -> Parser__) -> Parser__
  state_167__ k_105_1__ t__
      = let {
              goto_terminal'1__ v1 = k_105_1__ v1;
              goto_pat'1__ v1 = reduce_33__ goto_terminal'1__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1
                  = state_70__ (reduce_34__ goto_terminal'1__ v1)
                        (reduce_38__ goto_apat'1__ v1);
              goto_srcloc'1__ v1
                  = state_89__ (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_172__ :: (Nonterm -> Parser__) -> Parser__
  state_172__ k_109_2__ t__
      = let {
              goto_nonterminal'1__ v1 = k_109_2__ v1;
              goto_expr0'1__ v1
                  = state_65__ (reduce_31__ goto_nonterminal'1__ v1);
              goto_varid'1__ v1 = state_67__ (reduce_32__ goto_expr0'1__ v1);
              goto_srcloc'1__ v1 = state_86__ (reduce_42__ goto_varid'1__ v1)
            }
        in reduce_47__ goto_srcloc'1__ t__
  
  state_175__ :: (Expr -> Parser__) -> Parser__
  state_175__ k_111_2__ t__
      = let {
              goto_expr'1__ v1 = k_111_2__ v1;
              goto_aexpr'1__ v1 = reduce_112__ goto_expr'1__ v1;
              goto_varid'1__ v1
                  = state_178__ (reduce_113__ goto_expr'1__ v1)
                        (reduce_115__ goto_aexpr'1__ v1);
              goto_conid'1__ v1
                  = state_180__ (reduce_114__ goto_expr'1__ v1)
                        (reduce_116__ goto_aexpr'1__ v1);
              goto_literal'1__ v1 = reduce_117__ goto_aexpr'1__ v1;
              goto_srcloc'1__ v1
                  = state_87__ (reduce_42__ goto_varid'1__ v1)
                                    (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_120__ goto_aexpr'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_185__ (reduce_118__ goto_aexpr'1__);
               LeftBracket -> get >>= state_188__ (reduce_119__ goto_aexpr'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_178__ :: (([(Expr)]) -> Parser__) -> Parser__ -> Parser__
  state_178__ k_113_1__ k_115_1__ t__
      = let {
              goto_aexpr'1__ v1
                  = state_192__ (reduce_121__ goto_many1'1_aexpr'1__ v1);
              goto_varid'1__ v1 = reduce_115__ goto_aexpr'1__ v1;
              goto_conid'1__ v1 = reduce_116__ goto_aexpr'1__ v1;
              goto_literal'1__ v1 = reduce_117__ goto_aexpr'1__ v1;
              goto_srcloc'1__ v1
                  = state_87__ (reduce_42__ goto_varid'1__ v1)
                                    (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_120__ goto_aexpr'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1);
              goto_many1'1_aexpr'1__ v1 = k_113_1__ v1
            }
        in case t__ of {
               Comma -> k_115_1__ t__;
               LeftParen -> get >>= state_185__ (reduce_118__ goto_aexpr'1__);
               RightParen -> k_115_1__ t__;
               LeftBracket -> get >>= state_188__ (reduce_119__ goto_aexpr'1__);
               RightBracket -> k_115_1__ t__;
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_180__ :: (([(Expr)]) -> Parser__) -> Parser__ -> Parser__
  state_180__ k_114_1__ k_116_1__ t__
      = let {
              goto_aexpr'1__ v1
                  = state_192__ (reduce_121__ goto_many1'1_aexpr'1__ v1);
              goto_varid'1__ v1 = reduce_115__ goto_aexpr'1__ v1;
              goto_conid'1__ v1 = reduce_116__ goto_aexpr'1__ v1;
              goto_literal'1__ v1 = reduce_117__ goto_aexpr'1__ v1;
              goto_srcloc'1__ v1
                  = state_87__ (reduce_42__ goto_varid'1__ v1)
                                    (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_120__ goto_aexpr'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1);
              goto_many1'1_aexpr'1__ v1 = k_114_1__ v1
            }
        in case t__ of {
               Comma -> k_116_1__ t__;
               LeftParen -> get >>= state_185__ (reduce_118__ goto_aexpr'1__);
               RightParen -> k_116_1__ t__;
               LeftBracket -> get >>= state_188__ (reduce_119__ goto_aexpr'1__);
               RightBracket -> k_116_1__ t__;
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_185__ :: (([(Expr)]) -> Parser__) -> Parser__
  state_185__ k_118_1__ t__
      = let {
              goto_expr'1__ v1 = reduce_110__ goto_sepBy1''1_expr'1_22__ v1;
              goto_aexpr'1__ v1 = reduce_112__ goto_expr'1__ v1;
              goto_varid'1__ v1
                  = state_178__ (reduce_113__ goto_expr'1__ v1)
                        (reduce_115__ goto_aexpr'1__ v1);
              goto_conid'1__ v1
                  = state_180__ (reduce_114__ goto_expr'1__ v1)
                        (reduce_116__ goto_aexpr'1__ v1);
              goto_literal'1__ v1 = reduce_117__ goto_aexpr'1__ v1;
              goto_srcloc'1__ v1
                  = state_87__ (reduce_42__ goto_varid'1__ v1)
                                    (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_120__ goto_aexpr'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1);
              goto_sepBy'1_expr'1_22__ v1 = state_186__ (k_118_1__ v1);
              goto_sepBy1'1_expr'1_22__ v1
                  = reduce_77__ goto_sepBy'1_expr'1_22__ v1;
              goto_sepBy1''1_expr'1_22__ v1
                  = state_132__ (reduce_84__ goto_sepBy1'1_expr'1_22__ v1)
                        (reduce_111__ goto_sepBy1''1_expr'1_22__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_185__ (reduce_118__ goto_aexpr'1__);
               RightParen -> reduce_76__ goto_sepBy'1_expr'1_22__ t__;
               LeftBracket -> get >>= state_188__ (reduce_119__ goto_aexpr'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_186__ :: Parser__ -> Parser__
  state_186__ k_118_2__ t__
      = case t__ of { RightParen -> get >>= k_118_2__; _ -> frown [")"] t__ }
  
  state_188__ :: (([(Expr)]) -> Parser__) -> Parser__
  state_188__ k_119_1__ t__
      = let {
              goto_expr'1__ v1 = reduce_110__ goto_sepBy1''1_expr'1_22__ v1;
              goto_aexpr'1__ v1 = reduce_112__ goto_expr'1__ v1;
              goto_varid'1__ v1
                  = state_178__ (reduce_113__ goto_expr'1__ v1)
                        (reduce_115__ goto_aexpr'1__ v1);
              goto_conid'1__ v1
                  = state_180__ (reduce_114__ goto_expr'1__ v1)
                        (reduce_116__ goto_aexpr'1__ v1);
              goto_literal'1__ v1 = reduce_117__ goto_aexpr'1__ v1;
              goto_srcloc'1__ v1
                  = state_87__ (reduce_42__ goto_varid'1__ v1)
                                    (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_120__ goto_aexpr'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1);
              goto_sepBy'1_expr'1_22__ v1 = state_189__ (k_119_1__ v1);
              goto_sepBy1'1_expr'1_22__ v1
                  = reduce_77__ goto_sepBy'1_expr'1_22__ v1;
              goto_sepBy1''1_expr'1_22__ v1
                  = state_132__ (reduce_84__ goto_sepBy1'1_expr'1_22__ v1)
                        (reduce_111__ goto_sepBy1''1_expr'1_22__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_185__ (reduce_118__ goto_aexpr'1__);
               LeftBracket -> get >>= state_188__ (reduce_119__ goto_aexpr'1__);
               RightBracket -> reduce_76__ goto_sepBy'1_expr'1_22__ t__;
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_189__ :: Parser__ -> Parser__
  state_189__ k_119_2__ t__
      = case t__ of { RightBracket -> get >>= k_119_2__; _ -> frown ["]"] t__ }
  
  state_192__ :: (([((Expr))]) -> Parser__) -> Parser__
  state_192__ k_121_1__ t__
      = let {
              goto_many'1_aexpr'1__ v1 = k_121_1__ v1;
              goto_many''1_aexpr'1__ v1
                  = state_194__ (reduce_122__ goto_many'1_aexpr'1__ v1)
                        (reduce_124__ goto_many''1_aexpr'1__ v1)
            }
        in reduce_123__ goto_many''1_aexpr'1__ t__
  
  state_194__ :: Parser__ -> (Expr -> Parser__) -> Parser__
  state_194__ k_122_1__ k_124_1__ t__
      = let {
              goto_aexpr'1__ v1 = k_124_1__ v1;
              goto_varid'1__ v1 = reduce_115__ goto_aexpr'1__ v1;
              goto_conid'1__ v1 = reduce_116__ goto_aexpr'1__ v1;
              goto_literal'1__ v1 = reduce_117__ goto_aexpr'1__ v1;
              goto_srcloc'1__ v1
                  = state_87__ (reduce_42__ goto_varid'1__ v1)
                                    (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_120__ goto_aexpr'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               Comma -> k_122_1__ t__;
               LeftParen -> get >>= state_185__ (reduce_118__ goto_aexpr'1__);
               RightParen -> k_122_1__ t__;
               LeftBracket -> get >>= state_188__ (reduce_119__ goto_aexpr'1__);
               RightBracket -> k_122_1__ t__;
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  state_197__ :: (Pat -> Parser__) -> Parser__
  state_197__ k_126_2__ t__
      = let {
              goto_pat'1__ v1 = k_126_2__ v1;
              goto_apat'1__ v1 = reduce_35__ goto_pat'1__ v1;
              goto_conid'1__ v1
                  = state_75__ (reduce_36__ goto_pat'1__ v1)
                        (reduce_37__ goto_apat'1__ v1);
              goto_literal'1__ v1 = reduce_38__ goto_apat'1__ v1;
              goto_srcloc'1__ v1
                  = state_89__ (reduce_43__ goto_conid'1__ v1)
                                (reduce_44__ goto_literal'1__ v1)
                            (reduce_45__ goto_literal'1__ v1)
                        (reduce_46__ goto_literal'1__ v1);
              goto_haskell'1__ v1 = reduce_41__ goto_apat'1__ v1;
              goto_hsOpen'1__ v1 = state_97__ (reduce_50__ goto_haskell'1__ v1)
            }
        in case t__ of {
               LeftParen -> get >>= state_79__ (reduce_39__ goto_apat'1__);
               LeftBracket -> get >>= state_82__ (reduce_40__ goto_apat'1__);
               LeftCurly -> reduce_51__ goto_hsOpen'1__ t__;
               _ -> reduce_47__ goto_srcloc'1__ t__ }
  
  reduce_2__
      :: (Answer -> Parser__)
          ->
          ([(Terminal)]) -> (()) -> ([Decl]) -> (()) -> ([(Terminal)]) -> Parser__
  reduce_2__ g__ ts _ ds _ us ts__ = g__ ((ts, ds, us)) ts__
  
  reduce_3__ :: ((()) -> Parser__) -> Parser__
  reduce_3__ g__ ts__ = ( skipWhite True) >>= (\ v1 -> g__ v1 ts__)
  
  reduce_4__ :: ((()) -> Parser__) -> Parser__
  reduce_4__ g__ ts__ = ( skipWhite False) >>= (\ v1 -> g__ v1 ts__)
  
  reduce_5__ :: (([Decl]) -> Parser__) -> ([([Decl])]) -> Parser__
  reduce_5__ g__ dss ts__ = g__ (concat dss) ts__
  
  reduce_6__
      :: (([(Terminal)]) -> Parser__)
          ->
          ([((Terminal))] -> [((Terminal))]) -> Parser__
  reduce_6__ g__ s ts__ = g__ (s []) ts__
  
  reduce_7__
      :: (([([Decl])]) -> Parser__) -> ([(([Decl]))] -> [(([Decl]))]) -> Parser__
  reduce_7__ g__ s ts__ = g__ (s []) ts__
  
  reduce_8__ :: (([(Terminal)] -> [(Terminal)]) -> Parser__) -> Parser__
  reduce_8__ g__ ts__ = g__ (\ as -> as) ts__
  
  reduce_9__
      :: (([(Terminal)] -> [(Terminal)]) -> Parser__)
          ->
          ([((Terminal))] -> [((Terminal))]) -> Terminal -> Parser__
  reduce_9__ g__ s a ts__ = g__ (\ as -> s (a : as)) ts__
  
  reduce_10__ :: (([([Decl])] -> [([Decl])]) -> Parser__) -> Parser__
  reduce_10__ g__ ts__ = g__ (\ as -> as) ts__
  
  reduce_11__
      :: (([([Decl])] -> [([Decl])]) -> Parser__)
          ->
          ([(([Decl]))] -> [(([Decl]))]) -> ([Decl]) -> Parser__
  reduce_11__ g__ s a ts__ = g__ (\ as -> s (a : as)) ts__
  
  reduce_12__ :: (([Decl]) -> Parser__) -> Decl -> Parser__
  reduce_12__ g__ d ts__ = g__ ([d]) ts__
  
  reduce_13__ :: (([Decl]) -> Parser__) -> Decl -> Parser__
  reduce_13__ g__ d ts__ = g__ ([d]) ts__
  
  reduce_14__ :: (([Decl]) -> Parser__) -> Decl -> Parser__
  reduce_14__ g__ d ts__ = g__ ([d]) ts__
  
  reduce_15__ :: (([Decl]) -> Parser__) -> Decl -> Parser__
  reduce_15__ g__ d ts__ = g__ ([d]) ts__
  
  reduce_16__ :: (([Decl]) -> Parser__) -> ([Decl]) -> Parser__
  reduce_16__ g__ ds ts__ = g__ ds ts__
  
  reduce_17__ :: (Decl -> Parser__) -> ([(AnnTerm)]) -> Parser__
  reduce_17__ g__ cs ts__ = g__ (Terminals cs) ts__
  
  reduce_18__ :: (Decl -> Parser__) -> ([((Nonterm, Bool))]) -> Parser__
  reduce_18__ g__ cs ts__ = g__ (Nonterminals cs) ts__
  
  reduce_19__ :: (Decl -> Parser__) -> String -> Term -> Parser__
  reduce_19__ g__ n t ts__ = g__ (Fixity (left n) t) ts__
  
  reduce_20__ :: (Decl -> Parser__) -> String -> Term -> Parser__
  reduce_20__ g__ n t ts__ = g__ (Fixity (right n) t) ts__
  
  reduce_21__ :: (Decl -> Parser__) -> String -> Term -> Parser__
  reduce_21__ g__ n t ts__ = g__ (Fixity (nonassoc n) t) ts__
  
  reduce_22__ :: (Decl -> Parser__) -> Nonterm -> ([Nonterm]) -> Parser__
  reduce_22__ g__ n ns ts__ = g__ (TypeSig n ns False) ts__
  
  reduce_23__ :: (Decl -> Parser__) -> Nonterm -> ([Nonterm]) -> Parser__
  reduce_23__ g__ n ns ts__ = g__ (TypeSig n ns False) ts__
  
  reduce_24__ :: (Decl -> Parser__) -> Nonterm -> Parser__
  reduce_24__ g__ n ts__ = g__ (TypeSig n [] True) ts__
  
  reduce_25__ :: (Decl -> Parser__) -> Nonterm -> Parser__
  reduce_25__ g__ n ts__ = g__ (TypeSig n [] True) ts__
  
  reduce_26__ :: (([Nonterm]) -> Parser__) -> Parser__
  reduce_26__ g__ ts__ = g__ ([]) ts__
  
  reduce_27__ :: (([Nonterm]) -> Parser__) -> ([(Nonterm)]) -> Parser__
  reduce_27__ g__ ns ts__ = g__ ns ts__
  
  reduce_28__
      :: (([Decl]) -> Parser__)
          ->
          Nonterm
              ->
              ([((Modifier, Sym))])
                  ->
                  ([([Quoted], [(Modifier, Sym)])]) -> Parser__
  reduce_28__ g__ ((e, us)) vs alts ts__ = g__ (prods e ((us, vs) : alts)) ts__
  
  reduce_29__ :: (([([Quoted], [(Modifier, Sym)])]) -> Parser__) -> Parser__
  reduce_29__ g__ ts__ = g__ ([]) ts__
  
  reduce_30__
      :: (([([Quoted], [(Modifier, Sym)])]) -> Parser__)
          ->
          ([Quoted])
              ->
              ([((Modifier, Sym))])
                  ->
                  ([([Quoted], [(Modifier, Sym)])]) -> Parser__
  reduce_30__ g__ us vs alts ts__ = g__ ((us, vs) : alts) ts__
  
  reduce_31__ :: (Nonterm -> Parser__) -> Expr -> ([Quoted]) -> Parser__
  reduce_31__ g__ e qs ts__ = g__ ((e, qs)) ts__
  
  reduce_32__ :: (Expr -> Parser__) -> Ident -> ([(Expr)]) -> Parser__
  reduce_32__ g__ n es ts__ = g__ (Var n <$> es) ts__
  
  reduce_33__ :: (Term -> Parser__) -> Pat -> Parser__
  reduce_33__ g__ p ts__ = g__ p ts__
  
  reduce_34__
      :: (Term -> Parser__) -> Literal -> ([Token]) -> ([Quoted]) -> Parser__
  reduce_34__ g__ l q qs ts__ = g__ (Literal l <$> map Quoted (q : qs)) ts__
  
  reduce_35__ :: (Pat -> Parser__) -> Pat -> Parser__
  reduce_35__ g__ p ts__ = g__ p ts__
  
  reduce_36__ :: (Pat -> Parser__) -> Ident -> ([(Pat)]) -> Parser__
  reduce_36__ g__ k ps ts__ = g__ (Con k <$> ps) ts__
  
  reduce_37__ :: (Pat -> Parser__) -> Ident -> Parser__
  reduce_37__ g__ k ts__ = g__ (Con k) ts__
  
  reduce_38__ :: (Pat -> Parser__) -> Literal -> Parser__
  reduce_38__ g__ l ts__ = g__ (Literal l) ts__
  
  reduce_39__ :: (Pat -> Parser__) -> ([(Pat)]) -> Parser__
  reduce_39__ g__ ps ts__ = g__ (tuple ps) ts__
  
  reduce_40__ :: (Pat -> Parser__) -> ([(Pat)]) -> Parser__
  reduce_40__ g__ ps ts__ = g__ (List ps) ts__
  
  reduce_41__ :: (Pat -> Parser__) -> ([Token]) -> Parser__
  reduce_41__ g__ ts ts__ = g__ (Quoted ts) ts__
  
  reduce_42__ :: (Ident -> Parser__) -> SrcLoc -> String -> Parser__
  reduce_42__ g__ loc s ts__ = g__ (identAt s loc) ts__
  
  reduce_43__ :: (Ident -> Parser__) -> SrcLoc -> String -> Parser__
  reduce_43__ g__ loc s ts__ = g__ (identAt s loc) ts__
  
  reduce_44__ :: (Literal -> Parser__) -> SrcLoc -> String -> Parser__
  reduce_44__ g__ loc s ts__ = g__ (stringLitAt s loc) ts__
  
  reduce_45__ :: (Literal -> Parser__) -> SrcLoc -> String -> Parser__
  reduce_45__ g__ loc s ts__ = g__ (numeralAt   s loc) ts__
  
  reduce_46__ :: (Literal -> Parser__) -> SrcLoc -> String -> Parser__
  reduce_46__ g__ loc s ts__ = g__ (charLitAt   s loc) ts__
  
  reduce_47__ :: (SrcLoc -> Parser__) -> Parser__
  reduce_47__ g__ ts__ = ( srcloc) >>= (\ v1 -> g__ v1 ts__)
  
  reduce_48__ :: (([Quoted]) -> Parser__) -> Parser__
  reduce_48__ g__ ts__ = g__ ([]) ts__
  
  reduce_49__ :: (([Quoted]) -> Parser__) -> ([Token]) -> ([Quoted]) -> Parser__
  reduce_49__ g__ q qs ts__ = g__ (q : qs) ts__
  
  reduce_50__
      :: (([Token]) -> Parser__)
          ->
          (()) -> ([([Token] -> [Token])]) -> (()) -> Parser__
  reduce_50__ g__ _ ts _ ts__ = g__ (conc ts []) ts__
  
  reduce_51__ :: ((()) -> Parser__) -> Parser__
  reduce_51__ g__ ts__ = ( skipWhite False) >>= (\ v1 -> g__ v1 ts__)
  
  reduce_52__ :: ((()) -> Parser__) -> Parser__
  reduce_52__ g__ ts__ = ( skipWhite True) >>= (\ v1 -> g__ v1 ts__)
  
  reduce_53__
      :: (([(Expr)]) -> Parser__) -> ([((Expr))] -> [((Expr))]) -> Parser__
  reduce_53__ g__ s ts__ = g__ (s []) ts__
  
  reduce_54__
      :: (([([Token] -> [Token])]) -> Parser__)
          ->
          ([(([Token] -> [Token]))] -> [(([Token] -> [Token]))]) -> Parser__
  reduce_54__ g__ s ts__ = g__ (s []) ts__
  
  reduce_55__ :: (([(Expr)] -> [(Expr)]) -> Parser__) -> Parser__
  reduce_55__ g__ ts__ = g__ (\ as -> as) ts__
  
  reduce_56__
      :: (([(Expr)] -> [(Expr)]) -> Parser__)
          ->
          ([((Expr))] -> [((Expr))]) -> Expr -> Parser__
  reduce_56__ g__ s a ts__ = g__ (\ as -> s (a : as)) ts__
  
  reduce_57__ :: (Expr -> Parser__) -> Ident -> Parser__
  reduce_57__ g__ n ts__ = g__ (Var n) ts__
  
  reduce_58__ :: (Expr -> Parser__) -> Ident -> Parser__
  reduce_58__ g__ k ts__ = g__ (Con k) ts__
  
  reduce_59__ :: (Expr -> Parser__) -> Literal -> Parser__
  reduce_59__ g__ l ts__ = g__ (Literal l) ts__
  
  reduce_60__ :: (Expr -> Parser__) -> ([(Expr)]) -> Parser__
  reduce_60__ g__ es ts__ = g__ (tuple es) ts__
  
  reduce_61__ :: (Expr -> Parser__) -> ([(Expr)]) -> Parser__
  reduce_61__ g__ es ts__ = g__ (List es) ts__
  
  reduce_62__
      :: (([([Token] -> [Token])] -> [([Token] -> [Token])]) -> Parser__)
          ->
          Parser__
  reduce_62__ g__ ts__ = g__ (\ as -> as) ts__
  
  reduce_63__
      :: (([([Token] -> [Token])] -> [([Token] -> [Token])]) -> Parser__)
          ->
          ([(([Token] -> [Token]))] -> [(([Token] -> [Token]))])
              ->
              ([Token] -> [Token]) -> Parser__
  reduce_63__ g__ s a ts__ = g__ (\ as -> s (a : as)) ts__
  
  reduce_64__ :: (([Token] -> [Token]) -> Parser__) -> Terminal -> Parser__
  reduce_64__ g__ t ts__ = g__ (single t) ts__
  
  reduce_65__
      :: (([Token] -> [Token]) -> Parser__)
          ->
          ([([Token] -> [Token])]) -> Parser__
  reduce_65__ g__ ts ts__
      = g__ (single LeftCurly . conc ts . single RightCurly) ts__
  
  reduce_66__ :: (([(Pat)]) -> Parser__) -> Pat -> ([((Pat))]) -> Parser__
  reduce_66__ g__ a as ts__ = g__ (a : as) ts__
  
  reduce_67__ :: (([(Pat)]) -> Parser__) -> ([((Pat))] -> [((Pat))]) -> Parser__
  reduce_67__ g__ s ts__ = g__ (s []) ts__
  
  reduce_68__ :: (([(Pat)] -> [(Pat)]) -> Parser__) -> Parser__
  reduce_68__ g__ ts__ = g__ (\ as -> as) ts__
  
  reduce_69__
      :: (([(Pat)] -> [(Pat)]) -> Parser__)
          ->
          ([((Pat))] -> [((Pat))]) -> Pat -> Parser__
  reduce_69__ g__ s a ts__ = g__ (\ as -> s (a : as)) ts__
  
  reduce_70__ :: (([(AnnTerm)]) -> Parser__) -> Parser__
  reduce_70__ g__ ts__ = g__ ([]) ts__
  
  reduce_71__ :: (([(AnnTerm)]) -> Parser__) -> ([((AnnTerm))]) -> Parser__
  reduce_71__ g__ as ts__ = g__ as ts__
  
  reduce_72__ :: (([((Nonterm, Bool))]) -> Parser__) -> Parser__
  reduce_72__ g__ ts__ = g__ ([]) ts__
  
  reduce_73__
      :: (([((Nonterm, Bool))]) -> Parser__)
          ->
          ([(((Nonterm, Bool)))]) -> Parser__
  reduce_73__ g__ as ts__ = g__ as ts__
  
  reduce_74__ :: (([((Modifier, Sym))]) -> Parser__) -> Parser__
  reduce_74__ g__ ts__ = g__ ([]) ts__
  
  reduce_75__
      :: (([((Modifier, Sym))]) -> Parser__)
          ->
          ([(((Modifier, Sym)))]) -> Parser__
  reduce_75__ g__ as ts__ = g__ as ts__
  
  reduce_76__ :: (([(Expr)]) -> Parser__) -> Parser__
  reduce_76__ g__ ts__ = g__ ([]) ts__
  
  reduce_77__ :: (([(Expr)]) -> Parser__) -> ([((Expr))]) -> Parser__
  reduce_77__ g__ as ts__ = g__ as ts__
  
  reduce_78__ :: (([(Pat)]) -> Parser__) -> Parser__
  reduce_78__ g__ ts__ = g__ ([]) ts__
  
  reduce_79__ :: (([(Pat)]) -> Parser__) -> ([((Pat))]) -> Parser__
  reduce_79__ g__ as ts__ = g__ as ts__
  
  reduce_80__
      :: (([(AnnTerm)]) -> Parser__)
          ->
          ([((AnnTerm))] -> [((AnnTerm))]) -> Parser__
  reduce_80__ g__ s ts__ = g__ (s []) ts__
  
  reduce_81__
      :: (([((Nonterm, Bool))]) -> Parser__)
          ->
          ([(((Nonterm, Bool)))] -> [(((Nonterm, Bool)))]) -> Parser__
  reduce_81__ g__ s ts__ = g__ (s []) ts__
  
  reduce_82__
      :: (([((Modifier, Sym))]) -> Parser__)
          ->
          ([(((Modifier, Sym)))] -> [(((Modifier, Sym)))]) -> Parser__
  reduce_82__ g__ s ts__ = g__ (s []) ts__
  
  reduce_83__
      :: (([(Nonterm)]) -> Parser__)
          ->
          ([((Nonterm))] -> [((Nonterm))]) -> Parser__
  reduce_83__ g__ s ts__ = g__ (s []) ts__
  
  reduce_84__
      :: (([(Expr)]) -> Parser__) -> ([((Expr))] -> [((Expr))]) -> Parser__
  reduce_84__ g__ s ts__ = g__ (s []) ts__
  
  reduce_85__ :: (([(Pat)]) -> Parser__) -> ([((Pat))] -> [((Pat))]) -> Parser__
  reduce_85__ g__ s ts__ = g__ (s []) ts__
  
  reduce_86__ :: (([(AnnTerm)] -> [(AnnTerm)]) -> Parser__) -> AnnTerm -> Parser__
  reduce_86__ g__ a ts__ = g__ (\ as -> a : as) ts__
  
  reduce_87__
      :: (([(AnnTerm)] -> [(AnnTerm)]) -> Parser__)
          ->
          ([((AnnTerm))] -> [((AnnTerm))]) -> AnnTerm -> Parser__
  reduce_87__ g__ s a ts__ = g__ (\ as -> s (a : as)) ts__
  
  reduce_88__ :: (AnnTerm -> Parser__) -> Bool -> Assoc -> Term -> Parser__
  reduce_88__ g__ m a p ts__ = g__ ((p, m, a, Nothing)) ts__
  
  reduce_89__
      :: (AnnTerm -> Parser__) -> Bool -> Assoc -> Literal -> Term -> Parser__
  reduce_89__ g__ m a l p ts__ = g__ ((p, m, a, Just l)) ts__
  
  reduce_90__
      :: (AnnTerm -> Parser__) -> Bool -> Assoc -> Term -> Literal -> Parser__
  reduce_90__ g__ m a p l ts__ = g__ ((p, m, a, Just l)) ts__
  
  reduce_91__
      :: (AnnTerm -> Parser__)
          ->
          Bool -> Assoc -> ([Token]) -> Literal -> Parser__
  reduce_91__ g__ m a q l ts__ = g__ ((guard q, m, a, Just l)) ts__
  
  reduce_92__ :: (Bool -> Parser__) -> Parser__
  reduce_92__ g__ ts__ = g__ False ts__
  
  reduce_93__ :: (Bool -> Parser__) -> Parser__
  reduce_93__ g__ ts__ = g__ True ts__
  
  reduce_94__ :: (Assoc -> Parser__) -> Parser__
  reduce_94__ g__ ts__ = g__ Unspecified ts__
  
  reduce_95__ :: (Assoc -> Parser__) -> String -> Parser__
  reduce_95__ g__ n ts__ = g__ (left n) ts__
  
  reduce_96__ :: (Assoc -> Parser__) -> String -> Parser__
  reduce_96__ g__ n ts__ = g__ (right n) ts__
  
  reduce_97__ :: (Assoc -> Parser__) -> String -> Parser__
  reduce_97__ g__ n ts__ = g__ (nonassoc n) ts__
  
  reduce_98__
      :: (([((Nonterm, Bool))] -> [((Nonterm, Bool))]) -> Parser__)
          ->
          ((Nonterm, Bool)) -> Parser__
  reduce_98__ g__ a ts__ = g__ (\ as -> a : as) ts__
  
  reduce_99__
      :: (([((Nonterm, Bool))] -> [((Nonterm, Bool))]) -> Parser__)
          ->
          ([(((Nonterm, Bool)))] -> [(((Nonterm, Bool)))])
              ->
              ((Nonterm, Bool)) -> Parser__
  reduce_99__ g__ s a ts__ = g__ (\ as -> s (a : as)) ts__
  
  reduce_100__ :: (((Nonterm, Bool)) -> Parser__) -> Bool -> Nonterm -> Parser__
  reduce_100__ g__ m p ts__ = g__ ((p, m)) ts__
  
  reduce_101__
      :: (([((Modifier, Sym))] -> [((Modifier, Sym))]) -> Parser__)
          ->
          ((Modifier, Sym)) -> Parser__
  reduce_101__ g__ a ts__ = g__ (\ as -> a : as) ts__
  
  reduce_102__
      :: (([((Modifier, Sym))] -> [((Modifier, Sym))]) -> Parser__)
          ->
          ([(((Modifier, Sym)))] -> [(((Modifier, Sym)))])
              ->
              ((Modifier, Sym)) -> Parser__
  reduce_102__ g__ s a ts__ = g__ (\ as -> s (a : as)) ts__
  
  reduce_103__ :: (((Modifier, Sym)) -> Parser__) -> Term -> Parser__
  reduce_103__ g__ p ts__ = g__ ((Insert, Term p)) ts__
  
  reduce_104__ :: (((Modifier, Sym)) -> Parser__) -> Term -> Parser__
  reduce_104__ g__ p ts__ = g__ ((Delete, Term p)) ts__
  
  reduce_105__ :: (((Modifier, Sym)) -> Parser__) -> Term -> Parser__
  reduce_105__ g__ p ts__ = g__ ((Prec,   Term p)) ts__
  
  reduce_106__ :: (((Modifier, Sym)) -> Parser__) -> Term -> Parser__
  reduce_106__ g__ p ts__ = g__ ((Copy,   Term p)) ts__
  
  reduce_107__ :: (((Modifier, Sym)) -> Parser__) -> Nonterm -> Parser__
  reduce_107__ g__ n ts__ = g__ ((Copy,   Nonterm n)) ts__
  
  reduce_108__
      :: (([(Nonterm)] -> [(Nonterm)]) -> Parser__) -> Nonterm -> Parser__
  reduce_108__ g__ a ts__ = g__ (\ as -> a : as) ts__
  
  reduce_109__
      :: (([(Nonterm)] -> [(Nonterm)]) -> Parser__)
          ->
          ([((Nonterm))] -> [((Nonterm))]) -> Nonterm -> Parser__
  reduce_109__ g__ s a ts__ = g__ (\ as -> s (a : as)) ts__
  
  reduce_110__ :: (([(Expr)] -> [(Expr)]) -> Parser__) -> Expr -> Parser__
  reduce_110__ g__ a ts__ = g__ (\ as -> a : as) ts__
  
  reduce_111__
      :: (([(Expr)] -> [(Expr)]) -> Parser__)
          ->
          ([((Expr))] -> [((Expr))]) -> Expr -> Parser__
  reduce_111__ g__ s a ts__ = g__ (\ as -> s (a : as)) ts__
  
  reduce_112__ :: (Expr -> Parser__) -> Expr -> Parser__
  reduce_112__ g__ e ts__ = g__ e ts__
  
  reduce_113__ :: (Expr -> Parser__) -> Ident -> ([(Expr)]) -> Parser__
  reduce_113__ g__ n es ts__ = g__ (Var n <$> es) ts__
  
  reduce_114__ :: (Expr -> Parser__) -> Ident -> ([(Expr)]) -> Parser__
  reduce_114__ g__ k es ts__ = g__ (Con k <$> es) ts__
  
  reduce_115__ :: (Expr -> Parser__) -> Ident -> Parser__
  reduce_115__ g__ n ts__ = g__ (Var n) ts__
  
  reduce_116__ :: (Expr -> Parser__) -> Ident -> Parser__
  reduce_116__ g__ k ts__ = g__ (Con k) ts__
  
  reduce_117__ :: (Expr -> Parser__) -> Literal -> Parser__
  reduce_117__ g__ l ts__ = g__ (Literal l) ts__
  
  reduce_118__ :: (Expr -> Parser__) -> ([(Expr)]) -> Parser__
  reduce_118__ g__ es ts__ = g__ (tuple es) ts__
  
  reduce_119__ :: (Expr -> Parser__) -> ([(Expr)]) -> Parser__
  reduce_119__ g__ es ts__ = g__ (List es) ts__
  
  reduce_120__ :: (Expr -> Parser__) -> ([Token]) -> Parser__
  reduce_120__ g__ ts ts__ = g__ (Quoted ts) ts__
  
  reduce_121__ :: (([(Expr)]) -> Parser__) -> Expr -> ([((Expr))]) -> Parser__
  reduce_121__ g__ a as ts__ = g__ (a : as) ts__
  
  reduce_122__
      :: (([(Expr)]) -> Parser__) -> ([((Expr))] -> [((Expr))]) -> Parser__
  reduce_122__ g__ s ts__ = g__ (s []) ts__
  
  reduce_123__ :: (([(Expr)] -> [(Expr)]) -> Parser__) -> Parser__
  reduce_123__ g__ ts__ = g__ (\ as -> as) ts__
  
  reduce_124__
      :: (([(Expr)] -> [(Expr)]) -> Parser__)
          ->
          ([((Expr))] -> [((Expr))]) -> Expr -> Parser__
  reduce_124__ g__ s a ts__ = g__ (\ as -> s (a : as)) ts__
  
  reduce_125__ :: (([(Pat)] -> [(Pat)]) -> Parser__) -> Pat -> Parser__
  reduce_125__ g__ a ts__ = g__ (\ as -> a : as) ts__
  
  reduce_126__
      :: (([(Pat)] -> [(Pat)]) -> Parser__)
          ->
          ([((Pat))] -> [((Pat))]) -> Pat -> Parser__
  reduce_126__ g__ s a ts__ = g__ (\ as -> s (a : as)) ts__


{- )-: frown -}







  notSpecial, notBrace          :: Token -> Bool
  notSpecial LeftSpecial        =  False
  notSpecial RightSpecial       =  False
  notSpecial EOF                =  False  -- important to ensure termination!
  notSpecial _                  =  True
 
  notBrace LeftCurly            =  False
  notBrace RightCurly           =  False
  notBrace EOF                  =  False
  notBrace _                    =  True

  single                        :: a -> ([a] -> [a])
  single a                      =  \ x -> a : x
 
  conc                          :: [[a] -> [a]] -> ([a] -> [a])
  conc                          =  foldr (.) id

  prods                         :: Expr -> [([Quoted], [(Modifier, Sym)])] -> [Decl]
  prods e alts                  =  [ Production (e, us) vs | (us, vs) <- alts ]
 
  guard                         :: Quoted -> Pat
  guard q                       =  Guard (Quoted [Conid "Terminal"]) (Quoted q) -- HACK
 
  left, right, nonassoc         :: String -> Assoc
  left s                        =  LeftAssoc  (read s)
  right s                       =  RightAssoc (read s)
  nonassoc s                    =  NonAssoc   (read s)







  type CPS a ans                =  (a -> ans) -> ans
 
  data Lex m a                  =  Lex { unLex :: CPS a (Bool           -- skip white space?
                                                         -> String      -- input
                                                         -> Int         -- line count
                                                         -> [String]    -- past k lines
                                                         -> m Answer) }
 
  instance (Functor m) => Functor (Lex m) where
      f `fmap` Lex x            = Lex (\ k -> x (k ∘ f))

  instance (Applicative m, Monad m) => Applicative (Lex m) where
      pure a                    = Lex (\ k -> k a)
      (<*>)                     = ap

  instance (Monad m) => Monad (Lex m) where
      return a                  =  Lex (\ cont -> cont a)
      m >>= k                   =  Lex (\ cont -> unLex m (\ a -> unLex (k a) cont))
      fail s                    =  lift (fail s)
 
  lift                          :: (Monad m) => m a -> Lex m a
  lift m                        =  Lex (\cont skip inp n buf -> m >>= \ a -> cont a skip inp n buf)
 
  skipWhite                     :: Bool -> Result ()
  skipWhite flag                =  Lex (\ cont _skip inp n buf -> cont () flag inp n buf)
 
  rest                          :: String -> String
  rest s                        =  takeWhile (/= '\n') s
 
  srcloc                        :: (Monad m) => Lex m SrcLoc
  srcloc                        =  Lex (\cont skip inp n buf ->
                                     let k = length (last buf) - length (rest inp)  -- last character of the previous token!
                                     in  cont (At n k) skip inp n buf)





  frown                         :: [String] -> Token -> Result a
  frown la _t                   =  Lex (\ _cont _skip inp n buf ->
                                     fail ((if null buf
                                            then "syntax error: empty input"
                                            else "syntax error at "
                                                 ++ showLoc n (length (last buf) - length (rest inp)) ++ ":\n"
                                                 ++ context 2 inp buf)
                                           ++ "* expected: " ++ concat (intersperse ", " (map wrap la))))
 
  context                       :: Int -> String -> [String] -> String
  context n inp buf             =  unlines (buf
                                            ++ [replicate col' ' ' ++ "^"]
                                            ++ take n (lines (drop 1 (dropWhile (/= '\n') inp))
                                                       ++ ["<end of input>"]))
      where col'                =  length (last buf) - length (rest inp) - 1
 
  wrap                          :: String -> String
  wrap ""                       =  ""
  wrap s@('<' : _)              =  s
  wrap s                        =  "`" ++ s ++ "'"





  get                           :: Result Token
  get                           =
    Lex (\cont skip inp line buf ->
      let continue t            =  cont t skip
 
          possiblySkip t
            | skip              =  lex
            | otherwise         =  continue t
 
          lex "" n b            =  continue EOF "" n b
          lex ('%':'{':s) n b   =  continue LeftSpecial  s n b
          lex ('}':'%':s) n b   =  continue RightSpecial s n b
          lex ('\'':s) n b      =  let (t, u) = lexCharLit s in
                                   case match "\'" u of
                                     Nothing -> panic "character literal" ('\'' : s) n b
                                     Just v  -> let k = newlines t in
                                                continue (Char ("'" ++ t ++ "'")) v (n + k) (shift k b s)
          lex ('"':s) n b       =  let (t, u) = lexStrLit s in
                                   case match "\"" u of
                                     Nothing -> panic "string literal" ('"' : s) n b
                                     Just v  -> let k = newlines t in
                                                continue (String ("\"" ++ t ++ "\"")) v (n + k) (shift k b s)
          lex ('-':'-':s) n b   =  let (t, u) = break (== '\n') s in possiblySkip (Comment t) u n b
          lex ('{':'-':s) n b   =  let (t, u) = nested 0 s in
                                     case match "-}" u of
                                       Nothing -> panic "missing `-}'" ('{':'-':s) n b
                                       Just v  -> let k = newlines t in
                                                  possiblySkip (Nested t) v (n + k) (shift k b s)
          lex (',' : s) n b     =  continue Comma        s n b
          lex (';' : s) n b     =  continue Semicolon    s n b
          lex ('(' : s) n b     =  continue LeftParen    s n b
          lex (')' : s) n b     =  continue RightParen   s n b
          lex ('[' : s) n b     =  continue LeftBracket  s n b
          lex (']' : s) n b     =  continue RightBracket s n b
          lex ('{' : s) n b     =  continue LeftCurly    s n b
          lex ('}' : s) n b     =  continue RightCurly   s n b
          lex ('`' : s) n b     =  continue Backquote    s n b
          lex (c : s) n b
            | isSpace c         =  let (t, u) = span isSpace s
                                       k      = newlines (c : t)
                                   in  possiblySkip (White (c : t)) u (n + k) (shift k b (c : s))
            | isUpper c         =  let (t, u) = span isIdChar s in continue (Conid  (c : t)) u n b
            | isLower c || c == '_'
                                =  let (t, u) = span isIdChar s in continue (Varid  (c : t)) u n b
            | c == ':'          =  let (t, u) = span isSymbol s in continue (Consym (c : t)) u n b
            | isSymbol c        =  let (t, u) = span isSymbol s in continue (Varsym (c : t)) u n b
            | isDigit c         =  let (ds, t) = span isDigit s in
                                     case lexFracExp t of
                                        Nothing      -> panic "numeral" (c : s) n b
                                        Just (fe, u) -> continue (Numeral (c : ds ++ fe)) u n b
            | otherwise         =  panic "strange character" s n b
 
          panic msg s n b       =  fail ("lexical error (" ++ msg ++ ") at "
                                         ++ showLoc n (length (last b) - length (rest s)) ++ ":\n"
                                         ++ context 4 s b)
      in lex inp line buf)
 
  shift                         :: Int -> [String] -> String -> [String]
  shift k past inp              =  reverse (take 2 (reverse (past ++ take k (lines inp'))))
    where inp'                  =  drop 1 (dropWhile (/= '\n') inp)
 
  newlines                      :: String -> Int
  newlines ""                   =  0
  newlines (c : s)
    | c == '\n'                 =  1 + newlines s
    | otherwise                 =  newlines s





  parse                         :: [Flag] -> String -> IO Answer
  parse opts inp                =  do verb "* Parsing ..."
                                      (l, ds, r) <- case run file inp of
                                                        Fail s   -> panic s
                                                        Return x -> return x
                                      verb ("  " ++ show (length ds) ++ " declarations")
                                      return (l, ds, r)
      where verb                =  verbose opts
 
  run                           :: (Monad m) => Lex m Answer -> (String -> m Answer)
  run parser inp                =  unLex parser (\a _ _ _ _ -> return a) False inp 1 (take 1 (lines inp))
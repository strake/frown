{-

The following unambiguous grammar has a reduce/reduce conflict.

	frown --debug Type.g

Try

	tType [Int, LPAR, RPAR] :: [()]
	tType [Int, LPAR, Num, RPAR] :: [()]

Nice example for the use of nondeterminism.

Alternatively, we can use 2 tokens of lookahead. As the grammar is
LR(2) this yields a deterministic parser.

	frown --debug --lookahead=2 Type.g

Try

	tType [Int, LPAR, RPAR] >>= print
	tType [Int, LPAR, Num, RPAR] >>= print

-}

module Type
where
import Monad

type Result                   =  IO

%{

Terminal                      =  Int | Char | Void | "(" = LPAR | ")" = RPAR | Num;
Nonterminal                   =  tType | aType | vType | aBasicType | aSize | vBasicType | vSize;

tType                         :  aType;
                              |  vType;

aType                         :  aBasicType, aSize;

vType                         :  vBasicType, vSize;

aBasicType                    :  Int;
                              |  Char;

aSize                         :  "(", Num, ")";

vBasicType                    :  Int;
                              |  Char;
                              |  Void;

vSize                         :  "(", ")";

}%

data Terminal                 =  Int | Char | Void | LPAR | RPAR | Num

frown _                       =  fail "syntax error"

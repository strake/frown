{

module Parser
where

data Terminal                   =  A | B | C | D

}

%name parseS S
%name parseX X
%tokentype { Terminal }
%token
      a { A }
      b { B }
      c { C }
      d { D }

%%

S : X a   {}
  | b X c {}
  | Y c   {}
  | b Y a {}

X : d {}

Y : d {}

{
happyError _ = error "parse error"
}

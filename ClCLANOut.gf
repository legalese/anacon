--# -path=.:alltenses
concrete ClCLANOut of Cl =
  ClCLAN - [andC, BaseClause, ConsClause, when, always, anyAct, O, P, F] **
  open Prelude, Formal, ResEng in {

lin

  -- Despite my best efforts, I can't get this to properly match up
  -- with CLAN's output syntax (the parenthesis schemes don't match).
  -- John 16-01-2012

  andC cs = parenth cs ;
  BaseClause x y = join "^" (x) (y) ;
  ConsClause x y = join "^" (parenth x) (parenth y) ;

  when act c = "[" ++ (parenth act.s) ++ "]" ++ parenth c ;

  always c = "[ ( * 1 ) ]" ++ parenth c ;

  anyAct c = "[ 1 ]" ++ parenth c ;
  
  O act cl = case cl.ty of {
    True => parenth ("O" ++ parenth act.s) ++ "_" ++ cl.s ;
    _    => "O" ++ parenth act.s
  };
   
  P act = "P" ++ parenth act.s ;

  F act cl = case cl.ty of {
    True => "F" ++ parenth (parenth act.s) ++ "_" ++ cl.s ;
    _    => "F" ++ parenth (parenth act.s)
  };
 
}

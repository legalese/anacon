--# -path=.:alltenses
concrete ClCLAN of Cl = VerbsEng ** open Prelude, Formal, ResEng in {

param
  ActionOp = AAnd | ASeq | AChoice ;

lincat
  NP = SS;

  Act = TermPrec ;
  [Act] = ActionOp => Str ;

  Clause,
--  ClauseX,
  ClauseO,
  ClauseP,
  ClauseF,
  [Clause],
  [ClauseO],
  [ClauseP] = Str ;
  ClauseX = {s : Str; ty : Bool} ;

lin
  np s = s ;

  clauseO co = co ;
  clauseP cp = cp ;
  clauseF cf = cf ;

  -- Clauses are always joined by conjunction ∧
  andC cs = cs ;
  BaseClause = join "^" ;
  ConsClause = join "^" ;

  when act c = "[" ++ act.s ++ "]" ++ parenth c ;

  always c = "[ 1* ]" ++ parenth c ;

  anyAct c = "[ 1 ]" ++ parenth c ;
  
  reparation c = {s=c; ty=True} ;
  failure = {s=""; ty=False} ;

  O act cl = case cl.ty of {
    True => parenth ("O" ++ parenth act.s ++ "_" ++ parenth cl.s) ;
    _    => parenth ("O" ++ parenth act.s ++ cl.s)
  };
   
  choiceO lst = lst ;
  BaseClauseO = join "+" ;
  ConsClauseO = join "+" ;

  P act = parenth ("P" ++ parenth act.s) ;
  choiceP lst = lst ;
  BaseClauseP = join "+" ;
  ConsClauseP = join "+" ;

  F act cl = case cl.ty of {
    True => parenth ("F" ++ parenth act.s ++ "_" ++ parenth cl.s) ;
    _    => parenth ("F" ++ parenth act.s ++ cl.s)
  };
 
  atom agent verb action = constant ("{" ++ agent.s ++ "}" ++ verb.s ! VInf ++ "{" ++ action.s ++ "}") ;
  
  andAct    as = mkPrec 2 (as ! AAnd) ;
  choiceAct as = mkPrec 1 (as ! AChoice) ;
  seqAct    as = mkPrec 0 (as ! ASeq) ;

  BaseAct a1 a2 =
    table {
      AAnd    => join "&" (usePrec2 2 a1) (usePrec2 2 a2) ;
      AChoice => join "+" (usePrec2 1 a1) (usePrec2 1 a2) ;
      ASeq    => join "." (usePrec2 0 a1) (usePrec2 0 a2)
    } ;
  ConsAct a as =
    table {
      AAnd    => join "&" (usePrec2 2 a) (as ! AAnd) ;
      AChoice => join "+" (usePrec2 1 a) (as ! AChoice) ;
      ASeq    => join "." (usePrec2 0 a) (as ! ASeq)
    } ;

oper
  join : Str -> Str -> Str -> Str = \glue,x1,x2 -> x1 ++ glue ++ x2 ;

  usePrec2 : Prec -> TermPrec -> Str = \p,x ->
    case lessPrec x.p p of {
      True  => parenth x.s ;
      False => x.s
    } ;

}

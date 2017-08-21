abstract Cl = Verbs ** {

flags
  startcat = Clause ;
  literal = NP ;

cat
  NP;
  
  Act;       -- & -> + -> . -> action (diminishing precedence)
  [Act]{2};
  
  Clause;
  ClauseX;	-- a reparation clause, ie CTP or CTD
  ClauseO;
  ClauseP;
  ClauseF;
  [Clause]{2};
  [ClauseO]{2};
  [ClauseP]{2};

fun
  np : String -> NP ;

  clauseO : ClauseO -> Clause ;
  clauseP : ClauseP -> Clause ;
  clauseF : ClauseF -> Clause ;
  andC  : [Clause] -> Clause ;
  when : Act -> Clause -> Clause ;
  always : Clause -> Clause ;
  anyAct : Clause -> Clause ;

  reparation : Clause -> ClauseX ; -- a valid CTD
  failure    : ClauseX ;           -- the null CTD

  O : Act -> ClauseX -> ClauseO ;
  choiceO : [ClauseO] -> ClauseO ;
  
  P : Act -> ClauseP ;
  choiceP : [ClauseP] -> ClauseP ;

  F : Act -> ClauseX -> ClauseF ;

  atom   : NP -> V -> NP -> Act ;
  andAct : [Act] -> Act ;
  choiceAct : [Act] -> Act ;
  seqAct : [Act] -> Act ;
  
}

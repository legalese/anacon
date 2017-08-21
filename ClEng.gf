--# -path=.:alltenses
concrete ClEng of Cl = VerbsEng ** open SyntaxEng, ParadigmsEng, (R = ResEng), ExtraEng, Predef, Prelude, Formal in {

param
  Mode = Default | Obligation | Permission | Prohibition ;

lincat
  NP = SyntaxEng.NP;

  Act   = {s : Mode => S; p : Prec} ;
  [Act] = Prec => Mode => ListS ;
  ClauseX = {s : S; ty : Bool} ;
  Clause, ClauseO, ClauseP, ClauseF = S ;
  [Clause], [ClauseO], [ClauseP] = ListS ;

lin
  clauseO c = c ;
  clauseP c = c ;
  clauseF c = c ;

  andC lst =
    indentS ("both"|"each of") (mkS bullet_Conj lst) ;
  
  BaseClause = mkListS ;
  ConsClause = consBullet ;

  when act c =
    mkS if_then_Conj (act.s ! Default) c ;
    
  always c =
    mkS (mkAdv "always") c ;

  anyAct c =
    mkS (mkAdv "after any action") c ;

  reparation c = {
    s  = c ;
    ty = True ;
  } ;
  failure = {
    s  = lin S {s=""} ;
    ty = False
  } ;

  O act cl = case cl.ty of {
               True  => mkS (mkConj ", otherwise") (act.s ! Obligation) cl.s ;
               False => lin S {s = cl.s.s ++ (act.s ! Obligation).s}
             } ;

  choiceO lst = indentS "either" (mkS bullet_Conj lst) ;

  BaseClauseO = mkListS ;
  ConsClauseO = consBullet ;

  P act    = act.s ! Permission ;

  choiceP lst = indentS "either" (mkS bullet_Conj lst) ;
  
  BaseClauseP = mkListS ;
  ConsClauseP = consBullet ;

  F act cl = case cl.ty of {
               True  => mkS (mkConj ", otherwise") (act.s ! Prohibition) cl.s ;
               False => lin S {s = cl.s.s ++ (act.s ! Prohibition).s}
             } ;

  atom = mkAtom 0 | mkAtom 1 | mkAtom 2 ;

  andAct    as = {s = \\m => mkS and_Conj  (as ! 2 ! m); p = 2} ;
  choiceAct as = {s = \\m => mkS or_Conj   (as ! 1 ! m); p = 1} ;
  seqAct    as = {s = \\m => mkS then_Conj (as ! 0 ! m); p = 0} ;

  BaseAct a1 a2 = \\p,m => mkListS (actS p m a1) (actS p m a2) ;
  ConsAct a as  = \\p,m => mkListS (actS p m a ) (as ! p ! m) ;
  
  np s = lin NP (R.mkNP s.s s.s (s.s ++ "'s") (singular | plural) R.P3 R.Neutr) ;

oper
  then_Conj = mkConj "first" ", then" ; -- TODO

  bullet = "-" ;
  bullet_Conj = mkConj bullet bullet ;

  consBullet : S -> ListS -> ListS = \x,xs -> lin ListS {
    s1 = x.s ++ bullet ++ xs.s1 ;
    s2 = xs.s2 ;
  } ;

  indentS : Str -> S -> S = \keyword,sen -> lin S { 
    s = keyword ++ "[" ++ sen.s ++ "]" ;
  } ;
  
  actS : Prec -> Mode -> {s : Mode => S; p : Prec} -> S = \p,m,a -> lin S {
    s = usePrec2 p (mkPrec a.p (a.s ! m).s)
  } ;

  usePrec2 : Prec -> TermPrec -> Str = \p,x ->
    case lessPrec x.p p of {
      True  => parenth x.s ;
      False => x.s
    } ;

  -- This operation should be inlined but we need it in order to
  -- restrict the scope of the variants in the linearization of atom.
  mkAtom : Ints 2 -> ClEng.NP -> VerbsEng.V -> ClEng.NP -> {s : Mode => S; p : Prec} ;
  mkAtom n s p o = -- number, actor, verb, action
    {s = table {
           Default     => mkS (mkCl s (mkVP (mkV2 p) o)) ;
           Obligation  => case n of {
                            0 => mkS (mkCl s (PassVPSlash (mkVPSlash (mkV2V (mkV "require") noPrep to_Prep) (mkVP (mkV2 p) o)))) ;
                            1 => mkS presentTense positivePol (mkCl s (mkVP shall_VV (mkVP (mkV2 p) o))) ;
                            2 => mkS presentTense positivePol (mkCl s (mkVP ClEng.must_VV (mkVP (mkV2 p) o)))
                          } ;
           Permission  => case n of { 
                            0     => mkS (mkCl (mkNP it_Pron) (mkAP (mkAP (mkA "optional")) (mkSC (mkS (mkCl s (mkVP (mkV2 p) o)))))) ;
                            1 | 2 => mkS presentTense positivePol (mkCl s (mkVP may_VV (mkVP (mkV2 p) o)))
                          } ;
           Prohibition => case n of {
                            0 | 1 => mkS presentTense negativePol (mkCl s (mkVP shall_VV (mkVP (mkV2 p) o))) ;
                            2     => mkS presentTense negativePol (mkCl s (mkVP ClEng.must_VV (mkVP (mkV2 p) o)))
                          }
        } ;
     p = highest
    } ;

  shall_VV = lin VV {
    s = table { 
      R.VVF R.VInf => ["shall"] ;
      R.VVF R.VPres => "shall" ;
      R.VVF R.VPPart => ["shall"] ;
      R.VVF R.VPresPart => ["shall"] ;
      R.VVF R.VPast => "shall" ;
      R.VVPastNeg => "shall not" ;
      R.VVPresNeg => "shall not"
      } ;
    typ = R.VVAux
    } ;
  must_VV = lin VV {
    s = table {
      R.VVF R.VInf => ["have to"] ;
      R.VVF R.VPres => "must" ;
      R.VVF R.VPPart => ["had to"] ;
      R.VVF R.VPresPart => ["having to"] ;
      R.VVF R.VPast => ["had to"] ;
      R.VVPastNeg => ["hadn't to"] ;
      R.VVPresNeg => "must not"
      } ;
    typ = R.VVAux
    } ;
  may_VV = lin VV {
    s = table { 
      R.VVF R.VInf => ["may"] ;
      R.VVF R.VPres => "may" ;
      R.VVF R.VPPart => ["may"] ;
      R.VVF R.VPresPart => ["may"] ;
      R.VVF R.VPast => "may" ;
      R.VVPastNeg => "may not" ;
      R.VVPresNeg => "may not"
      } ;
    typ = R.VVAux
    } ;

}

concrete ExtraLat of ExtraLatAbs =
  CatLat, ExtraLexiconLat, ConjunctionLat ** 
  open ResLat, ParadigmsLat, Coordination, Prelude in {
  lin
    Nom_Prep = mkPrep "" Nom ;
    Gen_Prep = mkPrep "" Gen ;
    Acc_Prep = mkPrep "" Acc ;
    Dat_Prep = mkPrep "" Dat ;
    Abl_Prep = mkPrep "" Abl ;
    inAbl_Prep = mkPrep "in" Abl ;
    onAbl_Prep = mkPrep "in" Abl ; -- L...
} 

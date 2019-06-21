abstract Video = {

  flags startcat = S;
      
  cat N;
      A;
      NP;
      SUBJ;
      OBJ;
      S;

  fun
    Iuppiter: N;
    Ceres: N;
    magnus: A;
    mkNP1: N -> NP;
    mkNP2: N -> A -> NP;
    mkS: SUBJ -> OBJ -> S;
    mkSUBJ: NP -> SUBJ;
    mkOBJ: NP -> OBJ;

}

concrete CatLat of Cat = CommonX-[Adv] ** open ResLat, Prelude in {

  flags optimize=all_subs ;

  lincat
---- Structural
--
    Conj = ResLat.Conjunction; --{s1,s2 : Str ; n : Number} ;
    Subj = {s : Str} ;
    Prep = ResLat.Prep;
--
---- Open lexical classes, e.g. Lexicon

    V, VS, VQ, VA = ResLat.Verb ; -- = {s : VForm => Str} ;
    V2, V2A, V2Q, V2S = Verb2 ;
    V3 = Verb3 ;
    VV = ResLat.VV ;
    V2V = Verb ** {c2 : Str ; isAux : Bool} ;

    A = Adjective ;
    Adv = Adverb ;
    
    N = Noun ;
    N2 = Noun ** { c : Prep } ;
    N3 = Noun ** { c : Prep ; c2 : Prep } ;
    PN = Noun ;
    A2 = Adjective ** { c : Prep} ;

}

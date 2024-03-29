concrete Video = {

  param
    Case = Nom | Acc;
    Gender = Masc | Fem | Neut;
    Person = Sg1 | Sg2 | Sg3;
    Tense  = Prs | Imp;

  lincat
    N = { s : Case => Set; g : Gender };
    A = { s : Case => Gender => Set };
    NP = { s : Case => Set };
    SUBJ = { s : Set };
    OBJ = { s : Set };
    S = { s : Set };
    V = { s : Tense => Person => Set };

  lin
    Iuppiter : N = { s = table { Nom => "Iuppiter";
    	        	  	 Acc => "Iouem" };
		     g = Masc } ;
    Ceres : N = { s = table { Nom => "Ceres";
                              Acc => "Cererem" };
	          g = Fem } ;
    animal : N = { s = table { Nom => "animal";
                               Acc => "animal" };
	           g = Neut } ;
    magnus : A = { s = table { Nom => table { Masc => "magnus";
                                              Fem  => "magna";
					      Neut => "magnum" };
			       Acc => table { Masc => "magnum";
			                      Fem  => "magnam";
					      Neut => "magnum" }
			     } };
    uidit : V = { s = table { Prs => table { Sg1 => "uideo";
					     Sg2 => "uidis";
					     Sg3 => "uidit" };
			      Imp => table { Sg1 => "uidebam";
					     Sg2 => "uidebas";
					     Sg3 => "uidebat" }
		    }
      };
    
    mkNP (head : N) : NP = { s = head.s };
    mkNP (head : N) (amod : A) : NP = { s = \\c: Case => head.s ! c || amod.s ! c ! head.g };
    mkS (nsubj : SUBJ) (obj : OBJ) (head : V) : S = for p : Person do (for t : Tense do { s = nsubj.s || head.s ! t ! p || obj.s });
    mkSUBJ (head : NP) : SUBJ = { s = head.s ! Nom };
    mkOBJ (head : NP) : OBJ = { s = head.s ! Acc };

}

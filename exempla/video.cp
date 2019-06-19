concrete Video = {

  param
    Case = Nom | Acc;
    Gender = Masc | Fem | Neut;

  lincat
    N = { s : Case => Set; g : Gender };
    A = { s : Case => Gender => Set };
    NP = { s : Case => Set };
    SUBJ = { s : Set };
    OBJ = { s : Set };
    S = { s : Set };

  lin
    Iuppiter : N  = { s = table { Nom => "Iuppiter";
    	       	      	  	  Acc => "Iouem" };
		      g = Masc } ;
    Ceres : N = { s = table { Nom => "Ceres";
                              Acc => "Cererem" };
	          g = Fem } ;
    magnus : A = { s = table { Nom => table { Masc => "magnus";
                                              Fem  => "magna";
					      Neut => "magnum" };
			       Acc => table { Masc => "magnum";
			                      Fem  => "magnam";
					      Neut => "magnum" }
			     } };
			     --    mkNP (n : N) : NP = { s = \\c: Case => n.s ! c };
    mkNP (n : N) : NP = { s = n.s };
    mkNP (n : N) (a : A) : NP = { s = \\c: Case => n.s ! c || a.s ! c ! n.g };
    mkS (subj : SUBJ) (obj : OBJ) : S = { s = subj.s || "uidit" || obj.s };
    mkSUBJ (np : NP) : SUBJ = { s = np.s ! Nom };
    mkOBJ (np : NP) : OBJ = { s = np.s ! Acc };

}
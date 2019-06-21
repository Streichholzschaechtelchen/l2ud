concrete VideoLat of Video = {

  param
    Case = Nom | Acc;
    Gender = Masc | Fem | Neut;

  lincat
    N = { s : Case => Str; g : Gender };
    A = { s : Case => Gender => Str };
    NP = { s : Case => Str };
    SUBJ = { s : Str };
    OBJ = { s : Str };
    S = { s : Str };

  lin
    Iuppiter = { s = table { Nom => "Iuppiter";
    	       	      	     Acc => "Iouem" };
		 g = Masc } ;
    Ceres = { s = table { Nom => "Ceres";
                          Acc => "Cererem" };
	      g = Fem } ;
    magnus = { s = table { Nom => table { Masc => "magnus";
                                          Fem  => "magna";
					  Neut => "magnum" };
			   Acc => table { Masc => "magnum";
			                  Fem  => "magnam";
					  Neut => "magnum" }
		 } };
    mkNP1 n = { s = \\c => n.s ! c };
    mkNP2 n a = { s = \\c => n.s ! c ++ a.s ! c ! n.g };
    mkS subj obj = { s = subj.s ++ "uidit" ++ obj.s };
    mkSUBJ np = { s = np.s ! Nom };
    mkOBJ np = { s = np.s ! Acc };

}

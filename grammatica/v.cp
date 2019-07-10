concrete V = {

  param
    Case = Nom | Acc | Gen | Dat | Abl | Voc;
    Number = Sg | Pl;
    Gender = Masc | Fem | Neut;
    VActForm = IndPrf1 | IndPrf2 | IndPrf3 | SbjPrf1 | SbjPrf2 | SbjPrf3
             | IndPlp1 | IndPlp2 | IndPlp3 | SbjPlp1 | SbjPlp2 | SbjPlp3
             | IndPrs1 | IndPrs2 | IndPrs3 | SbjPrs1 | SbjPrs2 | SbjPrs3
             | IndImp1 | IndImp2 | IndImp3 | SbjImp1 | SbjImp2 | SbjImp3
             | IndFtp1 | IndFtp2 | IndFtp3 | IndFtr1 | IndFtr2 | IndFtr3;
    VImpForm = ImpPrs2 | ImpFtr2 | ImpFtr3;
    VInfForm = PrfPass | Prs | PrsPass | Ftr;
    VPartForm = PPrs | PFtr | PPrfPass | PFtrPass;
    VPassForm = IndPrs1Pass | IndPrs2Pass | IndPrs3Pass | SbjPrs1Pass | SbjPrs2Pass | SbjPrs3Pass 
              | IndImp1Pass | IndImp2Pass | IndImp3Pass | SbjImp1Pass | SbjImp2Pass | SbjImp3Pass
              | IndFtr1Pass | IndFtr2Pass | IndFtr3Pass;
    VSupine = SAcc | SAbl;
    Degree = Pos | Comp | Sup;
    Adpos = Pre | Post | PolyPre | PolyPost;

  lincat
    S = { s : Set };
    N = { s : Number => Case => Set;
	  g : Gender };
    PrepN = { s : Number => Set;
	      c : Case;
	      g : Gender };
    Relat = { s : Gender => Number => Case => Set };
    PrepRelat = { s : Gender => Number => Set;
		  c : Case };
    NP = { s : Case => Set;
	   n : Number;
	   g : Gender };
    RelNP = { s : Gender => Number => Case => Set;
	      n : Number;
	      g : Gender };
    PrepNP = { s : Set;
	       c : Case;
	       n : Number;
	       g : Gender };
    VP = { s : Set };
    RelVP = { s : Gender => Number => Set };
    V = { act   : Number => VActForm                    => Set;
	  imp   : Number => VImpForm                    => Set;
	  inf   : VInfForm                              => Set;
	  part  : VPartForm => Gender => Number => Case => Set;
	  pass  : Number => VPassForm                   => Set;
	  sup   : VSupine                               => Set };--non-conform
    V2 = { act   : Number => VActForm                    => Set;
	   imp   : Number => VImpForm                    => Set;
	   inf   : VInfForm                              => Set;
	   part  : VPartForm => Gender => Number => Case => Set;
	   pass  : Number => VPassForm                   => Set;
	   sup   : VSupine                               => Set };
    V3 = { act   : Number => VActForm                    => Set;
	   imp   : Number => VImpForm                    => Set;
	   inf   : VInfForm                              => Set;
	   part  : VPartForm => Gender => Number => Case => Set;
	   pass  : Number => VPassForm                   => Set;
	   sup   : VSupine                               => Set };
    A = { s   : Degree => Gender => Number => Case => Set;
	  adv : Degree => Set };
    PrepA = { s : Degree => Gender => Number => Set;
              c : Case };
    AP = { s : Degree => Gender => Number => Case => Set };
    PrepAP = { s : Degree => Gender => Number => Set;
	       c : Case };
    Adv = { s : Degree => Set };
    Coord = { s : Set };
    Prep = { s : Set; a : Adpos; c : Case };

  lin
    mkS (head : VP) : S = { s = head.s };
    --lexicon
    lupus : N = { s = table { Sg => table { Nom => "lupus";
					    Acc => "lupum";
					    Gen => "lupi";
					    Dat => "lupo";
					    Abl => "lupo";
					    Voc => "lupe" };
			      Pl => table { Nom => "lupi";
					    Acc => "lupos";
					    Gen => "luporum";
					    Dat => "lupis";
					    Abl => "lupis";
					    Voc => "lupe" } };
		  g = Masc };

    uulpis : N = { s = table { Sg => table { Nom => "uulpis";
					     Acc => "uulpem";
					     Gen => "uulpis";
					     Dat => "uulpi";
					     Abl => "uulpe";
					     Voc => "uulpis" };
			       Pl => table { Nom => "uulpes";
					     Acc => "uulpes";
					     Gen => "uulpium";
					     Dat => "uulpibus";
					     Abl => "uulpibus";
					     Voc => "uulpes" } };
		   g = Fem };
    
    magnus : A = { s = table { Pos => table { Masc => table { Sg => table { Nom => "magnus";
									    Acc => "magnum";
									    Gen => "magni";
									    Dat => "magno";
									    Abl => "magno";
									    Voc => "magne" };
							      Pl => table { Nom => "magni";
									    Acc => "magnos";
									    Gen => "magnorum";
									    Dat => "magnis";
									    Abl => "magnis";
									    Voc => "magni" } };
					      Fem => table { Sg => table { Nom => "magna";
									   Acc => "magnam";
									   Gen => "magnae";
									   Dat => "magnae";
									   Abl => "magna";
									   Voc => "magna" };
							     Pl => table { Nom => "magnae";
									   Acc => "magnas";
									   Gen => "magnarum";
									   Dat => "magnis";
									   Abl => "magnis";
									   Voc => "magnae" } };
					      Neut => table { Sg => table { Nom => "magnum";
									    Acc => "magnum";
									    Gen => "magni";
									    Dat => "magno";
									    Abl => "magno";
									    Voc => "magnum" };
							      Pl => table { Nom => "magna";
									    Acc => "magna";
									    Gen => "magnorum";
									    Dat => "magnis";
									    Abl => "magnis";
									    Voc => "magna" } } };
			       Comp => table { Masc => table { Sg => table { Nom => "maior";
									     Acc => "maiorem";
									     Gen => "maioris";
									     Dat => "maiori";
									     Abl => "maiore";
									     Voc => "maior" };
							       Pl => table { Nom => "maiores";
									     Acc => "maiores";
									     Gen => "maiorum";
									     Dat => "maioribus";
									     Abl => "maioribus";
									     Voc => "maiores" } };
					       Fem => table { Sg => table { Nom => "maior";
									    Acc => "maiorem";
									    Gen => "maioris";
									    Dat => "maiori";
									    Abl => "maiore";
									    Voc => "maior" };
							      Pl => table { Nom => "maiores";
									    Acc => "maiores";
									    Gen => "maiorum";
									    Dat => "maioribus";
									    Abl => "maioribus";
									    Voc => "maiores" } };
					       Neut => table { Sg => table { Nom => "maius";
									     Acc => "maius";
									     Gen => "maioris";
									     Dat => "maiori";
									     Abl => "maiore";
									     Voc => "maius" };
							       Pl => table { Nom => "maiora";
									     Acc => "maiora";
									     Gen => "maiorum";
									     Dat => "maioribus";
									     Abl => "maioribus";
									     Voc => "maiora" } } };
			       Sup => table { Masc => table { Sg => table { Nom => "maximus";
									    Acc => "maximum";
									    Gen => "maximi";
									    Dat => "maximo";
									    Abl => "maximo";
									    Voc => "maxime" };
							      Pl => table { Nom => "maximi";
									    Acc => "maximi";
									    Gen => "maximorum";
									    Dat => "maximis";
									    Abl => "maximis";
									    Voc => "maximi" } };
					      Fem => table { Sg => table { Nom => "maxima";
									   Acc => "maximam";
									   Gen => "maximae";
									   Dat => "maximae";
									   Abl => "maxima";
									   Voc => "maxima" };
							     Pl => table { Nom => "maximae";
									   Acc => "maximas";
									   Gen => "maximarum";
									   Dat => "maximis";
									   Abl => "maximis";
									   Voc => "maximae" } };
					      Neut => table { Sg => table { Nom => "maximum";
									    Acc => "maximum";
									    Gen => "maximi";
									    Dat => "maximo";
									    Abl => "maximo";
									    Voc => "maximum" };
							      Pl => table { Nom => "maxima";
									    Acc => "maxima";
									    Gen => "maximorum";
									    Dat => "maximis";
									    Abl => "maximis";
									    Voc => "maxima" } } } };
		   adv = table { Pos => "magne";
				 Comp => "maius";
				 Sup => "maxime" } };

    uideo : V2 = { act = table { Sg => table { IndPrf1 => "uidi";
					       IndPrf2 => "uidisti";
					       IndPrf3 => "uidit";
					       SbjPrf1 => "uiderim";
					       SbjPrf2 => "uideris";
					       SbjPrf3 => "uiderit";
					       IndPlp1 => "uideram";
					       IndPlp2 => "uideras";
					       IndPlp3 => "uiderat";
					       SbjPlp1 => "uidissem";
					       SbjPlp2 => "uidisses";
					       SbjPlp3 => "uidisset";
					       IndPrs1 => "uideo";
					       IndPrs2 => "uides";
					       IndPrs3 => "uidet";
					       SbjPrs1 => "uideam";
					       SbjPrs2 => "uideas";
					       SbjPrs3 => "uideat";
					       IndImp1 => "uidebam";
					       IndImp2 => "uidebas";
					       IndImp3 => "uidebat";
					       SbjImp1 => "uiderem";
					       SbjImp2 => "uideres";
					       SbjImp3 => "uideret";
					       IndFtp1 => "uidero";
					       IndFtp2 => "uideris";
					       IndFtp3 => "uiderit";
					       IndFtr1 => "uidebo";
					       IndFtr2 => "uidebis";
					       IndFtr3 => "uidebit" };
				 Pl => table { IndPrf1 => "uidimus";
					       IndPrf2 => "uidistis";
					       IndPrf3 => "uiderunt";
					       SbjPrf1 => "uiderimus";
					       SbjPrf2 => "uideritis";
					       SbjPrf3 => "uiderint";
					       IndPlp1 => "uideramus";
					       IndPlp2 => "uideratis";
					       IndPlp3 => "uiderant";
					       SbjPlp1 => "uidissemus";
					       SbjPlp2 => "uidissetis";
					       SbjPlp3 => "uidissent";
					       IndPrs1 => "uidemus";
					       IndPrs2 => "uidetis";
					       IndPrs3 => "uident";
					       SbjPrs1 => "uideamus";
					       SbjPrs2 => "uideatis";
					       SbjPrs3 => "uideant";
					       IndImp1 => "uidebamus";
					       IndImp2 => "uidebatis";
					       IndImp3 => "uidebant";
					       SbjImp1 => "uideremus";
					       SbjImp2 => "uideretis";
					       SbjImp3 => "uiderent";
					       IndFtp1 => "uiderimus";
					       IndFtp2 => "uideritis";
					       IndFtp3 => "uiderint";
					       IndFtr1 => "uidebimus";
					       IndFtr2 => "uidebitis";
					       IndFtr3 => "uidebunt" } };
		   imp = table { Sg => table { ImpPrs2 => "uide";
					       ImpFtr2 => "uideto";
					       ImpFtr3 => "uideto" };
				 Pl => table { ImpPrs2 => "uidete";
					       ImpFtr2 => "uidetote";
					       ImpFtr3 => "uidento" } };
		   inf = table { PrfPass => "uidisse";
				 Prs     => "uidere";
				 PrsPass => "uidi";
				 Ftr     => [] }; --[]
		   part = table { PPrs => table { Masc => table { Sg => table { Nom => "uidens";
										Acc => "uidentem";
										Gen => "uidentis";
										Dat => "uidenti";
										Abl => "uidente";
										Voc => "uidens" };
								  Pl => table { Nom => "uidentes";
										Acc => "uidentes";
										Gen => "uidentium";
										Dat => "uidentibus";
										Abl => "uidentibus";
										Voc => "uidentes" } };
						  Fem => table { Sg => table { Nom => "uidens";
									       Acc => "uidentem";
									       Gen => "uidentis";
									       Dat => "uidenti";
									       Abl => "uidente";
									       Voc => "uidens" };
								 Pl => table { Nom => "uidentes";
									       Acc => "uidentes";
									       Gen => "uidentium";
									       Dat => "uidentibus";
									       Abl => "uidentibus";
									       Voc => "uidentes" } };
						  Neut => table { Sg => table { Nom => "uidens";
										Acc => "uidens";
										Gen => "uidentis";
										Dat => "uidenti";
										Abl => "uidente";
										Voc => "uidens" };
								  Pl => table { Nom => "uidentia";
										Acc => "uidentia";
										Gen => "uidentium";
										Dat => "uidentibus";
										Abl => "uidentibus";
										Voc => "uidentia" } } };
				  PFtr => table { Masc => table { Sg => table { Nom => "uisurus";
										Acc => "uisurum";
										Gen => "uisuri";
										Dat => "uisuro";
										Abl => "uisuro";
										Voc => "uisure" };
								  Pl => table { Nom => "uisuri";
										Acc => "uisuros";
										Gen => "uisurorum";
										Dat => "uisuris";
										Abl => "uisuris";
										Voc => "uisuri" } };
						  Fem => table { Sg => table { Nom => "uisura";
									       Acc => "uisuram";
									       Gen => "uisurae";
									       Dat => "uisurae";
									       Abl => "uisura";
									       Voc => "uisura" };
								 Pl => table { Nom => "uisurae";
									       Acc => "uisuras";
									       Gen => "uisurarum";
									       Dat => "uisuris";
									       Abl => "uisuris";
									       Voc => "uisurae" } };
						  Neut => table { Sg => table { Nom => "uisurum";
										Acc => "uisurum";
										Gen => "uisuri";
										Dat => "uisuro";
										Abl => "uisuro";
										Voc => "uisurum" };
								  Pl => table { Nom => "uisura";
										Acc => "uisura";
										Gen => "uisurorum";
										Dat => "uisuris";
										Abl => "uisuris";
										Voc => "uisura" } } };
				  PPrfPass => table { Masc => table { Sg => table { Nom => "uisus";
										    Acc => "uisum";
										    Gen => "uisi";
										    Dat => "uiso";
										    Abl => "uiso";
										    Voc => "uise" };
								      Pl => table { Nom => "uisi";
										    Acc => "uisos";
										    Gen => "uisorum";
										    Dat => "uisis";
										    Abl => "uisis";
										    Voc => "uisi" } };
						      Fem => table { Sg => table { Nom => "uisa";
										   Acc => "uisam";
										   Gen => "uisae";
										   Dat => "uisae";
										   Abl => "uisa";
										   Voc => "uisa" };
								     Pl => table { Nom => "uisae";
										   Acc => "uisas";
										   Gen => "uisarum";
										   Dat => "uisis";
										   Abl => "uisis";
										   Voc => "uisae" } };
						      Neut => table { Sg => table { Nom => "uisum";
										    Acc => "uisum";
										    Gen => "uisi";
										    Dat => "uiso";
										    Abl => "uiso";
										    Voc => "uise" };
								      Pl => table { Nom => "uisa";
										    Acc => "uisa";
										    Gen => "uisorum";
										    Dat => "uisis";
										    Abl => "uisis";
										    Voc => "uisa" } } };
				  PFtrPass => table { Masc => table { Sg => table { Nom => "uidendus";
										    Acc => "uidendum";
										    Gen => "uidendi";
										    Dat => "uidendo";
										    Abl => "uidendo";
										    Voc => "uidende" };
								      Pl => table { Nom => "uidendi";
										    Acc => "uidendos";
										    Gen => "uidendorum";
										    Dat => "uidendis";
										    Abl => "uidendis";
										    Voc => "uidendi" } };
						      Fem => table { Sg => table { Nom => "uidenda";
										   Acc => "uidendam";
										   Gen => "uidendae";
										   Dat => "uidendae";
										   Abl => "uidenda";
										   Voc => "uidenda" };
								     Pl => table { Nom => "uidendae";
										   Acc => "uidendas";
										   Gen => "uidendarum";
										   Dat => "uidendis";
										   Abl => "uidendis";
										   Voc => "uidendae" } };
						      Neut => table { Sg => table { Nom => "uidendum";
										    Acc => "uidendum";
										    Gen => "uidendi";
										    Dat => "uidendo";
										    Abl => "uidendo";
										    Voc => "uidendum" };
								      Pl => table { Nom => "uidenda";
										    Acc => "uidenda";
										    Gen => "uidendorum";
										    Dat => "uidendis";
										    Abl => "uidendis";
										    Voc => "uidenda" } } } };
		   pass = table { Sg => table { IndPrs1Pass => "uideor";
						IndPrs2Pass => "uideris";
						IndPrs3Pass => "uiditur";
						SbjPrs1Pass => "uidear";
						SbjPrs2Pass => "uidearis";
						SbjPrs3Pass => "uideatur";
						IndImp1Pass => "uidebar";
						IndImp2Pass => "uidebaris";
						IndImp3Pass => "uidebatur";
						SbjImp1Pass => "uiderer";
						SbjImp2Pass => "uidereris";
						SbjImp3Pass => "uideretur";
						IndFtr1Pass => "uidebor";
						IndFtr2Pass => "uideberis";
						IndFtr3Pass => "uidebitur" };
				  Pl => table { IndPrs1Pass => "uidemur";
						IndPrs2Pass => "uidemini";
						IndPrs3Pass => "uidentur";
						SbjPrs1Pass => "uideamur";
						SbjPrs2Pass => "uideamini";
						SbjPrs3Pass => "uideantur";
						IndImp1Pass => "uidebamur";
						IndImp2Pass => "uidebamini";
						IndImp3Pass => "uidebantur";
						SbjImp1Pass => "uideremur";
						SbjImp2Pass => "uideremini";
						SbjImp3Pass => "uiderentur";
						IndFtr1Pass => "uidebimur";
						IndFtr2Pass => "uidebimini";
						IndFtr3Pass => "uidebuntur" } };
		   sup = table { SAcc => "uisum";
				 SAbl => "uisu" } };

    cum : Prep = { s = "cum"; a = Pre; c = Abl };

    quis : Relat = { s = table { Masc => table { Sg => table { Nom => "qui";
							       Voc => "qui";
							       Acc => "quem";
							       Gen => "cuius";
							       Dat => "cui";
							       Abl => "quo" };
						 Pl => table { Nom => "qui";
							       Voc => "qui";
							       Acc => "quos";
							       Gen => "quorum";
							       Dat => "quibus";
							       Abl => "quibus" } };
				 Fem => table { Sg => table { Nom => "quae";
							      Voc => "quae";
							      Acc => "quam";
							      Gen => "cuius";
							      Dat => "cui";
							      Abl => "qua" };
						Pl => table { Nom => "quae";
							      Voc => "quae";
							      Acc => "quas";
							      Gen => "quarum";
							      Dat => "quibus";
							      Abl => "quibus" } };
				 Neut => table { Sg => table { Nom => "quod";
							       Voc => "quod";
							       Acc => "quod";
							       Gen => "cuius";
							       Dat => "cui";
							       Abl => "quo" };
						 Pl => table { Nom => "quae";
							       Voc => "quae";
							       Acc => "quas";
							       Gen => "quarum";
							       Dat => "quibus";
							       Abl => "quibus" } } } };
					   
    -- Adv
    
    mkAdv (head : A) : Adv =
      { s = head.adv };
      
    -- V and VP p. 13

    mkVP (head : V) : VP =
      { s = for n : Number do for f : VActForm do head.act ! n ! f };
      
    mkVP (nsubj : NP) (head : V) : VP =
      { s = nsubj.s ! Nom || (for f : VActForm do head.act ! nsubj.n ! f) };

    mkRelVP (nsubj : RelNP) (head : V) : RelVP =
      { s = \\g : Gender => \\n : Number => nsubj.s ! g ! n ! Nom || (for f : VActForm do head.act ! nsubj.n ! f) };

    mkRelVP (nsubj : Relat) (head : V) : RelVP =
      { s = \\g : Gender => \\n : Number => nsubj.s ! g ! n ! Nom ++ (for f : VActForm do head.act ! n ! f) };

    mkVP (head : V2) (obj : NP) : VP =
      { s = (for n : Number do for f : VActForm do head.act ! n ! f) || obj.s ! Acc };

    mkRelVP (head : V2) (obj : RelNP) : RelVP =
      { s = \\g : Gender => \\n : Number => (for n2 : Number do for f : VActForm do head.act ! n2 ! f) || obj.s ! g ! n ! Acc };

    mkRelVP (head : V2) (obj : Relat) : RelVP =
      { s = \\g : Gender => \\n : Number => obj.s ! g ! n ! Acc ++ (for f : VActForm do head.act ! n ! f) };
	
    mkVP (nsubj : NP) (head : V2) (obj : NP) : VP =
      { s = nsubj.s ! Nom || (for f : VActForm do head.act ! nsubj.n ! f) || obj.s ! Acc };

    mkRelVP (nsubj : RelNP) (head : V2) (obj : NP) : RelVP =
      { s = \\g : Gender => \\n : Number => nsubj.s ! g ! n ! Nom || (for f : VActForm do head.act ! nsubj.n ! f) || obj.s ! Acc };

    mkRelVP (nsubj : Relat) (head : V2) (obj : NP) : RelVP =
      { s = \\g : Gender => \\n : Number => (nsubj.s ! g ! n ! Nom ++ (for f : VActForm do head.act ! n ! f)) || obj.s ! Acc };

    mkRelVP (nsubj : NP) (head : V2) (obj : RelNP) : RelVP =
      { s = \\g : Gender => \\n : Number => nsubj.s ! Nom || (for f : VActForm do head.act ! nsubj.n ! f) || obj.s ! g ! n ! Acc };

    mkRelVP (nsubj : NP) (head : V2) (obj : Relat) : RelVP =
      { s = \\g : Gender => \\n : Number => nsubj.s ! Nom || (obj.s ! g ! n ! Acc ++ (for f : VActForm do head.act ! nsubj.n ! f)) };

    mkVP (head : V3) (obj : NP) (iobj : NP) : VP =
      { s = (for n : Number do for f : VActForm do head.act ! n ! f) || obj.s ! Acc || iobj.s ! Dat };

    mkRelVP (head : V3) (obj : RelNP) (iobj : NP) : RelVP =
      { s = \\g : Gender => \\n : Number => (for n2 : Number do for f : VActForm do head.act ! n2 ! f) || obj.s ! g ! n ! Acc || iobj.s ! Dat };

    mkRelVP (head : V3) (obj : Relat) (iobj : NP) : RelVP =
      { s = \\g : Gender => \\n : Number => (obj.s ! g ! n ! Acc ++ (for n2 : Number do for f : VActForm do head.act ! n2 ! f)) || iobj.s ! Dat };

    mkRelVP (head : V3) (obj : NP) (iobj : RelNP) : RelVP =
      { s = \\g : Gender => \\n : Number => (for n2 : Number do for f : VActForm do head.act ! n2 ! f) || obj.s ! Acc || iobj.s ! g ! n ! Dat };

    mkRelVP (head : V3) (obj : NP) (iobj : Relat) : RelVP =
      { s = \\g : Gender => \\n : Number => (iobj.s ! g ! n ! Dat ++ (for n2 : Number do for f : VActForm do head.act ! n2 ! f)) || obj.s ! Acc };
	
    mkVP (nsubj : NP) (head : V3) (obj : NP) (iobj : NP) : VP =
      { s = nsubj.s ! Nom || (for f : VActForm do head.act ! nsubj.n ! f) || obj.s ! Acc || iobj.s ! Dat };

    mkRelVP (nsubj : RelNP) (head : V3) (obj : NP) (iobj : NP) : RelVP =
      { s = \\g : Gender => \\n : Number => nsubj.s ! g ! n ! Nom || (for f : VActForm do head.act ! nsubj.n ! f) || obj.s ! Acc || iobj.s ! Dat };

    mkRelVP (nsubj : Relat) (head : V3) (obj : NP) (iobj : NP) : RelVP =
      { s = \\g : Gender => \\n : Number => (nsubj.s ! g ! n ! Nom ++ (for f : VActForm do head.act ! n ! f)) || obj.s ! Acc || iobj.s ! Dat };

    mkRelVP (nsubj : NP) (head : V3) (obj : RelNP) (iobj : NP) : RelVP =
      { s = \\g : Gender => \\n : Number => nsubj.s ! Nom || (for f : VActForm do head.act ! nsubj.n ! f) || obj.s ! g ! n ! Acc || iobj.s ! Dat };

    mkRelVP (nsubj : NP) (head : V3) (obj : Relat) (iobj : NP) : RelVP =
      { s = \\g : Gender => \\n : Number => nsubj.s ! Nom || (obj.s ! g ! n ! Acc ++ (for f : VActForm do head.act ! nsubj.n ! f)) || iobj.s ! Dat };

    mkRelVP (nsubj : NP) (head : V3) (obj : NP) (iobj : RelNP) : RelVP =
      { s = \\g : Gender => \\n : Number => nsubj.s ! Nom || (for f : VActForm do head.act ! nsubj.n ! f) || obj.s ! Acc || iobj.s ! g ! n ! Dat };

    mkRelVP (nsubj : NP) (head : V3) (obj : NP) (iobj : RelNP) : RelVP =
      { s = \\g : Gender => \\n : Number => nsubj.s ! Nom || (for f : VActForm do head.act ! n ! f) || obj.s ! Acc || iobj.s ! g ! n ! Dat };

    mkVP (head : VP) (advmod : Adv) : VP =
      for d : Degree do { s = head.s || advmod.s ! d };

    mkRelVP (head : RelVP) (advmod : Adv) : RelVP =
      for d : Degree do { s = \\g : Gender => \\n : Number => head.s ! g ! n || advmod.s ! d };
      
    downcastV2 (head : V2) : V =
      { act = head.act; imp = head.imp; inf = head.inf; part = head.part; pass = head.pass; sup = head.sup };

    downcastV3 (head : V3) : V2 =
      { act = head.act; imp = head.imp; inf = head.inf; part = head.part; pass = head.pass; sup = head.sup };

    downcastV3 (head : V3) : V =
      { act = head.act; imp = head.imp; inf = head.inf; part = head.part; pass = head.pass; sup = head.sup };

    -- oblique

    attachOBL (head : VP) (obl : PrepNP) : VP =
      { s = head.s || obl.s };

    attachOBL (head : VP) (obl : PrepRelat) : RelVP =
      { s = \\g : Gender => \\n : Number => head.s || obl.s ! g ! n };

    -- N, A and NP p. 15

    mkNP (head : N) : NP =
      for n : Number do { s = head.s ! n;
			  n = n;
			  g = head.g };

    mkNP (head : NP) (amod : AP) : NP =
      for d : Degree do { s = \\c : Case => head.s ! c || amod.s ! d ! head.g ! head.n ! c;
			  n = head.n;
			  g = head.g };

    mkRelNP (head : RelNP) (amod : AP) : RelNP =
      for d : Degree do { s = \\g : Gender => \\n : Number => \\c : Case =>
			    head.s ! g ! n ! c || amod.s ! d ! head.g ! head.n ! c;
			  n = head.n;
			  g = head.g };

    -- no nmods of As so far
    
    mkAP (head : A) : AP =
      { s = head.s };
    
    mkAP (head : A) (advmod : Adv) : AP =
      for dAdv : Degree do { s = \\d : Degree => \\g : Gender => \\n : Number => \\c : Case =>
			       head.s ! d ! g ! n ! c || advmod.s ! dAdv };

    -- coordination

    

    -- ne quidem

    -- prepositions (no attachement before adverbs so far)

    mkPrepN (head : N) (case : Prep) : PrepN =
      { s = \\n : Number => (table { Pre      => case.s ++ head.s ! n ! case.c;
				     Post     => head.s ! n ! case.c ++ case.s;
				     PolyPre  => case.s || head.s ! n ! case.c;
				     PolyPost => case.s || head.s ! n ! case.c }) ! case.a;
	c = case.c;
	g = head.g };

    mkPrepRelat (head : Relat) (case : Prep) : PrepRelat =
      { s = \\g : Gender => \\n : Number => (table { Pre      => case.s ++ head.s ! g ! n ! case.c;
						     Post     => head.s ! g ! n ! case.c ++ case.s;
						     PolyPre  => case.s || head.s ! g ! n ! case.c;
						     PolyPost => case.s || head.s ! g ! n ! case.c }) ! case.a;
	c = case.c };
    
    mkPrepNP (head : PrepN) : PrepNP =
      for n : Number do { s = head.s ! n;
			  c = head.c;
			  n = n;
			  g = head.g };

    mkPrepNP (head : PrepNP) (amod : AP) : PrepNP =
      for d : Degree do { s = head.s || amod.s ! d ! head.g ! head.n ! head.c;
			  c = head.c;
			  n = head.n;
			  g = head.g };

    -- attachment of PrepNP to PrepNP?
    -- attachment of amod to PrepRelat?
    -- relatives in amods?

    mkPrepNP (head : N) (amod : PrepAP) : PrepNP =
      for d : Degree do for n : Number do { s = head.s ! n ! amod.c || amod.s ! d ! head.g ! n;
					    c = amod.c;
					    n = n;
					    g = head.g };    

    mkPrepAP (head : PrepA) : PrepAP =
      { s = head.s; c = head.c };

    mkPrepAP (head : PrepA) (advmod : Adv) : PrepAP =
      for dAdv : Degree do { s = \\d : Degree => \\g : Gender => \\n : Number =>
			       head.s ! d ! g ! n || advmod.s ! dAdv;
			     c = head.c };

    mkPrepA (head : A) (case : Prep) : PrepA =
      { s = \\d : Degree => \\g : Gender => \\n : Number
	  => (table { Pre      => case.s ++ head.s ! d ! g ! n ! case.c;
		      Post     => head.s ! d ! g ! n ! case.c ++ case.s;
		      PolyPre  => case.s || head.s ! d ! g ! n ! case.c;
		      PolyPost => case.s || head.s ! d ! g ! n ! case.c }) ! case.a;
	c = case.c };
    
    attachPP (head : NP) (nmod : PrepNP) : NP =
      { s = \\c : Case => head.s ! c || nmod.s;
	n = head.n;
	g = head.g };

    attachPPtoRelNP (head : RelNP) (nmod : PrepNP) : RelNP =
      { s = \\g : Gender => \\n : Number => \\c : Case => head.s ! g ! n ! c || nmod.s;
	n = head.n;
	g = head.g };
            
    -- genitive attachment

    genNP (head : NP) (nmod : NP) : NP =
      { s = \\c : Case => head.s ! c || nmod.s ! Gen;
	n = head.n;
	g = head.g };

    genRelNP (head : RelNP) (nmod : NP) : RelNP =
      { s = \\g : Gender => \\n : Number => \\c : Case => head.s ! g ! n ! c || nmod.s ! Gen;
	n = head.n;
	g = head.g };

    genRelNP (head : NP) (nmod : Relat) : RelNP =
      { s = \\g : Gender => \\n : Number => \\c : Case => head.s ! c || nmod.s ! g ! n ! Gen;
	n = head.n;
	g = head.g };
    
    -- relative clause attachment
    
    relatNP (head : NP) (acl : RelVP) : NP =
      { s = \\c : Case => head.s ! c || `(acl.s ! head.g ! head.n);
	n = head.n;
	g = head.g };

    relatPP (head : PrepNP) (acl : RelVP) : PrepNP =
      { s = head.s || `(acl.s ! head.g ! head.n);
	c = head.c;
	n = head.n;
	g = head.g };

}

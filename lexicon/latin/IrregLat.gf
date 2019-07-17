--# -path=.:prelude:../abstract:../common

concrete IrregLat of IrregLatAbs = CatLat ** open Prelude, ParadigmsLat, ResLat in {
--
--flags optimize=values ;
--

  lin
    be_V = esseAux ;

    -- Bayer-Lindauer 93 2.2
    can_V = 
      posseAux ;
	

    -- Bayer-Lindauer 94
    bring_V = 
      ferreAux ;

    -- Bayer-Lindauer 95
    want_V = 
      let
	pres_stem = "vel" ;
	pres_ind_base = "vol" ;
	pres_conj_base = "veli" ;
	impf_ind_base = "voleba" ;
	impf_conj_base = "volle" ;
	fut_I_base = "vole" ;
	imp_base = "" ;
	perf_stem = "volu" ;
	perf_ind_base = "volu" ;
	perf_conj_base = "volueri" ;
	pqperf_ind_base = "voluera" ;
	pqperf_conj_base = "voluisse" ;
	fut_II_base = "volueri" ;
	part_stem = "volet" ;
	verb = mkVerb "velle" pres_stem pres_ind_base pres_conj_base impf_ind_base impf_conj_base fut_I_base
    	  imp_base perf_stem perf_ind_base perf_conj_base pqperf_ind_base pqperf_conj_base fut_II_base part_stem ;
      in
      {
	act =
	  table {
	    Sg => table {
	      IndPrs1 => "volo";
	      IndPrs2 => "vis";
	      IndPrs3 => "vult";
	      a => verb.act ! Sg ! a };
	    Pl => table {
	      IndPrs1 => "volumus";
	      IndPrs2 => "vultis";
	      IndPrs3 => "volunt";
	      a => verb.act ! Pl ! a }
	  } ;
	  pass =
	    \\_,_ => skip ;
	  ger = 
	    \\_ => skip ;
	  geriv =
	    \\_ => skip ;
	  imp = 
	    \\_,_ => skip ;
	  inf = 
	    verb.inf ;
	  part = table {
	    VActFut =>
	      \\_,_,_ => skip ;
	    VActPres =>
	      verb.part ! VActPres ;
	    VPassPerf =>
	      \\_,_,_ => skip
	    } ; 
	  sup =
	    \\_ => skip ;
      } ;

    -- Bayer-Lindauer 96 1
    go_V = 
      let
	pres_stem = "i" ;
	pres_ind_base = "i" ;
	pres_conj_base = "ea" ;
	impf_ind_base = "iba" ;
	impf_conj_base = "ire" ;
	fut_I_base = "ibi" ;
	imp_base = "i" ;
	perf_stem = "i" ;
	perf_ind_base = "i" ;
	perf_conj_base = "ieri" ;
	pqperf_ind_base = "iera" ;
	pqperf_conj_base = "isse" ;
	fut_II_base = "ieri" ;
	part_stem = "it" ;
	verb = mkVerb "ire" pres_stem pres_ind_base pres_conj_base impf_ind_base impf_conj_base fut_I_base
    	  imp_base perf_stem perf_ind_base perf_conj_base pqperf_ind_base pqperf_conj_base fut_II_base part_stem ;
      in
      {
	act =
	  table {
	    Sg => table {
	      IndPrs1 => "eo";
	      IndPrs2 => "is";
	      IndPrs3 => "it";
	      IndPrf2 => "isti";
	      a => verb.act ! Sg ! a };
	    Pl => table {
	      IndPrs1 => "imus";
	      IndPrs2 => "itis";
	      IndPrs3 => "eunt";
	      IndPrf2 => "istis";
	      a => verb.act ! Pl ! a }
	  } ;
	pass = verb.pass ;
	ger = 
	  table VGerund [ "eundum" ; "eundi" ; "eundo" ; "eundo" ] ;
	geriv =
	  verb.geriv ;
	imp =
	  table {
	    Sg => \\form => verb.imp ! Sg ! form;
	    Pl => table {
	      ImpFtr3 => "eunto" ;
	      a       => verb.imp ! Pl ! a }
	  } ;
	inf =
	  table {
	    PrfPass => "isse";
	    a => verb.inf ! a
	  };
	part = table {
	  VActFut => 
	    verb.part ! VActFut ;
	  VActPres => 
	    table {
	      Fem | Masc => \\n,c =>
		( mkNoun ( "iens" ) ( "euntem" ) ( "euntis" ) 
		    ( "eunti" ) ( "eunte" ) ( "iens" ) 
		    ( "euntes" ) ( "euntes" ) ( "euntium" ) 
		    ( "euntibus" ) 
 		    Masc ).s ! n ! c ;
	      Neutr => \\n,c =>
		( mkNoun ( "iens" ) ( "iens" ) ( "euntis" ) 
		    ( "eunti" ) ( "eunte" ) ( "iens" ) 
		    ( "euntia" ) ( "euntia" ) ( "euntium" ) 
		    ( "euntibus" ) 
 		    Masc ).s ! n ! c
	    } ;
	  VPassPerf => 
	    \\_,_,_ => skip -- no such bparticiple
	  } ;
	sup = 
	  \\_ => skip -- really no such form?
      } ;

    -- Bayer-Lindauer 97
    become_V = 
      let
	pres_stem = "fi" ;
	pres_ind_base = "fi" ;
	pres_conj_base = "fia" ;
	impf_ind_base = "fieba" ;
	impf_conj_base = "fiere" ;
	fut_I_base = "fie" ;
	imp_base = "fi" ;
	perf_stem = "" ;
	perf_ind_base = "" ;
	perf_conj_base = "" ;
	pqperf_ind_base = "" ;
	pqperf_conj_base = "" ;
	fut_II_base = "" ;
	part_stem = "fact" ;

	verb = 
	  mkVerb "fieri" pres_stem pres_ind_base pres_conj_base impf_ind_base impf_conj_base fut_I_base imp_base
	  perf_stem perf_ind_base perf_conj_base pqperf_ind_base pqperf_conj_base fut_II_base part_stem ;
      in
      {
	act = 
	  table {
	    Sg => table {
	      IndPrs1 => "fio";
	      IndPrf1 => skip;
	      IndPrf2 => skip;
	      IndPrf3 => skip;
	      IndPlp1 => skip;
	      IndPlp2 => skip;
	      IndPlp3 => skip;
	      IndFtp1 => skip;
	      IndFtp2 => skip;
	      IndFtp3 => skip;
	      SbjPrf1 => skip;
	      SbjPrf2 => skip;
	      SbjPrf3 => skip;
	      SbjPlp1 => skip;
	      SbjPlp2 => skip;
	      SbjPlp3 => skip;
	      a => verb.act ! Sg ! a };
	    Pl => table {
	      IndPrf1 => skip;
	      IndPrf2 => skip;
	      IndPrf3 => skip;
	      IndPlp1 => skip;
	      IndPlp2 => skip;
	      IndPlp3 => skip;
	      IndFtp1 => skip;
	      IndFtp2 => skip;
	      IndFtp3 => skip;
	      SbjPrf1 => skip;
	      SbjPrf2 => skip;
	      SbjPrf3 => skip;
	      SbjPlp1 => skip;
	      SbjPlp2 => skip;
	      SbjPlp3 => skip;
	      a => verb.act ! Sg ! a }
	  } ;
	pass =
	  \\_,_ => skip ; -- no passive forms
	ger =
	  \\_ => skip ; -- no gerund form
	geriv = 
	  \\_ => skip ; -- no gerundive form
	imp = 
	  verb.imp ;
	inf = verb.inf;
	part = table {
	  VActFut =>
	    \\_,_,_ => skip ; -- no such participle
	  VActPres => 
	    \\_,_,_ => skip ; -- no such participle
	  VPassPerf =>
	    verb.part ! VPassPerf
	  } ;
	sup = 
	  \\_ => skip -- no supin
      } ;

    -- Source ?
    rain_V =
      {
	act = 
	  table {
	    Sg => table {
	      IndPrs3 => "pluit";
	      IndImp3 => "pluebat";
	      IndFtr3 => "pluet";
	      IndPrf3 => "pluvit";
	      IndPlp3 => "pluverat";
	      IndFtp3 => "pluverit";
	      SbjPrs3 => "pluat";
	      SbjImp3 => "plueret";
	      SbjPrf3 => "pluverit";
	      SbjPlp3 => "pluvisset";
	      _ => skip }; -- no such forms
	    Pl => table {
	      IndPrs3 => "pluunt" ;
	      IndImp3 => "pluebant" ;
	      IndFtr3 => "pluent" ;
	      IndPrf3 => "pluverunt" ;
	      IndPlp3 => "pluverant" ;
	      IndFtp3 => "pluverint" ;
	      SbjPrs3 => "pluant" ;
	      SbjImp3 => "pluerent" ;
	      SbjPrf3 => "pluverint" ;
	      SbjPlp3 => "pluvissent" ;
	      _ => skip } -- no such forms
	  } ;
	pass = 
	  \\_,_ => skip ; -- no passive forms
	inf = table {
	  PrfPass => "pluvisse";
	  Prs     => "pluere";
	  _       => skip
	  } ;
	imp =
	  table {
	    Sg => table {
	      ImpFtr3 => "pluito";
	      _ => skip };
	    Pl => \\form => skip
	  } ;
	ger = 
	  \\_ => skip ; -- no gerund forms
	geriv = 
	  \\_ => skip ; -- no gerundive forms
	sup = 
	  \\_ => skip ; -- no supin forms
	part = table { 
	  VActPres =>
	    \\_,_,_ => "pluens" ;
	  VActFut =>
	    \\_,_,_ => skip ; -- no such participle
	  VPassPerf =>
	    \\_,_,_ => skip -- no such participle
	  }
      } ;

    -- Bayer-Lindauer 98
    hate_V = 
      let  
	pres_stem = "" ;
	pres_ind_base = "" ;
	pres_conj_base = "" ;
	impf_ind_base = "" ;
	impf_conj_base = "" ;
	fut_I_base = "" ;
	imp_base = "" ;
	perf_stem = "od" ;
	perf_ind_base = "od" ;
	perf_conj_base = "oderi" ;
	pqperf_ind_base = "odera" ;
	pqperf_conj_base = "odissem" ;
	fut_II_base = "oderi" ;
	part_stem = "os" ;
	verb = 
	  mkVerb "odisse" pres_stem pres_ind_base pres_conj_base impf_ind_base impf_conj_base fut_I_base imp_base
	  perf_stem perf_ind_base perf_conj_base pqperf_ind_base pqperf_conj_base fut_II_base part_stem ;
      in {
	act = \\n => table {
	  IndPrs1 => verb.act ! n ! IndPrf1;
	  IndPrs2 => verb.act ! n ! IndPrf2;
	  IndPrs3 => verb.act ! n ! IndPrf3;
	  IndImp1 => verb.act ! n ! IndPlp1;
	  IndImp2 => verb.act ! n ! IndPlp2;
	  IndImp3 => verb.act ! n ! IndPlp3;
	  IndFtr1 => verb.act ! n ! IndFtp1;
	  IndFtr2 => verb.act ! n ! IndFtp2;
	  IndFtr3 => verb.act ! n ! IndFtp3;
	  SbjPrs1 => verb.act ! n ! SbjPrf1;
	  SbjPrs2 => verb.act ! n ! SbjPrf2;
	  SbjPrs3 => verb.act ! n ! SbjPrf3;
	  SbjImp1 => verb.act ! n ! SbjPlp1;
	  SbjImp2 => verb.act ! n ! SbjPlp2;
	  SbjImp3 => verb.act ! n ! SbjPlp3;
	  _ => skip -- no such verb forms
	  } ;
	pass = \\_,_ => skip ; -- no passive forms 
	ger = \\_ => skip ; -- no gerund forms
	geriv = \\_ => skip ; -- no gerundive forms
	imp = \\_,_ => skip ; -- no imperative form
	inf = table {
	  Prs => verb.inf ! Prs;
	  Ftr => verb.inf ! Ftr;
	  _   => skip
	  } ;
	part = table {
	  VActFut => 
	    verb.part ! VActFut ;
	  VActPres => 
	    \\_,_,_ => skip ; -- no such participle form
	  VPassPerf => 
	    \\_,_,_ => skip -- no such participle form
	  } ;
	sup = \\_ => skip ; -- no such supine form
      } ;

    not8want_V =
      let
	pres_stem = "nol" ;
	pres_ind_base = "nol" ;
	pres_conj_base = "noli" ;
	impf_ind_base = "noleba" ;
	impf_conj_base = "nolle" ;
	fut_I_base = "nole" ;
	imp_base = "nol" ;
	perf_stem = "nolu" ;
	perf_ind_base = "nolu" ;
	perf_conj_base = "nolueri" ;
	pqperf_ind_base = "noluera" ;
	pqperf_conj_base = "noluisse" ;
	fut_II_base = "nolueri" ;
	part_stem = "nolet" ;
	verb = mkVerb "nolle" pres_stem pres_ind_base pres_conj_base impf_ind_base impf_conj_base fut_I_base
    	  imp_base perf_stem perf_ind_base perf_conj_base pqperf_ind_base pqperf_conj_base fut_II_base part_stem ;
      in
      {
	act =
	  table {
	    Sg => table {
	      IndPrs1 => "nolo";
	      IndPrs2 => "non vis";
	      IndPrs3 => "non vult";
	      a => verb.act ! Sg ! a };
	    Pl => table {
	      IndPrs1 => "nolumus";
	      IndPrs2 => "non vultis";
	      IndPrs3 => "nolunt";
	      a => verb.act ! Pl ! a }
	  } ;
	  pass =
	    \\_,_ => skip ;
	  ger = 
	    \\_ => skip ;
	  geriv =
	    \\_ => skip ;
	  imp = table {
	    Sg => table {
	      ImpPrs2 => "noli";
	      a       => verb.imp ! Sg ! a };
	    Pl => \\a => verb.imp ! Pl ! a
	    } ;
	  inf = table {
	    PrfPass => "noluisse";
	    Prs     => "nolle";
	    _       => skip
	    } ;
	  part = table {
	    VActFut =>
	      \\_,_,_ => skip ;
	    VActPres =>
	      verb.part ! VActPres ;
	    VPassPerf =>
	      \\_,_,_ => skip
	    } ; 
	  sup =
	    \\_ => skip ;
      } ;

    make_V = 
      let
	pres_stem = "faci" ;
	pres_ind_base = "faci" ;
	pres_conj_base = "facia" ;
	impf_ind_base = "facieba" ;
	impf_conj_base = "facere" ;
	fut_I_base = "facie" ;
	imp_base = "faci" ;
	perf_stem = "fec" ;
	perf_ind_base = "fec" ;
	perf_conj_base = "feceri" ;
	pqperf_ind_base = "fecera" ;
	pqperf_conj_base = "fecisse" ;
	fut_II_base = "feceri" ;
	part_stem = "fact" ;
	verb = mkVerb "facere" pres_stem pres_ind_base pres_conj_base impf_ind_base impf_conj_base fut_I_base
    	  imp_base perf_stem perf_ind_base perf_conj_base pqperf_ind_base pqperf_conj_base fut_II_base part_stem ;
      in
      {
      	act = verb.act ;
      	pass =
      	  table {
	    Sg => table {
	      IndPrs1Pass => "fio";
	      IndPrs2Pass => "fi" + actPresEnding Sg P2 ;
	      IndPrs3Pass => "fi" + actPresEnding Sg P3 ;
	      SbjPrs1Pass => "fia" + actPresEnding Sg P1;
	      SbjPrs2Pass => "fia" + actPresEnding Sg P2;
	      SbjPrs3Pass => "fia" + actPresEnding Sg P3;
	      IndImp1Pass => "fieba" + actPresEnding Sg P1;
	      IndImp2Pass => "fieba" + actPresEnding Sg P2;
	      IndImp3Pass => "fieba" + actPresEnding Sg P3;
	      SbjImp1Pass => "fiere" + actPresEnding Sg P1 ;
	      SbjImp2Pass => "fiere" + actPresEnding Sg P2 ;
	      SbjImp3Pass => "fiere" + actPresEnding Sg P3 ;
	      IndFtr1Pass => "fiam";
	      IndFtr2Pass => "fie" + actPresEnding Sg P2;
	      IndFtr3Pass => "fie" + actPresEnding Sg P3 };
	    Pl => table {
	      IndPrs1Pass => "fi" + actPresEnding Pl P1 ;
	      IndPrs2Pass => "fi" + actPresEnding Pl P2 ;
	      InfPrs3Pass => "fiunt";
	      SbjPrs1Pass => "fia" + actPresEnding Pl P1;
	      SbjPrs2Pass => "fia" + actPresEnding Pl P2;
	      SbjPrs3Pass => "fia" + actPresEnding Pl P3;
	      IndImp1Pass => "fieba" + actPresEnding Pl P1;
	      IndImp2Pass => "fieba" + actPresEnding Pl P2;
	      IndImp3Pass => "fieba" + actPresEnding Pl P3;
	      SbjImp1Pass => "fiere" + actPresEnding Pl P1 ;
	      SbjImp2Pass => "fiere" + actPresEnding Pl P2 ;
	      SbjImp3Pass => "fiere" + actPresEnding Pl P3 ;
	      IndFtr1Pass => "fie" + actPresEnding Pl P1;
	      IndFtr2Pass => "fie" + actPresEnding Pl P2;
	      IndFtr3Pass => "fie" + actPresEnding Pl P3 }
	  } ;
      	ger = 
	  verb.ger ;
      	geriv =
	  verb.geriv ;
      	imp =
	  table {
	    Sg => table {
	      ImpPrs2 => "fac";
	      a       => verb.imp ! Sg ! a };
	    Pl => \\a => verb.imp ! Pl ! a 
	  } ;
      	inf =
	  table {
	    PrsPass => "fieri";
	    PrfPass => skip;
	    a       => verb.inf ! a
	  };
      	part = 
      	  verb.part ;
      	sup =
      	  table {
	    VSupAcc => "factum" ;
	    VSupAbl => "factu"
	  } ;
      } ;

    give_V = 
      let
    	pres_stem = "da" ;
    	pres_ind_base = "da" ;
    	pres_conj_base = "de" ;
    	impf_ind_base = "daba" ;
    	impf_conj_base = "dare" ;
    	fut_I_base = "dabi" ;
    	imp_base = "da" ;
    	perf_stem = "ded" ;
    	perf_ind_base = "ded" ;
    	perf_conj_base = "dederi" ;
    	pqperf_ind_base = "dedera" ;
    	pqperf_conj_base = "dedisse" ;
    	fut_II_base = "dederi" ;
    	part_stem = "dat" ;
    	verb = mkVerb "dare" pres_stem pres_ind_base pres_conj_base impf_ind_base impf_conj_base fut_I_base
    	  imp_base perf_stem perf_ind_base perf_conj_base pqperf_ind_base pqperf_conj_base fut_II_base part_stem ;
      in
      {
      	act = verb.act ;
      	pass =
    	  verb.pass ;
      	ger = 
    	  verb.ger ;
      	geriv =
    	  verb.geriv ;
      	imp =
    	  verb.imp ;
      	inf = verb.inf ;
      	part = 
      	  verb.part ;
      	sup =
    	  verb.sup ;
      } ;

}

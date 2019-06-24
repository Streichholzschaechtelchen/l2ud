concrete Gallia = {
  lincat
    S = { s: Set };
    NSUBJ = { s: Set };
    COP = { s: Set };
    OBL = { s: Set };
    RC = { s: Set };

  lin
    mkS (nsubj: NSUBJ) (cop: COP) : S = { s = nsubj.s || "est" || cop.s };
    
    mkNSUBJ : NSUBJ = { s = "Gallia" || "omnis" };
    
    mkCOP (obl: OBL) : COP = { s = "diuisa" || obl.s };
    
    mkOBL (rc: RC) : OBL = { s = ((`("in" ++ "partes") || "tres") \/ (`("in" ++ "tres") || "partes")) || rc.s };

    mkRC : RC = { s = `("quarum" ++ ("unam" || "incolunt" || "Belgae")) };
    
}

--./main.native -g test.cp -t "Gallia est omnis diuisa in tres partes quarum unam incolunt Belgae" -v
concrete A = {

  lincat
    S = { s : Set };

  lin
    oneA : S = { s = "a" };
    moreAs (s1 : S) (s2 : S) : S = { s = s1.s \/ s1.s };
   
}

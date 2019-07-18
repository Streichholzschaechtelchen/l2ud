let print_done () =
  print_string "\027[32;1mDone\027[0m.\n";
  flush_all ()

let print s =
  print_string s; flush_all ()

let printi i n s =
  print_string "\027[34m[";
  print_int i;
  print_string "/";
  print_int n;
  print_string "]\027[0m ";
  print_string s;
  flush_all ()

let print_after_done s =
  print_done ();
  print s

let printi_after_done i n s =
  print_done ();
  printi i n s


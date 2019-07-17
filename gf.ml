(* Uses both the Unix and Str libraries *)

module Com = struct

  type t = { outc: out_channel;
             inc : in_channel;
             pid : int }

  let create run =
    let pr1, pr2 = Unix.pipe () in
    let pw1, pw2 = Unix.pipe () in
    let pid  = Unix.create_process "gf" [| "-crun"; "-retain"; "-run"; run |] pr1 pw2 Unix.stderr in
    let outc = Unix.out_channel_of_descr pr2 in
    let inc  = Unix.in_channel_of_descr pw1 in
    { outc; inc; pid }

  let put ?(term="\n") t str =
    output_string t.outc (str ^ term);
    flush t.outc

  let get t =
    input_line t.inc

  let getn t n =
    let concat_ s1 s2 =
      match s1 with
        "" -> s2
      | _  -> s1 ^ "\n" ^ s2 in
    let rec loop acc = function
        0            -> acc
      | i when i > 0 -> loop (concat_ acc (get t)) (i - 1)
      | _            -> assert false
    in loop "" n

  let concat_ s1 s2 =
    match s1 with
      "" -> s2
    | _  -> s1 ^ "\n" ^ s2
      
  let getn t n a0 f =
    let rec loop acc = function
        0            -> acc
      | i when i > 0 -> loop (f acc (get t)) (i - 1)
      | _            -> assert false
    in loop a0 n

  let skip t =
    ignore (get t)

  let cmd_fold t str a0 f =
    put t (str ^ " | ? wc -l");
    let n_lines = get t in
    try
      let n_lines = 1 + int_of_string n_lines in
      put t str;
      skip t;
      let lines = getn t n_lines a0 f in
      lines
    with Failure _ -> a0

  let cmd t str =
    cmd_fold t str "" concat_

  let close t =
    put t "quit";
    (*Unix.close_process (t.inc, t.outc)*)

end

let fun_regexp = Str.regexp "^\\([a-zA-Z0-9_]+\\)[ ]*:[ ]*\\([a-zA-Z0-9_]+\\)[ ]*;$"
  
let parse_pgfun s =
  let matched = Str.string_match fun_regexp s 0 in
  if matched then Some (Str.matched_group 1 s, Str.matched_group 2 s)
  else None

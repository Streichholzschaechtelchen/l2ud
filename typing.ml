(* Decorate a Gfpm.file with types, producing a Tgfpm.file or throwing a typing error *)

exception TypingError of Lexing.position * string

module TypingErrors = struct

  let messages = [| "Parameter name conflicts with an already bound name" (*0*);
                    "Parameter value conflicts with an already bound name" (*1*);
                    "Unbound name" (*2*);
                    "Type of table keys does not match type of select value" (*3*);
                    "Selection can only be applied to table" (*4*);
                    "Only identifiers can be selected" (*5*);
                    "Selection can only be applied to a table/identifier pair" (*6*);
                    "Record field does not exist" (*7*);
                    "Projection can only be applied to record" (*8*);
                    "Right-hand side of IDL operator must be string set" (*9*);
                    "Left-hand side of IDL operator must be string set" (*10*);
                    "Operands of IDL operator must be string sets" (*11*);
                    "Operand of IDL operator must be string set" (*12*);
                    "Bound variable in lambda must be of a parameter type" (*13*);
                    "Field name conflicts with an already bound field name" (*14*);
                    "Right-hand side of field declaration is record type can only be a type" (*15*);
                    "Key name conflicts with an already bound key name" (*16*);
                    "Inconsistant value types in table" (*17*);
                    "Inconsistant key types in table" (*18*);
                    "Inconsistant types in table" (*19*);
                    "Right-hand side of => must be a type" (*20*);
                    "Left-hand side of => must be a type" (*21*);
                    "Operands of => must be type" (*22*);
                    "Lincat name conflicts with an already bound name" (*23*);
                    "Right-hand side of : in lincat declaration must be a type" (*24*);
                    "Lincat argument name conflicts with an already bound name" (*25*);
                    "Computed type of lin does not match declared type" (*26*);
                 |]
                   
  let throw ?(info="") (loc: Lexing.position) err_id =
    raise (TypingError (loc, "[TypingError " ^ (string_of_int err_id)
                             ^ "] " ^ messages.(err_id)
                             ^ (if info = "" then "" else " (" ^ info ^ ").")))

end

(* Hash table storing ident -> typ mappings *)
let types = Hashtbl.create 17

let type_ident (i: Gfpm.ident) =
  try Hashtbl.find types i.id
  with Not_found -> TypingErrors.throw ~info:i.id i.id_loc 2

let rec search i = function
    []                    -> raise Not_found
  | (j, t)::u when i = j  -> t
  | h::t                  -> search i t
      
let type_op (t_e1: Tgfpm.expr)
            (t_e2: Tgfpm.expr)
            (t_e: Tgfpm.expr_nod)
            (loc1: Lexing.position)
            (loc2: Lexing.position)
    : Tgfpm.expr =
  begin match t_e1.expr_type, t_e2.expr_type with
    Tset, Tset -> { expr_node = t_e;
                    expr_type = Tset }
  | Tset, _    -> TypingErrors.throw loc2 9
  | _   , Tset -> TypingErrors.throw loc1 10
  | _   , _    -> TypingErrors.throw loc1 11
  end
  
let rec type_expr (expr: Gfpm.expr)
        : Tgfpm.expr =
  match expr.expr_node with
    Gfpm.Estring s
    -> { expr_node = Tgfpm.Estring s;
         expr_type = Tgfpm.Tset }
  | Gfpm.Eident i
    -> let t_i = type_ident i in
       { expr_node = Tgfpm.Eident i.id;
         expr_type = t_i }
  | Gfpm.Eselect (e1, e2)
    -> let t_e1 = type_expr e1 and t_e2 = type_expr e2 in
       begin match t_e1.expr_type, t_e2.expr_type with
         Tgfpm.Ttable (i1, t), Tgfpm.Tparam i2 when i1 = i2 ->
         { expr_node = Tgfpm.Eselect (t_e1, t_e2);
           expr_type = t }
       | Tgfpm.Ttable (i1, _), Tgfpm.Tparam i2 ->
          TypingErrors.throw ~info:(i1 ^ ", " ^ i2) expr.expr_loc 3
       | _, Tgfpm.Tparam _ ->
          TypingErrors.throw e1.expr_loc 4
       | Tgfpm.Ttable (i1, _), _ ->
          TypingErrors.throw e2.expr_loc 5
       | _, _ ->
          TypingErrors.throw expr.expr_loc 6
       end
  | Gfpm.Eproject (e, i)
    -> let t_e = type_expr e in
       begin match t_e.expr_type with
         Trecord its -> let t = try search i.id its
                                with Not_found -> TypingErrors.throw ~info:i.id i.id_loc 7
                        in { expr_node = Tgfpm.Eproject (t_e, i.id);
                             expr_type = t }
       | _           -> TypingErrors.throw e.expr_loc 8
       end
  | Gfpm.Eblock e
    -> let t_e = type_expr e in
       { expr_node = Tgfpm.Eblock t_e;
         expr_type = t_e.expr_type }
  | Gfpm.Econcat (e1, e2)
    -> let t_e1 = type_expr e1 and t_e2 = type_expr e2 in
       type_op t_e1 t_e2 (Tgfpm.Econcat (t_e1, t_e2)) e1.expr_loc e2.expr_loc
  | Gfpm.Einterl (e1, e2)
    -> let t_e1 = type_expr e1 and t_e2 = type_expr e2 in
       type_op t_e1 t_e2 (Tgfpm.Einterl (t_e1, t_e2)) e1.expr_loc e2.expr_loc
  | Gfpm.Edisj (e1, e2)
    -> let t_e1 = type_expr e1 and t_e2 = type_expr e2 in
       type_op t_e1 t_e2 (Tgfpm.Edisj (t_e1, t_e2))  e1.expr_loc e2.expr_loc
  | Gfpm.Elock e
    -> let t_e = type_expr e in
       begin match t_e.expr_type with
         Tgfpm.Tset -> { expr_node = Tgfpm.Elock t_e;
                         expr_type = Tgfpm.Tset }
       | _          -> TypingErrors.throw e.expr_loc 12
       end
  | Gfpm.Elambda (i1, i2, e)
    -> begin match type_ident i2 with
         Tgfpm.Ttyp -> begin Hashtbl.add types i1.id (Tparam i2.id);
                             let t_e = type_expr e in
                             Hashtbl.remove types i1.id;
                             { expr_node = Tgfpm.Elambda (i1.id, i2.id, t_e);
                               expr_type = Tgfpm.Ttable (i2.id, t_e.expr_type) }
                       end
       | _          -> TypingErrors.throw i2.id_loc 13
       end
  | Gfpm.Erecord r
    -> let _, r_node, r_type =
         List.fold_left (fun (djavu, r_node, r_type) ((i:Gfpm.ident), e) ->
             if List.mem i.id djavu
             then TypingErrors.throw ~info:i.id i.id_loc 14
             else let t_e = type_expr e in
                  (i.id::djavu,
                   (i.id, t_e)::r_node,
                   (i.id, t_e.expr_type)::r_type)
           ) ([], [], []) r in
       { expr_node = Tgfpm.Erecord (List.sort compare r_node);
         expr_type = Tgfpm.Trecord (List.sort compare r_type) }
  | Gfpm.Etable t
    -> let _, t_node, param, typ =
         List.fold_left (fun (djavu, t_node, param, typ) ((i:Gfpm.ident), e) ->
             if List.mem i.id djavu
             then TypingErrors.throw ~info:i.id i.id_loc 16
             else begin let t_i = type_ident i and t_e = type_expr e in
                        match t_i, t_e.expr_type, param, typ with
                          Tgfpm.Tparam p, _, None, None ->
                           i.id::djavu, (i.id, t_e)::t_node, Some p, Some t_e.expr_type
                        | Tgfpm.Tparam p1, t1, Some p2, Some t2
                             when p1 = p2 && t1 = t2 ->
                           i.id::djavu, (i.id, t_e)::t_node, Some p2, Some t2
                        | Tgfpm.Tparam p1, _, Some p2, _ when p1 = p2  ->
                           TypingErrors.throw e.expr_loc 17
                        | _, t1, _, Some t2 when t1 = t2 ->
                           TypingErrors.throw i.id_loc 18
                        | _, _, _, _ ->
                           TypingErrors.throw expr.expr_loc 19
                  end)
           ([], [], None, None)
           t
       in
       let p, t = match param, typ with
           Some pp, Some tt -> pp, tt
         | _, _             -> assert false
       in { expr_node = Tgfpm.Etable (List.sort compare t_node);
            expr_type = Tgfpm.Ttable (p, t) }

let rec type_typ (t: Gfpm.typ) : Tgfpm.typ =
  match t with
    Gfpm.Tset     -> Tgfpm.Tset
  | Gfpm.Tident i -> begin match type_ident i with
                       Tgfpm.Ttyp -> Tgfpm.Tparam i.id
                     | _          -> TypingErrors.throw ~info:i.id i.id_loc 24
                     end
  | Gfpm.Trecord r
    -> let _, r_node =
         List.fold_left (fun (djavu, r_node) ((i:Gfpm.ident), t) ->
             if List.mem i djavu
             then TypingErrors.throw ~info:i.id i.id_loc 14
             else let t_t = type_typ t in
                  (i::djavu, (i.id, t_t)::r_node)
           ) ([], []) r in
       Tgfpm.Trecord (List.sort compare r_node);
  | Gfpm.Ttable (i, t) ->
     begin let t_i = type_ident i
           and t_t = type_typ t in
     match t_i with
       Tgfpm.Ttyp -> Tgfpm.Ttable (i.id, t_t)
     | _          -> TypingErrors.throw ~info:i.id i.id_loc 24
     end
       
let type_lincat (l: Gfpm.lincat) : Tgfpm.typ =
  let lincat_name, typ = l.lincat_name, l.lincat_type in
  let lincat_type = type_typ typ in
  if Hashtbl.mem types lincat_name.id
  then TypingErrors.throw ~info:lincat_name.id lincat_name.id_loc 23
  else Hashtbl.add types lincat_name.id lincat_type;
  lincat_type
                                       
let type_lin (lincats: Tgfpm.lincat_map) (l: Gfpm.lin) : Tgfpm.lin =
  let lin_name = l.lin_name
  and lin_outc = l.lin_outc.id
  and lin_args = List.map (fun (a: Gfpm.ident * Gfpm.ident)
                           -> (fst a).id, (snd a).id) l.lin_args
  and lin_expr = l.lin_expr in
  let ident_is_lincat i =
    match type_ident i with
      Tgfpm.Ttyp -> TypingErrors.throw ~info:i.id i.id_loc 24
    | _          -> ()
  in
  List.iter (fun a -> ident_is_lincat (snd a)) l.lin_args;
  ident_is_lincat l.lin_outc;
  let fill_arg_types (a: Gfpm.ident * Gfpm.ident) =
    if Hashtbl.mem types (fst a).id
    then TypingErrors.throw ~info:(fst a).id (fst a).id_loc 25
    else Hashtbl.add types (fst a).id (Hashtbl.find types (snd a).id)
  in
  List.iter fill_arg_types l.lin_args;
  let t_lin_expr = type_expr lin_expr in
  let expected_t = Tgfpm.M.find lin_outc lincats in
  let t_l = match t_lin_expr.expr_type with
      t when t = expected_t -> Tgfpm.{ lin_outc; lin_args; lin_expr = t_lin_expr }
    | _                     -> TypingErrors.throw ~info:lin_name.id lin_name.id_loc 26
  in
  List.iter (Hashtbl.remove types) (List.map fst lin_args);
  t_l
                                     
let fill_param_types (params: Gfpm.param list) =
  let fill_param_type (param: Gfpm.param) =
    let name = param.param_name in
    let fill_param_value_type (param_value: Gfpm.ident) =
      if Hashtbl.mem types param_value.id
      then TypingErrors.throw ~info:param_value.id param_value.id_loc 1
      else Hashtbl.add types param_value.id (Tgfpm.Tparam name.id)
    in
    if Hashtbl.mem types name.id
    then TypingErrors.throw ~info:name.id name.id_loc 0
    else (Hashtbl.add types name.id Tgfpm.Ttyp;
          List.iter fill_param_value_type param.param_values)
  in List.iter fill_param_type params

let process_lin (lincats: Tgfpm.lincat_map)
                (li: Tgfpm.lin Tgfpm.M.t)
                (l: Gfpm.lin)
    : Tgfpm.lin Tgfpm.M.t =
  let t_l = type_lin lincats l in
  let rec aux i =
    let name = l.lin_name.id ^ "_" ^ (string_of_int i) in
    try (ignore (Tgfpm.M.find name li);
         aux (i+1))
    with Not_found -> Tgfpm.M.add name t_l li
  in try (ignore (Tgfpm.M.find l.lin_name.id li);
          aux 2)
     with Not_found -> Tgfpm.M.add l.lin_name.id t_l li

let type_file (file: Gfpm.file) : Tgfpm.file =
  let name = file.name.id in
  let g_params = file.params in
  fill_param_types g_params;
  let params = List.fold_left (fun acc (p: Gfpm.param) ->
                   Tgfpm.M.add p.param_name.id (List.map (fun (v:Gfpm.ident)
                                                          -> v.id)
                                                  p.param_values) acc)
                     Tgfpm.M.empty g_params in
  let g_includeccs = file.includeccs in
  let includeccs = g_includeccs in
  let g_lincats = file.lincats in
  let lincats = List.fold_left (fun acc (l: Gfpm.lincat) ->
                    Tgfpm.M.add l.lincat_name.id (type_lincat l) acc) Tgfpm.M.empty g_lincats in
  let g_lins = file.lins in
  let lins =  List.fold_left (process_lin lincats) Tgfpm.M.empty g_lins in
  { name; includeccs; lincats; params; lins }

    (*TODO: several words in a string bspw. "the potato"*)

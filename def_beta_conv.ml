let new_defs = ref [];;

  let rec crude_printer_ty = function
      Tyvar(name) ->
      Printf.printf "%s" name
    | Tyapp(op,tylist) ->
      Printf.printf "%s (" op;
      List.iter
        (fun x -> crude_printer_ty x; Printf.printf " ; ")
        tylist;
      Printf.printf ")";;

  let rec crude_printer = function
      Var(name,ty) ->
        Printf.printf "%s" name
    | Const(name,_) -> Printf.printf "%s" name
    | Comb(Comb(Const(name,_),t1),t2) when (name = "=" || name = "==>") ->
      begin
        Printf.printf "((";
        crude_printer t1;
        Printf.printf ") %s (" name;
        crude_printer t2;
      end; 
      Printf.printf "))"
       | Comb(f,t) ->
      begin
        crude_printer f;
        Printf.printf " ";
      end;
      crude_printer t
    | Abs(Var(name,ty),t) ->

      begin
        Printf.printf "\%s:" name;
        crude_printer_ty ty;
        Printf.printf ". "
      end;
      crude_printer t
    | _ -> ();;


  let crude_printer_thm thm =
    let hyps = hyp thm in
    let concl = concl thm in
    List.iter crude_printer hyps;
    Printf.printf " |- ";
    crude_printer concl;
    Printf.printf "\n\n";;

let DEF_CONV tm =
  let rec apply_def tm =
    let (namecst,tycst) = dest_const tm in
    function
    | [] -> failwith "No definition matches this term"
    | (name,def)::other_new_defs when name=namecst ->
      INST_TYPE (type_match (type_of (rand (concl def))) tycst []) def
    | (name,def)::other_new_defs -> apply_def tm other_new_defs
  in
  apply_def tm (!new_defs);;

let DEF_BETA_CONV tm =
  try BETA_CONV tm with Failure _ ->
  try DEF_CONV tm with Failure _ ->
    failwith "Not a beta-redex or a definition";;

let DEF_BETA_DEPTH = REDEPTH_CONV DEF_BETA_CONV;;

let CONV_CONV tm1 tm2 =
  let thm1 = DEF_BETA_DEPTH tm1 in
  let thm2 = DEF_BETA_DEPTH tm2 in
  TRANS thm1 (SYM thm2);;

let CONV_CONV_rule tm2 thm =
  let tm1 = concl thm in
  try
    EQ_MP (CONV_CONV tm1 tm2) thm
  with Failure _ ->
    Printf.printf "Conversion issue between terms \n\n";
    crude_printer tm1;
    Printf.printf "\n\nand\n\n";
    crude_printer tm2;
    Printf.printf "\n\nand theorem\n\n";
    crude_printer_thm thm;
    failwith "CONV_CONV_rule";;

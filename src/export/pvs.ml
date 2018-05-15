open Ast

let current_module : string ref = ref ""

let with_types = ref false

let forallp = "\\rotatebox[origin=c]{180}{\\ensuremath{\\mathcal{A}}}"

let sanitize_name : string -> string =
 fun n -> String.concat "\\_" (String.split_on_char '_' n)

 let sanitize_name_pvs : string -> string =
  fun n ->
    if String.equal n "True" || String.equal n "False"
       || String.equal n "And" || String.equal n "Or"
       || String.equal n "Not" || String.equal n "ex"
       || String.equal n "nat"
       || String.equal n "O"   || String.equal n "S"
       || String.equal n "bool"
       || String.equal n "true" || String.equal n "false"
       || String.equal n "fact" || String.equal n "exp"
       || String.equal n "divides"
       || String.equal (String.sub n 0 1) "_" then
  "sttfa_" ^ n
 else n

let print_name : out_channel -> name -> unit =
 fun oc (m, n) ->
  let name = sanitize_name_pvs n in
  Printf.fprintf oc "%s" name

let print_qualified_name : out_channel -> name -> unit =
 fun oc (m, n) ->
  let name = sanitize_name_pvs n in
  Printf.fprintf oc "%s_sttfa.%s" m name

let print__ty_pvs : out_channel -> _ty -> unit =
  let rec print is_atom oc _ty =
    match _ty with
    | TyVar x -> output_string oc x
    | Arrow (_, _) when is_atom -> Printf.fprintf oc "%a" (print false) _ty
    | Arrow (a, b) ->
        Printf.fprintf oc "[%a -> %a]" (print true) a (print false) b
    | TyOp (op, l) -> print_qualified_name oc op
    | Prop       ->
        output_string oc "bool"
  in
  print false

let rec print_ty_pvs : out_channel -> ty -> unit =
 fun oc ty ->
  match ty with
  | ForallK (x, ty') -> print_ty_pvs oc ty'
  | Ty _ty -> print__ty_pvs oc _ty

let rec prefix_of_ty : ty -> string list =
 fun ty ->
  match ty with ForallK (x, ty') -> x :: prefix_of_ty ty' | Ty _ty -> []

let rec print_type_list_pvs : out_channel -> _ty list -> unit =
 fun oc l ->
  match l with
  | [] -> ()
  | [a] -> print__ty_pvs oc a
  | a :: l ->
      print__ty_pvs oc a ; Printf.fprintf oc "," ; print_type_list_pvs oc l

let rec print_type_list_b_pvs : out_channel -> _ty list -> unit =
 fun oc ty ->
  match ty with
  | [] -> ()
  | _ ->
      Printf.fprintf oc "[" ; print_type_list_pvs oc ty ; Printf.fprintf oc "]"


let rec print_string_type_list_pvs : out_channel -> string list -> unit =
 fun oc l ->
  match l with
  | [] -> ()
  | [a] -> Printf.fprintf oc "%s:TYPE+" a
  | a :: l ->
      Printf.fprintf oc "%s:TYPE+," a ;
      print_string_type_list_pvs oc l


let rec print_prenex_ty_pvs : out_channel -> ty -> unit =
 fun oc ty ->
  let p = prefix_of_ty ty in
  match p with
  | [] -> ()
  | _ ->
      Printf.fprintf oc "[" ;
      print_string_type_list_pvs oc p ;
      Printf.fprintf oc "]"


let rec vars acc t = match t with
  | TeVar x -> x::acc
  | Abs (x,_,u) -> x::(vars acc u)
  | App (u,v) -> (vars (vars acc u) v)
  | Forall (x,_,u) -> x::(vars acc u)
  | Impl(u,v) -> (vars (vars acc u) v)
  | AbsTy (_,u) -> (vars acc u)
  | Cst ((_,n),_) -> n::acc

let rec gensym k l =
  let v = "x"^(string_of_int k)
  in if List.mem v l then gensym (k+1) l else v

let print__te_pvs : out_channel ->_te -> unit =
  fun oc -> fun t ->
  let rec print stack oc t =
    match t with
    | TeVar x -> output_string oc (sanitize_name_pvs x);
                 print_stack oc stack
    | Abs (x, a, t) ->
        Printf.fprintf oc "(LAMBDA(%s:%a):%a)" (sanitize_name_pvs x)
          print__ty_pvs a (print []) t;
          print_stack oc stack
    | App (t, u) -> print (u::stack) oc t
    | Forall (x, a, t) ->
      Printf.fprintf oc
        "(FORALL(%s:%a):%a)"
        (sanitize_name_pvs x)
        print__ty_pvs a (print []) t;
        print_stack oc stack
    | Impl (t, u) -> Printf.fprintf oc
                       "(%a => %a)"
                       (print []) t (print []) u;
                       print_stack oc stack
    | AbsTy (x, t) -> Printf.fprintf oc "%a" (print []) t;
      print_stack oc stack
(*    | Cst ((_,"True"), []) -> output_string oc "TRUE";
                              print_stack oc stack
    | Cst ((_,"False"), []) -> output_string oc "FALSE";
                               print_stack oc stack
    | Cst ((_,"Not"), []) -> print_not oc stack
    | Cst ((_,"And"), []) -> print_and oc stack
    | Cst ((_,"Or"), []) -> print_or oc stack
    | Cst ((_,"ex"), [t]) -> print_ex oc t stack
      | Cst ((_,"equal"), [t]) -> print_equal oc t stack *)
    | Cst (name,l) ->
      print_qualified_name oc name;
      print_typeargs oc l;
      print_stack oc stack

(* and print_not oc stack =
      match stack with
      | a::s' ->
          Printf.fprintf oc "(NOT (%a))" (print []) a;
          print_stack oc s'
      | _ ->     output_string oc  "(LAMBDA(x:bool):(NOT x))";
        print_stack oc stack

and print_and oc stack =
      match stack with
        | [] ->  output_string oc  "(LAMBDA(x:bool)(y:bool):(x AND y))"
        | a::[] ->
          Printf.fprintf oc "(LAMBDA(y:bool):(%a AND y))" (print []) a
        | a::b::s' ->
          Printf.fprintf oc "((%a) AND (%a))" (print []) a (print []) b;
          print_stack oc s'

  and print_or oc stack =
      match stack with
        | [] ->  output_string oc  "(LAMBDA(x:bool)(y:bool):(x OR y))"
        | a::[] ->
          Printf.fprintf oc "(LAMBDA(y:bool):(%a OR y))" (print []) a
        | a::b::s' ->
          Printf.fprintf oc "((%a) OR (%a))" (print []) a (print []) b;
          print_stack oc s'

  and print_ex oc t stack =
      match stack with
        | [] ->   Printf.fprintf oc
                   "LAMBDA (p:%a -> bool):(EXISTS (x : %a): p(x))"
                    print__ty_pvs t
                    print__ty_pvs t
        | (Abs (x, _, a))::s' ->
                 Printf.fprintf oc "(EXISTS (%a : %a): %a)"
                 output_string x
                 print__ty_pvs t
                 (print []) a;
                 print_stack oc s'
        | a::s' ->
                 let x = gensym 0 (vars [] a)
                 in Printf.fprintf oc "(EXISTS (%a : %a): %a(%a))"
                 output_string x
                 print__ty_pvs t
                 (print []) a
                 output_string x;
                 print_stack oc s'

and print_equal oc t stack =
      match stack with
      | [] ->  Printf.fprintf oc  "(LAMBDA(x:%a)(y:%a):(x = y))"
                 print__ty_pvs t
                 print__ty_pvs t
      (*                 print__ty_pvs t *)
      | a::[] -> Printf.fprintf oc "(LAMBDA(y:%a):(%a = y))"
                   print__ty_pvs t
                   (print []) a
(*                   print__ty_pvs t *)
      | a::b::s' -> Printf.fprintf oc "(%a = %a)"
                      (print []) a
                      (*                      print__ty_pvs t *)
                      (print []) b;
        print_stack oc s'
*)
  and print_typeargs oc l =
  if l <> [] then (Printf.fprintf oc "[" ;print_type_list_pvs oc l;Printf.fprintf oc "]")


  and print_stack oc stack = match stack with
    | [] -> ()
    | a::l' -> Printf.fprintf oc "(%a)" (print []) a;
               print_stack oc l'
  in
  print [] oc t


let rec prefix_of_te : te -> string list =
 fun te ->
  match te with ForallP (x, te') -> x :: prefix_of_te te' | Te te' -> []


let rec print_prenex_te_pvs : out_channel -> te -> unit =
 fun oc te ->
  let p = prefix_of_te te in
  match p with
  | [] -> ()
  | _ ->
      Printf.fprintf oc "[" ;
      print_string_type_list_pvs oc p ;
      Printf.fprintf oc "]"


let rec print_te_pvs : out_channel -> te -> unit =
 fun oc te ->
  match te with
  | ForallP (x, te') -> print_te_pvs oc te'
  | Te te' -> print__te_pvs oc te'


let print__ty_ctx : out_channel -> ty_ctx -> unit =
 fun oc ctx ->
  match ctx with
  | [] -> Printf.fprintf oc "∅"
  | [_ty] -> Printf.fprintf oc "%s" _ty
  | _ty :: l ->
      Printf.fprintf oc "%s" _ty ;
      List.iter (fun _ty -> Printf.fprintf oc ", %s" _ty) l ;
      Printf.fprintf oc " "


let print_te_ctx : out_channel -> te_ctx -> unit =
 fun oc ctx ->
  match ctx with
  | [] -> Printf.fprintf oc "∅"
  | [(x, _ty)] ->
      if !with_types then Printf.fprintf oc "%s:%a" x print__ty_pvs _ty
      else Printf.fprintf oc "%s" x
  | (x, _ty) :: l ->
      if !with_types then (
        Printf.fprintf oc "%s:%a" x print__ty_pvs _ty ;
        List.iter
          (fun (x, _) -> Printf.fprintf oc ", %s:%a" x print__ty_pvs _ty)
          l ;
        Printf.fprintf oc " " )
      else (
        Printf.fprintf oc "%s" x ;
        List.iter (fun (x, _) -> Printf.fprintf oc ", %s" x) l ;
        Printf.fprintf oc " " )


let print_hyp : out_channel -> hyp -> unit =
 fun oc hyp ->
  let l = TeSet.elements hyp in
  match l with
  | [] -> Printf.fprintf oc "∅"
  | [_,x] -> Printf.fprintf oc "%a" print__te_pvs x
  | (_,x) :: l ->
      Printf.fprintf oc "%a" print__te_pvs x ;
      List.iter (fun (_,x) -> Printf.fprintf oc ", %a" print__te_pvs x) l ;
      Printf.fprintf oc " "

let conclusion_pvs : proof -> te = fun prf -> (judgment_of prf).thm

exception Error

let decompose_implication = fun p -> match p with
  | (Te (Impl(a,b))) -> (a,b)
  |_ -> raise Error


let rec listof = fun l -> match l with
  | [] -> []
  | (Beta _,_)::l' -> listof l'
  | (Delta (x,_),_)::l' -> x::(listof l')

let rec print_name_list = fun oc -> fun l -> match l with
  | [] -> ()
  | x::[] -> Printf.fprintf oc "\"";
             print_qualified_name oc x;
             Printf.fprintf oc "\""
  | x::l' -> (Printf.fprintf oc "\"";
              print_qualified_name oc x;
              Printf.fprintf oc "\" ";
              print_name_list oc l')


let print_proof_pvs : out_channel -> proof -> unit =
  fun oc prf ->
    let rec print acc oc prf =
    match prf with
      | Assume(j,_)         -> Printf.fprintf oc "%%|- (propax)"
    | Lemma((q,s),j)    -> Printf.fprintf oc "%%|- (sttfa-lemma \"%s_sttfa.%s%a\")" q s print_type_list_b_pvs acc
    | Conv(j,p,trace)         ->
      let pc = conclusion_pvs p in
      Printf.fprintf oc "%%|- (sttfa-conv \"%a\" (%a) (%a)\n%a)"
        print_te_pvs pc
        print_name_list  (listof trace.left)
        print_name_list  (listof trace.right)
        (print acc) p
    | ImplE(j,p,q)      ->
      let pc = conclusion_pvs p
      in let qc = conclusion_pvs q
      in
      Printf.fprintf oc "%%|- (sttfa-impl-e \"%a\" \"%a\"\n%a\n%a)"
        print_te_pvs pc
        print_te_pvs qc
        (print acc) q
        (print acc) p
    | ImplI(j,p,_)        ->
      let (a,b) = decompose_implication j.thm
      in Printf.fprintf oc "%%|- (sttfa-impl-i \"%a\" \"%a\"\n%a)"
        print__te_pvs a print__te_pvs b (print acc) p

    | ForallE(j,p,_te)  ->
      let pc = conclusion_pvs p
      in Printf.fprintf oc "%%|- (sttfa-forall-e \"%a\" \"%a\"\n%a)"
        print_te_pvs pc
        print__te_pvs _te
        (print acc) p
    | ForallI(j,p,n)      -> Printf.fprintf oc "%%|- (then@ (sttfa-forall-i \"%s\")\n%a)" (sanitize_name_pvs n) (print acc) p
    | ForallPE(j,p,_ty)   -> print (_ty::acc) oc p
    | ForallPI(j,p,n)     -> print acc oc p
  in
  print [] oc prf;
  Printf.fprintf oc "\n"


let print_ast : out_channel -> string -> ast -> unit =
 fun oc prefix ast ->
   let prefix = Filename.basename prefix in
   current_module := ast.md ;
   let line fmt = Printf.fprintf oc (fmt ^^ "\n") in
   let print_item it =
     match it with
     | TyOpDef (op, ar) -> Printf.fprintf oc "%a : TYPE+" print_name op;
       line "";
       line ""
     | Parameter (n, ty) ->
       line "%a %a: %a" print_name n print_prenex_ty_pvs ty print_ty_pvs ty ;
       line ""
     | Definition (n, ty, te) ->
       line "%a %a : %a = %a" print_name n print_prenex_ty_pvs ty print_ty_pvs
         ty print_te_pvs te ;
       line ""
     | Axiom (n, te) ->
       line "%a %a : AXIOM %a" print_name n print_prenex_te_pvs te
         print_te_pvs te ;
       line ""
     | Theorem (n, te, prf) ->
       line "%a %a : LEMMA %a" print_name n print_prenex_te_pvs te
         print_te_pvs te ;
       line "" ;
       line "%%|- %a : PROOF" print_name n ;
       print_proof_pvs oc prf ;
       line "%%|- QED" ;
       line ""
   in
   let pf = "_sttfa" in
   let postfix s = s^pf in
   line "%s : THEORY" (postfix prefix);
   line "BEGIN";
   (*  line "%%|- *TCC* : PROOF (sttfa-nonemptiness)  QED"; *)
   let deps oc deps =
     let l = QSet.elements (QSet.remove "sttfa" deps) in
     let l = List.map postfix l in
     let rec deps oc l =
       match l with
       | [] -> assert false
       | [x] -> Printf.fprintf oc "%s" x
       | x::t -> Printf.fprintf oc "%s,%a" x deps t
     in
     match l with
     | [] -> ()
     | _ -> line "IMPORTING %a" deps l
   in
   deps oc ast.dep ;
   line "";
   List.iter print_item ast.items;
   line "END %s_sttfa" prefix

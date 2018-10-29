open Ast

let sys = "pvs"

let current_module : string ref = ref ""

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

let print_name : string -> Format.formatter -> name -> unit =
 fun pvs_md oc (m, n) ->
  let name = sanitize_name_pvs n in
  Format.fprintf oc "%s" name

let rec print_arity oc arity =
  if arity = 0 then
    Format.fprintf oc "Type"
  else
    Format.fprintf oc "Type -> %a" print_arity (arity-1)

let sanitize id =
  if id = "return" then id ^ "_" else id

let print_var oc id =
  Format.fprintf oc "%s" (sanitize_name_pvs id)

let print_qualified_name : string -> Format.formatter -> name -> unit =
  fun pvs_md oc (m, n) ->
   let name = sanitize_name_pvs n in
   if m = pvs_md
   then Format.fprintf oc "%s_sttfa.%s" m name
   else Format.fprintf oc "%s_sttfa_th.%s" m name

let print__ty_pvs : string -> Format.formatter -> _ty -> unit =
  let rec print is_atom modul oc _ty =
    match _ty with
    | TyVar x -> Format.fprintf oc "%s" x
    | Arrow (_, _) when is_atom ->
      Format.fprintf oc "%a" (print false modul) _ty
    | Arrow (a, b) ->
      Format.fprintf oc "[%a -> %a]"
        (print true modul) a
        (print false modul) b
    | TyOp (op, l) -> print_qualified_name modul oc op
    | Prop       -> Format.fprintf oc "bool"
  in
  fun modul oc ty -> print false modul oc ty

let rec print_ty_pvs : string -> Format.formatter -> ty -> unit =
 fun pvs_md oc ty ->
  match ty with
  | ForallK (x, ty') -> print_ty_pvs pvs_md oc ty'
  | Ty _ty -> print__ty_pvs pvs_md oc _ty

let rec prefix_of_ty : ty -> string list =
 fun ty ->
  match ty with ForallK (x, ty') -> x :: prefix_of_ty ty' | Ty _ty -> []

let rec print_type_list_pvs : Format.formatter -> string -> _ty list -> unit =
 fun oc pvs_md l ->
  match l with
  | [] -> ()
  | [a] -> print__ty_pvs pvs_md oc a
  | a :: l ->
      print__ty_pvs pvs_md oc a ; Format.fprintf oc "," ; print_type_list_pvs oc pvs_md l

let rec print_type_list_b_pvs : string -> Format.formatter -> _ty list -> unit =
 fun pvs_md oc ty ->
  match ty with
  | [] -> ()
  | _ ->
      Format.fprintf oc "[" ; print_type_list_pvs oc pvs_md ty ; Format.fprintf oc "]"


let rec print_string_type_list_pvs : Format.formatter -> string list -> unit =
 fun oc l ->
  match l with
  | [] -> ()
  | [a] -> Format.fprintf oc "%s:TYPE+" a
  | a :: l ->
      Format.fprintf oc "%s:TYPE+," a ;
      print_string_type_list_pvs oc l


let rec print_prenex_ty_pvs : string -> Format.formatter -> ty -> unit =
 fun pvs_md oc ty ->
  let p = prefix_of_ty ty in
  match p with
  | [] -> ()
  | _ ->
      Format.fprintf oc "[" ;
      print_string_type_list_pvs oc p ;
      Format.fprintf oc "]"


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

let print__te_pvs : string -> Format.formatter -> _te -> unit =
  fun pvs_md oc t ->
    let rec print stack pvs_md oc t =
    match t with
      | TeVar x ->
        Format.fprintf oc "%s" (sanitize_name_pvs x);
        print_stack oc pvs_md stack
    | Abs (x, a, t) ->
      Format.fprintf oc "(LAMBDA(%s:%a):%a)"
        (sanitize_name_pvs x)
        (print__ty_pvs pvs_md) a
        (print [] pvs_md) t;
      print_stack oc pvs_md stack
    | App (t, u) -> print (u::stack) pvs_md oc t
    | Forall (x, a, t) ->
      Format.fprintf oc
        "(FORALL(%s:%a):%a)"
        (sanitize_name_pvs x)
        (print__ty_pvs pvs_md) a
        (print [] pvs_md) t;
      print_stack oc pvs_md stack
    | Impl (t, u) -> Format.fprintf oc
                       "(%a => %a)"
                       (print [] pvs_md) t
                       (print [] pvs_md) u;
                       print_stack oc pvs_md stack
    | AbsTy (x, t) -> Format.fprintf oc "%a"
                        (print [] pvs_md) t;
      print_stack oc pvs_md stack
    | Cst (name,l) ->
      print_qualified_name pvs_md oc name;
      print_typeargs oc pvs_md l;
      print_stack oc pvs_md stack

  and print_typeargs oc pvs_md l =
    if l <> [] then (Format.fprintf oc "[" ;print_type_list_pvs oc pvs_md l;
                     Format.fprintf oc "]")


  and print_stack oc pvs_md stack = match stack with
    | [] -> ()
    | a::l' -> Format.fprintf oc "(%a)"
               (print [] pvs_md) a;
               print_stack oc pvs_md l'
  in
  print [] pvs_md oc t


let rec prefix_of_te : te -> string list =
 fun te ->
  match te with ForallP (x, te') -> x :: prefix_of_te te' | Te te' -> []


let rec print_prenex_te_pvs : string -> Format.formatter -> te -> unit =
 fun pvs_md oc te ->
  let p = prefix_of_te te in
  match p with
  | [] -> ()
  | _ ->
      Format.fprintf oc "[" ;
      print_string_type_list_pvs oc p ;
      Format.fprintf oc "]"


let rec print_te_pvs : string -> Format.formatter -> te -> unit =
 fun pvs_md oc te ->
  match te with
  | ForallP (x, te') -> print_te_pvs pvs_md oc te'
  | Te te' -> print__te_pvs pvs_md oc te'


let print__ty_ctx : Format.formatter -> ty_ctx -> unit =
 fun oc ctx ->
  match ctx with
  | [] -> Format.fprintf oc "∅"
  | [_ty] -> Format.fprintf oc "%s" _ty
  | _ty :: l ->
      Format.fprintf oc "%s" _ty ;
      List.iter (fun _ty -> Format.fprintf oc ", %s" _ty) l ;
      Format.fprintf oc " "


let print_te_ctx : Format.formatter -> string -> te_ctx -> unit =
 fun oc pvs_md ctx ->
  match ctx with
  | [] -> Format.fprintf oc "∅"
  | [(x, _ty)] ->
      Format.fprintf oc "%s:%a" x (print__ty_pvs pvs_md) _ty
  | (x, _ty) :: l ->
    Format.fprintf oc "%s:%a" x (print__ty_pvs pvs_md) _ty ;
    List.iter
      (fun (x, _) -> Format.fprintf oc ", %s:%a" x (print__ty_pvs pvs_md) _ty)
      l ;
    Format.fprintf oc " "


let print_hyp : string -> Format.formatter -> hyp -> unit =
 fun pvs_md oc hyp ->
  let l = TeSet.elements hyp in
  match l with
  | [] -> Format.fprintf oc "∅"
  | [(_,x)] -> Format.fprintf oc "%a" (print__te_pvs pvs_md) x
  | (_,x) :: l ->
      Format.fprintf oc "%a" (print__te_pvs pvs_md) x ;
      List.iter (fun (_,x) -> Format.fprintf oc ", %a" (print__te_pvs pvs_md) x) l ;
      Format.fprintf oc " "

let conclusion_pvs : proof -> te =
 fun prf ->
  let j =
    match prf with
    | Assume(j,_) -> j
    | Lemma (_, j) -> j
    | Conv (j, p, _) -> j
    | ImplE (j, p, q) -> j
    | ImplI (j, p, _) -> j
    | ForallE (j, p, _te) -> j
    | ForallI (j, p, _) -> j
    | ForallPE (j, p, _ty) -> j
    | ForallPI (j, p, _) -> j
  in
  j.thm


exception Error

let decompose_implication = fun p -> match p with
  | (Te (Impl(a,b))) -> (a,b)
  |_ -> raise Error


let rec listof = fun l -> match l with
  | [] -> []
  | (Beta _,_)::l' -> listof l'
  | (Delta(x,_),_)::l' -> x::(listof l')

let rec print_name_list = fun pvs_md oc l -> match l with
  | [] -> ()
  | x::[] -> Format.fprintf oc "\"";
             print_qualified_name pvs_md oc x;
             Format.fprintf oc "\""
  | x::l' -> (Format.fprintf oc "\"";
              print_qualified_name pvs_md oc x;
              Format.fprintf oc "\" ";
              print_name_list pvs_md oc l')


let print_proof_pvs : string -> Format.formatter -> proof -> unit =
  fun pvs_md oc prf ->
    let rec print acc pvs_md oc prf =
    match prf with
      | Assume(j,_)         -> Format.fprintf oc "%%|- (propax)"
      | Lemma(n,j)    ->
         Format.fprintf oc "%%|- (sttfa-lemma \"%a%a\")"
          (print_qualified_name pvs_md) n
          (print_type_list_b_pvs pvs_md) acc
    | Conv(j,p,trace)         ->
      let pc = conclusion_pvs p in
      Format.fprintf oc "%%|- (sttfa-conv \"%a\" (%a) (%a)\n%a)"
        (print_te_pvs pvs_md) pc
        (print_name_list  pvs_md) (listof trace.left)
        (print_name_list  pvs_md) (listof trace.right)
        (print acc pvs_md) p
    | ImplE(j,p,q)      ->
      let pc = conclusion_pvs p
      in let qc = conclusion_pvs q
      in
      Format.fprintf oc "%%|- (sttfa-impl-e \"%a\" \"%a\"\n%a\n%a)"
        (print_te_pvs pvs_md) pc
        (print_te_pvs pvs_md) qc
        (print acc pvs_md) q
        (print acc pvs_md) p
    | ImplI(j,p,_)        ->
      let (a,b) = decompose_implication j.thm
      in Format.fprintf oc "%%|- (sttfa-impl-i \"%a\" \"%a\"\n%a)"
        (print__te_pvs pvs_md) a (print__te_pvs pvs_md) b (print acc pvs_md) p

    | ForallE(j,p,_te)  ->
      let pc = conclusion_pvs p
      in Format.fprintf oc "%%|- (sttfa-forall-e \"%a\" \"%a\"\n%a)"
        (print_te_pvs pvs_md) pc
        (print__te_pvs pvs_md) _te
        (print acc pvs_md) p
    | ForallI(j,p,n)      ->
      Format.fprintf oc "%%|- (then%@ (sttfa-forall-i \"%s\")\n%a)"
        (sanitize_name_pvs n)
        (print acc pvs_md) p
    | ForallPE(j,p,_ty)   -> print (_ty::acc) pvs_md oc p
    | ForallPI(j,p,n)     -> print acc pvs_md oc p
  in
  print [] pvs_md oc prf;
  Format.fprintf oc "\n"

let print_item oc pvs_md it =
  let line fmt = Format.fprintf oc (fmt ^^ "\n") in
  match it with
  | TyOpDef (op, ar) ->
    assert (ar = 0);
    Format.fprintf oc "%a : TYPE+" (print_name pvs_md) op;
    line "";
    line ""
  | Parameter (n, ty) ->
    line "%a %a: %a"
      (print_name pvs_md) n
      (print_prenex_ty_pvs pvs_md) ty
      (print_ty_pvs pvs_md) ty;
    line ""
  | Definition (n, ty, te) ->
    line "%a %a : %a = %a"
      (print_name pvs_md) n
      (print_prenex_ty_pvs pvs_md) ty
      (print_ty_pvs pvs_md) ty
      (print_te_pvs pvs_md) te;
    line ""
  | Axiom (n, te) ->
    line "%a %a : AXIOM %a"
      (print_name pvs_md) n
      (print_prenex_te_pvs pvs_md) te
      (print_te_pvs pvs_md) te;
    line ""
  | Theorem (n, te, prf) ->
    line "%a %a : LEMMA %a"
      (print_name pvs_md) n
      (print_prenex_te_pvs pvs_md) te
      (print_te_pvs pvs_md) te;
    line "" ;
    line "%%|- %a : PROOF"
      (print_name pvs_md) n ;
    (print_proof_pvs pvs_md oc) prf ;
    line "%%|- QED" ;
    line ""

let print_dep oc x = Format.fprintf oc
  "IMPORTING %s_sttfa AS %s_sttfa_th\n" x x

let print_dep2 oc x = Format.fprintf oc "%s_sttfa_th := %s_pvs_th\n" x x

let print_instance oc s deps x =
  Format.fprintf oc "%s_pvs_th : THEORY %s_pvs\n" x x;
  Format.fprintf oc "IMPORTING %s_sttfa {{\n" s;
    List.iter (print_dep2 oc) deps;
    Format.fprintf oc "}}\n"


let remove_transitive_deps deps =
  let remove_dep dep deps =
    let md = Basic.mk_mident dep in
    let md_deps = Signature.get_md_deps Basic.dloc md in
    QSet.diff deps (QSet.of_list (List.map Basic.string_of_mident md_deps))
  in
  QSet.fold remove_dep deps deps

let line oc fmt = Format.fprintf oc (fmt ^^ "\n")

(* FIXME: web will compute the correct dependencies but we should get rid off thi boolean *)
let web = ref false

let print_ast : Format.formatter -> ast -> unit =
 fun oc ast ->
   current_module := ast.md;
   let pf = "_sttfa" in
   let postfix s = s^pf in
   line oc "%s : THEORY" (postfix ast.md);
   line oc "BEGIN";
   let deps = ast.dep in
   let deps = QSet.remove "sttfa" deps in
   (* FIXME: buggy with the module web. This code should not rely on Signature *)
   let remove_transitive_deps deps =
     let remove_dep dep deps =
       let md = Basic.mk_mident dep in
       let md_deps = Signature.get_md_deps Basic.dloc md in
       QSet.diff deps (QSet.of_list (List.map Basic.string_of_mident md_deps))
     in
     QSet.fold remove_dep deps deps
   in
   let deps = if !web then deps else remove_transitive_deps deps in
   QSet.iter (print_dep oc) deps;
   line oc "";
   List.iter (print_item oc ast.md) ast.items;
   line oc "END %s_sttfa" ast.md

let to_string fmt = Format.asprintf "%a" fmt

let pretty_print_item = function
  | Parameter((md,id),ty) ->
    Format.asprintf "%a %a : %a"
      (print_name md) (md,id)
      (print_prenex_ty_pvs md) ty
      (print_ty_pvs md) ty
  | Definition((md,id),ty,te) ->
    Format.asprintf "%a %a : %a = %a"
      (print_name md) (md,id)
      (print_prenex_ty_pvs md) ty
      (print_ty_pvs md) ty
      (print_te_pvs md) te
  | Axiom((md,id),te) ->
    Format.asprintf "%a %a : AXIOM %a"
      (print_name md) (md,id)
      (print_prenex_te_pvs md) te
      (print_te_pvs md) te
  | Theorem((md,id),te,proof) ->
    Format.asprintf "%a %a : LEMMA %a"
      (print_name md) (md,id)
      (print_prenex_te_pvs md) te
      (print_te_pvs md) te
  | TyOpDef((md,id),arity) ->
    assert(arity = 0);
    Format.asprintf "%a : TYPE+" (print_name md) (md,id)

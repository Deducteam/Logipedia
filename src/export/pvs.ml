open Ast
module Basic = Kernel.Basic
module Signature = Kernel.Signature
module F = Format

let line oc fmt = F.fprintf oc (fmt ^^ "\n")

let sys = "pvs"

let current_module : string ref = ref ""

let sanitize_name_pvs : string -> string = fun n ->
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

let print_name : string -> F.formatter -> name -> unit =
 fun _ oc (_, n) ->
 let name = sanitize_name_pvs n in
 F.fprintf oc "%s" name

let rec print_arity oc arity =
  if arity = 0 then
    F.fprintf oc "Type"
  else
    F.fprintf oc "Type -> %a" print_arity (arity-1)

let print_qualified_name : string -> F.formatter -> name -> unit =
  fun pvs_md oc (m, n) ->
   let name = sanitize_name_pvs n in
   if m = pvs_md
   then F.fprintf oc "%s_sttfa.%s" m name
   else F.fprintf oc "%s_sttfa_th.%s" m name

let print__ty_pvs : string -> F.formatter -> _ty -> unit =
  let rec print is_atom modul oc _ty =
    match _ty with
    | TyVar x -> F.fprintf oc "%s" x
    | Arrow (_, _) when is_atom ->
      F.fprintf oc "%a" (print false modul) _ty
    | Arrow (a, b) ->
      F.fprintf oc "[%a -> %a]"
        (print true modul) a
        (print false modul) b
    | TyOp (op, _) -> print_qualified_name modul oc op
    | Prop       -> F.fprintf oc "bool"
  in
  fun modul oc ty -> print false modul oc ty

let rec print_ty_pvs : string -> F.formatter -> ty -> unit =
 fun pvs_md oc ty ->
  match ty with
  | ForallK (_, ty') -> print_ty_pvs pvs_md oc ty'
  | Ty _ty -> print__ty_pvs pvs_md oc _ty

let rec prefix_of_ty : ty -> string list =
 fun ty ->
  match ty with ForallK (x, ty') -> x :: prefix_of_ty ty' | Ty _ty -> []

let print_type_list_pvs : F.formatter -> string -> _ty list -> unit =
 fun oc pvs_md l ->
 let pp_sep fmt () = F.pp_print_string fmt "," in
 F.pp_print_list ~pp_sep (print__ty_pvs pvs_md) oc l

let print_type_list_b_pvs : string -> F.formatter -> _ty list -> unit =
 fun pvs_md oc ty ->
  match ty with
  | [] -> ()
  | _ ->
    let pp_tlp fmt = print_type_list_pvs fmt pvs_md in
    F.fprintf oc "[%a]" pp_tlp ty

let print_string_type_list_pvs : F.formatter -> string list -> unit =
 fun oc l ->
 let pp_sep fmt () = F.pp_print_string fmt "," in
 let pp_str_type fmt s = F.fprintf fmt "%s:TYPE+" s in
 F.pp_print_list ~pp_sep pp_str_type oc l

let print_prenex_ty_pvs : string -> F.formatter -> ty -> unit =
 fun _ oc ty ->
  let p = prefix_of_ty ty in
  match p with
  | [] -> ()
  | _ ->
      F.fprintf oc "[" ;
      print_string_type_list_pvs oc p ;
      F.fprintf oc "]"

let print__te_pvs : string -> F.formatter -> _te -> unit =
  fun pvs_md oc t ->
    let rec print stack pvs_md oc t =
    match t with
      | TeVar x ->
        F.fprintf oc "%s" (sanitize_name_pvs x);
        print_stack oc pvs_md stack
    | Abs (x, a, t) ->
      F.fprintf oc "(LAMBDA(%s:%a):%a)"
        (sanitize_name_pvs x)
        (print__ty_pvs pvs_md) a
        (print [] pvs_md) t;
      print_stack oc pvs_md stack
    | App (t, u) -> print (u::stack) pvs_md oc t
    | Forall (x, a, t) ->
      F.fprintf oc
        "(FORALL(%s:%a):%a)"
        (sanitize_name_pvs x)
        (print__ty_pvs pvs_md) a
        (print [] pvs_md) t;
      print_stack oc pvs_md stack
    | Impl (t, u) -> F.fprintf oc
                       "(%a => %a)"
                       (print [] pvs_md) t
                       (print [] pvs_md) u;
                       print_stack oc pvs_md stack
    | AbsTy (_, t) -> F.fprintf oc "%a"
                        (print [] pvs_md) t;
      print_stack oc pvs_md stack
    | Cst (name,l) ->
      print_qualified_name pvs_md oc name;
      print_typeargs oc pvs_md l;
      print_stack oc pvs_md stack

  and print_typeargs oc pvs_md l =
    if l <> [] then (F.fprintf oc "[" ;print_type_list_pvs oc pvs_md l;
                     F.fprintf oc "]")

  and print_stack oc pvs_md stack = match stack with
    | [] -> ()
    | a::l' -> F.fprintf oc "(%a)"
               (print [] pvs_md) a;
               print_stack oc pvs_md l'
  in
  print [] pvs_md oc t

let rec prefix_of_te : te -> string list =
 fun te ->
  match te with ForallP (x, te') -> x :: prefix_of_te te' | Te _ -> []

let print_prenex_te_pvs : string -> F.formatter -> te -> unit =
 fun _  oc te ->
  let p = prefix_of_te te in
  match p with
  | [] -> ()
  | _ ->
      F.fprintf oc "[" ;
      print_string_type_list_pvs oc p ;
      F.fprintf oc "]"

let rec print_te_pvs : string -> F.formatter -> te -> unit =
 fun pvs_md oc te ->
  match te with
  | ForallP (_, te') -> print_te_pvs pvs_md oc te'
  | Te te' -> print__te_pvs pvs_md oc te'

let conclusion_pvs : proof -> te =
 fun prf ->
  let j =
    match prf with
    | Assume(j,_) -> j
    | Lemma (_, j) -> j
    | Conv (j, _, _) -> j
    | ImplE (j, _, _) -> j
    | ImplI (j, _, _) -> j
    | ForallE (j, _, _te) -> j
    | ForallI (j, _, _) -> j
    | ForallPE (j, _, _ty) -> j
    | ForallPI (j, _, _) -> j
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

let print_name_list = fun pvs_md oc l ->
  let pp_sep = F.pp_print_space in
  let pp_quote fmt n =
    F.fprintf fmt "\"%a\"" (print_qualified_name pvs_md) n
  in
  F.pp_print_list ~pp_sep pp_quote oc l

let print_proof_pvs : string -> F.formatter -> proof -> unit =
  fun pvs_md oc prf ->
    let rec print acc pvs_md oc prf =
    match prf with
      | Assume(_,_)         -> F.fprintf oc "%%|- (propax)"
      | Lemma(n,_)    ->
         F.fprintf oc "%%|- (sttfa-lemma \"%a%a\")"
          (print_qualified_name pvs_md) n
          (print_type_list_b_pvs pvs_md) acc
    | Conv(_,p,trace)         ->
      let pc = conclusion_pvs p in
      F.fprintf oc "%%|- (sttfa-conv \"%a\" (%a) (%a)\n%a)"
        (print_te_pvs pvs_md) pc
        (print_name_list  pvs_md) (listof trace.left)
        (print_name_list  pvs_md) (listof trace.right)
        (print acc pvs_md) p
    | ImplE(_,p,q)      ->
      let pc = conclusion_pvs p
      in let qc = conclusion_pvs q
      in
      F.fprintf oc "%%|- (sttfa-impl-e \"%a\" \"%a\"\n%a\n%a)"
        (print_te_pvs pvs_md) pc
        (print_te_pvs pvs_md) qc
        (print acc pvs_md) q
        (print acc pvs_md) p
    | ImplI(j,p,_)        ->
      let (a,b) = decompose_implication j.thm
      in F.fprintf oc "%%|- (sttfa-impl-i \"%a\" \"%a\"\n%a)"
        (print__te_pvs pvs_md) a (print__te_pvs pvs_md) b (print acc pvs_md) p

    | ForallE(_,p,_te)  ->
      let pc = conclusion_pvs p
      in F.fprintf oc "%%|- (sttfa-forall-e \"%a\" \"%a\"\n%a)"
        (print_te_pvs pvs_md) pc
        (print__te_pvs pvs_md) _te
        (print acc pvs_md) p
    | ForallI(_,p,n)      ->
      F.fprintf oc "%%|- (then%@ (sttfa-forall-i \"%s\")\n%a)"
        (sanitize_name_pvs n)
        (print acc pvs_md) p
    | ForallPE(_,p,_ty)   -> print (_ty::acc) pvs_md oc p
    | ForallPI(_,p,_)     -> print acc pvs_md oc p
  in
  print [] pvs_md oc prf;
  F.fprintf oc "\n"

let print_item oc pvs_md it =
  let line fmt = line oc fmt in
  match it with
  | TypeDecl (op, ar) ->
    assert (ar = 0);
    F.fprintf oc "%a : TYPE+" (print_name pvs_md) op;
    line "";
    line ""
  | TypeDef _ -> failwith "[PVS] Type definitions not handled right now"
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

(** [print_alignment_item fmt m it] prints item [it] of module [m] for
    alignment. *)
let print_alignment_item: F.formatter -> string -> item -> unit =
  fun fmt pvs_md it ->
  let prel () = F.fprintf fmt "%%%% " in
  match it with
  | TypeDecl(o,_) ->
    prel ();
    F.fprintf fmt "@[%a := ...@]" (print_name pvs_md) o
  | Parameter(n,y) ->
    prel ();
    F.fprintf fmt "@[%a%a := ...@]"
      (print_name pvs_md) n
      (print_prenex_ty_pvs pvs_md) y
  | _             -> ()

let print_dep oc x =
  line oc "IMPORTING %s_sttfa AS %s_sttfa_th" x x

(** Printing dependencies for concept alignment theory. *)
let print_dep_al oc x =
  line oc "IMPORTING %s_pvs" x

let remove_transitive_deps mdeps deps =
  let remove_dep dep deps =
    let md = Basic.mk_mident dep in
    let deps_from_signature md =
      let deps = Signature.get_md_deps Basic.dloc md in
      (QSet.of_list (List.map Basic.string_of_mident deps))
    in
    let md_deps =
      match mdeps with
      | None ->  deps_from_signature md
      | Some l ->
        if List.mem_assoc (Basic.string_of_mident md) l then
          List.assoc (Basic.string_of_mident md) l
        else
          deps_from_signature md
    in
    QSet.diff deps md_deps
  in
  QSet.fold remove_dep deps deps

(** [print_alignment fmt md deps] prints the alignment import in the theory with
    [md] the (Dedukti) module, [deps] the [QSet] of dependencies. *)
let print_alignment : F.formatter -> string -> QSet.t -> item list ->
  unit = fun fmt md deps items ->
  let uninterpreted = function
    | Parameter(_) | TypeDecl(_) -> true
    | _                          -> false
  in
  let pp_deps fmt deps =
    let pp_dep fmt d = F.fprintf fmt "@[%s_sttfa_th := %s_pvs@]" d d in
    let pp_sep fmt () = F.fprintf fmt ",@," in
    F.pp_print_list ~pp_sep pp_dep fmt (QSet.to_seq deps |> List.of_seq)
  in
  let pp_its fmt its =
    let pp_it fmt it = (* Type assignment *)
      F.fprintf fmt "@[%a@]" (fun f -> print_alignment_item f md) it
    in
    let pp_sep fmt () = F.fprintf fmt ",@," in
    F.pp_print_list ~pp_sep pp_it fmt its
  in
  F.fprintf fmt "IMPORTING %s_sttfa {{@[<v 2>  " md;
  F.fprintf fmt "%a@," pp_deps deps;
  F.fprintf fmt "@[%% Type and definition assignments@]@,";
  F.fprintf fmt "%a" pp_its (List.filter uninterpreted items);
  F.fprintf fmt "@]@,}}@\n"

let print_ast : F.formatter -> ?mdeps:mdeps -> ast -> unit =
  fun oc ?mdeps ast ->
  current_module := ast.md;
  let deps = remove_transitive_deps mdeps ast.dep in
  (* Actual theory *)
  line oc "%s_sttfa : THEORY" ast.md;
  line oc "BEGIN";
  QSet.iter (print_dep oc) deps;
  line oc "";
  List.iter (print_item oc ast.md) ast.items;
  line oc "END %s_sttfa" ast.md;
  (* Concept alignment theory *)
  line oc "%s_pvs : THEORY" ast.md;
  line oc "BEGIN";
  QSet.iter (print_dep_al oc) deps;
  print_alignment oc ast.md deps ast.items;
  line oc "END %s_pvs@\n@." ast.md

let to_string fmt = F.asprintf "%a" fmt

let string_of_item = function
  | Parameter((md,id),ty) ->
    F.asprintf "%a %a : %a"
      (print_name md) (md,id)
      (print_prenex_ty_pvs md) ty
      (print_ty_pvs md) ty
  | Definition((md,id),ty,te) ->
    F.asprintf "%a %a : %a = %a"
      (print_name md) (md,id)
      (print_prenex_ty_pvs md) ty
      (print_ty_pvs md) ty
      (print_te_pvs md) te
  | Axiom((md,id),te) ->
    F.asprintf "%a %a : AXIOM %a"
      (print_name md) (md,id)
      (print_prenex_te_pvs md) te
      (print_te_pvs md) te
  | Theorem((md,id),te,_) ->
    F.asprintf "%a %a : LEMMA %a"
      (print_name md) (md,id)
      (print_prenex_te_pvs md) te
      (print_te_pvs md) te
  | TypeDecl((md,id),arity) ->
    assert(arity = 0);
    F.asprintf "%a : TYPE+" (print_name md) (md,id)
  | TypeDef _ -> failwith "[PVS] Type definitions not handled right now"

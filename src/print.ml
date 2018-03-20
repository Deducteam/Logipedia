open Ast

let current_module : string ref = ref ""

let print_name : out_channel -> name -> unit = fun oc (m,n) ->
  output_string oc (if m = !current_module then n else m ^ "." ^ n)

let print__ty : out_channel -> _ty -> unit =
  let rec print is_atom oc _ty =
    match _ty with
    | TyVar(x)   ->
        output_string oc x
    | Arrow(_,_) when is_atom ->
        Printf.fprintf oc "(%a)" (print false) _ty
    | Arrow(a,b) ->
        Printf.fprintf oc "%a → %a" (print true) a (print false) b
    | TyOp(op,l) ->
        print_name oc op;
        List.iter (Printf.fprintf oc " %a" (print true)) l
    | Prop       ->
        output_string oc "ο"
  in
  print false

let rec print_ty : out_channel -> ty -> unit = fun oc ty ->
  match ty with
  | ForallK(x,ty) -> Printf.fprintf oc "∀%s.%a" x print_ty ty
  | Ty(_ty)       -> print__ty oc _ty

let print__te : out_channel -> _te -> unit =
  let rec print is_atom oc _te =
    match _te with
    | TeVar(x)      ->
        output_string oc x
    | Abs(x,a,t)    ->
        Printf.fprintf oc "λ%s:%a.%a" x print__ty a (print false) t
    | App(_,_) when is_atom ->
        Printf.fprintf oc "(%a)" (print false) _te
    | App(t,u)      ->
        Printf.fprintf oc "%a %a" (print false) t (print true) u
    | Forall(x,a,t) ->
        Printf.fprintf oc "∀%s:%a.%a" x print__ty a (print false) t
    | Impl(t,u)     ->
        Printf.fprintf oc "(%a ⇒ %a)" (print false) t (print false) u
    | AbsTy(x,t)    ->
        Printf.fprintf oc "Λ%s.%a" x (print false) t
    | Cst(c,l)      ->
        print_name oc c;
        List.iter (Printf.fprintf oc " %a" print__ty) l
  in
  print false

let rec print_te : out_channel -> te -> unit = fun oc te ->
  match te with
  | ForallP(x,te) -> Printf.fprintf oc "∀%s.%a" x print_te te
  | Te(_te)       -> print__te oc _te

let print_te_ctx : out_channel -> te_ctx -> unit = fun oc ctx ->
  match ctx with
  | []         -> ()
  | [(x,_ty)]  ->
      Printf.fprintf oc "%s : %a " x print__ty _ty
  | (x,_ty)::l ->
      Printf.fprintf oc "%s : %a" x print__ty _ty;
      List.iter (fun (x,a) -> Printf.fprintf oc "; %s : %a" x print__ty a) l;
      Printf.fprintf oc " "

let print_judgment : out_channel -> judgment -> unit = fun oc j ->
  Printf.fprintf oc "%a⊢ %a" print_te_ctx j.te print_te j.thm

let print_proof : out_channel -> proof -> unit = fun oc prf ->
  Printf.fprintf oc "Proof:\n%!";
  let rec print off oc prf =
    match prf with
    | Assume(j)         ->
        Printf.fprintf oc "%sAssume   %a\n" off print_judgment j
    | Lemma(j)          ->
        Printf.fprintf oc "%sLemma    %a\n" off print_judgment j
    | Conv(j,p)         ->
        Printf.fprintf oc "%sConv     %a\n" off print_judgment j;
        print (off ^ " ") oc p
    | ImplE(j,p,q)      ->
        Printf.fprintf oc "%sImplE    %a\n" off print_judgment j;
        print (off ^ " ") oc p;
        print (off ^ " ") oc q
    | ImplI(j,p)        ->
        Printf.fprintf oc "%sImplI    %a\n" off print_judgment j;
        print (off ^ " ") oc p
    | ForallE(j,p,_te)  ->
        Printf.fprintf oc "%sForallI  %a\n" off print_judgment j;
        print (off ^ " ") oc p;
        Printf.fprintf oc "%s%a\n" off print__te _te
    | ForallI(j,p)      ->
        Printf.fprintf oc "%sForallI  %a\n" off print_judgment j;
        print (off ^ " ") oc p
    | ForallPE(j,p,_ty) ->
        Printf.fprintf oc "%sForallPE %a\n" off print_judgment j;
        print (off ^ " ") oc p;
        Printf.fprintf oc "%s%a\n" off print__ty _ty
    | ForallPI(j,p)     ->
        Printf.fprintf oc "%sForallPI %a\n" off print_judgment j;
        print (off ^ " ") oc p
  in
  print "  " oc prf;
  Printf.fprintf oc "QED\n%!"

let print_item : out_channel -> item -> unit = fun oc it ->
  let out fmt = Printf.fprintf oc fmt in
  match it with
  | TyOpDef(op,ar)      ->
      out "%a has arity %i\n%!" print_name op ar
  | Parameter(n,ty)     ->
      out "%a : %a\n%!" print_name n print_ty ty
  | Definition(n,ty,te) ->
      out "%a : %a := %a\n%!" print_name n print_ty ty print_te te
  | Axiom(n,te)         ->
      out "Axiom %a := %a\n%!" print_name n print_te te
  | Theorem(n,te,prf)   ->
      out "Theorem %a := %a\n" print_name n print_te te;
      print_proof oc prf

let print_ast : out_channel -> ast -> unit = fun oc ast ->
  List.iter (print_item oc) ast

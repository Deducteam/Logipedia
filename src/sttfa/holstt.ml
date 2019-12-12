module BasicHol = struct

  (* Defining basic hol structures: terms, types, proofs *)
  (* Inspired from HOL Light fusion files *)

  type tyOp = string
  type const = string

  type hol_type =
      Tyvar of string
    | Tyapp of tyOp * hol_type list

  let dummy_type = Tyvar("dummy")

  type term =
      Var of string * hol_type
    | Const of const * hol_type
    | Comb of term * term
    | Abs of term * term

  let dummy_term = Var("dummy",dummy_type)

  type hyp = term list

  type proof =
      Axiom_proof of term
    | Thm of string
    | Rule of string * term list * proof list
    | Refl_proof of term
    | Sym_proof of proof
    | Trans_proof of proof * proof
    | Mk_comb_proof of proof * proof
    | Abs_proof of term * proof
    | Beta_conv_proof of term
    | Assume_proof of term
    | Eq_mp_proof of proof * proof
    | Deduct_antisym_rule_proof of proof * proof
    | Prove_hyp_proof of proof * proof
    | Subst_proof of ((hol_type * hol_type) list * (term * term) list) * proof
    | New_basic_definition_proof of string * string * term
    | New_basic_type_definition_proof_left of string * (string * string) * proof
    | New_basic_type_definition_proof_right of string * (string * string) * proof

  let dummy_proof = Axiom_proof(dummy_term)

  type thm = Sequent of (string * hyp * term * proof)

  (* Handling names, namespaces, and basic names coming from sttfa *)

  let mk_name namespace name =
    let mk_namespace md = List.fold_left (fun s x -> s^x^".") "" md in
    (mk_namespace namespace)^name

  let arrow_name () = mk_name [] "->"

  let bool_name () = mk_name [] "bool"

  let equal_name () = mk_name [] "="

  let impl_name () = mk_name [] "==>"

  let forall_name () = mk_name [] "!"

  (* Building hol types *)

  let mk_tyOp name =
    name

  let arrow_tyOp () = mk_tyOp (arrow_name ())

  let bool_tyOp () = mk_tyOp (bool_name ())

  let mk_varType name =
    Tyvar(name)

  let mk_list l =
    l

  let ty_of_tyOp (tyop: tyOp) (tys:hol_type list) : hol_type =
    Tyapp (tyop,tys)

  let mk_arrow_type tyl tyr : hol_type =
    ty_of_tyOp (arrow_tyOp ()) [tyl;tyr]

  let mk_bool_type () : hol_type =
    ty_of_tyOp (bool_tyOp ()) []

  let mk_equal_type ty : hol_type =
    mk_arrow_type ty (mk_arrow_type ty (mk_bool_type ()))

  let mk_impl_type () =
    mk_arrow_type (mk_bool_type ()) (mk_arrow_type (mk_bool_type ()) (mk_bool_type()))

  let mk_forall_type ty =
    mk_arrow_type (mk_arrow_type ty (mk_bool_type ())) (mk_bool_type ())

  (* Building hol terms *)
  let mk_var name ty : term =
    Var (name,ty)

  let mk_var_term v = v

  let mk_app_term f t : term =
    Comb (f,t)

  let mk_abs_term v t : term =
    Abs (v,t)

  let term_of_const cst ty : term =
    Const (cst,ty)

 let const_of_name name = name

  let mk_equal_term left right ty =
    let ty = mk_equal_type ty in
    let cst = term_of_const (const_of_name (equal_name ())) ty in
    mk_app_term (mk_app_term cst left) right

  (* Building hol proofs *)
  let mk_refl term =
    Refl_proof(term)

  let mk_appThm pil pir =
    Mk_comb_proof(pil,pir)

  let mk_absThm var pi =
    Abs_proof(var,pi)

  let mk_sym pi =
    Sym_proof(pi)

  let mk_betaConv term =
    Beta_conv_proof(term)

  let mk_trans pil pir =
    Trans_proof(pil,pir)

  let mk_subst tylist tlist pi =
    Subst_proof((tylist,tlist),pi)

  let mk_eqMp pil pir =
    Eq_mp_proof(pir,pil)

  let mk_proveHyp pil pir =
    Prove_hyp_proof(pil,pir)

  let mk_assume term =
    Assume_proof(term)

  let mk_deductAntiSym pil pir =
    Deduct_antisym_rule_proof(pil,pir)

  let true_name () = mk_name [] "T"

  let and_name () = mk_name [] "/\\\\"


  let mk_true_type () = mk_bool_type ()

  let mk_and_type () = mk_arrow_type
      (mk_bool_type ()) (mk_arrow_type (mk_bool_type ()) (mk_bool_type()))

  let mk_true_term () = term_of_const (const_of_name (true_name ())) (mk_true_type ())

  let mk_and () = term_of_const (const_of_name (and_name ())) (mk_and_type ())

  let mk_and_term left right =
    mk_app_term (mk_app_term (mk_and ()) left) right

  let mk_impl () = term_of_const (const_of_name (impl_name ())) (mk_impl_type ())

  let mk_impl_term left right =
    mk_app_term (mk_app_term (mk_impl ()) left) right

  let mk_forall ty = term_of_const (const_of_name (forall_name ())) (mk_forall_type ty)

  let mk_forall_term f ty =
    mk_app_term (mk_forall ty) f

  let mk_axiom_true () = (*Sequent("TRUTH",[],mk_true_term (),*) Thm("TRUTH")

  (* Define conjunction *)
  let mk_axiom_and_ () = Thm("AND_DEF")

  (* proof of tl'=tr' with tl ->B tl' et tr ->B tr' and thm a proof of tl = tr *)
  let beta_equal thm tl tr =
    let left = mk_sym (mk_betaConv tl) in
    let right = mk_betaConv tr in
    let trans = mk_trans left thm in
    mk_trans trans right

  (* Apply a beta-conversion to the right-hand side/left-hand side of a binary op *)
  let beta_right pi = Rule("CONV_RULE (RAND_CONV BETA_CONV)",[],[pi])

  let beta_left pi = Rule("CONV_RULE (LAND_CONV BETA_CONV)",[],[pi])

  (* Beta-reduce right-hand side of the definition of and *)
  let mk_axiom_and () = beta_right (beta_right (mk_axiom_and_ ()))

  (* Same for implication *)
  let mk_axiom_impl_ () = Thm("IMP_DEF")

  let mk_axiom_impl () = beta_right (beta_right (mk_axiom_impl_ ()))

  (* Same for forall *)
  let mk_axiom_forall_ () = Thm("FORALL_DEF")

  let mk_axiom_forall () = beta_right (beta_right (mk_axiom_forall_ ()))

  (* Natural deduction rules *)
  let mk_rule_intro_forall name ty pi = Rule("GEN",[mk_var name ty],[pi])

  let mk_rule_elim_forall comment t pi = Rule(comment^"SPEC",[t],[pi])

  let proj_left pi = Rule("CONJUNCT1",[],[pi])

  let proj_right pi = Rule("CONJUNCT2",[],[pi])

  let mk_rule_intro_impl t pi = Rule("DISCH",[t],[pi])
  let mk_rule_elim_impl piimp pi = Rule("MP",[],[piimp;pi])

  let mk_impl_equal eqp eqq =
    Mk_comb_proof(Mk_comb_proof(Refl_proof(mk_impl ()),eqp),eqq)

  let mk_forall_equal eq ty =
    Mk_comb_proof(Refl_proof(mk_forall ty),eq)

  (* Prove beta-delta-conversion between terms *)
  let conv_proof comment t pi =
    Rule(comment^"CONV_CONV_rule",[t],[pi])

end


module PrintHol = struct
  include BasicHol

  (* Print HOL objects, recognizable by HOL Light parser*)

  let oc = ref Format.std_formatter
  let set_oc f = oc := f

  let rec print_list s d1 d2 print_object out = function
      [] -> ()
    | [obj] -> Format.fprintf out "%s%a%s" d1 print_object obj d2
    | hd::q ->
      Format.fprintf out "%s%a%s%s%a" d1 print_object hd d2 s (print_list s d1 d2 print_object) q

  (* Print HOL types *)
  let rec print_type_rec out = function
      Tyvar name -> Format.fprintf out "%s" name
    | Tyapp (tyop,[]) -> Format.fprintf out "%s" tyop
    | Tyapp (tyop,tylist)
        when tyop = "->" -> Format.fprintf out "%a -> %a"
                               print_type_atomic (List.hd tylist)
                               print_type_atomic (List.hd (List.tl tylist))
    | Tyapp (tyop,tylist) -> Format.fprintf out "%s %a"
                               tyop
                               (print_list " " "" "" print_type_atomic)
                               tylist

  and print_type_atomic out t =
    match t with
      Tyapp(tyop,_) when tyop = "->" -> Format.fprintf out "(%a)" print_type_rec t
    | _ -> print_type_rec out t

  let print_type out t =
    Format.fprintf out "`:%a`" print_type_rec t

  (* Print HOL terms *)

  let print_var out = function
    | Var(name,ty) -> Format.fprintf out "%s : %a" name print_type_rec ty
    | _ -> failwith "Not a variable\n"

  let rec print_term_rec b out = function
      Var(name,ty) when b -> Format.fprintf out "%a" (print_term_atomic b) (Var(name,ty))
    | Var(name,_) -> Format.fprintf out "%s" name
    | Const(cst,_) -> Format.fprintf out "%s" cst
    | Comb(f,t) ->
      begin
      match f,t with
          Const("!",_),Abs(v,t) ->
          Format.fprintf out "! %a. %a" print_var v (print_term_atomic b) t
        |  Comb(Const(s,_),t1),_ when (s = "==>" || s = "/\\" || s = "=") ->
          Format.fprintf out "%a %s %a" (print_term_atomic b) t1 s (print_term_atomic b) t
        | _ ->
          Format.fprintf out "%a %a" (print_term_atomic b) f (print_term_atomic b) t
    end
    | Abs(v,t) -> Format.fprintf out "\\ %a. %a" print_var v (print_term_atomic b) t

  and print_term_atomic b out t = match t with
      Comb _ | Abs _ -> Format.fprintf out "(%a)" (print_term_rec b) t
    | Var _ when b ->
      Format.fprintf out "(%a)" print_var t
    | _ -> print_term_rec b out t

  let print_term b out t =
    Format.fprintf out "`%a`" (print_term_rec b) t

  (* Print HOL proofs *)
  (* Observation: no "proof" object in HOL Light! Here: using proof squeleton to build HOL Light theorems*)
  let rec print_proof_rec out =
    let print_term_proof = print_term true in
    function
      Axiom_proof(t) ->
      Format.fprintf out "@[new_axiom %a@]@," print_term_proof t
    | Thm(th) ->
      Format.fprintf out "@[%s@]@," th
    | Rule (rule,tlist,pilist) ->
      Format.fprintf out "@[<hov>%s %a @,@[<hov>%a@]@,@]@,"
        rule
        (print_list " " "" "" print_term_proof) tlist
        (print_list " " "(" ")" print_proof_rec) pilist
    | Refl_proof(t) ->
      Format.fprintf out "@[<hov>REFL %a@]@," print_term_proof t
    | Sym_proof(pi) ->
      Format.fprintf out "@[<hov>SYM (%a)@]@," print_proof_rec pi
    | Trans_proof(pi1,pi2) ->
      Format.fprintf out "@;@[<hov>TRANS @[<hov>(%a)@]@, @[<hov>(%a)@]@,@]@," print_proof_rec pi1 print_proof_rec pi2
    | Mk_comb_proof(pi1,pi2) ->
      Format.fprintf out "@\n@[<hov>MK_COMB (@[%a@]@,,@[%a@]@,)@]@," print_proof_rec pi1 print_proof_rec pi2
    | Abs_proof(t,pi) ->
      Format.fprintf out "@[<hov>ABS %a @[(%a)@]@,@]@," print_term_proof t print_proof_rec pi
    | Beta_conv_proof(t) ->
      Format.fprintf out "@[<hov>BETA_CONV %a@]@," print_term_proof t
    | Assume_proof(t) ->
      Format.fprintf out "@[<hov>ASSUME %a@]@," print_term_proof t
    | Eq_mp_proof(pi1,pi2) ->
      Format.fprintf out "@[<hov>EQ_MP @[<hov>(%a)@]@, @[<hov>(%a)@]@,@]@," print_proof_rec pi1 print_proof_rec pi2
    | Deduct_antisym_rule_proof(pi1,pi2) ->
      Format.fprintf out "@[<hov>DEDUCT_ANTISYM_RULE @[<hov>(%a)@]@, @[<hov>(%a)@]@,@]@," print_proof_rec pi1 print_proof_rec pi2
    | Prove_hyp_proof(pi1,pi2) ->
      Format.fprintf out "@[<hov>PROVE_HYP @[<hov>(%a)@]@, @[<hov>(%a)@]@,@]@," print_proof_rec pi1 print_proof_rec pi2
    | Subst_proof(lists,pi) ->
      let tylist,tlist = lists in
      let print_couple_rev print_object out pair =
        let (x,y)=pair in
        Format.fprintf out "(%a,%a)" print_object y print_object x in
      Format.fprintf out "@[<hov>PINST [%a] [%a] @[<hov>(%a)@]@,@]@,"
        (print_list ";" "" "" (print_couple_rev print_type)) tylist
        (print_list ";" "" "" (print_couple_rev print_term_proof)) tlist
        print_proof_rec pi
     (* Basic definitions must stand by themselves *)
    | New_basic_definition_proof(thm_name,name,t) ->
      Format.fprintf out "@[<hov>new_basic_definition %a@]@,;;\n\n" print_term_proof t;
      Format.fprintf out "let _ = (new_defs := (\"%s\",%s)::!new_defs)" name thm_name
    | New_basic_type_definition_proof_left(s,(s1,s2),pi) ->
      Format.fprintf out "@[<hov>fst (new_basic_type_definition (%s) (%s,%s) (%a))@]@," s s1 s2 print_proof_rec pi
    | New_basic_type_definition_proof_right(s,(s1,s2),pi) ->
      Format.fprintf out "@[<hov>snd (new_basic_type_definition (%s) (%s,%s) (%a))@]@," s s1 s2 print_proof_rec pi

  (* Print statement of theorems (not useful to build them, but useful to visualize them)*)
  let print_thm_debug out = function
      Sequent(name,hyps,t,_) ->
      Format.fprintf out "(*%s : %a |- %a*)\n"
        name
        (print_list ";" "" "" (print_term false)) hyps
        (print_term false) t

  (* Define whole theorem *)
  let print_thm out = function
      Sequent(name,_,_,pi) -> Format.fprintf out "let %s =@\n@[<v 1>@,%a@];;\n\n" name print_proof_rec pi

  let print_parameter name ty out =
    Format.fprintf out "new_constant(\"%s\",%a);;\n\n" name print_type ty
end


module HolSTT = struct
  include PrintHol

  (** Keeping tracks of previous definitions/theorems/etc *)
  let defined_csts = Hashtbl.create 100

  let declared_axioms = Hashtbl.create 100

  let defined_thms = Hashtbl.create 300

  let declared_types = Hashtbl.create 100

  let reset () =
    Hashtbl.reset defined_thms;
    Hashtbl.reset defined_csts;
    Hashtbl.reset declared_axioms

  (** Find type variables in a type*)

  module VarsT = Set.Make(String)

  let rec free_vartypes vars = function
    | Tyvar(v) -> VarsT.add v vars
    | Tyapp(_,l) -> List.fold_left
                      (fun setvars -> fun ty -> VarsT.union setvars (free_vartypes vars ty))
                      vars
                      l

  (* Handle hypotheses as implications, notably for axioms which accept none *)

  let mk_hyp ts : hyp = mk_list ts

  let mk_imps (hyps:hyp) (t:term) =
    List.fold_right
      (fun x -> fun y ->
         mk_app_term
           (mk_app_term
              (term_of_const (impl_name ()) (mk_impl_type ()))
              x)
           y)
      hyps t

  (** Printing axioms *)
  let mk_axiom name hyps term =
    let imp_term = mk_imps hyps term in
    Hashtbl.add declared_axioms name (List.length hyps);
    Sequent(name,[],imp_term,Axiom_proof(imp_term))

  (** Printing a comment, useful for debugging *)
  let mk_comment comment =
    Format.fprintf !oc "------> comment : %s\n" comment

  (** Printing types *)
  let mk_type name arity =
    Format.fprintf !oc "new_type(\"%s\",%i);;\n\n" name arity;
    Hashtbl.add declared_types name arity

  (** Printing deifnitions *)
  let mk_def thm_name name eqt (ty:hol_type) =
    let thm_def = Sequent(name,[],eqt,New_basic_definition_proof(thm_name,name,eqt)) in
    Hashtbl.add defined_csts name (thm_def,ty);
    thm_def

  (** Finding the definition theorem associated with a constant *)
  let thm_of_const_name name =
    if Hashtbl.mem defined_csts name then
      (fst @@ Hashtbl.find defined_csts name)
    else
      failwith (Format.sprintf "Const %s not found" name)

  (** Printing theorems *)
  let mk_thm name term hyp pi =
    let thm = Sequent(name,hyp,term,pi) in
    Hashtbl.add defined_thms name thm;
    print_thm_debug !oc thm;
    print_thm !oc thm

  (** Printing parameters *)
  let mk_parameter name ty =
    print_parameter name ty !oc

  (** Finding theorem using its name *)
  let thm_of_lemma name =
    if Hashtbl.mem defined_thms name then
      Hashtbl.find defined_thms name
    else
      failwith (Format.sprintf "Theorem %s not found" name)

end

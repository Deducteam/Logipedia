open Ast

let rec print__ty fmt = function
    | TyVar x -> Format.fprintf fmt "%s" x
    | Arrow(left,right) -> Format.fprintf fmt "%a → %a" print__ty_wp left print__ty right
    | TyOp(tyOp,[]) -> Format.fprintf fmt "%s.%s" (fst tyOp) (snd tyOp)
    | TyOp _ -> failwith "Type Operator of arity >= 1 is not implemented but it should be"
    | Prop -> Format.fprintf fmt "ℙ"

  and print__ty_wp fmt _ty =
    match _ty with
    | TyVar _
    | Prop
    | TyOp _ -> print__ty fmt _ty
    | Arrow _ -> Format.fprintf fmt "(%a)" print__ty _ty
  let rec print_ty fmt = function
    | Ty _ty -> Format.fprintf fmt "%a" print__ty _ty
    | ForallK(var,ty) -> Format.fprintf fmt "∀ %s, %a" var print_ty ty
  let is_operator (_te:Ast._te) = (* Only binary operator right now *)
    match _te with
    | App(App(Cst _,_),_) -> true
    | _ -> false

  let uop = Hashtbl.create 11

  let bop = Hashtbl.create 11

  let top = Hashtbl.create 11

  let _ =
    Hashtbl.add uop ("connectives","Not") (fun x -> "¬"^x);
    Hashtbl.add uop ("fact","fact") (fun x -> "!"^x);
    Hashtbl.add uop ("nat","pred") (fun x -> x^"-1");
    Hashtbl.add uop ("nat","S") (fun x -> x^"+1");
    Hashtbl.add bop ("connectives","Or") (fun x y -> x^" ∨ "^y);
    Hashtbl.add bop ("connectives","And") (fun x y -> x^" ∧ "^y);
    Hashtbl.add bop ("logic","eq") (fun x y -> x^" = "^y);
    Hashtbl.add bop ("nat","plus") (fun x y -> x^" + "^y);
    Hashtbl.add bop ("nat","minus") (fun x y -> x^" - "^y);
    Hashtbl.add bop ("nat","times") (fun x y -> x^" × "^y);
    Hashtbl.add bop ("nat","lt") (fun x y -> x^" < "^y);
    Hashtbl.add bop ("nat","le") (fun x y -> x^" ≤ "^y);
    Hashtbl.add bop ("nat","eqb") (fun x y -> x^" = "^y);
    Hashtbl.add bop ("primes","divides") (fun x y -> x^" | "^y);
    Hashtbl.add bop ("exp","exp") (fun x y -> x^" ^ "^y);
    Hashtbl.add top ("cong","congruent") (fun x y z -> x^" ≡ "^y^" ["^z^"]");
    Hashtbl.add top ("bool","match_bool_type") (fun x y z -> "if "^z^" then "^x^" else "^y)

  let symbol_of_unary_operator cst =
    Hashtbl.find uop cst

  let symbol_of_binary_operator cst =
    Hashtbl.find bop cst

  let symbol_of_ternary_operator cst =
    Hashtbl.find top cst

  let rec is_integer t =
    match t with
    | Cst(cst,_) -> (fst cst) = "nat" && (snd cst) = "O"
    | App(Cst(cst,_),l) ->
      (fst cst) = "nat" && (snd cst) = "S" && is_integer l
    | _ -> false

  let rec to_integer t =
    match t with
    | Cst _ -> 0
    | App(_,l) -> 1+(to_integer l)
    | _ -> assert false

  let rec print__te fmt = function
    | TeVar x -> Format.fprintf fmt "%s" x
    | Abs(var,_, _te) -> Format.fprintf fmt "λ%s. %a" var print__te _te (*No need to print type *)
    | App(f,a) -> print_app fmt f a
    | Forall(var, _, _te) -> print_forall fmt [var] _te
    | Impl(l,r) -> Format.fprintf fmt "%a ⇒ %a" print__te l print__te r
    | AbsTy(_, _te) -> Format.fprintf fmt "%a" print__te _te (* Types are not printed *)
    | Cst(cst,_) -> Format.fprintf fmt "%s" (snd cst)

  and print_forall fmt vars _te =
    match _te with
    | Forall(var',_,_te') ->
      print_forall fmt (var'::vars) _te'
    | _ ->
      let vars = (List.rev vars) in
      Format.fprintf fmt "∀ %a, %a" (Basic.pp_list " " Format.pp_print_string) vars print__te _te

  and print__te_wp fmt _te =
    match _te with
    | TeVar _
    | Cst _
    | AbsTy _ -> print__te fmt _te
    | App _ when is_integer _te -> print__te fmt _te
    | _ -> Format.fprintf fmt "(%a)" print__te _te

  and print_app fmt f r =
    let to_integer t = string_of_int (to_integer t) in
    match f with
    | Cst(cst,_) ->
      if is_integer (App(f,r)) then
        Format.fprintf fmt "%s@." (to_integer (App(f,r)))
      else
        begin
          try
            let r' = Format.asprintf "%a" print__te_wp r in
            Format.fprintf fmt "%s" (symbol_of_unary_operator cst r');
          with Not_found ->
            Format.fprintf fmt "%s %a" (snd cst) print__te_wp r
        end
    | App(Cst(cst,_),l) ->
      begin
        try
          let l' = Format.asprintf "%a" print__te_wp l in
          let r' = Format.asprintf "%a" print__te_wp r in
          Format.fprintf fmt "%s" (symbol_of_binary_operator cst l' r')
        with _ ->
          Format.fprintf fmt "%s %a %a" (snd cst) print__te_wp l print__te_wp r
      end
    | App(App(Cst(cst,_),l),m) ->
      begin
        try
          let l' = Format.asprintf "%a" print__te_wp l in
          let m' = Format.asprintf "%a" print__te_wp m in
          let r' = Format.asprintf "%a" print__te_wp r in
          Format.fprintf fmt "%s" (symbol_of_ternary_operator cst l' m' r')
        with _ ->
          Format.fprintf fmt "%s %a %a %a" (snd cst) print__te_wp l print__te_wp m print__te_wp r
      end
    | _ ->
      Format.fprintf fmt "%a %a" print__te f print__te_wp r

  let te_of_hyp j proof =
    let j' = judgment_of proof in
    let hyp = TeSet.diff j'.hyp j.hyp in
    assert (TeSet.cardinal hyp = 1);
    snd @@ (TeSet.choose hyp)

  let rec print_te fmt = function
    | Te _te -> Format.fprintf fmt "%a" print__te _te
    | ForallP(_,te) -> Format.fprintf fmt "%a" print_te te


  let pretty_print_item = function
    | Parameter(_, ty) ->
      Format.asprintf "%a" print_ty ty
    | TypeDecl(_,arity) ->
      Format.asprintf "%d" arity
    | TypeDef(_,_,ty) ->
      Format.asprintf "%a" print_ty ty
    | Definition(_,_,te) ->
      Format.asprintf "%a" print_te te
    | Theorem(_,te,_) ->
      Format.asprintf "%a" print_te te
    | Axiom(_,te) ->
      Format.asprintf "%a" print_te te

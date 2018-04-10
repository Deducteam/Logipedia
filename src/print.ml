open Ast

let tex_mode : bool ref = ref false

let current_module : string ref = ref ""

let with_types = ref false

let forallp = "\\rotatebox[origin=c]{180}{\\ensuremath{\\mathcal{A}}}"

let sanitize_name : string -> string =
 fun n -> String.concat "\\_" (String.split_on_char '_' n)


let print_name : out_channel -> name -> unit =
 fun oc (m, n) ->
  let name = if m = !current_module then n else m ^ "." ^ n in
  let name = if !tex_mode then sanitize_name name else name in
  output_string oc name


let print__ty : out_channel -> _ty -> unit =
  let rec print is_atom oc _ty =
    match _ty with
    | TyVar x -> output_string oc x
    | Arrow (_, _) when is_atom -> Printf.fprintf oc "(%a)" (print false) _ty
    | Arrow (a, b) ->
        Printf.fprintf oc "%a → %a" (print true) a (print false) b
    | TyOp (op, l) ->
        print_name oc op ;
        List.iter (Printf.fprintf oc " %a" (print true)) l
    | Prop -> output_string oc "Prop"
  in
  print false


let rec print_ty : out_channel -> ty -> unit =
 fun oc ty ->
  match ty with
  | ForallK (x, ty) -> Printf.fprintf oc "∀%s.%a" x print_ty ty
  | Ty _ty -> print__ty oc _ty


let print__te : out_channel -> _te -> unit =
  let rec print is_atom oc _te =
    match _te with
    | TeVar x -> output_string oc x
    | Abs (x, a, t) ->
        if !with_types then
          Printf.fprintf oc "λ%s^{%a}.%a" x print__ty a (print false) t
        else Printf.fprintf oc "λ%s.%a" x (print false) t
    | App (_, _) when is_atom -> Printf.fprintf oc "(%a)" (print false) _te
    | App (t, u) ->
        let sep = if !tex_mode then "\\;" else " " in
        Printf.fprintf oc "%a%s%a" (print false) t sep (print true) u
    | Forall (x, a, t) ->
        if !with_types then
          Printf.fprintf oc "∀%s^{%a}.%a" x print__ty a (print false) t
        else Printf.fprintf oc "∀%s.%a" x (print false) t
    | Impl (t, u) ->
        Printf.fprintf oc "(%a ⇒ %a)" (print false) t (print false) u
    | AbsTy (x, t) -> Printf.fprintf oc "Λ%s.%a" x (print false) t
    | Cst (c, l) ->
        print_name oc c ;
        List.iter (Printf.fprintf oc " %a" print__ty) l
  in
  print false


let rec print_te : out_channel -> te -> unit =
 fun oc te ->
  match te with
  | ForallP (x, te) -> Printf.fprintf oc "%s%s.%a" forallp x print_te te
  | Te _te -> print__te oc _te


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
      if !with_types then Printf.fprintf oc "%s:%a" x print__ty _ty
      else Printf.fprintf oc "%s" x
  | (x, _ty) :: l ->
      if !with_types then (
        Printf.fprintf oc "%s:%a" x print__ty _ty ;
        List.iter (fun (x, _) -> Printf.fprintf oc ", %s:%a" x print__ty _ty) l ;
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
  | [x] -> Printf.fprintf oc "%a" print__te x
  | x :: l ->
      Printf.fprintf oc "%a" print__te x ;
      List.iter (fun x -> Printf.fprintf oc ", %a" print__te x) l ;
      Printf.fprintf oc " "


let print_judgment : out_channel -> judgment -> unit =
 fun oc j ->
  if !with_types then
    Printf.fprintf oc "%a; %a; %a ⊢ %a" print__ty_ctx (List.rev j.ty)
      print_te_ctx (List.rev j.te) print_hyp j.hyp print_te j.thm
  else
    Printf.fprintf oc "%a; %a ⊢ %a" print_te_ctx (List.rev j.te) print_hyp
      j.hyp print_te j.thm


let print_proof : out_channel -> proof -> unit =
 fun oc prf ->
  Printf.fprintf oc "Proof:\n%!" ;
  let rec print off oc prf =
    match prf with
    | Assume j -> Printf.fprintf oc "%sAssume   %a\n" off print_judgment j
    | Lemma (_, j) -> Printf.fprintf oc "%sLemma    %a\n" off print_judgment j
    | Conv (j, p) ->
        Printf.fprintf oc "%sConv     %a\n" off print_judgment j ;
        print (off ^ " ") oc p
    | ImplE (j, p, q) ->
        Printf.fprintf oc "%sImplE    %a\n" off print_judgment j ;
        print (off ^ " ") oc p ;
        print (off ^ " ") oc q
    | ImplI (j, p) ->
        Printf.fprintf oc "%sImplI    %a\n" off print_judgment j ;
        print (off ^ " ") oc p
    | ForallE (j, p, _te) ->
        Printf.fprintf oc "%sForallE  %a\n" off print_judgment j ;
        print (off ^ " ") oc p ;
        Printf.fprintf oc "%s%a\n" off print__te _te
    | ForallI (j, p, _) ->
        Printf.fprintf oc "%sForallI  %a\n" off print_judgment j ;
        print (off ^ " ") oc p
    | ForallPE (j, p, _ty) ->
        Printf.fprintf oc "%s%sE %a\n" off forallp print_judgment j ;
        print (off ^ " ") oc p ;
        Printf.fprintf oc "%s%a\n" off print__ty _ty
    | ForallPI (j, p, _) ->
        Printf.fprintf oc "%s%sI %a\n" off forallp print_judgment j ;
        print (off ^ " ") oc p
  in
  print "  " oc prf ;
  Printf.fprintf oc "QED\n%!"


let print_ast : out_channel -> ast -> unit =
 fun oc ast ->
  tex_mode := false ;
  let print_item oc it =
    let out fmt = Printf.fprintf oc fmt in
    match it with
    | TyOpDef (op, ar) -> out "%a has arity %i\n%!" print_name op ar
    | Parameter (n, ty) -> out "%a : %a\n%!" print_name n print_ty ty
    | Definition (n, ty, te) ->
        out "%a : %a := %a\n%!" print_name n print_ty ty print_te te
    | Axiom (n, te) -> out "Axiom %a := %a\n%!" print_name n print_te te
    | Theorem (n, te, prf) ->
        out "Theorem %a := %a\n" print_name n print_te te ;
        print_proof oc prf
  in
  List.iter (print_item oc) ast.items


(* Printing LaTeX. *)

let print_proof_tex : out_channel -> proof -> unit =
 fun oc prf ->
  let line fmt = Printf.fprintf oc (fmt ^^ "\n") in
  let rec print prf =
    let line fmt = Printf.fprintf oc ("  " ^^ fmt ^^ "\n") in
    match prf with
    | Assume j ->
        line "\\AxiomC{}" ;
        line "\\RightLabel{Assume}" ;
        line "\\UnaryInfC{$%a$}" print_judgment j
    | Lemma (_, j) ->
        line "\\AxiomC{}" ;
        line "\\RightLabel{Lemma}" ;
        line "\\UnaryInfC{$%a$}" print_judgment j
    | Conv (j, p) ->
        print p ;
        line "\\RightLabel{Conv}" ;
        line "\\UnaryInfC{$%a$}" print_judgment j
    | ImplE (j, p, q) ->
        print p ;
        print q ;
        line "\\RightLabel{$⇒_\\text{E}$}" ;
        line "\\BinaryInfC{$%a$}" print_judgment j
    | ImplI (j, p) ->
        print p ;
        line "\\RightLabel{$⇒_\\text{I}$}" ;
        line "\\UnaryInfC{$%a$}" print_judgment j
    | ForallE (j, p, _te) ->
        print p ;
        line "\\RightLabel{$∀_\\text{E}$}" ;
        line "\\UnaryInfC{$%a$}" print_judgment j
    | ForallI (j, p, _) ->
        print p ;
        line "\\RightLabel{$∀_\\text{I}$}" ;
        line "\\UnaryInfC{$%a$}" print_judgment j
    | ForallPE (j, p, _ty) ->
        print p ;
        line "\\RightLabel{$%s_\\text{E}$}" forallp ;
        line "\\UnaryInfC{$%a$}" print_judgment j
    | ForallPI (j, p, _) ->
        print p ;
        line "\\RightLabel{$%s_\\text{I}$}" forallp ;
        line "\\UnaryInfC{$%a$}" print_judgment j
  in
  line "\\begin{scprooftree}{0.2}" ;
  print prf ;
  line "\\end{scprooftree}"


let print_ast_tex : out_channel -> ast -> unit =
 fun oc ast ->
  tex_mode := true ;
  let line fmt = Printf.fprintf oc (fmt ^^ "\n") in
  let print_item it =
    match it with
    | TyOpDef (op, ar) ->
        line "\\noindent" ;
        line "The type \\texttt{%a} has $%i$ parameters." print_name op ar ;
        line ""
    | Parameter (n, ty) ->
        line "\\noindent" ;
        line "The constant \\texttt{%a} has type $%a$." print_name n print_ty
          ty ;
        line ""
    | Definition (n, ty, te) ->
        line "\\noindent" ;
        line "The symbol \\texttt{%a} of type $%a$ is defined as $%a$."
          print_name n print_ty ty print_te te ;
        line ""
    | Axiom (n, te) ->
        line "\\noindent" ;
        line "\\textbf{Axiom} \\texttt{%a}: $%a$." print_name n print_te te ;
        line ""
    | Theorem (n, te, prf) ->
        line "\\noindent" ;
        line "\\textbf{Theorem} \\texttt{%a}: $%a$." print_name n print_te te ;
        line "" ;
        line "\\noindent" ;
        line "\\textit{Proof:}" ;
        print_proof_tex oc prf ;
        line ""
  in
  line "\\documentclass{article}" ;
  (*
  line "\\setlength{\\pdfpagewidth}{1000pt}";
  line "\\setlength{\\paperwidth}{1000pt}";
  *)
  line "\\usepackage{fullpage}" ;
  line "\\usepackage{graphicx}" ;
  line "\\usepackage{bussproofs}" ;
  line "\\usepackage{amssymb}" ;
  line "\\usepackage{rotate}" ;
  line "\\usepackage{amsmath}" ;
  line "\\usepackage{amsthm}" ;
  line "\\usepackage[mathletters]{ucs}" ;
  line "\\usepackage[utf8x]{inputenc}" ;
  line "" ;
  line "\\newenvironment{scprooftree}[1]%%" ;
  line
    "  {\\gdef\\scalefactor{#1}\\begin{center}\\proofSkipAmount\\leavevmode}%%" ;
  line
    "  {\\scalebox{\\scalefactor}{\\DisplayProof}\\proofSkipAmount \
     \\end{center} }" ;
  line "" ;
  line "\\title{Generated document for module \\texttt{%s}}"
    (sanitize_name !current_module) ;
  line "\\date{}" ;
  line "" ;
  line "\\begin{document}" ;
  line "\\maketitle" ;
  line "" ;
  List.iter print_item ast.items ;
  line "\\end{document}"

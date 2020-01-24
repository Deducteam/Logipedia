open Parsing.Entry
open Kernel.Term
open Kernel.Basic
open Format
open Core

let enc = "cupicef"
let enc_md = mk_mident enc

let escape_underscore =
  let regexp_coq = Str.regexp "Coq__" in
  let regexp = Str.regexp "_" in
  fun s -> Str.global_replace regexp "\\_" (Str.global_replace regexp_coq "" s)

let pp_ident fmt id = fprintf fmt "%s"
    (escape_underscore (string_of_ident id))

let pp_mident fmt id = fprintf fmt "%s"
    (escape_underscore (string_of_mident id))

let dest_name name = string_of_mident (md name), string_of_ident (id name)


let enc_ident name = mident_eq (md name) enc_md

let enc_ident_eq name s = mident_eq (md name) enc_md && string_of_ident (id name) = s

let replace_list =
  [
    ("Coq__Init__Logic","and"), ("\\wedge", true)
  ; ("Coq__Init__Logic","or" ), ("\\vee", true)
  ; ("Coq__Init__Logic","iff" ), ("\\Leftrightarrow", true)
  ]

let pp_term curmod =

let pp_name fmt name =
  if mident_eq curmod (md name)
  then fprintf fmt    "%a"                     pp_ident (id name)
  else fprintf fmt "%a.%a" pp_mident (md name) pp_ident (id name) in

let rec pp_term fmt = function
  |      Const (_,name)         -> pp_const fmt (name, []     )
  | App (Const (_,name),a,args) -> pp_const fmt (name, a::args)

  | Kind -> assert false
  | Type _             -> fprintf fmt "\\mathbb{T}"
  | DB (_,id,_)        -> pp_ident fmt id
  | App (f,a,args)     -> pp_list "\\ " pp_term_wp fmt (f::a::args)
  | Lam (_,x,None  ,t) -> fprintf fmt "\\lambda %a.%a" pp_ident x pp_term t
  | Lam (_,x,Some a,t) -> fprintf fmt "\\lambda %a:%a.%a" pp_ident x pp_term_wp a pp_term t
  | Pi  (_,x,a,b)      -> pp_forall fmt (x,a,b)
and pp_forall fmt (x,a,b) =
  if Kernel.Subst.occurs 0 b
  then fprintf fmt "\\forall %a : %a, %a" pp_ident x pp_term_wp a pp_term b
  else fprintf fmt "%a \\rightarrow %a" pp_term_wp a pp_term b
and pp_term_wp fmt = function
  | Kind | Type _ | DB _ | Const _ as t -> pp_term fmt t
  | App (Const (_,name),a,args) -> pp_const_wp fmt (name, a::args)
  | t -> fprintf fmt "(%a)" pp_term t

and pp_const_wp fmt ((name,args) as c) =
  if enc_ident name
  then match string_of_ident (id name), args with
       (* Special cases, parentheses go on subterm (if necessary) *)
       | "Term" , [_; a]     -> pp_term_wp fmt a
       | "Univ" , [u]        -> pp_term_wp fmt u
       | "univ" , [u;_;_]    -> pp_term_wp fmt u
       | "cast",[_;_;_;_;_;t]-> pp_term_wp fmt t

       (* Parentheses non necessary *)
       | "eps"  , [_]
         | "Axiom", [_;_]
         | "Rule" , [_;_;_]
         | "Cumul", [_;_]
         | "type" , [_]
         | "max", [_;_] -> pp_const fmt c

       (* Adding parentheses *)
       | _ -> fprintf fmt "(%a)" pp_const c
  else        fprintf fmt "(%a)" pp_const c

and pp_const fmt (name, args) =
  if enc_ident name
  then match string_of_ident (id name), args with
  | "Sort" , []         -> fprintf fmt "\\mathbb{S}"
  | "sup"  , [s1; s2]   -> fprintf fmt "\\textbf{sup}(%a,%a)" pp_term s1 pp_term s2
  | "Univ" , [u]        -> pp_term fmt u
  | "Term" , [_; a]     -> pp_term fmt a
  | "Bool" , []         -> fprintf fmt "\\mathbb{B}"
  | "eps"  , [b]        -> fprintf fmt "[%a]" pp_term b
  | "true" , []         -> fprintf fmt "\\top"
  | "I"    , []         -> fprintf fmt "\\top"
  | "Axiom", [s1;s2]    -> fprintf fmt "\\mathcal{A}_{%a, %a}" pp_term s1 pp_term s2
  | "Rule" , [s1;s2;s3] -> fprintf fmt "\\mathcal{A}_{%a, %a, %a}"
                             pp_term s1 pp_term s2 pp_term s3
  | "Cumul", [s1;s2]    -> fprintf fmt "\\mathcal{C}_{%a, %a}" pp_term s1 pp_term s2
  | "Eq"   , [s1;s2]    -> fprintf fmt "%a = %a" pp_term s1 pp_term s2
  | "and"  , [c1;c2]    -> fprintf fmt "%a \\wedge %a" pp_term c1 pp_term c2
  | "univ" , [u;_;_]    -> pp_term fmt u
  | "prod" , [_;_;_;_;a;Lam (_,x,_,b)] -> pp_forall fmt (x, a, b)
  | "SubType",[_;_;a;b] -> fprintf fmt "%a \\preceq %a}" pp_term a pp_term b
  | "cast",[_;_;_;_;_;t]-> pp_term fmt t
  | "Nat", []           -> fprintf fmt "\\mathcal{L}"
  | "z", []             -> fprintf fmt "\\text{Set}"
  | "s", [l]            -> pp_lvl 0 fmt l
  | "prop", []          -> fprintf fmt "\\text{Prop}"
  | "type" , [Const (_,name)] when enc_ident_eq name "z" -> fprintf fmt "\\text{Set}"
  | "type" , [s]        -> fprintf fmt "\\text{Type}_{%a}" pp_term s
  | "set", []           -> fprintf fmt "\\text{Set}"
  | "type0", []         -> fprintf fmt "\\text{Type}_0"
  | "max", [l1;l2]      -> fprintf fmt "\\text{max}(%a,%a)" pp_term l1 pp_term l2

  | c, []               -> fprintf fmt "\\textbf{%s}" c
  | c, l                -> fprintf fmt "\\textbf{%s}\\ %a" c (pp_list "\\ " pp_term_wp) l
  else match List.assoc_opt (dest_name name) replace_list, args with
       | Some (s,true), [a;b] ->
          fprintf fmt "%a %s %a" pp_term_wp a s pp_term_wp b
       | Some (s,_), [] -> fprintf fmt "%s" s
       | Some (s,_), args ->
          fprintf fmt "(%s %a)" s (pp_list "\\ " pp_term_wp) args
       | None, [] -> pp_name fmt name
       | None, _ -> fprintf fmt "(%a %a)" pp_name name (pp_list "\\ " pp_term_wp) args

and pp_lvl n fmt = function
  | Const (_,name)
       when enc_ident_eq name "z" -> fprintf fmt "%i" n
  | App (Const (_,name),i,[])
       when enc_ident_eq name "s" -> pp_lvl (n+1) fmt i
  | c -> fprintf fmt "(%i + %a)" n pp_term c
in
pp_term


let pp_entry md fmt = function
  | Decl  (_,_,_, ty)
  | Def   (_,_, _,Some ty,_) -> pp_term md fmt ty
  | _ -> assert false

let export_to_string md e = asprintf "%a" (pp_entry md) e

let get_exporter target : (module Export.S) = match target with
  | Systems.Latex ->
     (module struct
        type ast = unit
        let target = Systems.Latex
        let compile _ _ = ()
        let decompile _ = assert false
        let export _ () = ()
      end)
  | sys -> Console.exit_with
             "Encoding cupicef doesn't support target: %s" (Systems.to_string sys)

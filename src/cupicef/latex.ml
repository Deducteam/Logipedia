open Parsing.Entry
open Kernel.Term
open Kernel.Basic
open Format
open Core

let enc_md = mk_mident "cupicef"

let escape_underscore =
  let regexp = Str.regexp "_" in
  fun s -> Str.global_replace regexp "\\_" s

let pp_ident fmt id = fprintf fmt "%s"
    (escape_underscore (Kernel.Basic.string_of_ident id))

let pp_mident fmt id = fprintf fmt "%s"
    (escape_underscore (Kernel.Basic.string_of_mident id))

let pp_name fmt name = fprintf fmt "%a.%a" pp_mident (md name) pp_ident (id name)

let rec pp_term fmt = function
  | Const (_,name) when mident_eq (md name) enc_md ->
    pp_const fmt (string_of_ident (id name)) []

  | App (Const (_,name),a,args) when mident_eq (md name) enc_md ->
    pp_const fmt (string_of_ident (id name)) (a::args)

  | Kind -> assert false
  | Type _             -> fprintf fmt "\\mathbb{T}"
  | DB (_,id,_)        -> pp_ident fmt id
  | Const (_,name)     -> pp_name fmt name
  | App (f,a,args)     -> pp_list "\\ " pp_term_wp fmt (f::a::args)
  | Lam (_,x,None  ,t) -> fprintf fmt "\\lambda %a.%a" pp_ident x pp_term t
  | Lam (_,x,Some a,t) -> fprintf fmt "\\lambda %a:%a.%a" pp_ident x pp_term_wp a pp_term t
  | Pi  (_,x,a,b)      ->
    if Kernel.Subst.occurs 0 b
    then fprintf fmt "\\Pi %a:%a.%a" pp_ident x pp_term_wp a pp_term b
    else fprintf fmt "%a \rightarrow %a" pp_term_wp a pp_term b

and pp_term_wp fmt = function
  | Kind | Type _ | DB _ | Const _ as t -> pp_term fmt t
  | t                                   -> fprintf fmt "(%a)" pp_term t

and pp_const fmt i args = match i, args with
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
  | "prod" , [_;_;_;_;a;Lam (_,x,_,b)]
                        -> fprintf fmt "\\Pi %a:%a.%a"     pp_ident x pp_term_wp a pp_term b
  | "cast" , [_;_;_;_;_;t] -> pp_term fmt t

  | "type" , [s]        -> fprintf fmt "\\text{Type}_{%a}" pp_term s
  | c, []               -> fprintf fmt "\\textbf{%s}" c
  | c, l                -> fprintf fmt "\\textbf{%s}\\ %a" c (pp_list "\\ " pp_term_wp) l

let pp_entry fmt = function
  | Decl  (_,_,_, ty)
  | Def   (_,_, _,Some ty,_) -> pp_term fmt ty
  | _ -> assert false

let export_to_string _ e = asprintf "$$%a$$" pp_entry e

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

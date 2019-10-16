open Extras
module B = Kernel.Basic

exception IllFormedUri of string

(** A uri of the form [protocol:path/name.extension]. *)
type t =
  { protocol : string
  ; path : string list
  ; name : string
  ; extension : string }

let to_string : t -> string = fun {protocol; path; name; extension} ->
  protocol ^ ":" ^ String.concat "/" path ^ "/" ^ name ^ "." ^ extension

let of_string s =
  let r_prot = Str.regexp "^[^:]+:" in
  let r_ext = Str.regexp "[^\\.]+$" in
  let r_path = Str.regexp ":.*/" in
  let r_nm = Str.regexp "/[^\\.]+" in
  let protocol =
    begin try
      ignore @@ Str.search_forward r_prot s 0
    with Not_found ->
      raise (IllFormedUri(Format.sprintf "Protocol in %s" s)) end;
    Str.matched_string s
  in
  let extension =
    begin try
      ignore @@ Str.search_forward r_ext s 0
    with Not_found ->
      raise (IllFormedUri(Format.sprintf "Extension in %s" s)) end;
    Str.matched_string s
  in
  let name =
    ignore @@ Str.search_forward r_nm s 0;
    (* Drop the first element *)
    String.drop (Str.matched_string s) 0
  in
  let path =
    ignore @@ Str.search_forward r_path s 0;
    let r_sep = Str.regexp "/" in
    let nocolon = String.drop (Str.matched_string s) 0 in
    Str.split r_sep nocolon
  in
  {protocol; path; name; extension}

let name_of_uri {path; name; _} =
  B.mk_name (B.mk_mident @@ String.concat "." path) (B.mk_ident name)

let uri_of_dkid : B.mident -> B.ident -> string -> string -> t =
  fun md id th tx ->
  { protocol = th ; path = [B.string_of_mident md]
  ; name = B.string_of_ident id ; extension = tx}

let of_dkname : B.name -> string -> string -> t = fun n th tx ->
  uri_of_dkid (B.md n) (B.id n) th tx

let ext_of_uri : t -> string = fun u -> u.extension

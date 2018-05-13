open Ast
open Basic
open Environ

(* TODO: Enhance error messages *)

module CType = Compile_type

let infer env _te =
  let tedk = Decompile.decompile__term env.dk _te in
  match Env.infer ~ctx:env.dk tedk with
  | OK ty -> CType.compile_type env ty
  | Err err -> Errors.fail_env_error err

let _infer env _te =
  let tedk = Decompile.decompile__term env.dk _te in
  match Env.infer ~ctx:env.dk tedk with
  | OK ty ->
    begin
      try
        CType.compile__type env ty
      with _ -> Errors.fail dloc "Inference failed because type is polymorphic"
    end
  | Err err -> Errors.fail_env_error err

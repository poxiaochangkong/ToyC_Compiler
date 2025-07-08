(* This module defines the types and structures used for symbol tables in a type checker *)
module StringMap = Map.Make(String)

type var_env = typ StringMap.t
type func_env = (typ list * typ) StringMap.t

type env = {
  vars: var_env;
  funcs: func_env;
}

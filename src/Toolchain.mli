(* Takes care of setting up toolchain: - selects package manager - installs
   dependencies for bsb project - ... *)

type t

val setup :
  env:string Js.Dict.t -> folder:string -> (t, string) result Js.Promise.t

val lsp : t -> string * string array

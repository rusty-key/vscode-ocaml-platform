type t

type resources

val setup :
  env:string Js.Dict.t -> folder:string -> (t, string) result Js.Promise.t

val lsp : t -> string * string array

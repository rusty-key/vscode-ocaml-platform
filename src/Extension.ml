open Bindings
module P = Js.Promise

let handleError f =
  P.then_ (function
    | Ok () -> P.resolve ()
    | Error msg -> f msg)

module Client = struct
  let make () : Vscode.LanguageClient.clientOptions =
    { documentSelector =
        [| { scheme = "file"; language = "ocaml" }
         ; { scheme = "file"; language = "reason" }
        |]
    }
end

module Server = struct
  let make (toolchain : Toolchain.t) : Vscode.LanguageClient.serverOptions =
    let command, args = Toolchain.lsp toolchain in
    { command; args; options = { env = Process.env } }
end

let activate _context =
  Js.Dict.set Process.env "OCAMLRUNPARAM" "b";
  Js.Dict.set Process.env "OCAML_LSP_SERVER_LOG" "-";
  Toolchain.setup ~env:Process.env ~folder:Workspace.rootPath
  |> P.then_ (function
       | Error msg -> P.resolve (Error msg)
       | Ok toolchain ->
         let serverOptions = Server.make toolchain in
         let client =
           LanguageClient.make ~id:"ocaml" ~name:"OCaml Language Server"
             ~serverOptions ~clientOptions:(Client.make ())
         in
         client.start ();
         P.resolve (Ok ()))
  |> handleError Window.showErrorMessage
  |> P.catch (fun e ->
         let message = Bindings.JsError.ofPromiseError e in
         Window.showErrorMessage {j|Error: $message|j})

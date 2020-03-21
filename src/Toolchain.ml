open Bindings
open Utils
module R = Result
module P = Js.Promise

type commandAndArgs = string * string array

let promptEsySetup ~f =
  Window.showQuickPick [| "yes"; "no" |]
    (Window.QuickPickOptions.make ~canPickMany:false
       ~placeHolder:{j|Setup this project's toolchain with 'esy'?|j} ())
  |> P.then_ (function
       | Some choice when choice = "yes" -> f ()
       | Some _
       | None ->
         P.resolve (Error "Please setup the toolchain"))

let setupWithProgressIndicator esyCmd ~envWithUnzip:esyEnv folder =
  Window.withProgress
    [%bs.obj
      { location = Window.locationToJs Window.Notification
      ; title = "Setting up toolchain..."
      }]
    (fun progress ->
      let succeeded = ref (Ok ()) in
      let eventEmitter = Setup.Bsb.make () in
      Setup.Bsb.onProgress eventEmitter (fun percent ->
          Js.log2 "Percentage:" percent;
          (progress.report
             [%bs.obj { increment = int_of_float (percent *. 100.) }] [@bs]));
      Setup.Bsb.onEnd eventEmitter (fun () ->
          (progress.report [%bs.obj { increment = 100 }] [@bs]));
      Setup.Bsb.onError eventEmitter (fun errorMsg ->
          succeeded := Error errorMsg);
      Setup.Bsb.run esyCmd esyEnv eventEmitter folder
      |> P.then_ (fun () -> P.resolve !succeeded))

module Binaries = struct
  let esy = "esy"

  let opam = "opam"
end

module PackageManager : sig
  type kind

  type t

  module Set : sig
    include Set.S with type elt = kind
  end

  val ofName : Fpath.t -> string -> (kind, string) result

  val toName : kind -> string

  val toString : kind -> string

  val specOfName :
       env:string Js.Dict.t
    -> name:string
    -> root:Fpath.t
    -> (t, string) result P.t

  val toSpec : env:string Js.Dict.t -> kind:kind -> (t, string) result P.t

  val setupToolChain : Fpath.t -> t -> (unit, string) result P.t

  val available :
    env:string Js.Dict.t -> root:Fpath.t -> (kind list, string) result P.t

  val env : t -> (string Js.Dict.t, string) result P.t

  val lsp : t -> commandAndArgs

  val find : string -> kind list -> kind option

  val makeEsy : Fpath.t -> kind

  val makeOpam : Fpath.t -> kind
end = struct
  type kind =
    | Opam of Fpath.t
    | Esy of Fpath.t
    | Global

  type t =
    { cmd : Cmd.t
    ; kind : kind
    }

  module Set = Set.Make (struct
    type t = kind

    let compare x y =
      match (x, y) with
      | Esy root1, Esy root2 -> Fpath.compare root1 root2
      | Opam root1, Opam root2 -> Fpath.compare root1 root2
      | Global, Global -> 0
      | Esy _, Opam _ -> 1
      | Esy _, Global -> -1
      | Opam _, _ -> -1
      | Global, _ -> 1
  end)

  let makeEsy root = Esy root

  let makeOpam root = Opam root

  let toName = function
    | Opam _ -> Binaries.opam
    | Esy _ -> Binaries.esy
    | Global -> "global"

  let ofName root = function
    | "opam" -> Ok (Opam root)
    | "esy" -> Ok (Esy root)
    | "global" -> Ok Global
    | n -> Error {j|Invalid name $n was provided|j}

  let toString = function
    | Esy root -> Printf.sprintf "esy(%s)" (Fpath.toString root)
    | Opam root -> Printf.sprintf "opam(%s)" (Fpath.toString root)
    | Global -> "global"

  let toCmdString = function
    | Opam _ -> "opam"
    | Esy _ -> "esy"
    | Global -> "bash"

  let toSpec ~env ~kind =
    Cmd.make ~env ~cmd:(toCmdString kind)
    |> P.then_ (function
         | Error msg -> P.resolve (Error msg)
         | Ok cmd -> P.resolve (Ok { cmd; kind }))

  (* TODO: probably not part of the module *)
  let specOfName ~env ~name ~root =
    match name with
    | x when x = Binaries.opam -> toSpec ~env ~kind:(makeOpam root)
    | x when x = Binaries.esy -> toSpec ~env ~kind:(makeEsy root)
    | x -> "Invalid package manager name: " ^ x |> R.fail |> P.resolve

  let available ~env ~root =
    let supportedPackageManagers =
      [ Esy root; Esy Fpath.(root / ".vscode" / "esy"); Opam root ]
    in
    supportedPackageManagers
    |> List.map (fun (pm : kind) ->
           let name = toName pm in
           Cmd.make ~env ~cmd:name
           |> P.then_ (function
                | Ok _ -> (pm, true) |> P.resolve
                | Error _ -> (pm, false) |> P.resolve))
    |> Array.of_list |> P.all
    |> P.then_ (fun r ->
           r |> Belt.List.fromArray
           |. Belt.List.keepMap (fun (pm, available) ->
                  if available then
                    Some pm
                  else
                    None)
           |> R.return |> P.resolve)

  (* TODO: not part of PM? *)
  let env spec =
    let { cmd; kind } = spec in
    match kind with
    | Global -> Process.env |> R.return |> P.resolve
    | Esy root ->
      Cmd.output cmd
        ~args:[| "command-env"; "--json"; "-P"; Fpath.toString root |]
        ~cwd:(Fpath.toString root)
      |> okThen (fun stdout ->
             match Json.parse stdout with
             | Some json ->
               json |> Json.Decode.dict Json.Decode.string |> R.return
             | None ->
               Error ("'esy command-env' returned non-json output: " ^ stdout))
    | Opam root ->
      Cmd.output cmd ~args:[| "exec"; "env" |] ~cwd:(Fpath.toString root)
      |> okThen (fun stdout ->
             stdout |> Js.String.split "\n"
             |> Js.Array.map (fun x -> Js.String.split "=" x)
             |> Js.Array.map (fun r ->
                    match Array.to_list r with
                    | [] ->
                      (* TODO Environment entries are not necessarily key value
                         pairs *)
                      Error "Splitting on '=' in env output failed"
                    | [ k; v ] -> Ok (k, v)
                    | l ->
                      Js.log l;
                      Error
                        "Splitting on '=' in env output returned more than two \
                         items")
             |> Js.Array.reduce
                  (fun acc kv ->
                    match kv with
                    | Ok kv -> kv :: acc
                    | Error msg ->
                      Js.log msg;
                      acc)
                  []
             |> Js.Dict.fromList |> R.return)

  let setupToolChain workspaceRoot spec =
    let { cmd; kind } = spec in
    match kind with
    | Global -> Ok () |> P.resolve
    | Esy root ->
      let rootStr = root |> Fpath.toString in
      Cmd.output cmd ~args:[| "status"; "-P"; rootStr |] ~cwd:rootStr
      |> P.then_ (fun r ->
             let r =
               match r with
               | Error _ -> R.return false
               | Ok stdout -> (
                 match Json.parse stdout with
                 | None -> R.return false
                 | Some json ->
                   json
                   |> Json.Decode.field "isProjectReadyForDev" Json.Decode.bool
                   |> R.return )
             in
             P.resolve r)
      |> P.then_ (function
           | Error e -> e |> R.fail |> P.resolve
           | Ok isProjectReadyForDev ->
             if isProjectReadyForDev then
               () |> R.return |> P.resolve
             else if Fpath.compare root workspaceRoot = 0 then (
               Js.log "esy project";
               promptEsySetup ~f:(fun () ->
                   Window.withProgress
                     [%bs.obj
                       { location = Window.locationToJs Window.Notification
                       ; title = "Setting up toolchain..."
                       }]
                     (fun progress ->
                       (progress.report
                          [%bs.obj { increment = int_of_float 1. }] [@bs]);
                       Cmd.output cmd ~cwd:(root |> Fpath.toString) ~args:[||]
                       |> P.then_ (fun _ ->
                              (progress.report
                                 [%bs.obj { increment = int_of_float 100. }]
                               [@bs]);
                              P.resolve (Ok ()))))
             ) else (
               Js.log "bsb project";
               setupWithProgressIndicator cmd ~envWithUnzip:Process.env
                 (root |> Fpath.toString)
             ))
    | Opam _ -> P.resolve (Ok ())

  let lsp spec =
    let { cmd; kind } = spec in
    match kind with
    | Opam _ -> (Cmd.binPath cmd, [| "exec"; "ocamllsp" |])
    | Esy root -> (Cmd.binPath cmd, [| "-P"; Fpath.toString root; "ocamllsp" |])
    | Global -> ("ocamllsp", [||])

  let rec find name = function
    | [] -> None
    | x :: xs -> (
      match x with
      | Global -> None
      | Esy root
      | Opam root -> (
        match ofName root name with
        | Ok y ->
          if compare x y = 0 then
            Some x
          else
            find name xs
        | Error _ -> None ) )
end

module Manifest : sig
  val lookup : Fpath.t -> (PackageManager.kind list, string) result P.t
end = struct
  let parse projectRoot = function
    | "esy.json" -> Some (PackageManager.makeEsy projectRoot) |> P.resolve
    | "opam" ->
      Fs.stat (Fpath.(projectRoot / "opam") |> Fpath.toString)
      |> P.then_ (fun r ->
             let r =
               match r with
               | Error _ -> None
               | Ok stats -> (
                 match Fs.Stat.isDirectory stats with
                 | true -> None
                 | false -> Some (PackageManager.makeOpam projectRoot) )
             in
             P.resolve r)
    | "package.json" ->
      let manifestFile = Fpath.(projectRoot / "package.json") |> Fpath.show in
      Fs.readFile manifestFile
      |> P.then_ (fun manifest ->
             match Json.parse manifest with
             | None -> None |> P.resolve
             | Some json ->
               if
                 Utils.propertyExists json "dependencies"
                 || Utils.propertyExists json "devDependencies"
               then
                 if Utils.propertyExists json "esy" then
                   Some (PackageManager.makeEsy projectRoot) |> P.resolve
                 else
                   Some
                     (PackageManager.makeEsy
                        Fpath.(projectRoot / ".vscode" / "esy"))
                   |> P.resolve
               else
                 None |> P.resolve)
    | file -> (
      let manifestFile = Fpath.(projectRoot / file) |> Fpath.show in
      match Path.extname file with
      | ".json" ->
        Fs.readFile manifestFile
        |> P.then_ (fun manifest ->
               match Json.parse manifest with
               | Some json ->
                 if
                   Utils.propertyExists json "dependencies"
                   || Utils.propertyExists json "devDependencies"
                 then
                   Some (PackageManager.makeEsy projectRoot) |> P.resolve
                 else
                   None |> P.resolve
               | None ->
                 Js.log3 "Invalid JSON file found. Ignoring..." manifest
                   manifestFile;
                 None |> P.resolve)
      | ".opam" ->
        Fs.readFile manifestFile
        |> P.then_ (function
             | "" -> None |> P.resolve
             | _ -> Some (PackageManager.makeOpam projectRoot) |> P.resolve)
      | _ -> None |> P.resolve )

  let lookup projectRoot =
    Fs.readDir (Fpath.toString projectRoot)
    |> P.then_ (function
         | Error msg -> P.resolve (Error msg)
         | Ok files ->
           files
           |> Js.Array.map (parse projectRoot)
           |> P.all
           |> P.then_ (fun l ->
                  Ok
                    ( Js.Array.reduce
                        (fun acc x ->
                          Js.Array.concat acc
                            ( match x with
                            | Some x -> Array.of_list [ x ]
                            | None -> [||] ))
                        [||] l
                    |> Array.to_list )
                  |> P.resolve))
end

type t =
  { spec : PackageManager.t
  ; projectRoot : Fpath.t
  }

let packageManagerSetOfPackageManagerList ~debugMsg lst =
  Js.Console.info debugMsg;
  match lst with
  | Ok lst ->
    List.iter (fun x -> x |> PackageManager.toString |> Js.Console.info) lst;
    lst |> PackageManager.Set.of_list
  | Error msg ->
    Js.Console.error2
      (Printf.sprintf "Error during extracting %s", debugMsg)
      msg;
    PackageManager.Set.empty

let packageManagerOfMultipleChoices ~env ~projectRoot multipleChoices =
  let config = Vscode.Workspace.getConfiguration "ocaml" in
  match
    ( config |. Vscode.WorkspaceConfiguration.get "packageManager"
    , config |. Vscode.WorkspaceConfiguration.get "toolChainRoot" )
  with
  | Some name, Some root ->
    PackageManager.specOfName ~env ~name ~root:(Fpath.ofString root)
  | Some name, None -> PackageManager.specOfName ~env ~name ~root:projectRoot
  | _ ->
    Window.showQuickPick
      ( multipleChoices
      |> List.map (fun pm -> PackageManager.toName pm)
      |> Array.of_list )
      (Window.QuickPickOptions.make ~canPickMany:false
         ~placeHolder:
           "Which package manager would you like to manage the toolchain?" ())
    |> P.then_ (function
         | None -> P.resolve (Error "showQuickPick() returned undefined")
         | Some packageManager ->
           let open Vscode.WorkspaceConfiguration in
           update config "packageManager" packageManager
             (configurationTargetToJs Workspace)
           (* Workspace *)
           |> P.then_ (fun _ ->
                  match PackageManager.find packageManager multipleChoices with
                  | Some pm -> PackageManager.toSpec ~env ~kind:pm
                  | None ->
                    P.resolve
                      (Error
                         "Weird invalid state: selected choice was not found \
                          in the list")))

let init ~env ~folder =
  let projectRoot = Fpath.ofString folder in
  P.all2
    ( PackageManager.available ~root:projectRoot ~env
    , Manifest.lookup projectRoot
      |> okThen (fun pms ->
             if pms = [] then
               Error "TODO: global toolchain"
             else
               Ok pms) )
  |> P.then_ (fun (availablePackageManagers, alreadyUsedPackageManagers) ->
         let availablePackageManagers =
           packageManagerSetOfPackageManagerList
             ~debugMsg:"available package managers" availablePackageManagers
         in
         let alreadyUsedPackageManagers =
           packageManagerSetOfPackageManagerList
             ~debugMsg:"possibly used package managers"
             alreadyUsedPackageManagers
         in
         match
           PackageManager.Set.inter availablePackageManagers
             alreadyUsedPackageManagers
           |> PackageManager.Set.elements
         with
         | [] -> (
           Js.Console.info "Will lookup toolchain from global env";
           match PackageManager.ofName projectRoot "<global>" with
           | Ok kind -> PackageManager.toSpec ~env ~kind
           | Error msg -> Error msg |> P.resolve )
         | [ obviousChoice ] ->
           Js.Console.info2 "Toolchain detected"
             (PackageManager.toString obviousChoice);
           PackageManager.toSpec ~env ~kind:obviousChoice
         | multipleChoices ->
           packageManagerOfMultipleChoices ~env ~projectRoot multipleChoices)
  |> okThen (fun spec -> Ok { spec; projectRoot })

let setupWithResources { spec; projectRoot } =
  PackageManager.setupToolChain projectRoot spec
  |> P.then_ (function
       | Error msg -> Error msg |> P.resolve
       | Ok () -> PackageManager.env spec)
  |> P.then_ (function
       | Error e -> e |> R.fail |> P.resolve
       | Ok env -> Cmd.make ~cmd:"ocamllsp" ~env)
  |> P.then_ (fun r ->
         let r =
           match r with
           | Ok _ -> Ok ({ spec; projectRoot } : t)
           | Error msg -> Error {j| Toolchain initialisation failed: $msg |j}
         in
         P.resolve r)

let setup ~env ~folder =
  init ~env ~folder
  |> P.then_ (function
       | Error msg -> P.resolve (Error msg)
       | Ok toolchain -> setupWithResources toolchain)

let lsp (t : t) = PackageManager.lsp t.spec

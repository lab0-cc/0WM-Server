open Lwt.Syntax
module Store = Irmin_git_unix.FS.KV (Irmin.Contents.Json_value)
module Proj = Irmin.Json_tree (Store)

let env_path env path =
  List.find_map Sys.getenv_opt env
  |> Option.fold ~none:path ~some:(fun dir -> Filename.concat dir path)
let var = env_path ["ZWM_VAR"; "STATE_DIRECTORY"]
let static = env_path ["ZWM_SHARE"]
let config = var "database" |> Irmin_git.config ~bare:true
let socket = env_path ["ZWM_RUN"; "RUNTIME_DIRECTORY"] "0wmd.sock"
let store : Store.Repo.t option ref = ref None
let rtree : Zwmlib.Rtree.t option ref = ref None

let rtree_mutex = Mutex.create ()

let rtree_push id =
  Mutex.lock rtree_mutex;
  let* () = match !rtree with
    | None -> Lwt.return (rtree := Some (Zwmlib.Rtree.singleton id))
    | Some r ->
        let* r = Zwmlib.Rtree.add id r store in
        Lwt.return (rtree := Some r) in
  Mutex.unlock rtree_mutex |> Lwt.return

let rtree_reinsert id =
  Mutex.lock rtree_mutex;
  let* () = match%lwt match !rtree with
      | None -> Lwt.return None
      | Some r -> Zwmlib.Rtree.remove id r store with
    | None -> Lwt.return (rtree := Some (Zwmlib.Rtree.singleton id))
    | Some r ->
        let* r = Zwmlib.Rtree.add id r store in
        Lwt.return (rtree := Some r) in
  Mutex.unlock rtree_mutex |> Lwt.return

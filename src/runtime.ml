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

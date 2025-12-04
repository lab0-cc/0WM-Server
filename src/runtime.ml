module Store = Irmin_git_unix.FS.KV (Irmin.Contents.Json_value)
module Proj = Irmin.Json_tree (Store)

let config =
  Sys.getenv_opt "ZWM_DATABASE" |> Option.value ~default:"storage" |> Irmin_git.config ~bare:true
let store : Store.Repo.t option ref = ref None
let rtree : Zwmlib.Rtree.t option ref = ref None

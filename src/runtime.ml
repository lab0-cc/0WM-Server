module Store = Irmin_git_unix.FS.KV (Irmin.Contents.Json_value)
module Proj = Irmin.Json_tree (Store)

let config = Irmin_git.config ~bare:true "storage"
let store : Store.Repo.t option ref = ref None
let rtree : Zwmlib.Rtree.t option ref = ref None

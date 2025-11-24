module Store : module type of Irmin_git_unix.FS.KV (Irmin.Contents.Json_value)
module Proj : module type of Irmin.Json_tree (Store)
val config : Irmin.config
val store : Store.Repo.t option ref
val rtree : Zwmlib.Rtree.t option ref

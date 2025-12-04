(** This module provides access to all the runtime objects *)

module Store : module type of Irmin_git_unix.FS.KV (Irmin.Contents.Json_value)
module Proj : module type of Irmin.Json_tree (Store)

(** Irmin configuration *)
val config : Irmin.config

(** Monitor socket path *)
val socket : string

(** Irmin repository *)
val store : Store.Repo.t option ref

(** Dynamic R-tree *)
val rtree : Zwmlib.Rtree.t option ref

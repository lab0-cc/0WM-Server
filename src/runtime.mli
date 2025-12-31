(** This module provides access to all the runtime objects *)

module Store : module type of Irmin_git_unix.FS.KV (Irmin.Contents.Json_value)
module Proj : module type of Irmin.Json_tree (Store)

(** Build a path in the dynamic data directory *)
val var : string -> string

(** Build a path in the static data directory *)
val static : string -> string

(** Irmin configuration *)
val config : Irmin.config

(** Monitor socket path *)
val socket : string

(** Irmin repository *)
val store : Store.Repo.t option ref

(** Dynamic R-tree *)
val rtree : Zwmlib.Rtree.t option ref

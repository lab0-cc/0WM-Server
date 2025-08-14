(** This module implements R-trees *)

(** The type of R-trees *)
type t [@@marshal]

(** Create a singleton R-tree *)
val singleton : string -> t

(** Create a geo object from an R-tree *)
val geo_of : Object_store.t -> t -> Geo.obj

(** Sort an R-tree relative to a point *)
val sort : ?limit:int -> Geo.point -> t -> Object_store.t -> (float * string) list

(** Add an object to an R-tree *)
val add : string -> t -> Object_store.t -> t

(** Convert an R-tree to a list *)
val to_list : t -> string list

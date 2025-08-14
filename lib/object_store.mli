(** This module implements 0WM’s object store *)

(** The type of stored objects *)
type obj = { zmin: float; zmax: float; path: string; anchors: Anchor.set; height: float;
             width: float; shape: Geo.obj } [@@marshal]

(** The type of object stores *)
type t = (string, obj) Hashtbl.t

(** Find an object in a store *)
val find : string -> t -> obj

(** Create an empty store *)
val empty : t

(** Add an object to a store *)
val push : string -> obj -> t -> unit

(** This module implements functions used by the API *)

(** Get a map by its ID *)
val get_map : string -> Dream.response Lwt.t

(** Delete a map by its ID *)
val delete_map : string -> Dream.response Lwt.t

(** Get a raw heatmap and its bounding box by its ID *)
val get_heatmap_s : ssids:string list -> string -> (string * Zwmlib.Linalg.Box2.t) Lwt.t

(** Get a SVG heatmap by its ID *)
val get_heatmap : ssids:string list -> string -> Dream.response Lwt.t

(** Get a list of maps around the given position with the given accuracy *)
val get_maps : ?latitude:float -> ?longitude:float -> ?altitude:float -> ?accuracy:float
                               -> ?altitude_accuracy:float -> ?limit:int -> bool -> unit
                               -> Dream.response Dream.promise

(** Add a map to the store *)
val push_map : Types.payload -> Dream.response Lwt.t

(** Add the object with the given ID to the internal R-tree *)
val push_to_rtree : string -> unit Lwt.t

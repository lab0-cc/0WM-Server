val get_map : string -> Dream.response Lwt.t
val get_heatmap_s : ssids:string list -> string -> (string * Zwmlib.Linalg.Box2.t) Lwt.t
val get_heatmap : ssids:string list -> string -> Dream.response Lwt.t
val get_maps : ?latitude:float -> ?longitude:float -> ?altitude:float -> ?accuracy:float
                               -> ?altitude_accuracy:float -> ?limit:int -> bool -> unit
                               -> Dream.response Dream.promise
val update_rtree : string -> unit Lwt.t
val push_map : Types.payload -> Dream.response Lwt.t

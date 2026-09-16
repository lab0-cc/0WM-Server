(** This module handles the [/heatmaps] API endpoint *)

(** Get a raw heatmap and its bounding box by its ID *)
val get_heatmap_s : ssids:string list -> string -> (string * Zwmlib.Linalg.Box2.t) Lwt.t

(** The API endpoints *)
val endpoints : Dream.route list

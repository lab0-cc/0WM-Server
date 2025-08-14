(** This module implements floorplan anchors *)

(** The type of anchors *)
type t [@@marshal]

(** Convert an anchor to a point pair *)
val to_points : t -> Linalg.point * Linalg.point

(** The type of georef anchor sets *)
type set = t * t * t [@@marshal]

(** This module implements geolocation confidence levels *)

(** The type of geolocation confidence *)
type t = Invalid | Valid2D | Valid3D [@@marshal]

(** Comparison function used for sorting *)
val compare : t -> t -> int

(** This module implements primitives on colors *)

(** The type of RGB colors *)
type t = { r : int; g : int; b : int; a : int }

(** Black color *)
val black : t

(** White color *)
val white : t

(** Convert an RGB color to its hex value *)
val to_hex : t -> string

(** Linear interpolation between two colors *)
val lerp : t -> t -> float -> t

(** Convert a scalar in [0;1] to a color *)
val of_scalar : float -> t

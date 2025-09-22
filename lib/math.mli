(** This module implements maths primitives *)

(** Clamp a value within two bounds *)
val clamp : min:'a -> max:'a -> 'a -> 'a

(** Linear interpolation between two values *)
val lerp : float -> float -> float -> float

(** Normalize a value within two bounds *)
val normalize : min:float -> max:float -> float -> float

(** Float list median *)
val median : float list -> float

(** Matérn-3/2 covariance *)
val matern32 : float -> float

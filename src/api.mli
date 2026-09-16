(** This module implements functions used by the API *)

(** Get a float URL parameter *)
val get_float : Dream.request -> string -> float option

(** Get an int URL parameter *)
val get_int : Dream.request -> string -> int option

(** Get a bool URL parameter *)
val get_toggle : Dream.request -> string -> bool

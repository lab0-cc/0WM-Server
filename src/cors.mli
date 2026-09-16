(** This module handles the CORS preflight endpoints *)

(** The preflight endpoint *)
val endpoint : ?methods:Dream.method_ list -> string -> Dream.route

(** This module provides helper functions to the 0WM server *)

(** Bad parameted provided to the API *)
exception Bad_parameter of string

(** Write a CSV response *)
val csv : string -> Dream.response Lwt.t

(** Write a JSON response *)
val json : string -> Dream.response Lwt.t

(** Write a TOML response *)
val toml : string -> Dream.response Lwt.t

(** Write a YAML response *)
val yaml : string -> Dream.response Lwt.t

(** Write an HTML response *)
val html : string -> Dream.response Lwt.t

(** Write an SVG response *)
val svg : string -> Dream.response Lwt.t

(** Write an empty response *)
val ok : unit -> Dream.response Lwt.t

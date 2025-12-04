(** This module implements the formatting logic for the 0WM CLI *)

(** The type of formats *)
type fmt = Csv | Json | Toml | Yaml

(** Get the default file extension from a format *)
val ext_of_fmt : fmt -> string

(** Format a string from an API object *)
val of_api : fmt -> v:string -> 'a Gendarme.ty -> string

(** Format an API object from a string *)
val to_api : fmt -> v:string -> 'a Gendarme.ty -> string

(** This module implements image storage primitives *)

(** Exception raised when an image format is not recognized *)
exception Unrecognized_format

(** The type of base64-encoded images *)
type t

(** Load a base64-encoded image *)
val of_base64 : string -> t

(** Save an image and its thumbnail *)
val save : path:string -> string -> t -> string

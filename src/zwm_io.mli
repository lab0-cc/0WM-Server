(** This module implements the I/O logic for the 0WM CLI *)

(** 0WM server error *)
exception Server_error

(** Get an API object *)
val get : string -> string Lwt.t

(** Post an API object *)
val post : string -> string -> unit Lwt.t

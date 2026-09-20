(** This module implements the I/O logic for the 0WM CLI *)

(** 0WM server error *)
exception Server_error

(** Get an API object *)
val get : string -> string Lwt.t

(** Patch an API object *)
val patch : string -> string -> unit Lwt.t

(** Put an API object *)
val put : string -> string -> unit Lwt.t

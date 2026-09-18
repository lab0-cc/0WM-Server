(** This module implements the 0WM server monitor *)

(** The monitor server *)
val server : error_handler:Dream.error_handler -> stop:unit Lwt.t -> unit Lwt.t

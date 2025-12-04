(** This module implements the 0WM server monitor *)

(** The monitor server *)
val server : error_handler:Dream.error_handler -> unit Lwt.t

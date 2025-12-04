(** This module implements the 0WM web server *)

(** The web server *)
val server : error_handler:Dream.error_handler -> Runtime.Store.repo -> unit Lwt.t

(** The server exit signal *)
val signal : unit Lwt.u option ref

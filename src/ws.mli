(** This module implements the WebSocket dance for the API *)

(** The type of session contexts *)
type context

(** The dance entrypoint *)
val live : ?context:context -> Dream.websocket -> unit Lwt.t

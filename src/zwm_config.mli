(** This module implements the [config] command *)

(** The command *)
val cmd : int Lwt.t Cmdliner.Cmd.t

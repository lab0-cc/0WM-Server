(** This module handles the [/debug] API endpoint *)

(** The API endpoints *)
val endpoints : Runtime.Store.Repo.t -> Dream.route list

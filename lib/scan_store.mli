(** This module implements 0WM’s scan store *)

(** The type of stored scans *)
type scan = { position: Linalg.point3; timestamp: int;
              measurements: Dot11.measurement list } [@@marshal]

(** The type of scan stores *)
type t = (string, scan) Hashtbl.t [@@marshal]

(** Create an empty store *)
val empty : t

(** Add a scan to a store *)
val push : string -> scan -> t -> unit

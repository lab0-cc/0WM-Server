(** This module implements 0WM’s scan store *)

(** The type of stored scans *)
type scan = { position: Linalg.point3; timestamp: int;
              measurements: Dot11.measurement list } [@@marshal]

(** The type of scan metadata *)
type metadata = { map: string; transform: Linalg.Matrix3.t } [@@marshal]

(** The type of scan stores *)
type t [@@marshal]

(** Create an empty store *)
val empty : t

(** Add a scan to a scan set *)
val push : string -> scan -> t -> unit

(** Get a scan set *)
val find : string -> t -> metadata option * scan list

(** Add metadata to a scan set *)
val meta : string -> metadata -> t -> unit

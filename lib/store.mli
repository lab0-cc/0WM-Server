(** This module implements 0WM’s datastore *)

module Backend : module type of Irmin_git_unix.FS.KV (Irmin.Contents.Json_value)

(** The type of stored objects *)
type obj = {
  zmin : float;
  zmax : float;
  path : string;
  anchors : Anchor.set;
  height : float;
  width : float;
  structure : Geo.obj;
  walls : Linalg.Segment2.t list;
  name : string;
} [@@marshal]

(** The type of individual scans in scan sessions *)
type s_data = { position : Linalg.point3; measurements : Dot11.measurement list } [@@marshal]

(** The type of scan session metadata *)
type s_meta = { map : string; transform : Linalg.Matrix3.t } [@@marshal]

(** The type of scan sessions *)
type scan = { meta : s_meta option; data : (int * s_data) list } [@@marshal]

(** The type of stores *)
type t = Backend.repo option ref

(** Find an object in the store *)
val find_object : string -> t -> obj Lwt.t

(** Find a JSON object in the store *)
val find_object_j : string -> t -> string Lwt.t

(** Add an object to the store *)
val push_object : string -> obj -> t -> unit Lwt.t

(** Remove an object from the store *)
val remove_object : string -> t -> unit Lwt.t

(** Find a scan set in the store *)
val find_scan : string -> t -> scan Lwt.t

(** Begin a scan session *)
val begin_scan : string -> t -> Backend.t Lwt.t

(** Push a scan to a scan session *)
val push_scan : string -> int -> s_data -> Backend.t -> unit Lwt.t

(** Set metadata to a scan session *)
val set_meta : string -> s_meta -> Backend.t -> unit Lwt.t

(** Cleanup an open scan session *)
val cleanup_scan : ?recovery:bool -> string -> t -> unit Lwt.t

(** End a scan session *)
val end_scan : string -> t -> unit Lwt.t

(** Get the global configuration in the store *)
val get_conf : t -> Types.config Lwt.t

(** Set the global configuration in the store *)
val set_conf : Types.config -> t -> unit Lwt.t

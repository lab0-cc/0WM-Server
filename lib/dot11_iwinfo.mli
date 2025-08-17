(** This module processes the 802.11 fields useful to 0WM through iwinfo *)

(** The type of AP modes *)
type mode = Unknown | Master | Ad_hoc | Client | Monitor | Master_VLAN | WDS | Mesh_point
          | P2P_client | P2P_go

(** The type of iwinfo HT operation data *)
type ht_oper = { primary_channel: int; secondary_channel_offset: string; channel_width: int }

(** The type of iwinfo VHT operation data *)
type vht_oper = { channel_width: int; center_freq_1: int; center_freq_2: int }

(** The type of iwinfo encryption reporting *)
type encryption = { enabled: bool; wep: string list option; wpa: int list option;
                    authentication: string list; ciphers: string list }

(** The type of iwinfo data *)
type t = { ssid: string option; bssid: string; mode: mode; band: int; channel: int; mhz: int;
           signal: int; quality: int; quality_max: int; ht_operation: ht_oper option;
           vht_operation: vht_oper option; encryption: encryption } [@@marshal]

(** Convert an iwinfo measurement to our common 802.11 format *)
val measurement : t -> Dot11.measurement

(** This module processes the 802.11 fields useful to 0WM *)

(** The type of 802.11n channel widths *)
type ht_width = HT_20 | HT_any

(** The type of 802.11n secondary channel offsets *)
type ht_off = HT_SCN | HT_SCA | HT_SCX | HT_SCB

(** The type of HT operation elements *)
type ht_op = { ht_width: ht_width; ht_chan: int; ht_off: ht_off }

(** The type of 802.11ac channel widths *)
type vht_width = VHT_20_40 | VHT_80_160_8080 | VHT_160_deprecated | VHT_8080_deprecated
               | VHT_reserved of int

(** The type of VHT operation elements *)
type vht_op = { vht_width: vht_width; vht_chan: int; vht_sec: int }

(** The type of 802.11ax channel widths *)
type he_width = HE_20 | HE_40 | HE_80 | HE_160_8080

(** The type of HE operation elements *)
type he_op = { he_width: he_width; he_chan: int; he_sec: int }

(** The type of 802.11be channel widths *)
type eht_width = EHT_20 | EHT_40 | EHT_80 | EHT_160 | EHT_320 | EHT_reserved of int

(** The type of EHT operation elements *)
type eht_op = { eht_width: eht_width; eht_chan: int; eht_sec: int }

(** The type of 802.11n/be 2.4 GHz operation fields. Before 802.11n, HT operation fields didn’t
    exist, so they are optional. HE is ignored here as its operation element only targets 6 GHz
    APs. *)
type f_2_4_ops = { f_2_4_ht: ht_op option; f_2_4_eht: eht_op option }

(** The type of 802.11n/ac/be 5 GHz operation fields. 5GHz exists since 802.11a, but before 802.11n,
    HT and VHT operation fields didn’t exist, so they are optional. HE is ignored here as its
    operation element only targets 6 GHz APs. *)
type f_5_ops = { f_5_ht: ht_op option; f_5_vht: vht_op option; f_5_eht: eht_op option }

(** The type of 802.11ax/be 6 GHz operation fields *)
type f_6_ops = { f_6_he: he_op; f_6_eht: eht_op option }

(** The type of 802.11n/ac/ax/be operation fields *)
type ops = Ops_2_4 of f_2_4_ops | Ops_5 of f_5_ops | Ops_6 of f_6_ops

(** Return the bandwidth from operation fields *)
val width_of_dot11_ops_opt : ops -> int option

(** Return the channel occupation ranges from operation fields *)
val ranges_of_dot11_ops_opt : ops -> (int * int) list option

(** The type of 802.11 channels *)
type channel = Chan_2_4 of int | Chan_5 of int | Chan_6 of int

(** Return the integer band (in GHz) from a channel *)
val band_of_channel : channel -> int

(** The type of WEP kinds *)
type wep_kind = Open | Shared

(** The type of authentication protocols *)
type protocol = WEP of wep_kind | WPA of int

(** The type of authentications *)
type auth = Dot1X | OWE | PSK | SAE

(** The type of ciphers *)
type cipher = AES_OCB | CCMP | CCMP_256 | CKIP | GCMP | GCMP_256 | TKIP | WEP_104 | WEP_40 | WRAP

(** The type of AP encryption configurations *)
type encryption = { protocols: protocol list; auth: auth option list; ciphers: cipher option list }

(** The type of APs *)
type ap = { ssid: string option; bssid: string; channel: channel; ops: ops;
            encryption: encryption option }

(** The type of AP measurements *)
type measurement = { ap: ap; signal: int } [@@marshal]

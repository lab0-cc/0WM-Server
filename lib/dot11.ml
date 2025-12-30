[%%marshal.load Json]

type ht_width = HT_20 | HT_any [@@marshal]
type ht_off = HT_SCN | HT_SCA | HT_SCX | HT_SCB [@@marshal]
type ht_op = {
  ht_width : ht_width [@json] [@default HT_any];
  ht_chan : int [@json];
  ht_off : ht_off [@json] [@default HT_SCB];
} [@@marshal]

exception Reserved of int

let width_of_ht_op = function
  | { ht_width = HT_20; _ } -> 20
  | _ -> 40 (* max value, can be incorrect *)

let range_of_ht_op = function
  | { ht_width = HT_20; ht_chan; _ } -> (ht_chan, ht_chan)
  | { ht_width = HT_any; ht_chan; ht_off = HT_SCN } -> (ht_chan, ht_chan)
  | { ht_width = HT_any; ht_chan; ht_off = HT_SCA } -> (ht_chan, ht_chan + 4)
  | { ht_width = HT_any; ht_off = HT_SCX; _ } -> raise (Reserved 2)
  | { ht_width = HT_any; ht_chan; ht_off = HT_SCB } -> (ht_chan - 4, ht_chan)

let ranges_of_ht_op op = [range_of_ht_op op]

type vht_width =
  | VHT_20_40
  | VHT_80_160_8080
  | VHT_160_deprecated
  | VHT_8080_deprecated
  | VHT_reserved of int
[@@marshal]

type vht_op = {
  vht_width : vht_width [@json] [@default VHT_80_160_8080];
  vht_chan : int [@json];
  vht_sec : int [@json];
} [@@marshal]

let width_of_vht_op = function
  | { vht_width = VHT_20_40; vht_chan; _ } when vht_chan mod 4 >= 2 -> 20
  | { vht_width = VHT_20_40; _ } -> 40
  | { vht_width = VHT_80_160_8080; vht_sec = 0; _ } -> 80
  | { vht_width = VHT_reserved i; _ } -> raise (Reserved i)
  | _ -> 160 (* max value, can be incorrect *)

let ranges_of_vht_op = function
  | { vht_width = VHT_20_40; vht_chan; _ } when vht_chan mod 4 >= 2 -> [(vht_chan, vht_chan)]
  | { vht_width = VHT_20_40; vht_chan; _ } -> [(vht_chan - 2, vht_chan + 2)]
  | { vht_width = VHT_80_160_8080; vht_chan; vht_sec = 0 } -> [(vht_chan - 6, vht_chan + 6)]
  | { vht_width = VHT_80_160_8080; vht_chan; vht_sec } when vht_sec - vht_chan = 8 ->
      [(vht_sec - 14, vht_sec + 14)]
  | { vht_width = VHT_80_160_8080 | VHT_8080_deprecated; vht_chan; vht_sec } ->
      [(vht_chan - 6, vht_chan + 6); (vht_sec - 6, vht_sec + 6)]
  | { vht_width = VHT_160_deprecated; vht_chan; _ } -> [(vht_chan - 14, vht_chan + 14)]
  | { vht_width = VHT_reserved i; _ } -> raise (Reserved i)

type he_width = HE_20 | HE_40 | HE_80 | HE_160_8080 [@@marshal]
type he_op = {
  he_width : he_width [@json] [@default HE_80];
  he_chan : int [@json];
  he_sec : int [@json];
} [@@marshal]

let width_of_he_op = function
  | { he_width = HE_20; _ } -> 20
  | { he_width = HE_40; _ } -> 40
  | { he_width = HE_80; _ } -> 80
  | { he_width = HE_160_8080; _ } -> 160 (* max value, can be incorrect *)

let ranges_of_he_op = function
  | { he_width = HE_20; he_chan; _ } -> [(he_chan, he_chan)]
  | { he_width = HE_40; he_chan; _ } -> [(he_chan - 2, he_chan + 2)]
  | { he_width = HE_80; he_chan; _ } -> [(he_chan - 6, he_chan + 6)]
  | { he_width = HE_160_8080; he_chan; he_sec } when he_sec - he_chan = 8 ->
      [(he_sec - 14, he_sec + 14)]
  | { he_chan; he_sec; _ } -> [(he_chan - 6, he_chan + 6); (he_sec - 6, he_sec + 6)]

type eht_width = EHT_20 | EHT_40 | EHT_80 | EHT_160 | EHT_320 | EHT_reserved of int [@@marshal]
type eht_op = {
  eht_width : eht_width [@json] [@default EHT_80];
  eht_chan : int [@json];
  eht_sec : int [@json];
} [@@marshal]

let width_of_eht_op = function
  | { eht_width = EHT_20; _ } -> 20
  | { eht_width = EHT_40; _ } -> 40
  | { eht_width = EHT_80; _ } -> 80
  | { eht_width = EHT_160; _ } -> 160
  | { eht_width = EHT_320; _ } -> 320
  | { eht_width = EHT_reserved i; _ } -> raise (Reserved i)

let range_of_eht_op = function
  | { eht_width = EHT_20; eht_chan; _ } -> (eht_chan, eht_chan)
  | { eht_width = EHT_40; eht_chan; _ } -> (eht_chan - 2, eht_chan + 2)
  | { eht_width = EHT_80; eht_chan; _ } -> (eht_chan - 6, eht_chan + 6)
  | { eht_width = EHT_160; eht_sec; _ } -> (eht_sec - 14, eht_sec + 14)
  | { eht_width = EHT_320; eht_sec; _ } -> (eht_sec - 30, eht_sec + 30)
  | { eht_width = EHT_reserved i; _ } -> raise (Reserved i)

let ranges_of_eht_op op = [range_of_eht_op op]

type f_2_4_ops = { f_2_4_ht : ht_op option [@json]; f_2_4_eht : eht_op option [@json] } [@@marshal]
type f_5_ops = {
  f_5_ht : ht_op option [@json];
  f_5_vht : vht_op option [@json];
  f_5_eht : eht_op option [@json];
} [@@marshal]

(** For 6 GHz APs, introduced in 802.11ax, the HE operation field is mandatory *)
type f_6_ops = { f_6_he : he_op [@json]; f_6_eht : eht_op option [@json] } [@@marshal]

type ops = Ops_2_4 of f_2_4_ops | Ops_5 of f_5_ops | Ops_6 of f_6_ops [@@marshal]

let width_of_dot11_ops = function
  | Ops_2_4 { f_2_4_eht = Some op; _ } -> width_of_eht_op op
  | Ops_2_4 { f_2_4_ht = Some op; _ } -> width_of_ht_op op
  | Ops_5 { f_5_eht = Some op; _ } -> width_of_eht_op op
  | Ops_5 { f_5_vht = Some op; _ } -> width_of_vht_op op
  | Ops_5 { f_5_ht = Some op; _ } -> width_of_ht_op op
  | Ops_6 { f_6_eht = Some op; _ } -> width_of_eht_op op
  | Ops_6 { f_6_he; _ } -> width_of_he_op f_6_he
  | _ -> 20

let ranges_of_dot11_ops_opt = function
  | Ops_2_4 { f_2_4_eht = Some op; _ } -> Some (ranges_of_eht_op op)
  | Ops_2_4 { f_2_4_ht = Some op; _ } -> Some (ranges_of_ht_op op)
  | Ops_5 { f_5_eht = Some op; _ } -> Some (ranges_of_eht_op op)
  | Ops_5 { f_5_vht = Some op; _ } -> Some (ranges_of_vht_op op)
  | Ops_5 { f_5_ht = Some op; _ } -> Some (ranges_of_ht_op op)
  | Ops_6 { f_6_eht = Some op; _ } -> Some (ranges_of_eht_op op)
  | Ops_6 { f_6_he; _ } -> Some (ranges_of_he_op f_6_he)
  | _ -> None

type channel = Chan_2_4 of int | Chan_5 of int | Chan_6 of int [@@marshal]

let band_of_channel = function
  | Chan_2_4 _ -> 2
  | Chan_5 _ -> 5
  | Chan_6 _ -> 6

type wep_kind = Open | Shared [@@marshal]
type protocol = WEP of wep_kind | WPA of int [@@marshal]
type auth = Dot1X | OWE | PSK | SAE [@@marshal]
type cipher = AES_OCB | CCMP | CCMP_256 | CKIP | GCMP | GCMP_256 | TKIP | WEP_104 | WEP_40 | WRAP
[@@marshal]

type encryption = {
  protocols : protocol list [@json];
  auth : auth option list [@json];
  ciphers : cipher option list [@json];
} [@@marshal]

type ap = {
  ssid : string option [@json];
  bssid : string [@json];
  channel : channel [@json] [@default Chan_2_4 (-1)];
  ops : ops [@json] [@default Ops_2_4 (Gendarme.get f_2_4_ops)];
  encryption : encryption option [@json];
} [@@marshal]

let center_freq { channel; ops; _ } =
  let offset = match ops with
    | Ops_2_4 _ -> 2_407_000
    | Ops_5 _ -> 5_000_000
    | Ops_6 _ -> 5_950_000 in
  match ranges_of_dot11_ops_opt ops with
  | Some ((min, max)::[]) | Some ((min, _)::(_, max)::[]) -> offset + 2500 * (min + max)
  | _ -> offset + 5000 * (function Chan_2_4 i | Chan_5 i | Chan_6 i -> i) channel

type measurement = { ap : ap [@json]; signal : int [@json] } [@@marshal]

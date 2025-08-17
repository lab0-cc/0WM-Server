[%%marshal.load Json]

open Dot11

type mode = Unknown | Master | Ad_hoc | Client | Monitor | Master_VLAN | WDS | Mesh_point
          | P2P_client | P2P_go [@@marshal]

type ht_oper = { primary_channel: int [@json]; secondary_channel_offset: string [@json];
                 channel_width: int [@json] } [@@marshal]

type vht_oper = { channel_width: int [@json]; center_freq_1: int [@json];
                  center_freq_2: int [@json] } [@@marshal]

type encryption = { enabled: bool [@json]; wep: string list option [@json];
                    wpa: int list option [@json]; authentication: string list [@json];
                    ciphers: string list [@json] } [@@marshal]

type t = { ssid: string option [@json]; bssid: string [@json];
           mode: mode [@json] [@default Unknown]; band: int [@json]; channel: int [@json];
           mhz: int [@json]; signal: int [@json]; quality: int [@json]; quality_max: int [@json];
           ht_operation: ht_oper option [@json]; vht_operation: vht_oper option [@json];
           encryption: encryption [@json] } [@@marshal]

let ht_op { primary_channel; secondary_channel_offset; channel_width } =
  let ht_width = match channel_width with
    | 20 -> HT_20
    | _ -> HT_any in
  let ht_off = match secondary_channel_offset with
    | "no secondary" -> HT_SCN
    | "above" -> HT_SCA
    | "below" -> HT_SCB
    | _ -> HT_SCX in
  { ht_width; ht_chan = primary_channel; ht_off }

let vht_op { channel_width; center_freq_1; center_freq_2 } =
  let vht_width = match channel_width with
    | 80 -> VHT_80_160_8080
    | 160 -> VHT_160_deprecated
    | 8080 -> VHT_8080_deprecated
    | _ -> VHT_20_40 in
  { vht_width; vht_chan = center_freq_1; vht_sec = center_freq_2 }

let ops { band; ht_operation; vht_operation; _ } =
  let ht = Option.map ht_op ht_operation in
  let vht = Option.map vht_op vht_operation in
  match band with
    | 2 -> Ops_2_4 { f_2_4_ht = ht; f_2_4_eht = None }
    | _ -> Ops_5 { f_5_ht = ht; f_5_vht = vht; f_5_eht = None }

let channel { band; channel; _ } = match band with
  | 2 -> Chan_2_4 channel
  | _ -> Chan_5 channel

let parse_wep = function
  | "open" -> WEP Open
  | _ -> WEP Shared

let protocols { wep; wpa; _ } = match wep with
  | Some l -> List.map parse_wep l
  | None -> Option.get wpa |> List.map (fun i -> WPA i)

let auth = function
  | "802.1x" -> Some Dot1X
  | "owe" -> Some OWE
  | "psk" -> Some PSK
  | "sae" -> Some SAE
  | _ -> None

let cipher = function
  | "aes-ocb" -> Some AES_OCB
  | "ccmp" -> Some CCMP
  | "ccmp-256" -> Some CCMP_256
  | "ckip" -> Some CKIP
  | "gcmp" -> Some GCMP
  | "gcmp-256" -> Some GCMP_256
  | "tkip" -> Some TKIP
  | "wep-104" -> Some WEP_104
  | "wep-40" -> Some WEP_40
  | "wrap" -> Some WRAP
  | _ -> None

let encr ({ enabled; authentication; ciphers; _ } as encr) = match enabled with
  | false -> None
  | true ->
      let protocols = protocols encr in
      let auth = List.map auth authentication in
      let ciphers = List.map cipher ciphers in
      Some { protocols; auth; ciphers }

let ap ({ ssid; bssid; encryption; _ } as info) = 
  let channel = channel info in
  let ops = ops info in
  let encryption = encr encryption in
  { ssid; bssid; channel; ops; encryption }

let measurement ({ signal; _ } as info) = { ap = ap info; signal }

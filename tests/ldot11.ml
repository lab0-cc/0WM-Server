(** This module tests {!Zwmlib.Dot11} *)

open Alcotest
open Alcotest_lwt
open Zwmlib.Dot11

let ops = Ops_2_4 { f_2_4_ht = Some { ht_width = HT_any; ht_chan = 5; ht_off = HT_SCA };
                    f_2_4_eht = None }
let ops' = Ops_5 { f_5_ht = Some { ht_width = HT_any; ht_chan = 52; ht_off = HT_SCA };
                   f_5_vht = Some { vht_width = VHT_80_160_8080; vht_chan = 58; vht_sec = 0 };
                   f_5_eht = None }
let ops'' = Ops_6 { f_6_he = { he_width = HE_160_8080; he_chan = 39; he_sec = 47 };
                    f_6_eht = Some { eht_width = EHT_160; eht_chan = 39; eht_sec = 47 } }

let ap = { ssid = None; bssid = ""; channel = Chan_2_4 5; ops; encryption = None }
let ap' = { ssid = Some "foo"; bssid = "bar"; channel = Chan_5 52; ops = ops'; encryption = None }
let ap'' = { ssid = None; bssid = ""; channel = Chan_6 39; ops = ops''; encryption = None }

(** Test for {!Zwmlib.Dot11.width_of_dot11_ops} *)
let test_width_of_dot11_ops () =
  width_of_dot11_ops ops |> check int "2.4" 40;
  width_of_dot11_ops ops' |> check int "5" 80;
  width_of_dot11_ops ops'' |> check int "6" 160

(** Test for {!Zwmlib.Dot11.ranges_of_dot11_ops_opt} *)
let test_ranges_of_dot11_ops_opt () =
  check bool "2.4" true (ranges_of_dot11_ops_opt ops = Some [(5, 9)]);
  check bool "5" true (ranges_of_dot11_ops_opt ops' = Some [(52, 64)]);
  check bool "6" true (ranges_of_dot11_ops_opt ops'' = Some [(33, 61)])

(** Test for {!Zwmlib.Dot11.band_of_channel} *)
let test_band_of_channel () =
  band_of_channel ap.channel |> check int "2.4" 2;
  band_of_channel ap'.channel |> check int "5" 5;
  band_of_channel ap''.channel |> check int "6" 6

(** Test for {!Zwmlib.Dot11.center_freq} *)
let test_center_freq () =
  center_freq ap |> check int "2.4" 2442000;
  center_freq ap' |> check int "5" 5290000;
  center_freq ap'' |> check int "6" 6185000

let suite = [
  test_case_sync "test_width_of_dot11_ops" `Quick test_width_of_dot11_ops;
  test_case_sync "test_ranges_of_dot11_ops_opt" `Quick test_ranges_of_dot11_ops_opt;
  test_case_sync "test_band_of_channel" `Quick test_band_of_channel;
  test_case_sync "test_center_freq" `Quick test_center_freq;
]

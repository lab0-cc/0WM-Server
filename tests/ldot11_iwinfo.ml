(** This module tests {!Zwmlib.Dot11_iwinfo} *)

open Alcotest
open Alcotest_lwt
open Zwmlib.Dot11_iwinfo

let ht_op = { primary_channel = 5; secondary_channel_offset = "above"; channel_width = 2040 }
let ht_op' = { primary_channel = 52; secondary_channel_offset = "above"; channel_width = 2040 }
let vht_op = { channel_width = 80; center_freq_1 = 58; center_freq_2 = 0 }

let encryption = { enabled = false; wep = None; wpa = None; authentication = []; ciphers = [] }

(** The type of iwinfo VHT operation data *)
type vht_oper = { channel_width : int; center_freq_1 : int; center_freq_2 : int }

let m = { ssid = None; bssid = ""; mode = Master; band = 2; channel = 5; mhz = 2432; signal = -60;
          quality = 50; quality_max = 70; ht_operation = Some ht_op; vht_operation = None;
          encryption }
let m' = { ssid = Some "foo"; bssid = "bar"; mode = Master; band = 5; channel = 52; mhz = 5260;
           signal = -50; quality = 60; quality_max = 70; ht_operation = Some ht_op';
           vht_operation = Some vht_op; encryption }

(** Test for {!Zwmlib.Dot11_iwinfo.measurement} *)
let test_measurement () =
  let mm = measurement m in
  let mm' = measurement m' in
  check int "2.4 signal" (-60) mm.signal;
  check bool "2.4 ap" true (Ldot11.ap = mm.ap);
  check int "5 signal" (-50) mm'.signal;
  check bool "5 ap" true (Ldot11.ap' = mm'.ap)

let suite = [test_case_sync "test_measurement" `Quick test_measurement]

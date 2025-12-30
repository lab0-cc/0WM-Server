(** This module tests {!Zwmlib.Power} *)

open Alcotest
open Alcotest_lwt
open Zwmlib.Power

(** Test for {!Zwmlib.Power.mW} *)
let test_mW () =
  mW 0. |> check Util.f8 "0" 1.;
  mW 1. |> check Util.f3 "1" 1.259;
  mW 2. |> check Util.f3 "2" 1.585;
  mW 10. |> check Util.f8 "10" 10.

(** Test for {!Zwmlib.Power.dBm} *)
let test_dBm () =
  dBm 1. |> check Util.f8 "0" 0.;
  dBm 1.259 |> check Util.f3 "1" 1.;
  dBm 1.585 |> check Util.f3 "2" 2.;
  dBm 10. |> check Util.f8 "10" 10.

let suite = [
  test_case_sync "test_mW" `Quick test_mW;
  test_case_sync "test_dBm" `Quick test_dBm;
]

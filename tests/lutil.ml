(** This module tests {!Zwmlib.Util} *)

open Alcotest
open Alcotest_lwt
open Zwmlib.Util

(** Test for {!Zwmlib.Util.fold_range} *)
let test_fold_range () =
  fold_range (fun acc i -> i::acc) [] 5 8 |> check (list int) "int list" [8; 7; 6; 5];
  fold_range (fun acc i -> acc ^ Char.(chr i |> escaped)) "" 65 68 |> check string "string" "ABCD"

(** Test for {!Zwmlib.Util.quickselect} *)
let test_quickselect () =
  let l = [1; 5; 3; 4; 2; 5; 0] in
  let e = Invalid_argument "quickselect" in
  check_raises "empty" e (fun () -> quickselect 1 [] |> ignore);
  quickselect 0 l |> check int "1/7" 0;
  quickselect 2 l |> check int "3/7" 2;
  quickselect 6 l |> check int "7/7" 5;
  check_raises "8/7" e (fun () -> quickselect 7 l |> ignore)

let suite = [
  test_case_sync "test_fold_range" `Quick test_fold_range;
  test_case_sync "test_quickselect" `Quick test_quickselect;
]
